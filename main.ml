(* main loop: reads commands on the standard input, and prints results on the standard output *)
 
(** print the "end of text" character and flush the output *)
let print_eot	() =
	print_char '\003';
	flush stdout
 
let rec handle_command command commands = match commands with
	| (c, f) :: _ when c = command -> f()
	| x :: r -> handle_command command r
	| [] -> prerr_endline ("Command not understood: " ^ command);;
 
let cmd_end () =
	print_endline "Exiting...";
	exit 0;;
 
let cmd_print () =
	let str = read_line() in
	print_endline str;;
 
let parse_file filepath =
	let (p_stdout, camlp4_stdout) = Unix.pipe() in
	let _ = Unix.create_process
			"camlp4" [| "camlp4"; "-parser"; "Ocaml"; "-printer"; "Camlp4AstDumper"; filepath |]
			Unix.stdin camlp4_stdout Unix.stderr in
	Unix.close camlp4_stdout;
	Unix.in_channel_of_descr p_stdout
 
let parse_input str =
	try
		let (p_stdout, camlp4_stdout) = Unix.pipe() in
		(* TODO: named pipe on systems that support it, instead of temp file *)
		let (temp_file_name, temp_file_outchan) = Filename.open_temp_file "ocaide" ".ml" in
		Pervasives.at_exit (fun () ->
						try
							close_out temp_file_outchan;
							Sys.remove temp_file_name
						with
							_ -> ());
		output_string temp_file_outchan str;
		close_out temp_file_outchan;
		let _ = Unix.create_process
				"camlp4" [| "camlp4"; "-parser"; "Ocaml"; "-printer"; "Camlp4AstDumper"; temp_file_name |]
				Unix.stdin camlp4_stdout Unix.stderr in
		Unix.close camlp4_stdout;
		Some (Unix.in_channel_of_descr p_stdout)
	with End_of_file -> None;;
 
let cmd_print_ast_xml () =
	let filepath = read_line() in
	print_endline (ASTToXML.print_ast_in_xml (parse_file filepath))
;;

let cmd_print_occvar_xml () =
	let argument = read_line() in
	  let argument2 = read_line() in
		  let filepath = read_line() in
			  print_endline (ASTVarOcc.print_ast_in_xml (parse_file filepath) argument argument2);
				print_eot ()
;;
 
let cmd_print_occvar_xml_input () =
	let strNLines = read_line() in
		let argument = read_line() in
	  	let argument2 = read_line() in
	let nLines =
		try
			int_of_string (strNLines)
		with Failure "int_of_string" ->
				failwith ("Wrong input: expected number of lines, got '" ^ strNLines ^ "'")
	in
	let buffer = Buffer.create 10000 in
	for i = 1 to nLines do
		let line = read_line() in
		Buffer.add_string buffer line;
		Buffer.add_string buffer "\n"
	done;
	(match parse_input (Buffer.contents buffer) with
		| Some channel -> print_endline (ASTVarOcc.print_ast_in_xml (channel) argument argument2);
		| None -> ());
	print_eot ()
;;

let cmd_print_ast_xml_input () =
	let strNLines = read_line() in
	let nLines =
		try
			int_of_string (strNLines)
		with Failure "int_of_string" ->
				failwith ("Wrong input: expected number of lines, got '" ^ strNLines ^ "'")
	in
	let buffer = Buffer.create 10000 in
	for i = 1 to nLines do
		let line = read_line() in
		Buffer.add_string buffer line;
		Buffer.add_string buffer "\n"
	done;
	(match parse_input (Buffer.contents buffer) with
		| Some channel -> print_endline (ASTToXML.print_ast_in_xml (channel));
		| None -> ());
	print_eot ()
;;
 
let commands = [
	("end", cmd_end);
	("exit", cmd_end);
	("quit", cmd_end);
	("print", cmd_print);
	("astFromFile", cmd_print_ast_xml);
	("astFromInput", cmd_print_ast_xml_input);
	("astOccVarFromFile", cmd_print_occvar_xml);
	("astOccVarFromInput", cmd_print_occvar_xml_input);
	("ast", cmd_print_ast_xml_input)
	]
;;
 
(* handle_command "ast" commands;; *)
 
let _ =
	(* signal that the indexer is successfully started *)
	print_endline "ok";
	print_eot();
	(* main loop: wait for commands on the standard input and print results on the standard output *)
	while true do
		let line = read_line() in
		handle_command line commands
	done