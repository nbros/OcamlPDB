(* main loop: reads commands on the standard input, and prints results on  *)
(* the standard output                                                     *)

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

let parse () =
(*	let filepath = read_line() in*)
	let filepath = "C:\\Users\\Nicolas\\workspaceOcaIDE\\OcamlPDB\\_build\\main.ml" in
	let (p_stdout, camlp4_stdout) = Unix.pipe() in
	let _ = Unix.create_process
			"camlp4" [| "camlp4"; "-parser"; "Ocaml"; "-printer"; "Camlp4AstDumper"; filepath |]
			Unix.stdin camlp4_stdout Unix.stderr in
	Unix.close camlp4_stdout;
	Unix.in_channel_of_descr p_stdout

let cmd_print_ast_xml () =
	print_endline (ASTToXML.print_ast_in_xml (parse()))
;;

let commands = [
	("end", cmd_end);
	("exit", cmd_end);
	("quit", cmd_end);
	("print", cmd_print);
	("ast", cmd_print_ast_xml)
	]
;;

handle_command "ast" commands;;

(*let _ =                         *)
(*	while true do                 *)
(*		let line = read_line() in   *)
(*		handle_command line commands*)
(*	done                          *)

