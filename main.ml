(* main loop: reads commands on the standard input, and prints results on  *)
(* the standard output                                                     *)

let rec handle_command command commands =	match commands with
	| (c, f) :: _ when c = command -> f()
	| x :: r -> handle_command command r
	| [] -> prerr_endline ("Command not understood: " ^ command);;

let cmd_end () =
	print_endline "Exiting...";
	exit 0;;

let cmd_print () =
	let str = read_line() in
	print_endline str;;

let cmd_parse () =
	let filepath = read_line() in
	(* camlp4 -parser Ocaml -printer Camlp4AstDumper test.ml > test.ast *)
(*	let (camlp4_in, p_in) = Unix.pipe() in*)
	let (p_stdout, camlp4_stdout) = Unix.pipe() in
(*	let (p_stderr, camlp4_stderr) = Unix.pipe() in*)
	let _ = Unix.create_process 
		"camlp4" [| "camlp4"; "-parser"; "Ocaml"; "-printer"; "Camlp4AstDumper"; filepath |] 
		Unix.stdin camlp4_stdout Unix.stderr in
		(*camlp4_in camlp4_stdout camlp4_stderr in*)
		Unix.close camlp4_stdout;
		try
			Deserializerp4.print_ast_chan (Unix.in_channel_of_descr p_stdout)
		with 
			| End_of_file -> ()
		;;



let commands = [
	("end", cmd_end);
	("print", cmd_print);
	("parse", cmd_parse)
	];;

let _ =
	while true do
		let line = read_line() in
		handle_command line commands
	done

