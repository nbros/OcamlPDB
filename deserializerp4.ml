(* compile with '-I +camlp4' *)

open Camlp4
open PreCast
open Ast

(** Reads the magic number from the given input channel and fails with an
error message if it is not the expected one *)
let read_magic_number in_chan =
	let expected = "Camlp42006M001" in
	let length = String.length expected in
	let magic_number = String.make length ' ' in
	really_input in_chan magic_number 0 length;
	if magic_number <> expected then
		failwith (Format.sprintf "Wrong AST version: '%s'; expected '%s'"
					magic_number expected)
;;

let deserialize_chan in_chan =
	try
		read_magic_number in_chan;
		let ast = (input_value in_chan :> Ast.str_item) in
		close_in in_chan;
		Some ast
	with End_of_file -> None

let deserialize_file file =
	let in_chan = open_in_bin file in
	deserialize_chan in_chan;;

(* print_file_ast "C:\\cygwin\\home\\Nicolas\\char.ast" *)
