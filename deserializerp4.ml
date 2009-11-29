(* compile with '-I +camlp4' *)

open Camlp4
open PreCast
open Ast

let rec string_of_struct_item = function
	| StNil(loc) -> "StNil"
	(* class cice *)
	| StCls(loc, class_expr) -> "StCls"
	(* class type cict *)
	| StClt (loc, class_type) -> "StClt"
	(* st ; st *)
	| StSem (loc, str_item1, str_item2) ->
			"StSem (" ^
			(string_of_struct_item str_item1) ^ "," ^
			(string_of_struct_item str_item2) ^ ")"
	(* # s or # s e *)
	| StDir (loc, s, expr) -> "StDir"
	(* exception t or exception t = i *)
	| StExc (loc, ctyp, ident) -> "StExc"
	(* e *)
	| StExp(loc, expr) -> "StExp"
	(* external s : t = s ... s *)
	| StExt (loc, s, ctyp, strs) -> "StExt"
	(* include me *)
	| StInc (loc, module_expr) -> "StInc"
	(* module s = me *)
	| StMod(loc, s, module_expr) -> "StMod"
	(* module rec mb *)
	| StRecMod (loc, module_binding) -> "StRecMod"
	(* module type s = mt *)
	| StMty (loc, s, module_type) -> "StMty"
	(* open i *)
	| StOpn(loc, ident) -> "StOpn"
	(* type t *)
	| StTyp(loc, ctyp) -> "StTyp"
	(* value (rec)? bi *)
	| StVal(loc, meta_bool, binding) -> "StVal"
	(* $s$ *)
	| StAnt(loc, s) -> "StAnt"
;;

let rec print_ast ast = print_string "."; match ast with
	| struct_item:: struct_items ->
			print_endline (string_of_struct_item struct_item);
			print_ast struct_items
	| [] -> ();;

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
	read_magic_number in_chan;
	let ast = (input_value in_chan :> Ast.str_item) in
	close_in in_chan;
	ast;;

let deserialize_file file =
	let in_chan = open_in_bin file in
	deserialize_chan in_chan;;

let print_file_ast file =
	let parse_tree = deserialize_file file in
	print_endline (string_of_struct_item parse_tree);;

let print_ast_chan channel =
	let parse_tree = deserialize_chan channel in
	print_endline (string_of_struct_item parse_tree);;

(*print_file_ast "C:\\cygwin\\home\\Nicolas\\char.ast"*)
