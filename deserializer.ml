(* compile with '-I +parsing' *)
  
(*	module Ast = Camlp4.Sig.Ast;;*)

(*  let parse ast_magic ?directive_handler:(_) _loc strm =                    *)
(*    let str =                                                               *)
(*      let buf = Buffer.create 2047 in                                       *)
(*      let () = Stream.iter (Buffer.add_char buf) strm                       *)
(*      in Buffer.contents buf in                                             *)
(*    let magic_len = String.length ast_magic in                              *)
(*    let buffer = String.create magic_len in                                 *)
(*      String.blit str 0 buffer 0 magic_len;                                 *)
(*      if buffer = ast_magic then ()                                         *)
(*      else failwith (Format.sprintf "Bad magic: %S vs %S" buffer ast_magic);*)
(*      Marshal.from_string str magic_len;;                                   *)
(*                                                                            *)
(*  let parse_implem = parse Camlp4_config.camlp4_ast_impl_magic_number;;     *)
(*  let parse_interf = parse Camlp4_config.camlp4_ast_intf_magic_number;;     *)

open Camlp4

open Parsetree
open Location
open Lexing

let string_of_loc loc = string_of_int loc.loc_start.pos_cnum

let rec print_types = function
	| [] -> ""
	| (name,decl)::[] -> name
	| (name,decl)::r -> name ^ " " ^ (print_types r);;

let string_of_struct_item = function
	Pstr_eval(_) -> "Pstr_eval"
  | Pstr_value(_) -> "Pstr_value"
  | Pstr_primitive(name,_) -> "Pstr_primitive: " ^ name
  | Pstr_type(l) -> "Pstr_type: " ^ (print_types l)
  | Pstr_exception(_) -> "Pstr_exception"
  | Pstr_exn_rebind(_) -> "Pstr_exn_rebind"
  | Pstr_module(_) -> "Pstr_module"
  | Pstr_recmodule(_) -> "Pstr_recmodule"
  | Pstr_modtype(_) -> "Pstr_modtype"
  | Pstr_open(_) -> "Pstr_open"
  | Pstr_class(_) -> "Pstr_class"
  | Pstr_class_type(_) -> "Pstr_class_type"
  | Pstr_include(_) -> "Pstr_include";;

let rec print_ast ast = match ast with
	| struct_item::struct_items -> 
			print_endline 
				("[" ^ (string_of_loc struct_item.pstr_loc) ^ "] " ^
				(string_of_struct_item struct_item.pstr_desc));
			print_ast struct_items
	| _ -> ();;

let deserialize file =
	let in_chan = open_in_bin file in
	let magic_number = "            " in
	really_input in_chan magic_number 0 12;
	print_endline magic_number;
	let filepath = (input_value in_chan :> string) in
	print_endline ("\"" ^ filepath ^ "\"");
	let parse_tree = (input_value in_chan :> structure_item list) in
	print_ast parse_tree;
	close_in in_chan;;

		
	

deserialize "C:\\cygwin\\home\\Nicolas\\char.ast"