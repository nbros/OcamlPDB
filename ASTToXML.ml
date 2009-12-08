open Camlp4
open PreCast
open Ast

let pp = Format.fprintf

(*C:\Users\Nicolas\workspaceOcaIDE\OcamlPDB\_build\main.ml*)

let string_of_loc loc = 
	(string_of_int (Loc.start_off loc)) ^ "," ^ (string_of_int (Loc.stop_off loc))
;;

let string_of_metabool = function
	| BTrue -> "true"
	| BFalse -> "false"
	| BAnt _ -> "?"
;;
	
let rec print_class_expr f = function
    | CeNil (loc) -> pp f "<CeNil/>"
    | CeApp (loc,class_expr,expr) -> pp f "<CeApp>"
    | CeCon (loc, meta_bool, ident, ctyp) -> pp f "<CeCon/>"
    | CeFun (loc, patt, class_expr) -> pp f "<CeFun/>"
    | CeLet (loc, meta_bool, binding, class_expr) -> pp f "<CeLet/>"
    | CeStr (loc, patt, class_str_item) -> pp f "<CeStr/>"
    | CeTyc (loc, class_expr, class_type) -> pp f "<CeTyc/>"
    | CeAnd (loc, class_expr1, class_expr2) -> pp f "<CeAnd/>"
    | CeEq  (loc, class_expr1, class_expr2) -> pp f "<CeEq/>"
    | CeAnt (loc, string) -> pp f "<CeAnt/>"

and print_struct_item f = function
	| StNil(loc) -> pp f "<StNil/>"
	(* class cice *)
	| StCls(loc, class_expr) -> pp f "<StCls>%a</StCls>" print_class_expr class_expr
	(* class type cict *)
	| StClt (loc, class_type) -> pp f "<StClt/>"
	(* st ; st *)
	| StSem (loc, str_item1, str_item2) -> 
		pp f "@[<v><StSem loc='%s'>@,@[<v 1><str_item1>@,%a@]@,</str_item1>@,@[<v 1><str_item2>@,%a@]@,</str_item2>@,</StSem>@]" (string_of_loc loc) print_struct_item str_item1 print_struct_item str_item2
(*		pp f "@[<v><StSem>@,@[<v 1><str_item1>@,@[<v 1>%a@]@,</str_item1>@]<str_item2>%a</str_item2>@,</StSem>@]" print_struct_item str_item1 print_struct_item str_item2*)
	(* # s or # s e *)
	| StDir (loc, s, expr) -> pp f "<StDir/>"
	(* exception t or exception t = i *)
	| StExc (loc, ctyp, ident) -> pp f "<StExc/>"
	(* e *)
	| StExp(loc, expr) -> pp f "<StExp/>"
	(* external s : t = s ... s *)
	| StExt (loc, s, ctyp, strs) -> pp f "<StExt/>"
	(* include me *)
	| StInc (loc, module_expr) -> pp f "<StInc/>"
	(* module s = me *)
	| StMod(loc, s, module_expr) -> pp f "<StMod/>"
	(* module rec mb *)
	| StRecMod (loc, module_binding) -> pp f "<StRecMod/>"
	(* module type s = mt *)
	| StMty (loc, s, module_type) -> pp f "<StMty/>"
	(* open i *)
	| StOpn(loc, ident) -> pp f "<StOpn/>"
	(* type t *)
	| StTyp(loc, ctyp) -> pp f "<StTyp/>"
	(* value (rec)? bi *)
	| StVal(loc, meta_bool, binding) -> pp f "<StVal loc='%s' rec='%s'><binding>%a</binding></StVal>" (string_of_loc loc) (string_of_metabool meta_bool) print_binding binding
	(* $s$ *)
	| StAnt(loc, s) -> pp f "<StAnt/>"

and print_binding f = function
    | BiNil(loc) -> pp f "<BiNil/>"
    | BiAnd (loc, binding1, binding2) -> pp f "<BiAnd loc='%s'><binding1>%a</binding1><binding2>%a</binding2></BiAnd>" (string_of_loc loc) print_binding binding1 print_binding binding2
    | BiEq (loc, patt, expr) -> pp f "<BiEq/>"
    | BiAnt (loc, string) -> pp f "<BiEq/>"

;;



let print_ast_in_xml channel =
	let parse_tree = Deserializerp4.deserialize_chan channel in
	print_struct_item Format.str_formatter parse_tree;
	Format.flush_str_formatter ()
;;
