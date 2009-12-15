open Camlp4
open PreCast
open Ast

let pp = Format.fprintf

let string_of_loc loc =
	(string_of_int (Loc.start_off loc)) ^ "," ^ (string_of_int (Loc.stop_off loc))

let lt = (Str.regexp "<")
let gt = (Str.regexp ">")
let amp = (Str.regexp "&")
let apos = (Str.regexp "'")
let quot = (Str.regexp "\"")

(* escape XML chars *)
let escape_string str: string =
	let str = Str.global_replace lt "&lt;" str in
	let str = Str.global_replace gt "&gt;" str in
	let str = Str.global_replace amp "&amp;" str in
	let str = Str.global_replace apos "&apos;" str in
	let str = Str.global_replace quot "&quot;" str in
	str

let string_of_metabool = function
	| BTrue -> "true"
	| BFalse -> "false"
	| BAnt _ -> "?"

let print_meta_bool f = function
	| BTrue -> pp f "true"
	| BFalse -> pp f "false"
	| BAnt str -> pp f "<antiquotation>%s</antiquotation>" (escape_string str)

let rec print_strings f = function
	| LNil -> pp f ""
	| LCons(str, r) -> pp f "<string>%s</string>%a" str print_strings r
	| LAnt(str) -> pp f "<antiquotation/>%s</antiquotation>" (escape_string str)

let rec print_ident f = function (* The type of identifiers (including path like Foo(X).Bar.y) *)
	(* i . i *) (** Access in module *)
	| IdAcc(loc, ident1, ident2) -> pp f "<IdAcc loc='%s'><ident1>%a</ident1><ident2>%a</ident2></IdAcc>" (string_of_loc loc) print_ident ident1 print_ident ident2
	(* i i *) (** Application *)
	| IdApp(loc, ident1, ident2) -> pp f "<IdApp loc='%s'><ident1>%a</ident1><ident2>%a</ident2></IdApp>" (string_of_loc loc) print_ident ident1 print_ident ident2
	(* foo *) (** Lowercase identifier *)
	| IdLid(loc, name) -> pp f "<IdLid loc='%s'><name>%s</name></IdLid>" (string_of_loc loc) (escape_string name)
	(* Bar *) (** Uppercase identifier *)
	| IdUid(loc, name) -> pp f "<IdUid loc='%s'><name>%s</name></IdUid>" (string_of_loc loc) (escape_string name)
	(* $s$ *) (** Antiquotation *)
	| IdAnt(loc, name) -> pp f "<IdAnt loc='%s'><name>%s</name></IdAnt>" (string_of_loc loc) (escape_string name)

and print_ctyp f = function (* Representation of types                                     *)
	(** Empty type *)
	| TyNil(loc) -> pp f "<TyNil loc='%s'/>" (string_of_loc loc)
	(* t as t *) (* list 'a as 'a *) (** Type aliasing *)
	| TyAli(loc, ctyp1, ctyp2) -> pp f "<TyAli loc='%s'><ctyp1>%a</ctyp1><ctyp2>%a</ctyp2></TyAli>" (string_of_loc loc) print_ctyp ctyp1 print_ctyp ctyp2
	(* _ *) (** Wildcard *)
	| TyAny(loc) -> pp f "<TyAny loc='%s'/>" (string_of_loc loc)
	(* t t *) (* list 'a *) (** Application *)
	| TyApp(loc, ctyp1, ctyp2) -> pp f "<TyApp loc='%s'><ctyp1>%a</ctyp1><ctyp2>%a</ctyp2></TyApp>" (string_of_loc loc) print_ctyp ctyp1 print_ctyp ctyp2
	(* t) -> t *) (* int) -> string *) (** Arrow *)
	| TyArr(loc, ctyp1, ctyp2) -> pp f "<TyArr loc='%s'><ctyp1>%a</ctyp1><ctyp2>%a</ctyp2></TyArr>" (string_of_loc loc) print_ctyp ctyp1 print_ctyp ctyp2
	(* #i *) (* #point *) (** Class type *)
	| TyCls(loc, ident1) -> pp f "<TyCls loc='%s'><ident>%a</ident></TyCls>" (string_of_loc loc) print_ident ident1
	(* ~s:t *) (** Label type *)
	| TyLab(loc, name, ctyp1) -> pp f "<TyLab loc='%s'><name>%s</name><ctyp>%a</ctyp></TyLab>" (string_of_loc loc) (escape_string name) print_ctyp ctyp1
	(* i *) (* Lazy.t *) (** Type identifier *)
	| TyId(loc, ident1) -> pp f "<TyId loc='%s'><ident>%a</ident></TyId>" (string_of_loc loc) print_ident ident1
	(* t == t *) (* type t = [ A | B ] == Foo.t *) (** Type manifest *)
	| TyMan(loc, ctyp1, ctyp2) -> pp f "<TyMan loc='%s'><ctyp1>%a</ctyp1><ctyp2>%a</ctyp2></TyMan>" (string_of_loc loc) print_ctyp ctyp1 print_ctyp ctyp2
	(* type t 'a 'b 'c = t constraint t = t constraint t = t *) (** Type declaration *)
	| TyDcl(loc, name, ctyps, ctyp1, (*TODO*) constraints) -> pp f "<TyDcl loc='%s'><name>%s</name><ctyps>%a</ctyps><ctyp>%a</ctyp><constraints>%a</constraints></TyDcl>" (string_of_loc loc) (escape_string name) print_ctyps ctyps print_ctyp ctyp1 print_constraints constraints
	(* < (t)? (..)? > *) (* < move : int) -> 'a .. > as 'a  *) (**   Object type *)
	| TyObj(loc, ctyp1, meta_bool1) -> pp f "<TyObj loc='%s'><ctyp>%a</ctyp><meta_bool>%a</meta_bool></TyObj>" (string_of_loc loc) print_ctyp ctyp1 print_meta_bool meta_bool1
	(* ?s:t *) (** Optional label type *)
	| TyOlb(loc, name, ctyp1) -> pp f "<TyOlb loc='%s'><name>%s</name><ctyp>%a</ctyp></TyOlb>" (string_of_loc loc) (escape_string name) print_ctyp ctyp1
	(* ! t . t *) (* ! 'a . list 'a) -> 'a *) (** Polymorphic type *)
	| TyPol(loc, ctyp1, ctyp2) -> pp f "<TyPol loc='%s'><ctyp1>%a</ctyp1><ctyp2>%a</ctyp2></TyPol>" (string_of_loc loc) print_ctyp ctyp1 print_ctyp ctyp2
	(* 's *)
	| TyQuo(loc, name) -> pp f "<TyQuo loc='%s'><name>%s</name></TyQuo>" (string_of_loc loc) (escape_string name)
	(* +'s *)
	| TyQuP(loc, name) -> pp f "<TyQuP loc='%s'><name>%s</name></TyQuP>" (string_of_loc loc) (escape_string name)
	(* -'s *)
	| TyQuM(loc, name) -> pp f "<TyQuM loc='%s'><name>%s</name></TyQuM>" (string_of_loc loc) (escape_string name)
	(* `s *) (** Polymorphic variant *)
	| TyVrn(loc, name) -> pp f "<TyVrn loc='%s'><name>%s</name></TyVrn>" (string_of_loc loc) (escape_string name)
	(* { t } *) (* { foo : int ; bar : mutable string } *) (** Record *)
	| TyRec(loc, ctyp1) -> pp f "<TyRec loc='%s'><ctyp>%a</ctyp></TyRec>" (string_of_loc loc) print_ctyp ctyp1
	(* t : t *) (** Field declaration *)
	| TyCol(loc, ctyp1, ctyp2) -> pp f "<TyCol loc='%s'><ctyp1>%a</ctyp1><ctyp2>%a</ctyp2></TyCol>" (string_of_loc loc) print_ctyp ctyp1 print_ctyp ctyp2
	(* t; t *) (** Semicolon-separated type list *)
	| TySem(loc, ctyp1, ctyp2) -> pp f "<TySem loc='%s'><ctyp1>%a</ctyp1><ctyp2>%a</ctyp2></TySem>" (string_of_loc loc) print_ctyp ctyp1 print_ctyp ctyp2
	(* t, t *) (** Comma-separated type list *)
	| TyCom(loc, ctyp1, ctyp2) -> pp f "<TyCom loc='%s'><ctyp1>%a</ctyp1><ctyp2>%a</ctyp2></TyCom>" (string_of_loc loc) print_ctyp ctyp1 print_ctyp ctyp2
	(* [ t ] *) (* [ A of int, string | B ] *) (** Sum type *)
	| TySum(loc, ctyp1) -> pp f "<TySum loc='%s'><ctyp>%a</ctyp></TySum>" (string_of_loc loc) print_ctyp ctyp1
	(* t of t *) (* A of int *)
	| TyOf(loc, ctyp1, ctyp2) -> pp f "<TyOf loc='%s'><ctyp1>%a</ctyp1><ctyp2>%a</ctyp2></TyOf>" (string_of_loc loc) print_ctyp ctyp1 print_ctyp ctyp2
	(* t, t *)
	| TyAnd(loc, ctyp1, ctyp2) -> pp f "<TyAnd loc='%s'><ctyp1>%a</ctyp1><ctyp2>%a</ctyp2></TyAnd>" (string_of_loc loc) print_ctyp ctyp1 print_ctyp ctyp2
	(* t | t *) (** "Or" pattern between types *)
	| TyOr(loc, ctyp1, ctyp2) -> pp f "<TyOr loc='%s'><ctyp1>%a</ctyp1><ctyp2>%a</ctyp2></TyOr>" (string_of_loc loc) print_ctyp ctyp1 print_ctyp ctyp2
	(* private t *) (** Private type *)
	| TyPrv(loc, ctyp1) -> pp f "<TyPrv loc='%s'><ctyp>%a</ctyp></TyPrv>" (string_of_loc loc) print_ctyp ctyp1
	(* mutable t *) (** Mutable type *)
	| TyMut(loc, ctyp1) -> pp f "<TyMut loc='%s'><ctyp>%a</ctyp></TyMut>" (string_of_loc loc) print_ctyp ctyp1
	(* ( t ) *) (* (int * string) *) (** Tuple *)
	| TyTup(loc, ctyp1) -> pp f "<TyTup loc='%s'><ctyp>%a</ctyp></TyTup>" (string_of_loc loc) print_ctyp ctyp1
	(* t * t *)
	| TySta(loc, ctyp1, ctyp2) -> pp f "<TySta loc='%s'><ctyp1>%a</ctyp1><ctyp2>%a</ctyp2></TySta>" (string_of_loc loc) print_ctyp ctyp1 print_ctyp ctyp2
	(* [ = t ] *)
	| TyVrnEq(loc, ctyp1) -> pp f "<TyVrnEq loc='%s'><ctyp>%a</ctyp></TyVrnEq>" (string_of_loc loc) print_ctyp ctyp1
	(* [ > t ] *)
	| TyVrnSup(loc, ctyp1) -> pp f "<TyVrnSup loc='%s'><ctyp>%a</ctyp></TyVrnSup>" (string_of_loc loc) print_ctyp ctyp1
	(* [ < t ] *)
	| TyVrnInf(loc, ctyp1) -> pp f "<TyVrnInf loc='%s'><ctyp>%a</ctyp></TyVrnInf>" (string_of_loc loc) print_ctyp ctyp1
	(* [ < t > t ] *)
	| TyVrnInfSup(loc, ctyp1, ctyp2) -> pp f "<TyVrnInfSup loc='%s'><ctyp1>%a</ctyp1><ctyp2>%a</ctyp2></TyVrnInfSup>" (string_of_loc loc) print_ctyp ctyp1 print_ctyp ctyp2
	(* t & t *)
	| TyAmp(loc, ctyp1, ctyp2) -> pp f "<TyAmp loc='%s'><ctyp1>%a</ctyp1><ctyp2>%a</ctyp2></TyAmp>" (string_of_loc loc) print_ctyp ctyp1 print_ctyp ctyp2
	(* t of & t *)
	| TyOfAmp(loc, ctyp1, ctyp2) -> pp f "<TyOfAmp loc='%s'><ctyp1>%a</ctyp1><ctyp2>%a</ctyp2></TyOfAmp>" (string_of_loc loc) print_ctyp ctyp1 print_ctyp ctyp2
	(* $s$ *) (** Antiquotation *)
	| TyAnt(loc, name) -> pp f "<TyAnt loc='%s'><name>%s</name></TyAnt>" (string_of_loc loc) (escape_string name)

and print_patt f = function (* The type of patterns                                       *)
	(**   Empty pattern *)
	| PaNil(loc) -> pp f "<PaNil loc='%s'/>" (string_of_loc loc)
	(* i *) (** Identifier *)
	| PaId(loc, ident1) -> pp f "<PaId loc='%s'><ident>%a</ident></PaId>" (string_of_loc loc) print_ident ident1
	(* p as p *) (* (Node x y as n) *) (** Alias *)
	| PaAli(loc, patt1, patt2) -> pp f "<PaAli loc='%s'><patt1>%a</patt1><patt2>%a</patt2></PaAli>" (string_of_loc loc) print_patt patt1 print_patt patt2
	(* $s$ *) (** Antiquotation *)
	| PaAnt(loc, name) -> pp f "<PaAnt loc='%s'><name>%s</name></PaAnt>" (string_of_loc loc) (escape_string name)
	(* _ *) (** Wildcard *)
	| PaAny(loc) -> pp f "<PaAny loc='%s'></PaAny>" (string_of_loc loc)
	(* p p *) (* fun x y) -> *) (** Application *)
	| PaApp(loc, patt1, patt2) -> pp f "<PaApp loc='%s'><patt1>%a</patt1><patt2>%a</patt2></PaApp>" (string_of_loc loc) print_patt patt1 print_patt patt2
	(* [| p |] *) (** Array *)
	| PaArr(loc, patt1) -> pp f "<PaArr loc='%s'><patt>%a</patt></PaArr>" (string_of_loc loc) print_patt patt1
	(* p, p *) (** Comma-separated pattern list *)
	| PaCom(loc, patt1, patt2) -> pp f "<PaCom loc='%s'><patt1>%a</patt1><patt2>%a</patt2></PaCom>" (string_of_loc loc) print_patt patt1 print_patt patt2
	(* p; p *) (** Semicolon-separated pattern list *)
	| PaSem(loc, patt1, patt2) -> pp f "<PaSem loc='%s'><patt1>%a</patt1><patt2>%a</patt2></PaSem>" (string_of_loc loc) print_patt patt1 print_patt patt2
	(* c *) (* 'x' *) (** Character *)
	| PaChr(loc, name) -> pp f "<PaChr loc='%s'><name>%s</name></PaChr>" (string_of_loc loc) (escape_string name)
	(** Integer *)
	| PaInt(loc, name) -> pp f "<PaInt loc='%s'><name>%s</name></PaInt>" (string_of_loc loc) (escape_string name)
	(** Int32 *)
	| PaInt32(loc, name) -> pp f "<PaInt32 loc='%s'><name>%s</name></PaInt32>" (string_of_loc loc) (escape_string name)
	(** Int64 *)
	| PaInt64(loc, name) -> pp f "<PaInt64 loc='%s'><name>%s</name></PaInt64>" (string_of_loc loc) (escape_string name)
	(** NativeInt *)
	| PaNativeInt(loc, name) -> pp f "<PaNativeInt loc='%s'><name>%s</name></PaNativeInt>" (string_of_loc loc) (escape_string name)
	(** Float *)
	| PaFlo(loc, name) -> pp f "<PaFlo loc='%s'><name>%s</name></PaFlo>" (string_of_loc loc) (escape_string name)
	(* ~s or ~s:(p) *) (** Label *)
	| PaLab(loc, name, patt1) -> pp f "<PaLab loc='%s'><name>%s</name><patt>%a</patt></PaLab>" (string_of_loc loc) (escape_string name) print_patt patt1
	(* ?s or ?s:(p) *) (** Optional label *)
	| PaOlb(loc, name, patt1) -> pp f "<PaOlb loc='%s'><name>%s</name><patt>%a</patt></PaOlb>" (string_of_loc loc) (escape_string name) print_patt patt1
	(* ?s:(p = e) or ?(p = e) *) (** Optional label with default value *)
	| PaOlbi(loc, name, patt1, expr1) -> pp f "<PaOlbi loc='%s'><name>%s</name><patt>%a</patt><expr>%a</expr></PaOlbi>" (string_of_loc loc) (escape_string name) print_patt patt1 print_expr expr1
	(* p | p *) (** Or *)
	| PaOrp(loc, patt1, patt2) -> pp f "<PaOrp loc='%s'><patt1>%a</patt1><patt2>%a</patt2></PaOrp>" (string_of_loc loc) print_patt patt1 print_patt patt2
	(* p .. p *) (** Pattern range *)
	| PaRng(loc, patt1, patt2) -> pp f "<PaRng loc='%s'><patt1>%a</patt1><patt2>%a</patt2></PaRng>" (string_of_loc loc) print_patt patt1 print_patt patt2
	(* { p } *) (** Record *)
	| PaRec(loc, patt1) -> pp f "<PaRec loc='%s'><patt>%a</patt></PaRec>" (string_of_loc loc) print_patt patt1
	(* i = p *) (** Equality *)
	| PaEq(loc, ident1, patt1) -> pp f "<PaEq loc='%s'><ident>%a</ident><patt>%a</patt></PaEq>" (string_of_loc loc) print_ident ident1 print_patt patt1
	(* s *) (** String *)
	| PaStr(loc, name) -> pp f "<PaStr loc='%s'><name>%s</name></PaStr>" (string_of_loc loc) (escape_string name)
	(* ( p ) *) (** Tuple *)
	| PaTup(loc, patt1) -> pp f "<PaTup loc='%s'><patt>%a</patt></PaTup>" (string_of_loc loc) print_patt patt1
	(* (p : t) *) (** Type constraint *)
	| PaTyc(loc, patt1, ctyp1) -> pp f "<PaTyc loc='%s'><patt>%a</patt><ctyp>%a</ctyp></PaTyc>" (string_of_loc loc) print_patt patt1 print_ctyp ctyp1
	(* #i *)
	| PaTyp(loc, ident1) -> pp f "<PaTyp loc='%s'><ident>%a</ident></PaTyp>" (string_of_loc loc) print_ident ident1
	(* `s *) (** Polymorphic variant *)
	| PaVrn(loc, name) -> pp f "<PaVrn loc='%s'><name>%s</name></PaVrn>" (string_of_loc loc) (escape_string name)
	(* lazy p *)
	| PaLaz(loc, patt1) -> pp f "<PaLaz loc='%s'><patt>%a</patt></PaLaz>" (string_of_loc loc) print_patt patt1
and print_expr f = function (* The type of expressions                                    *)
	(** Empty expression *)
	| ExNil(loc) -> pp f "<ExNil loc='%s'/>" (string_of_loc loc)
	(* i *) (**   Identifier *)
	| ExId(loc, ident1) -> pp f "<ExId loc='%s'><ident>%a</ident></ExId>" (string_of_loc loc) print_ident ident1
	(* e.e *) (** Access in module *)
	| ExAcc(loc, expr1, expr2) -> pp f "<ExAcc loc='%s'><expr1>%a</expr1><expr2>%a</expr2></ExAcc>" (string_of_loc loc) print_expr expr1 print_expr expr2
	(* $s$ *) (** Antiquotation *)
	| ExAnt(loc, name) -> pp f "<ExAnt loc='%s'><name>%s</name></ExAnt>" (string_of_loc loc) (escape_string name)
	(* e e *) (** Application *)
	| ExApp(loc, expr1, expr2) -> pp f "<ExApp loc='%s'><expr1>%a</expr1><expr2>%a</expr2></ExApp>" (string_of_loc loc) print_expr expr1 print_expr expr2
	(* e.(e) *) (** Array access *)
	| ExAre(loc, expr1, expr2) -> pp f "<ExAre loc='%s'><expr1>%a</expr1><expr2>%a</expr2></ExAre>" (string_of_loc loc) print_expr expr1 print_expr expr2
	(* [| e |] *) (** Array declaration *)
	| ExArr(loc, expr1) -> pp f "<ExArr loc='%s'><expr>%a</expr></ExArr>" (string_of_loc loc) print_expr expr1
	(* e; e *) (** Semicolon-separated expression list *)
	| ExSem(loc, expr1, expr2) -> pp f "<ExSem loc='%s'><expr1>%a</expr1><expr2>%a</expr2></ExSem>" (string_of_loc loc) print_expr expr1 print_expr expr2
	(* assert False *) (** assert False *)
	| ExAsf(loc) -> pp f "<ExAsf loc='%s'></ExAsf>" (string_of_loc loc)
	(* assert e *) (** assert e *)
	| ExAsr(loc, expr1) -> pp f "<ExAsr loc='%s'><expr>%a</expr></ExAsr>" (string_of_loc loc) print_expr expr1
	(* e := e *) (** Assignment *)
	| ExAss(loc, expr1, expr2) -> pp f "<ExAss loc='%s'><expr1>%a</expr1><expr2>%a</expr2></ExAss>" (string_of_loc loc) print_expr expr1 print_expr expr2
	(* 'c' *) (** Character *)
	| ExChr(loc, name) -> pp f "<ExChr loc='%s'><name>%s</name></ExChr>" (string_of_loc loc) (escape_string name)
	(* (e : t) or (e : t :> t) *) (** Coercion *)
	| ExCoe(loc, expr1, ctyp1, ctyp2) -> pp f "<ExCoe loc='%s'><expr>%a</expr><ctyp1>%a</ctyp1><ctyp2>%a</ctyp2></ExCoe>" (string_of_loc loc) print_expr expr1 print_ctyp ctyp1 print_ctyp ctyp2
	(* 3.14 *) (** Float *)
	| ExFlo(loc, name) -> pp f "<ExFlo loc='%s'><name>%s</name></ExFlo>" (string_of_loc loc) (escape_string name)
	(* for s = e to/downto e do { e } *) (** For loop *)
	| ExFor(loc, name, expr1, expr2, meta_bool1, expr3) -> pp f "<ExFor loc='%s'><name>%s</name><expr1>%a</expr1><expr2>%a</expr2><is_downto>%a</is_downto><expr3>%a</expr3></ExFor>" (string_of_loc loc) (escape_string name) print_expr expr1 print_expr expr2 print_meta_bool meta_bool1 print_expr expr3
	(* fun [ mc ] *) (** Function with match case *)
	| ExFun(loc, match_case1) -> pp f "<ExFun loc='%s'><match_case>%a</match_case></ExFun>" (string_of_loc loc) print_match_case match_case1
	(* if e then e else e *) (** if/then/else *)
	| ExIfe(loc, expr1, expr2, expr3) -> pp f "<ExIfe loc='%s'><expr1>%a</expr1><expr2>%a</expr2><expr3>%a</expr3></ExIfe>" (string_of_loc loc) print_expr expr1 print_expr expr2 print_expr expr3
	(* 42 *) (** Int *)
	| ExInt(loc, name) -> pp f "<ExInt loc='%s'><name>%s</name></ExInt>" (string_of_loc loc) (escape_string name)
	(** Int32 *)
	| ExInt32(loc, name) -> pp f "<ExInt32 loc='%s'><name>%s</name></ExInt32>" (string_of_loc loc) (escape_string name)
	(** Int64 *)
	| ExInt64(loc, name) -> pp f "<ExInt64 loc='%s'><name>%s</name></ExInt64>" (string_of_loc loc) (escape_string name)
	(** NativeInt *)
	| ExNativeInt(loc, name) -> pp f "<ExNativeInt loc='%s'><name>%s</name></ExNativeInt>" (string_of_loc loc) (escape_string name)
	(* ~s or ~s:e *) (** Label argument with/without expression *)
	| ExLab(loc, name, expr1) -> pp f "<ExLab loc='%s'><name>%s</name><expr>%a</expr></ExLab>" (string_of_loc loc) (escape_string name) print_expr expr1
	(* lazy e *) (** Lazy evaluation *)
	| ExLaz(loc, expr1) -> pp f "<ExLaz loc='%s'><expr>%a</expr></ExLaz>" (string_of_loc loc) print_expr expr1
	(* let b in e or let rec b in e *) (** Let statement with/without recursion *)
	| ExLet(loc, meta_bool1, binding1, expr1) -> pp f "<ExLet loc='%s'><rec>%a</rec><binding>%a</binding><expr>%a</expr></ExLet>" (string_of_loc loc) print_meta_bool meta_bool1 print_binding binding1 print_expr expr1
	(* let module s = me in e *) (** "Let module in" construct *)
	| ExLmd(loc, name, module_expr1, expr1) -> pp f "<ExLmd loc='%s'><name>%s</name><module_expr>%a</module_expr><expr>%a</expr></ExLmd>" (string_of_loc loc) (escape_string name) print_module_expr module_expr1 print_expr expr1
	(* match e with [ mc ] *) (** Match case *)
	| ExMat(loc, expr1, match_case1) -> pp f "<ExMat loc='%s'><expr>%a</expr><match_case>%a</match_case></ExMat>" (string_of_loc loc) print_expr expr1 print_match_case match_case1
	(* new i *) (** New object *)
	| ExNew(loc, ident1) -> pp f "<ExNew loc='%s'><ident>%a</ident></ExNew>" (string_of_loc loc) print_ident ident1
	(* object ((p))? (cst)? end *) (** Object declaration *)
	| ExObj(loc, patt1, class_str_item1) -> pp f "<ExObj loc='%s'><patt>%a</patt><class_str_item>%a</class_str_item></ExObj>" (string_of_loc loc) print_patt patt1 print_class_str_item class_str_item1
	(* ?s or ?s:e *) (** Optional label *)
	| ExOlb(loc, name, expr1) -> pp f "<ExOlb loc='%s'><name>%s</name><expr>%a</expr></ExOlb>" (string_of_loc loc) (escape_string name) print_expr expr1
	(* {< rb >} *) (** Overloading *)
	| ExOvr(loc, rec_binding1) -> pp f "<ExOvr loc='%s'><rec_binding>%a</rec_binding></ExOvr>" (string_of_loc loc) print_rec_binding rec_binding1
	(* { rb } or { (e) with rb } *) (** Record *)
	| ExRec(loc, rec_binding1, expr1) -> pp f "<ExRec loc='%s'><rec_binding>%a</rec_binding><expr>%a</expr></ExRec>" (string_of_loc loc) print_rec_binding rec_binding1 print_expr expr1
	(* do { e } *) (** Sequence with "do" statement *)
	| ExSeq(loc, expr1) -> pp f "<ExSeq loc='%s'><expr>%a</expr></ExSeq>" (string_of_loc loc) print_expr expr1
	(* e#s *) (** Method call *)
	| ExSnd(loc, expr1, name) -> pp f "<ExSnd loc='%s'><expr>%a</expr><name>%s</name></ExSnd>" (string_of_loc loc) print_expr expr1 (escape_string name)
	(* e.[e] *) (** String access *)
	| ExSte(loc, expr1, expr2) -> pp f "<ExSte loc='%s'><expr1>%a</expr1><expr2>%a</expr2></ExSte>" (string_of_loc loc) print_expr expr1 print_expr expr2
	(* s *) (* "foo" *) (** String *)
	| ExStr(loc, name) -> pp f "<ExStr loc='%s'><name>%s</name></ExStr>" (string_of_loc loc) (escape_string name)
	(* try e with [ mc ] *) (** "Try .. with" construct *)
	| ExTry(loc, expr1, match_case1) -> pp f "<ExTry loc='%s'><expr>%a</expr><match_case>%a</match_case></ExTry>" (string_of_loc loc) print_expr expr1 print_match_case match_case1
	(* (e) *) (** Tuple *)
	| ExTup(loc, expr1) -> pp f "<ExTup loc='%s'><expr>%a</expr></ExTup>" (string_of_loc loc) print_expr expr1
	(* e, e *) (** Comma-separated expression list *)
	| ExCom(loc, expr1, expr2) -> pp f "<ExCom loc='%s'><expr1>%a</expr1><expr2>%a</expr2></ExCom>" (string_of_loc loc) print_expr expr1 print_expr expr2
	(* (e : t) *) (** Type constraint *)
	| ExTyc(loc, expr1, ctyp1) -> pp f "<ExTyc loc='%s'><expr>%a</expr><ctyp>%a</ctyp></ExTyc>" (string_of_loc loc) print_expr expr1 print_ctyp ctyp1
	(* `s *) (** Polymorphic variant *)
	| ExVrn(loc, name) -> pp f "<ExVrn loc='%s'><name>%s</name></ExVrn>" (string_of_loc loc) (escape_string name)
	(* while e do { e } *) (** "While .. do" constraint *)
	| ExWhi(loc, expr1, expr2) -> pp f "<ExWhi loc='%s'><expr1>%a</expr1><expr2>%a</expr2></ExWhi>" (string_of_loc loc) print_expr expr1 print_expr expr2
and print_module_type f = function (* The type of module types                                   *)
	| MtNil(loc) -> pp f "<MtNil loc='%s'></MtNil>" (string_of_loc loc)
	(* i *) (* A.B.C *)
	| MtId(loc, ident1) -> pp f "<MtId loc='%s'><ident>%a</ident></MtId>" (string_of_loc loc) print_ident ident1
	(* functor (s : mt)) -> mt *)
	| MtFun(loc, name, module_type1, module_type2) -> pp f "<MtFun loc='%s'><name>%s</name><module_type1>%a</module_type1><module_type2>%a</module_type2></MtFun>" (string_of_loc loc) (escape_string name) print_module_type module_type1 print_module_type module_type2
	(* 's *)
	| MtQuo(loc, name) -> pp f "<MtQuo loc='%s'><name>%s</name></MtQuo>" (string_of_loc loc) (escape_string name)
	(* sig sg end *)
	| MtSig(loc, sig_item1) -> pp f "<MtSig loc='%s'><sig_item>%a</sig_item></MtSig>" (string_of_loc loc) print_sig_item sig_item1
	(* mt with wc *)
	| MtWit(loc, module_type1, with_constr1) -> pp f "<MtWit loc='%s'><module_type>%a</module_type><with_constr>%a</with_constr></MtWit>" (string_of_loc loc) print_module_type module_type1 print_with_constr with_constr1
	(* $s$ *)
	| MtAnt(loc, name) -> pp f "<MtAnt loc='%s'><name>%s</name></MtAnt>" (string_of_loc loc) (escape_string name)
and print_sig_item f = function (* The type of signature items                                *)
	| SgNil(loc) -> pp f "<SgNil loc='%s'></SgNil>" (string_of_loc loc)
	(* class cict *)
	| SgCls(loc, class_type1) -> pp f "<SgCls loc='%s'><class_type>%a</class_type></SgCls>" (string_of_loc loc) print_class_type class_type1
	(* class type cict *)
	| SgClt(loc, class_type1) -> pp f "<SgClt loc='%s'><class_type>%a</class_type></SgClt>" (string_of_loc loc) print_class_type class_type1
	(* sg ; sg *)
	| SgSem(loc, sig_item1, sig_item2) -> pp f "<SgSem loc='%s'><sig_item1>%a</sig_item1><sig_item2>%a</sig_item2></SgSem>" (string_of_loc loc) print_sig_item sig_item1 print_sig_item sig_item2
	(* # s or # s e *)
	| SgDir(loc, name, expr1) -> pp f "<SgDir loc='%s'><name>%s</name><expr>%a</expr></SgDir>" (string_of_loc loc) (escape_string name) print_expr expr1
	(* exception t *)
	| SgExc(loc, ctyp1) -> pp f "<SgExc loc='%s'><ctyp>%a</ctyp></SgExc>" (string_of_loc loc) print_ctyp ctyp1
	(* external s : t = s ... s *)
	| SgExt(loc, name, ctyp1, strings (*meta_list string*)) -> pp f "<SgExt loc='%s'><name>%s</name><ctyp>%a</ctyp><strings>%a</strings></SgExt>" (string_of_loc loc) (escape_string name) print_ctyp ctyp1 print_strings strings
	(* include mt *)
	| SgInc(loc, module_type1) -> pp f "<SgInc loc='%s'><module_type>%a</module_type></SgInc>" (string_of_loc loc) print_module_type module_type1
	(* module s : mt *)
	| SgMod(loc, name, module_type1) -> pp f "<SgMod loc='%s'><name>%s</name><module_type>%a</module_type></SgMod>" (string_of_loc loc) (escape_string name) print_module_type module_type1
	(* module rec mb *)
	| SgRecMod(loc, module_binding1) -> pp f "<SgRecMod loc='%s'><module_binding>%a</module_binding></SgRecMod>" (string_of_loc loc) print_module_binding module_binding1
	(* module type s = mt *)
	| SgMty(loc, name, module_type1) -> pp f "<SgMty loc='%s'><name>%s</name><module_type>%a</module_type></SgMty>" (string_of_loc loc) (escape_string name) print_module_type module_type1
	(* open i *)
	| SgOpn(loc, ident1) -> pp f "<SgOpn loc='%s'><ident>%a</ident></SgOpn>" (string_of_loc loc) print_ident ident1
	(* type t *)
	| SgTyp(loc, ctyp1) -> pp f "<SgTyp loc='%s'><ctyp>%a</ctyp></SgTyp>" (string_of_loc loc) print_ctyp ctyp1
	(* value s : t *)
	| SgVal(loc, name, ctyp1) -> pp f "<SgVal loc='%s'><name>%s</name><ctyp>%a</ctyp></SgVal>" (string_of_loc loc) (escape_string name) print_ctyp ctyp1
	(* $s$ *)
	| SgAnt(loc, name) -> pp f "<SgAnt loc='%s'><name>%s</name></SgAnt>" (string_of_loc loc) (escape_string name)
and print_with_constr f = function (* The type of `with' constraints                             *)
	| WcNil(loc) -> pp f "<WcNil loc='%s'/>" (string_of_loc loc)
	(* type t = t *)
	| WcTyp(loc, ctyp1, ctyp2) -> pp f "<WcTyp loc='%s'><ctyp1>%a</ctyp1><ctyp2>%a</ctyp2></WcTyp>" (string_of_loc loc) print_ctyp ctyp1 print_ctyp ctyp2
	(* module i = i *)
	| WcMod(loc, ident1, ident2) -> pp f "<WcMod loc='%s'><ident1>%a</ident1><ident2>%a</ident2></WcMod>" (string_of_loc loc) print_ident ident1 print_ident ident2
	(* wc, wc *)
	| WcAnd(loc, with_constr1, with_constr2) -> pp f "<WcAnd loc='%s'><with_constr1>%a</with_constr1><with_constr2>%a</with_constr2></WcAnd>" (string_of_loc loc) print_with_constr with_constr1 print_with_constr with_constr2
	(* $s$ *)
	| WcAnt(loc, name) -> pp f "<WcAnt loc='%s'><name>%s</name></WcAnt>" (string_of_loc loc) (escape_string name)
and print_binding f = function (* The type of let bindings                                   *)
	| BiNil(loc) -> pp f "<BiNil loc='%s'/>" (string_of_loc loc)
	(* bi, bi *) (* let a = 42, print_c f = function 43 *)
	| BiAnd(loc, binding1, binding2) -> pp f "<BiAnd loc='%s'><binding1>%a</binding1><binding2>%a</binding2></BiAnd>" (string_of_loc loc) print_binding binding1 print_binding binding2
	(* p = e *) (* let patt = expr *)
	| BiEq(loc, patt1, expr1) -> pp f "<BiEq loc='%s'><patt>%a</patt><expr>%a</expr></BiEq>" (string_of_loc loc) print_patt patt1 print_expr expr1
	(* $s$ *)
	| BiAnt(loc, name) -> pp f "<BiAnt loc='%s'><name>%s</name></BiAnt>" (string_of_loc loc) (escape_string name)
and print_rec_binding f = function (* The type of record definitions                             *)
	(** Empty record definition *)
	| RbNil(loc) -> pp f "<RbNil loc='%s'/>" (string_of_loc loc)
	(* rb ; rb *)
	| RbSem(loc, rec_binding1, rec_binding2) -> pp f "<RbSem loc='%s'><rec_binding1>%a</rec_binding1><rec_binding2>%a</rec_binding2></RbSem>" (string_of_loc loc) print_rec_binding rec_binding1 print_rec_binding rec_binding2
	(* i = e *)
	| RbEq(loc, ident1, expr1) -> pp f "<RbEq loc='%s'><ident>%a</ident><expr>%a</expr></RbEq>" (string_of_loc loc) print_ident ident1 print_expr expr1
	(* $s$ *)
	| RbAnt(loc, name) -> pp f "<RbAnt loc='%s'><name>%s</name></RbAnt>" (string_of_loc loc) (escape_string name)
and print_module_binding f = function (* The type of recursive module definitions                   *)
	(** Empty module definition *)
	| MbNil(loc) -> pp f "<MbNil loc='%s'/>" (string_of_loc loc)
	(* mb, mb *) (* module rec (s : mt) = me, (s : mt) = me *)
	| MbAnd(loc, module_binding1, module_binding2) -> pp f "<MbAnd loc='%s'><module_binding1>%a</module_binding1><module_binding2>%a</module_binding2></MbAnd>" (string_of_loc loc) print_module_binding module_binding1 print_module_binding module_binding2
	(* s : mt = me *)
	| MbColEq(loc, name, module_type1, module_expr1) -> pp f "<MbColEq loc='%s'><name>%s</name><module_type>%a</module_type><module_expr>%a</module_expr></MbColEq>" (string_of_loc loc) (escape_string name) print_module_type module_type1 print_module_expr module_expr1
	(* s : mt *)
	| MbCol(loc, name, module_type1) -> pp f "<MbCol loc='%s'><name>%s</name><module_type>%a</module_type></MbCol>" (string_of_loc loc) (escape_string name) print_module_type module_type1
	(* $s$ *)
	| MbAnt(loc, name) -> pp f "<MbAnt loc='%s'><name>%s</name></MbAnt>" (string_of_loc loc) (escape_string name)
and print_match_case f = function (* The type of cases for match/function/try constructions     *)
	(** Empty case *)
	| McNil(loc) -> pp f "<McNil loc='%s'/>" (string_of_loc loc)
	(* a | a *)
	| McOr(loc, match_case1, match_case2) -> pp f "<McOr loc='%s'><match_case1>%a</match_case1><match_case2>%a</match_case2></McOr>" (string_of_loc loc) print_match_case match_case1 print_match_case match_case2
	(* p (when e)?) -> e *)
	| McArr(loc, patt1, expr1, expr2) -> pp f "<McArr loc='%s'><patt>%a</patt><expr1>%a</expr1><expr2>%a</expr2></McArr>" (string_of_loc loc) print_patt patt1 print_expr expr1 print_expr expr2
	(* $s$ *)
	| McAnt(loc, name) -> pp f "<McAnt loc='%s'><name>%s</name></McAnt>" (string_of_loc loc) (escape_string name)
and print_module_expr f = function (* The type of module expressions                             *)
	(** Empty module expression *)
	| MeNil(loc) -> pp f "<MeNil loc='%s'/>" (string_of_loc loc)
	(* i *)
	| MeId(loc, ident1) -> pp f "<MeId loc='%s'><ident>%a</ident></MeId>" (string_of_loc loc) print_ident ident1
	(* me me *)
	| MeApp(loc, module_expr1, module_expr2) -> pp f "<MeApp loc='%s'><module_expr1>%a</module_expr1><module_expr2>%a</module_expr2></MeApp>" (string_of_loc loc) print_module_expr module_expr1 print_module_expr module_expr2
	(* functor (s : mt)) -> me *)
	| MeFun(loc, name, module_type1, module_expr1) -> pp f "<MeFun loc='%s'><name>%s</name><module_type>%a</module_type><module_expr>%a</module_expr></MeFun>" (string_of_loc loc) (escape_string name) print_module_type module_type1 print_module_expr module_expr1
	(* struct st end *)
	| MeStr(loc, str_item1) -> pp f "<MeStr loc='%s'><str_item>%a</str_item></MeStr>" (string_of_loc loc) print_str_item str_item1
	(* (me : mt) *)
	| MeTyc(loc, module_expr1, module_type1) -> pp f "<MeTyc loc='%s'><module_expr1>%a</module_expr1><module_type1>%a</module_type1></MeTyc>" (string_of_loc loc) print_module_expr module_expr1 print_module_type module_type1
	(* $s$ *)
	| MeAnt(loc, name) -> pp f "<MeAnt loc='%s'><name>%s</name></MeAnt>" (string_of_loc loc) (escape_string name)

and print_str_item f = function (* The type of structure items                                *)
	| StNil(loc) -> pp f "<StNil loc='%s'/>" (string_of_loc loc)
	(* class cice *)
	| StCls(loc, class_expr1) -> pp f "<StCls loc='%s'><class_expr>%a</class_expr></StCls>" (string_of_loc loc) print_class_expr class_expr1
	(* class type cict *)
	| StClt(loc, class_type1) -> pp f "<StClt loc='%s'><class_type>%a</class_type></StClt>" (string_of_loc loc) print_class_type class_type1
	(* st ; st *)
	| StSem(loc, str_item1, str_item2) -> pp f "<StSem loc='%s'><str_item1>%a</str_item1><str_item2>%a</str_item2></StSem>" (string_of_loc loc) print_str_item str_item1 print_str_item str_item2
	(* # s or # s e *)
	| StDir(loc, name, expr1) -> pp f "<StDir loc='%s'><name>%s</name><expr>%a</expr></StDir>" (string_of_loc loc) (escape_string name) print_expr expr1
	(* exception t or exception t = i *)
	| StExc(loc, ctyp1, option_ident) -> pp f "<StExc loc='%s'><ctyp>%a</ctyp><option_ident>%a</option_ident></StExc>" (string_of_loc loc) print_ctyp ctyp1 print_option_ident option_ident
	(* e *)
	| StExp(loc, expr1) -> pp f "<StExp loc='%s'><expr>%a</expr></StExp>" (string_of_loc loc) print_expr expr1
	(* external s : t = s ... s *)
	| StExt(loc, name, ctyp1, (*TODO*) strings(*meta_list string*)) -> pp f "<StExt loc='%s'><name>%s</name><ctyp>%a</ctyp><strings>%a</strings></StExt>" (string_of_loc loc) (escape_string name) print_ctyp ctyp1 print_strings strings
	(* include me *)
	| StInc(loc, module_expr1) -> pp f "<StInc loc='%s'><module_expr>%a</module_expr></StInc>" (string_of_loc loc) print_module_expr module_expr1
	(* module s = me *)
	| StMod(loc, name, module_expr1) -> pp f "<StMod loc='%s'><name>%s</name><module_expr1>%a</module_expr1></StMod>" (string_of_loc loc) (escape_string name) print_module_expr module_expr1
	(* module rec mb *)
	| StRecMod(loc, module_binding1) -> pp f "<StRecMod loc='%s'><module_binding>%a</module_binding></StRecMod>" (string_of_loc loc) print_module_binding module_binding1
	(* module type s = mt *)
	| StMty(loc, name, module_type1) -> pp f "<StMty loc='%s'><name>%s</name><module_type>%a</module_type></StMty>" (string_of_loc loc) (escape_string name) print_module_type module_type1
	(* open i *)
	| StOpn(loc, ident1) -> pp f "<StOpn loc='%s'><ident>%a</ident></StOpn>" (string_of_loc loc) print_ident ident1
	(* type t *)
	| StTyp(loc, ctyp1) -> pp f "<StTyp loc='%s'><ctyp>%a</ctyp></StTyp>" (string_of_loc loc) print_ctyp ctyp1
	(* value (rec)? bi *)
	| StVal(loc, meta_bool1, binding1) -> pp f "<StVal loc='%s'><rec>%a</rec><binding>%a</binding></StVal>" (string_of_loc loc) print_meta_bool meta_bool1 print_binding binding1
	(* $s$ *)
	| StAnt(loc, name) -> pp f "<StAnt loc='%s'><name>%s</name></StAnt>" (string_of_loc loc) (escape_string name)
and print_class_type f = function (* The type of class types                                    *)
	| CtNil(loc) -> pp f "<CtNil loc='%s'/>" (string_of_loc loc)
	(* (virtual)? i ([ t ])? *)
	| CtCon(loc, meta_bool1, ident1, ctyp1) -> pp f "<CtCon loc='%s'><is_virtual>%a</is_virtual><ident>%a</ident><ctyp>%a</ctyp></CtCon>" (string_of_loc loc) print_meta_bool meta_bool1 print_ident ident1 print_ctyp ctyp1
	(* [t]) -> ct *)
	| CtFun(loc, ctyp1, class_type1) -> pp f "<CtFun loc='%s'><ctyp>%a</ctyp><class_type>%a</class_type></CtFun>" (string_of_loc loc) print_ctyp ctyp1 print_class_type class_type1
	(* object ((t))? (csg)? end *)
	| CtSig(loc, ctyp1, class_sig_item1) -> pp f "<CtSig loc='%s'><ctyp>%a</ctyp><class_sig_item>%a</class_sig_item></CtSig>" (string_of_loc loc) print_ctyp ctyp1 print_class_sig_item class_sig_item1
	(* ct, ct *)
	| CtAnd(loc, class_type1, class_type2) -> pp f "<CtAnd loc='%s'><class_type1>%a</class_type1><class_type2>%a</class_type2></CtAnd>" (string_of_loc loc) print_class_type class_type1 print_class_type class_type2
	(* ct : ct *)
	| CtCol(loc, class_type1, class_type2) -> pp f "<CtCol loc='%s'><class_type1>%a</class_type1><class_type2>%a</class_type2></CtCol>" (string_of_loc loc) print_class_type class_type1 print_class_type class_type2
	(* ct = ct *)
	| CtEq(loc, class_type1, class_type2) -> pp f "<CtEq loc='%s'><class_type1>%a</class_type1><class_type2>%a</class_type2></CtEq>" (string_of_loc loc) print_class_type class_type1 print_class_type class_type2
	(* $s$ *)
	| CtAnt(loc, name) -> pp f "<CtAnt loc='%s'><name>%s</name></CtAnt>" (string_of_loc loc) (escape_string name)
and print_class_sig_item f = function (* The type of class signature items                          *)
	| CgNil(loc) -> pp f "<CgNil loc='%s'/>" (string_of_loc loc)
	(* type t = t *)
	| CgCtr(loc, ctyp1, ctyp2) -> pp f "<CgCtr loc='%s'><ctyp1>%a</ctyp1><ctyp2>%a</ctyp2></CgCtr>" (string_of_loc loc) print_ctyp ctyp1 print_ctyp ctyp2
	(* csg ; csg *)
	| CgSem(loc, class_sig_item1, class_sig_item2) -> pp f "<CgSem loc='%s'><class_sig_item1>%a</class_sig_item1><class_sig_item2>%a</class_sig_item2></CgSem>" (string_of_loc loc) print_class_sig_item class_sig_item1 print_class_sig_item class_sig_item2
	(* inherit ct *)
	| CgInh(loc, class_type1) -> pp f "<CgInh loc='%s'><class_type>%a</class_type></CgInh>" (string_of_loc loc) print_class_type class_type1
	(* method s : t or method private s : t *)
	| CgMth(loc, name, meta_bool1, ctyp1) -> pp f "<CgMth loc='%s'><name>%s</name><is_private>%a</is_private><ctyp>%a</ctyp></CgMth>" (string_of_loc loc) (escape_string name) print_meta_bool meta_bool1 print_ctyp ctyp1
	(* value (virtual)? (mutable)? s : t *)
	| CgVal(loc, name, meta_bool1, meta_bool2, ctyp1) -> pp f "<CgVal loc='%s'><name>%s</name><is_virtual>%a</is_virtual><is_mutable>%a</is_mutable><ctyp>%a</ctyp></CgVal>" (string_of_loc loc) (escape_string name) print_meta_bool meta_bool1 print_meta_bool meta_bool2 print_ctyp ctyp1
	(* method virtual (mutable)? s : t *)
	| CgVir(loc, name, meta_bool1, ctyp1) -> pp f "<CgVir loc='%s'><name>%s</name><is_mutable>%a</is_mutable><ctyp>%a</ctyp></CgVir>" (string_of_loc loc) (escape_string name) print_meta_bool meta_bool1 print_ctyp ctyp1
	(* $s$ *)
	| CgAnt(loc, name) -> pp f "<CgAnt loc='%s'><name>%s</name></CgAnt>" (string_of_loc loc) (escape_string name)
and print_class_expr f = function (* The type of class expressions                              *)
	| CeNil(loc) -> pp f "<CeNil loc='%s'/>" (string_of_loc loc)
	(* ce e *)
	| CeApp(loc, class_expr1, expr1) -> pp f "<CeApp loc='%s'><class_expr>%a</class_expr><expr>%a</expr></CeApp>" (string_of_loc loc) print_class_expr class_expr1 print_expr expr1
	(* (virtual)? i ([ t ])? *)
	| CeCon(loc, meta_bool1, ident1, ctyp1) -> pp f "<CeCon loc='%s'><is_virtual>%a</is_virtual><ident>%a</ident><ctyp>%a</ctyp></CeCon>" (string_of_loc loc) print_meta_bool meta_bool1 print_ident ident1 print_ctyp ctyp1
	(* fun p -> ce *)
	| CeFun(loc, patt1, class_expr1) -> pp f "<CeFun loc='%s'><patt>%a</patt><class_expr>%a</class_expr></CeFun>" (string_of_loc loc) print_patt patt1 print_class_expr class_expr1
	(* let (rec)? bi in ce *)
	| CeLet(loc, meta_bool1, binding1, class_expr1) -> pp f "<CeLet loc='%s'><rec>%a</rec><binding>%a</binding><class_expr>%a</class_expr></CeLet>" (string_of_loc loc) print_meta_bool meta_bool1 print_binding binding1 print_class_expr class_expr1
	(* object ((p))? (cst)? end *)
	| CeStr(loc, patt1, class_str_item1) -> pp f "<CeStr loc='%s'><patt>%a</patt><class_str_item>%a</class_str_item></CeStr>" (string_of_loc loc) print_patt patt1 print_class_str_item class_str_item1
	(* ce : ct *)
	| CeTyc(loc, class_expr1, class_type1) -> pp f "<CeTyc loc='%s'><class_expr>%a</class_expr><class_type>%a</class_type></CeTyc>" (string_of_loc loc) print_class_expr class_expr1 print_class_type class_type1
	(* ce, ce *)
	| CeAnd(loc, class_expr1, class_expr2) -> pp f "<CeAnd loc='%s'><class_expr1>%a</class_expr1><class_expr2>%a</class_expr2></CeAnd>" (string_of_loc loc) print_class_expr class_expr1 print_class_expr class_expr2
	(* ce = ce *)
	| CeEq(loc, class_expr1, class_expr2) -> pp f "<CeEq loc='%s'><class_expr1>%a</class_expr1><class_expr2>%a</class_expr2></CeEq>" (string_of_loc loc) print_class_expr class_expr1 print_class_expr class_expr2
	(* $s$ *)
	| CeAnt(loc, name) -> pp f "<CeAnt loc='%s'><name>%s</name></CeAnt>" (string_of_loc loc) (escape_string name)
and print_class_str_item f = function (* The type of class structure items                          *)
	| CrNil(loc) -> pp f "<CrNil loc='%s'/>" (string_of_loc loc)
	(* cst ; cst *)
	| CrSem(loc, class_str_item1, class_str_item2) -> pp f "<CrSem loc='%s'><class_str_item1>%a</class_str_item1><class_str_item2>%a</class_str_item2></CrSem>" (string_of_loc loc) print_class_str_item class_str_item1 print_class_str_item class_str_item2
	(* type t = t *)
	| CrCtr(loc, ctyp1, ctyp2) -> pp f "<CrCtr loc='%s'><ctyp1>%a</ctyp1><ctyp2>%a</ctyp2></CrCtr>" (string_of_loc loc) print_ctyp ctyp1 print_ctyp ctyp2
	(* inherit ce or inherit ce as s *)
	| CrInh(loc, class_expr1, name) -> pp f "<CrInh loc='%s'><class_expr>%a</class_expr><name>%s</name></CrInh>" (string_of_loc loc) print_class_expr class_expr1 (escape_string name)
	(* initializer e *)
	| CrIni(loc, expr1) -> pp f "<CrIni loc='%s'><expr>%a</expr></CrIni>" (string_of_loc loc) print_expr expr1
	(* method (private)? s : t = e or method (private)? s = e *)
	| CrMth(loc, name, meta_bool1, expr1, ctyp1) -> pp f "<CrMth loc='%s'><name>%s</name><is_private>%a</is_private><expr>%a</expr><ctyp>%a</ctyp></CrMth>" (string_of_loc loc) (escape_string name) print_meta_bool meta_bool1 print_expr expr1 print_ctyp ctyp1
	(* value (mutable)? s = e *)
	| CrVal(loc, name, meta_bool1, expr1) -> pp f "<CrVal loc='%s'><name>%s</name><is_mutable>%a</is_mutable><expr>%a</expr></CrVal>" (string_of_loc loc) (escape_string name) print_meta_bool meta_bool1 print_expr expr1
	(* method virtual (private)? s : t *)
	| CrVir(loc, name, meta_bool1, ctyp1) -> pp f "<CrVir loc='%s'><name>%s</name><is_private>%a</is_private><ctyp>%a</ctyp></CrVir>" (string_of_loc loc) (escape_string name) print_meta_bool meta_bool1 print_ctyp ctyp1
	(* value virtual (private)? s : t *)
	| CrVvr(loc, name, meta_bool1, ctyp1) -> pp f "<CrVvr loc='%s'><name>%s</name><is_private>%a</is_private><ctyp>%a</ctyp></CrVvr>" (string_of_loc loc) (escape_string name) print_meta_bool meta_bool1 print_ctyp ctyp1
	(* $s$ *)
	| CrAnt(loc, name) -> pp f "<CrAnt loc='%s'><name>%s</name></CrAnt>" (string_of_loc loc) (escape_string name)

and print_ctyps f = function
	| [] -> pp f ""
	| x:: r -> pp f "<ctyp>%a</ctyp>%a" print_ctyp x print_ctyps r

and print_constraints f = function
	| [] -> pp f ""
	| (ctyp1, ctyp2):: r -> pp f "<ctyp1>%a</ctyp1><ctyp2>%a</ctyp2>%a" print_ctyp ctyp1 print_ctyp ctyp2 print_constraints r

and print_option_ident f = function
	| ONone -> pp f ""
	| OSome(x) -> pp f "<ident>%a</ident>" print_ident x
	| OAnt(str) -> pp f "<antiquotation>%s</antiquotation>" (escape_string str)

let print_ast_in_xml channel =
	match Deserializerp4.deserialize_chan channel with
	| Some parse_tree ->
			print_str_item Format.str_formatter parse_tree;
			Format.flush_str_formatter ()
	| None -> ""

