(* pp: -parser OcamlRevised *)
  type loc = Loc.t
   and meta_bool =
    [ BTrue
    | BFalse
    | BAnt of string ]
   and rec_flag =
    [ ReRecursive
    | ReNil
    | ReAnt of string ]
   and direction_flag =
    [ DiTo
    | DiDownto
    | DiAnt of string ]
   and mutable_flag =
    [ MuMutable
    | MuNil
    | MuAnt of string ]
   and private_flag =
    [ PrPrivate
    | PrNil
    | PrAnt of string ]
   and virtual_flag =
    [ ViVirtual
    | ViNil
    | ViAnt of string ]
   and override_flag =
    [ OvOverride
    | OvNil
    | OvAnt of string ]
   and row_var_flag =
    [ RvRowVar
    | RvNil
    | RvAnt of string ]
   and meta_option 'a =
    [ ONone
    | OSome of 'a
    | OAnt of string ]
   and meta_list 'a =
    [ LNil
    | LCons of 'a and meta_list 'a
    | LAnt of string ]
   and ident = (* The type of identifiers (including path like Foo(X).Bar.y) *)
    [ IdAcc of loc and ident and ident (* i . i *) (** Access in module *)
    | IdApp of loc and ident and ident (* i i *) (** Application *)
    | IdLid of loc and string (* foo *) (** Lowercase identifier *)
    | IdUid of loc and string (* Bar *) (** Uppercase identifier *)
    | IdAnt of loc and string (* $s$ *) (** Antiquotation *) ]
   and ctyp = (* Representation of types                                     *)
    [ TyNil of loc (** Empty type *)
    | TyAli of loc and ctyp and ctyp (* t as t *) (* list 'a as 'a *) (** Type aliasing *)
    | TyAny of loc (* _ *) (** Wildcard *)
    | TyApp of loc and ctyp and ctyp (* t t *) (* list 'a *) (** Application *)
    | TyArr of loc and ctyp and ctyp (* t -> t *) (* int -> string *) (** Arrow *)
    | TyCls of loc and ident (* #i *) (* #point *) (** Class type *)
    | TyLab of loc and string and ctyp (* ~s:t *) (** Label type *)
    | TyId  of loc and ident (* i *) (* Lazy.t *) (** Type identifier *)
    | TyMan of loc and ctyp and ctyp (* t == t *) (* type t = [ A | B ] == Foo.t *) (** Type manifest *)
    | TyDcl of loc and string and list ctyp and ctyp and list (ctyp * ctyp) (* type t 'a 'b 'c = t constraint t = t constraint t = t *) (** Type declaration *)
    | TyObj of loc and ctyp and row_var_flag (* < (t)? (..)? > *) (* < move : int -> 'a .. > as 'a  *) (**   Object type *)
    | TyOlb of loc and string and ctyp (* ?s:t *) (** Optional label type *)
    | TyPol of loc and ctyp and ctyp (* ! t . t *) (* ! 'a . list 'a -> 'a *) (** Polymorphic type *)
    | TyQuo of loc and string (* 's *)
    | TyQuP of loc and string (* +'s *)
    | TyQuM of loc and string (* -'s *)
    | TyVrn of loc and string (* `s *) (** Polymorphic variant *)
    | TyRec of loc and ctyp (* { t } *) (* { foo : int ; bar : mutable string } *) (** Record *)
    | TyCol of loc and ctyp and ctyp (* t : t *) (** Field declaration *)
    | TySem of loc and ctyp and ctyp (* t; t *) (** Semicolon-separated type list *)
    | TyCom of loc and ctyp and ctyp (* t, t *) (** Comma-separated type list *)
    | TySum of loc and ctyp (* [ t ] *) (* [ A of int and string | B ] *) (** Sum type *)
    | TyOf  of loc and ctyp and ctyp (* t of t *) (* A of int *)
    | TyAnd of loc and ctyp and ctyp (* t and t *)
    | TyOr  of loc and ctyp and ctyp (* t | t *) (** "Or" pattern between types *)
    | TyPrv of loc and ctyp (* private t *) (** Private type *)
    | TyMut of loc and ctyp (* mutable t *) (** Mutable type *)
    | TyTup of loc and ctyp (* ( t ) *) (* (int * string) *) (** Tuple *)
    | TySta of loc and ctyp and ctyp (* t * t *)
    | TyVrnEq of loc and ctyp (* [ = t ] *)
    | TyVrnSup of loc and ctyp (* [ > t ] *)
    | TyVrnInf of loc and ctyp (* [ < t ] *)
    | TyVrnInfSup of loc and ctyp and ctyp (* [ < t > t ] *)
    | TyAmp of loc and ctyp and ctyp (* t & t *)
    | TyOfAmp of loc and ctyp and ctyp (* t of & t *)
    | TyPkg of loc and module_type (* (module S) (only on trunk) *)
    | TyAnt of loc and string (* $s$ *) (** Antiquotation *)
    ]
   and patt = (* The type of patterns                                       *)
    [ PaNil of loc (**   Empty pattern *)
    | PaId  of loc and ident (* i *) (** Identifier *)
    | PaAli of loc and patt and patt (* p as p *) (* (Node x y as n) *) (** Alias *)
    | PaAnt of loc and string (* $s$ *) (** Antiquotation *)
    | PaAny of loc (* _ *) (** Wildcard *)
    | PaApp of loc and patt and patt (* p p *) (* fun x y -> *) (** Application *)
    | PaArr of loc and patt (* [| p |] *) (** Array *)
    | PaCom of loc and patt and patt (* p, p *) (** Comma-separated pattern list *)
    | PaSem of loc and patt and patt (* p; p *) (** Semicolon-separated pattern list *)
    | PaChr of loc and string (* c *) (* 'x' *) (** Character *)
    | PaInt of loc and string (** Integer *)
    | PaInt32 of loc and string (** Int32 *)
    | PaInt64 of loc and string (** Int64 *)
    | PaNativeInt of loc and string (** NativeInt *)
    | PaFlo of loc and string (** Float *)
    | PaLab of loc and string and patt (* ~s or ~s:(p) *) (** Label *)
    | PaOlb of loc and string and patt (* ?s or ?s:(p) *) (** Optional label *)
    | PaOlbi of loc and string and patt and expr (* ?s:(p = e) or ?(p = e) *) (** Optional label with default value *)
    | PaOrp of loc and patt and patt (* p | p *) (** Or *)
    | PaRng of loc and patt and patt (* p .. p *) (** Pattern range *)
    | PaRec of loc and patt (* { p } *) (** Record *)
    | PaEq  of loc and ident and patt (* i = p *) (** Equality *)
    | PaStr of loc and string (* s *) (** String *)
    | PaTup of loc and patt (* ( p ) *) (** Tuple *)
    | PaTyc of loc and patt and ctyp (* (p : t) *) (** Type constraint *)
    | PaTyp of loc and ident (* #i *)
    | PaVrn of loc and string (* `s *) (** Polymorphic variant *)
    | PaLaz of loc and patt (* lazy p *)
    | PaMod of loc and string (* (module M) *) ]
  and expr = (* The type of expressions                                    *)
    [ ExNil of loc (** Empty expression *)
    | ExId  of loc and ident (* i *) (**   Identifier *)
    | ExAcc of loc and expr and expr (* e.e *) (** Access in module *)
    | ExAnt of loc and string (* $s$ *) (** Antiquotation *)
    | ExApp of loc and expr and expr (* e e *) (** Application *)
    | ExAre of loc and expr and expr (* e.(e) *) (** Array access *)
    | ExArr of loc and expr (* [| e |] *) (** Array declaration *)
    | ExSem of loc and expr and expr (* e; e *) (** Semicolon-separated expression list *)
    | ExAsf of loc (* assert False *) (** assert False *)
    | ExAsr of loc and expr (* assert e *) (** assert e *)
    | ExAss of loc and expr and expr (* e := e *) (** Assignment *)
    | ExChr of loc and string (* 'c' *) (** Character *)
    | ExCoe of loc and expr and ctyp and ctyp (* (e : t) or (e : t :> t) *) (** Coercion *)
    | ExFlo of loc and string (* 3.14 *) (** Float *)
    | ExFor of loc and string and expr and expr and direction_flag and expr (* for s = e to/downto e do { e } *) (** For loop *)
    | ExFun of loc and match_case (* fun [ mc ] *) (** Function with match case *)
    | ExIfe of loc and expr and expr and expr (* if e then e else e *) (** if/then/else *)
    | ExInt of loc and string (* 42 *) (** Int *)
    | ExInt32 of loc and string (** Int32 *)
    | ExInt64 of loc and string (** Int64 *)
    | ExNativeInt of loc and string (** NativeInt *)
    | ExLab of loc and string and expr (* ~s or ~s:e *) (** Label argument with/without expression *)
    | ExLaz of loc and expr (* lazy e *) (** Lazy evaluation *)
    | ExLet of loc and rec_flag and binding and expr (* let b in e or let rec b in e *) (** Let statement with/without recursion *)
    | ExLmd of loc and string and module_expr and expr (* let module s = me in e *) (** "Let module in" construct *)
    | ExMat of loc and expr and match_case (* match e with [ mc ] *) (** Match case *)
    | ExNew of loc and ident (* new i *) (** New object *)
    | ExObj of loc and patt and class_str_item (* object ((p))? (cst)? end *) (** Object declaration *)
    | ExOlb of loc and string and expr (* ?s or ?s:e *) (** Optional label *)
    | ExOvr of loc and rec_binding (* {< rb >} *) (** Overloading *)
    | ExRec of loc and rec_binding and expr (* { rb } or { (e) with rb } *) (** Record *)
    | ExSeq of loc and expr (* do { e } *) (** Sequence with "do" statement *)
    | ExSnd of loc and expr and string (* e#s *) (** Method call *)
    | ExSte of loc and expr and expr (* e.[e] *) (** String access *)
    | ExStr of loc and string (* s *) (* "foo" *) (** String *)
    | ExTry of loc and expr and match_case (* try e with [ mc ] *) (** "Try .. with" construct *)
    | ExTup of loc and expr (* (e) *) (** Tuple *)
    | ExCom of loc and expr and expr (* e, e *) (** Comma-separated expression list *)
    | ExTyc of loc and expr and ctyp (* (e : t) *) (** Type constraint *)
    | ExVrn of loc and string (* `s *) (** Polymorphic variant *)
    | ExWhi of loc and expr and expr (* while e do { e } *) (** "While .. do" constraint *)
    | ExOpI of loc and ident and expr (* let open i in e *)
    | ExFUN of loc and string and expr (* fun (type t) -> e *) (* let f x (type t) y z = e *)
    | ExPkg of loc and module_expr (* (module ME : S) which is represented as (module (ME : S)) *) ]
  and module_type = (* The type of module types                                   *)
    [ MtNil of loc
    | MtId  of loc and ident (* i *) (* A.B.C *)
    | MtFun of loc and string and module_type and module_type (* functor (s : mt) -> mt *)
    | MtQuo of loc and string (* 's *)
    | MtSig of loc and sig_item (* sig sg end *)
    | MtWit of loc and module_type and with_constr (* mt with wc *)
    | MtAnt of loc and string (* $s$ *) ]
  and sig_item = (* The type of signature items                                *)
    [ SgNil of loc
    | SgCls of loc and class_type (* class cict *)
    | SgClt of loc and class_type (* class type cict *)
    | SgSem of loc and sig_item and sig_item (* sg ; sg *)
    | SgDir of loc and string and expr (* # s or # s e *)
    | SgExc of loc and ctyp (* exception t *)
    | SgExt of loc and string and ctyp and meta_list string (* external s : t = s ... s *)
    | SgInc of loc and module_type (* include mt *)
    | SgMod of loc and string and module_type (* module s : mt *)
    | SgRecMod of loc and module_binding (* module rec mb *)
    | SgMty of loc and string and module_type (* module type s = mt *)
    | SgOpn of loc and ident (* open i *)
    | SgTyp of loc and ctyp (* type t *)
    | SgVal of loc and string and ctyp (* value s : t *)
    | SgAnt of loc and string (* $s$ *) ]
  and with_constr = (* The type of `with' constraints                             *)
    [ WcNil of loc
    | WcTyp of loc and ctyp and ctyp (* type t = t *)
    | WcMod of loc and ident and ident (* module i = i *)
    | WcTyS of loc and ctyp and ctyp (* type t := t *)
    | WcMoS of loc and ident and ident (* module i := i *)
    | WcAnd of loc and with_constr and with_constr (* wc and wc *)
    | WcAnt of loc and string (* $s$ *) ]
  and binding = (* The type of let bindings                                   *)
    [ BiNil of loc
    | BiAnd of loc and binding and binding (* bi and bi *) (* let a = 42 and c = 43 *)
    | BiEq  of loc and patt and expr (* p = e *) (* let patt = expr *)
    | BiAnt of loc and string (* $s$ *) ]
  and rec_binding = (* The type of record definitions                             *)
    [ RbNil of loc
    | RbSem of loc and rec_binding and rec_binding (* rb ; rb *)
    | RbEq  of loc and ident and expr (* i = e *)
    | RbAnt of loc and string (* $s$ *) ]
  and module_binding = (* The type of recursive module definitions                   *)
    [ MbNil of loc
    | MbAnd of loc and module_binding and module_binding (* mb and mb *) (* module rec (s : mt) = me and (s : mt) = me *)
    | MbColEq  of loc and string and module_type and module_expr (* s : mt = me *)
    | MbCol  of loc and string and module_type (* s : mt *)
    | MbAnt of loc and string (* $s$ *) ]
  and match_case = (* The type of cases for match/function/try constructions     *)
    [ McNil of loc
    | McOr of loc and match_case and match_case (* a | a *)
    | McArr of loc and patt and expr and expr (* p (when e)? -> e *)
    | McAnt of loc and string (* $s$ *) ]
  and module_expr = (* The type of module expressions                             *)
    [ MeNil of loc
    | MeId  of loc and ident (* i *)
    | MeApp of loc and module_expr and module_expr (* me me *)
    | MeFun of loc and string and module_type and module_expr (* functor (s : mt) -> me *)
    | MeStr of loc and str_item (* struct st end *)
    | MeTyc of loc and module_expr and module_type (* (me : mt) *)
		| MePkg of loc and expr (* (value e) *) (* (value e : S) which is represented as (value (e : S)) *)
    | MeAnt of loc and string (* $s$ *) ]
  and str_item = (* The type of structure items                                *)
    [ StNil of loc
    | StCls of loc and class_expr (* class cice *)
    | StClt of loc and class_type (* class type cict *)
    | StSem of loc and str_item and str_item (* st ; st *)
    | StDir of loc and string and expr (* # s or # s e *)
    | StExc of loc and ctyp and meta_option(*FIXME*) ident (* exception t or exception t = i *)
    | StExp of loc and expr (* e *)
    | StExt of loc and string and ctyp and meta_list string (* external s : t = s ... s *)
    | StInc of loc and module_expr (* include me *)
    | StMod of loc and string and module_expr (* module s = me *)
    | StRecMod of loc and module_binding (* module rec mb *)
    | StMty of loc and string and module_type (* module type s = mt *)
    | StOpn of loc and ident (* open i *)
    | StTyp of loc and ctyp (* type t *)
    | StVal of loc and rec_flag and binding (* value (rec)? bi *)
    | StAnt of loc and string (* $s$ *) ]
  and class_type = (* The type of class types                                    *)
    [ CtNil of loc
    | CtCon of loc and virtual_flag and ident and ctyp (* (virtual)? i ([ t ])? *)
    | CtFun of loc and ctyp and class_type (* [t] -> ct *)
    | CtSig of loc and ctyp and class_sig_item (* object ((t))? (csg)? end *)
    | CtAnd of loc and class_type and class_type (* ct and ct *)
    | CtCol of loc and class_type and class_type (* ct : ct *)
    | CtEq  of loc and class_type and class_type (* ct = ct *)
    | CtAnt of loc and string (* $s$ *) ]
  and class_sig_item = (* The type of class signature items                          *)
    [ CgNil of loc
    | CgCtr of loc and ctyp and ctyp (* type t = t *)
    | CgSem of loc and class_sig_item and class_sig_item (* csg ; csg *)
    | CgInh of loc and class_type (* inherit ct *)
    | CgMth of loc and string and private_flag and ctyp (* method s : t or method private s : t *)
    | CgVal of loc and string and mutable_flag and virtual_flag and ctyp (* value (virtual)? (mutable)? s : t *)
    | CgVir of loc and string and private_flag and ctyp (* method virtual (private)? s : t *)
    | CgAnt of loc and string (* $s$ *) ]
  and class_expr = (* The type of class expressions                              *)
    [ CeNil of loc
    | CeApp of loc and class_expr and expr (* ce e *)
    | CeCon of loc and virtual_flag and ident and ctyp (* (virtual)? i ([ t ])? *)
    | CeFun of loc and patt and class_expr (* fun p -> ce *)
    | CeLet of loc and rec_flag and binding and class_expr (* let (rec)? bi in ce *)
    | CeStr of loc and patt and class_str_item (* object ((p))? (cst)? end *)
    | CeTyc of loc and class_expr and class_type (* ce : ct *)
    | CeAnd of loc and class_expr and class_expr (* ce and ce *)
    | CeEq  of loc and class_expr and class_expr (* ce = ce *)
    | CeAnt of loc and string (* $s$ *) ]
  and class_str_item = (* The type of class structure items                          *)
    [ CrNil of loc
    | CrSem of loc and class_str_item and class_str_item (* cst ; cst *)
    | CrCtr of loc and ctyp and ctyp (* type t = t *)
    | CrInh of loc and override_flag and class_expr and string (* inherit(!)? ce (as s)? *)
    | CrIni of loc and expr (* initializer e *)
    | CrMth of loc and string and override_flag and private_flag and expr and ctyp (* method(!)? (private)? s : t = e or method(!)? (private)? s = e *)
    | CrVal of loc and string and override_flag and mutable_flag and expr (* value(!)? (mutable)? s = e *)
    | CrVir of loc and string and private_flag and ctyp (* method virtual (private)? s : t *)
    | CrVvr of loc and string and mutable_flag and ctyp (* value virtual (mutable)? s : t *)
    | CrAnt of loc and string (* $s$ *) ];
