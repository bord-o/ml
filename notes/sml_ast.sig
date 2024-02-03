type fixity
type symbol
val infixleft : int -> fixity
val infixright : int -> fixity
type literal = IntInf.int
type srcpos
type region
type path
type 'a fixitem

datatype 'a sigConst
  = NoSig
  | Transparent of 'a
  | Opaque of 'a
datatype exp
  = VarExp of path
  | FnExp of rule list
  | FlatAppExp of exp fixitem list
  | AppExp of {function : exp, argument : exp}
  | CaseExp of {expr : exp, rules : rule list}
  | LetExp of {dec : dec, expr : exp}
  | SeqExp of exp list
  | IntExp of literal
  | WordExp of literal
  | RealExp of string
  | StringExp of string
  | CharExp of string
  | RecordExp of (symbol * exp) list
  | ListExp of exp list
  | TupleExp of exp list
  | SelectorExp of symbol
  | ConstraintExp of {expr : exp, constraint : ty}
  | HandleExp of {expr : exp, rules : rule list}
  | RaiseExp of exp
  | IfExp of {test : exp, thenCase : exp, elseCase : exp}
  | AndalsoExp of (exp * exp)
  | OrelseExp of (exp * exp)
  | VectorExp of exp list
  | WhileExp of {test : exp, expr : exp}
  | MarkExp of (exp * region)
datatype rule
  = Rule of {pat : pat, exp : exp}
datatype pat
  = WildPat
  | VarPat of path
  | IntPat of literal
  | WordPat of literal
  | StringPat of string
  | CharPat of string
  | RecordPat of {def : (symbol * pat) list, flexibility : bool}
  | ListPat of pat list
  | TuplePat of pat list
  | FlatAppPat of pat fixitem list
  | AppPat of {constr : pat, argument : pat}
  | ConstraintPat of {pattern : pat, constraint : ty}
  | LayeredPat of {varPat : pat, expPat : pat}
  | VectorPat of pat list
  | MarkPat of (pat * region)
  | OrPat of pat list
datatype strexp
  = VarStr of path
  | StructStr of dec
  | ConstrainedStr of (strexp * sigexp sigConst)
  | AppStr of (path * (strexp * bool) list)
  | LetStr of (dec * strexp)
  | MarkStr of (strexp * region)
datatype fctexp
  = VarFct of (path * fsigexp sigConst)
  | FctFct of {params : (symbol option * sigexp) list, body : strexp, constraint : sigexp sigConst}
  | LetFct of (dec * fctexp)
  | AppFct of (path * (strexp * bool) list * fsigexp sigConst)
  | MarkFct of (fctexp * region)
datatype wherespec
  = WhType of (symbol list * tyvar list * ty)
  | WhStruct of (symbol list * symbol list)
datatype sigexp
  = VarSig of symbol
  | AugSig of (sigexp * wherespec list)
  | SigSig of spec list
  | MarkSig of (sigexp * region)
datatype fsigexp
  = VarFsig of symbol
  | FsigFsig of {param : (symbol option * sigexp) list, def : sigexp}
  | MarkFsig of (fsigexp * region)
datatype spec
  = StrSpec of (symbol * sigexp option * path option) list
  | TycSpec of ((symbol * tyvar list * ty option) list * bool)
  | FctSpec of (symbol * fsigexp) list
  | ValSpec of (symbol * ty) list
  | DataSpec of {datatycs : db list, withtycs : tb list}
  | ExceSpec of (symbol * ty option) list
  | FixSpec of {fixity : fixity, ops : symbol list}
  | ShareStrSpec of path list
  | ShareTycSpec of path list
  | IncludeSpec of sigexp
  | MarkSpec of (spec * region)
datatype dec
  = ValDec of (vb list * tyvar list)
  | ValrecDec of (rvb list * tyvar list)
  | FunDec of (fb list * tyvar list)
  | TypeDec of tb list
  | DatatypeDec of {datatycs : db list, withtycs : tb list}
  | AbstypeDec of {abstycs : db list, withtycs : tb list, body : dec}
  | ExceptionDec of eb list
  | StrDec of strb list
  | AbsDec of strb list
  | FctDec of fctb list
  | SigDec of sigb list
  | FsigDec of fsigb list
  | LocalDec of (dec * dec)
  | SeqDec of dec list
  | OpenDec of path list
  | OvldDec of (symbol * ty * exp list)
  | FixDec of {fixity : fixity, ops : symbol list}
  | ImportDec of string list
  | MarkDec of (dec * region)
datatype vb
  = Vb of {pat : pat, exp : exp}
  | LVb of {pat : pat, exp : exp}
  | MarkVb of (vb * region)
datatype rvb
  = Rvb of {var : symbol, fixity : (symbol * region) option, exp : exp, resultty : ty option}
  | LRvb of {var : symbol, fixity : (symbol * region) option, exp : exp, resultty : ty option}
  | MarkRvb of (rvb * region)
datatype fb
  = Fb of clause list
  | LFb of clause list
  | MarkFb of (fb * region)
datatype clause
  = Clause of {pats : pat fixitem list, resultty : ty option, exp : exp}
datatype tb
  = Tb of {tyc : symbol, def : ty, tyvars : tyvar list}
  | MarkTb of (tb * region)
datatype db
  = Db of {tyc : symbol, tyvars : tyvar list, rhs : dbrhs}
  | LDb of {tyc : symbol, tyvars : tyvar list, rhs : dbrhs}
  | MarkDb of (db * region)
datatype dbrhs
  = Constrs of (symbol * ty option) list
  | Repl of symbol list
datatype eb
  = EbGen of {exn : symbol, etype : ty option}
  | EbDef of {exn : symbol, edef : path}
  | MarkEb of (eb * region)
datatype strb
  = Strb of {name : symbol, def : strexp, constraint : sigexp sigConst}
  | MarkStrb of (strb * region)
datatype fctb
  = Fctb of {name : symbol, def : fctexp}
  | MarkFctb of (fctb * region)
datatype sigb
  = Sigb of {name : symbol, def : sigexp}
  | MarkSigb of (sigb * region)
datatype fsigb
  = Fsigb of {name : symbol, def : fsigexp}
  | MarkFsigb of (fsigb * region)
datatype tyvar
  = Tyv of symbol
  | MarkTyv of (tyvar * region)
datatype ty
  = VarTy of tyvar
  | ConTy of (symbol list * ty list)
  | RecordTy of (symbol * ty) list
  | TupleTy of ty list
  | MarkTy of (ty * region) 
