(** Homework 4

    E(PCF)PS(FPC)F as K (stack machine):
      E (numbers and strings),
      PCF (general recursion),
      P (products),
      S (sums),
      FPC (recursive types),
      and F (parametric polymorphism).

    You may simply implement the PCF subset (naturals and general
    recursion) for K.
*)

(**********************************************************************)
(** {1 Utilities} *)

let unimp: string -> 'a = fun s -> failwith ("Unimplemented: " ^ s)

(**********************************************************************)
(** {1 Syntax}

    The pretty-printing functions

      {C [pp_]{i typ}[ : ]{i typ}[ -> string] }

    are defined here in terms of the {!Format} module in the standard
    library. Using the {!Format} module is optional. 
*)

module F = Format
open Base

let pp_of_fmt (f: F.formatter -> 'a -> unit): 'a -> string = fun a ->
  f F.str_formatter a; F.flush_str_formatter ()

type var = string [@@deriving sexp_of, compare, equal]
let f_var = F.pp_print_string
let pp_var: var -> string = fun x -> x

type num = int [@@deriving sexp_of, compare, equal]
let f_num = F.pp_print_int
let pp_num = Int.to_string

type str = string [@@deriving sexp_of, compare, equal]
let f_str = F.pp_print_string
let pp_str: str -> string = fun s -> s

type typvar = string [@@deriving sexp_of, compare, equal]
let f_typvar = F.pp_print_string
let pp_typvar: str -> string = fun s -> s

type typ =
  | TNum
  | TStr
  | Nat
  | Arr of typ * typ
  | Unit
  | Prod of typ * typ
  | Void
  | Sum of typ * typ
  | TVar of typvar
  | TRec of typvar * typ
  | All of typvar * typ
[@@deriving sexp_of, compare, equal] (* automatically derive code *)
let f_typ f = function
  | TNum -> F.fprintf f "num"
  | _ -> unimp "f_typ"
let pp_typ: typ -> string = pp_of_fmt f_typ

type exp =
  | Var of var
  | Num of num
  | Str of str
  | Plus of exp * exp
  | Times of exp * exp
  | Cat of exp * exp
  | Len of exp
  | Let of exp * var * exp
  | Z
  | S of exp
  | IfZ of exp * var * exp * exp
  | Lam of var * typ * exp
  | Ap of exp * exp
  | Fix of var * typ * exp
  | Triv
  | Pair of exp * exp
  | PrL of exp
  | PrR of exp
  | Abort of typ * exp
  | InL of typ * typ * exp
  | InR of typ * typ * exp
  | Case of exp * var * exp * var * exp
  | Fold of typvar * typ * exp
  | Unfold of exp
  | TLam of typvar * exp
  | TApp of exp * typ
[@@deriving sexp_of, compare, equal]
let rec f_exp f =
  function
  | Var x -> F.fprintf f "%a" f_var x
  | Num n -> F.fprintf f "%a" f_num n
  | Plus (e1,e2) -> F.fprintf f "(@[%a@ +@ %a@])" f_exp e1 f_exp e2
  | _ -> unimp "f_exp"
let pp_exp: exp -> string = pp_of_fmt f_exp

(** An alternative to writing custom pretty-printers is to use a generic s-expression printer. *)
let f_sexp: F.formatter -> Ppx_sexp_conv_lib.Sexp.t -> unit =
  Ppx_sexp_conv_lib.Sexp.pp_hum

let pp_exp_sexp e = pp_of_fmt f_sexp (sexp_of_exp e)
let pp_exp = pp_exp_sexp

(**********************************************************************)
(** {1 Values} *)

let is_val: exp -> bool = function
  | Num _ -> true
  | _ -> unimp "is_val"

(**********************************************************************)
(** {1 Typing} *)

type typctx = unit (* TODO: replace *)
let pp_typctx: typctx -> string = fun _ -> "todo"

let emp: typctx = () (* TODO: replace *)
let lookup: typctx -> var -> typ option = fun gamma x -> unimp "lookup"
let extend: typctx -> var -> typ -> typctx = fun gamma x tau -> unimp "extend"

type kindctx = unit (* TODO: replace *)
let pp_kindctx: kindctx -> string = fun _ -> "todo"

let kemp: kindctx = () (* TODO: replace *)
let klookup: kindctx -> typvar -> bool = fun delta t -> unimp "klookup"
let kextend: kindctx -> typvar -> kindctx = fun delta t -> unimp "kextend"

let rec exp_typ: kindctx -> typctx -> exp -> typ option = fun delta gamma ->
  (* Open the Base.Option library for some convenience functions on
     options. Comment out the following line to remove the library
     dependency on Base. *)
  let open Base.Option in
  (* As an alternative, Let_syntax enables the syntax shown below in
     the "Times" case, which is similar to Haskell do notation.  Plus
     and Times cases here are functionally identical, so just choose
     whichever syntax you're more comfortable with.*)
  let open Base.Option.Let_syntax in
  function
  | Num _ -> Some TNum
  | Plus (e1, e2) ->
     exp_typ delta gamma e1 >>= fun tau1 ->
     exp_typ delta gamma e2 >>= fun tau2 ->
     some_if (equal_typ tau1 TNum && equal_typ tau2 TNum) TNum
  | Times (e1, e2) ->
     let%bind tau1 = exp_typ delta gamma e1 in
     let%bind tau2 = exp_typ delta gamma e2 in
     some_if (equal_typ tau1 TNum && equal_typ tau2 TNum) TNum
  | _ -> unimp "exp_typ"

let rec typ_form: kindctx -> typ -> bool = fun delta tau -> unimp "typ_form" 

(**********************************************************************)
(** {1 Substitution} *)

let rec subst: exp -> var -> exp -> exp = fun e' x ->
  function
  (* Be very careful with Var expressions. *)
  | Var y when equal_var x y -> unimp "subst"
  | Var y -> unimp "subst"
  | _ -> unimp "subst"

(**********************************************************************)
(** {1 Evaluation} *)

let rec eval: exp -> exp = fun e ->
  match e with
  | Num _ -> e
  | Plus (e1, e2) ->
     begin match eval e1, eval e2 with
     | Num n1, Num n2 -> Num (n1 + n2)
     | _ -> invalid_arg (pp_exp e)
     end
  | _ -> unimp "eval"

(**********************************************************************)
(** {1 Reduction} *)

let step: exp -> exp = fun e -> unimp "step"
let rec steps_pap: typ -> exp -> exp = fun tau e -> unimp "step_pap"

(**********************************************************************)
(** {1 K (Stack Machine)} *)

type frame = unit (* TODO: replace *) [@@deriving sexp_of, compare, equal]
type stack = frame list [@@deriving sexp_of, compare, equal]

type state =
  | Eval of stack * exp
  | Ret of stack * exp
[@@deriving sexp_of, compare, equal]

let pp_frame(f: frame): string = pp_of_fmt f_sexp (sexp_of_frame f)
let pp_stack(k: stack): string = pp_of_fmt f_sexp (sexp_of_stack k)
let pp_state(s: state): string = pp_of_fmt f_sexp (sexp_of_state s)

let kstep: state -> state = fun s -> unimp "kstep"

let initial(e: exp): state = Eval([], e)
let final(e: exp): state =
  if is_val e then Ret([], e)
  else invalid_arg (pp_exp e)

let is_final(s: state): bool =
  match s with
  | Ret([], e) when is_val e -> true
  | _ -> false

let rec frame_typ: frame -> typ -> typ option = fun f tau -> unimp "frame_typ"
let stack_typ: stack -> typ option = fun s -> unimp "stack_typ"
let rec is_ok: state -> bool = fun s -> unimp "is_ok"

let rec ksteps_pap(s: state): state =
  if not (is_ok s) then F.sprintf "not (is_ok @[%s@])" (pp_state s) |> failwith
  else if is_final s then s
  else let s' = kstep s in ksteps_pap s'
