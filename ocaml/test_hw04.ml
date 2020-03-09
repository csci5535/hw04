open OUnit2

(**********************************************************************)
(** {1 Utilities} *)

let assert_invalid_arg: (unit -> 'a) -> unit = fun test ->
  try
    ignore (test ());
    assert_failure "expected an Invalid_argument exception"
  with Invalid_argument _ -> ()
     | e -> raise e

(**********************************************************************)
(** {1 Interface for Homework} *)

module type HW04 = sig
  type var = string
  val pp_var : var -> string

  type num = int
  val pp_num : num -> string

  type str = string
  val pp_str : str -> string

  type typvar = string [@@deriving sexp_of, compare, equal]
  val pp_typvar : str -> string

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
  val pp_typ : typ -> string

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
  val pp_exp : exp -> string

  val is_val : exp -> bool

  type typctx
  val pp_typctx : typctx -> string

  val emp : typctx
  val lookup : typctx -> var -> typ option
  val extend : typctx -> var -> typ -> typctx

  type kindctx
  val pp_kindctx: kindctx -> string

  val kemp: kindctx
  val klookup: kindctx -> typvar -> bool
  val kextend: kindctx -> typvar -> kindctx

  val exp_typ : kindctx -> typctx -> exp -> typ option
  val typ_form : kindctx -> typ -> bool
  val subst : exp -> var -> exp -> exp
  val eval : exp -> exp
  val step : exp -> exp
  val steps_pap : typ -> exp -> exp

  type frame
  type stack
  type state
  val pp_frame: frame -> string
  val pp_stack: stack -> string
  val pp_state: state -> string

  val kstep: state -> state

  val initial: exp -> state
  val final: exp -> state
  val is_final: state -> bool

  val frame_typ: frame -> typ -> typ option
  val stack_typ: stack -> typ option
  val is_ok: state -> bool

  val ksteps_pap: state -> state
end

(**********************************************************************)
(** {1 Testing} *)

module Make(HW: HW04) = struct

  (********************************************************************)
  (** {2 Utilities} *)

  module F = Format
  let pp_option (pp: 'a -> string): 'a option -> string = function
  | None -> "None"
  | Some a -> F.sprintf "Some @[%a@]" (fun () -> pp) a

  (********************************************************************)
  (** {2 Expressions} *)

  open HW

  let num_1 = Num 1
  let num_2 = Num 2
  let plus_1_1 = Plus (num_1, num_1)

  (********************************************************************)
  (** {2 Tests} *)

  let test_is_val: test = 
    "is_val" >:::
      List.map
        (fun (expected,e) ->
          let lbl = F.sprintf "(is_val %s)" (pp_exp e) in
          let (===) = assert_equal ~printer:string_of_bool in
          lbl >:: (fun _ -> expected === is_val e))
        [
          true, num_1;
          false, plus_1_1;
        ]

  let test_exp_typ: test =
    "exp_typ" >:::
      List.map
        (fun (expected,(delta,gamma,e)) ->
          let lbl = F.sprintf "(exp_typ %s %s %s)"
                      (pp_kindctx delta) (pp_typctx gamma) (pp_exp e)
          in
          let (===) = assert_equal ~printer:(pp_option pp_typ) in
          lbl >:: (fun _ -> expected === exp_typ delta gamma e))
        [
          Some TNum, (kemp, emp, num_1);
          Some TNum, (kemp, emp, plus_1_1);
        ]

  let test_typ_form: test =
    "typ_form" >::: []

  let test_subst: test =
    "subst" >::: []

  let exp_rewrite_tester
        (lblpre: string)
        (f: exp -> exp): exp option * exp -> test = (fun (expected,e) ->
      let lbl = F.sprintf "(%s %s)" lblpre (pp_exp e) in
      let (===) = assert_equal ~printer:pp_exp in
      lbl >:: (fun _ ->
        match expected with
        | None -> assert_invalid_arg (fun () -> f e) 
        | Some e' -> e' === f e))

  let test_step: test =
    "step" >:::
      List.map
        (exp_rewrite_tester "step" step)
        [ ]

  let eval_tests: (exp option * exp) list = [
      Some num_1, num_1;
      Some num_2, plus_1_1;
    ]

  let test_eval: test =
    "eval" >::: List.map (exp_rewrite_tester "eval" eval) eval_tests

  let test_steps_pap: test =
    "steps_pap" >:::
      List.fold_right
        (fun ((_,e) as test) acc ->
          match exp_typ kemp emp e with
          | None -> acc
          | Some tau ->
             let lblpre = "steps_pap " ^ (pp_typ tau) in
             (exp_rewrite_tester lblpre (steps_pap tau) test) :: acc)
        eval_tests
        []

  let test_kstep: test =
    "kstep" >::: []

  let test_frame_typ: test =
    "frame_typ" >::: []

  let test_stack_typ: test =
    "stack_typ" >::: []

  let test_is_ok: test =
    "is_ok" >::: []

  let test_ksteps_pap: test =
    "ksteps_pap" >:::
      List.fold_right
        (fun (expected, e) acc ->
           let lbl = F.sprintf "(ksteps_pap %s)" (pp_exp e) in
           let (===) = assert_equal ~printer:pp_state in
           let doit = fun () -> ksteps_pap (initial e) in
           let test = lbl >:: (fun _ ->
             match expected with
             | None -> assert_invalid_arg doit
             | Some e' -> (final e') === (doit ()))
           in
           test :: acc)
        eval_tests
        []

  let tests =
    "Hw04" >::: [
        test_is_val;
        test_exp_typ;
        test_typ_form;
        test_subst;
        test_step;
        test_eval;
        test_steps_pap;
        test_kstep;
        test_frame_typ;
        test_stack_typ;
        test_is_ok;
        test_ksteps_pap;
      ]
end

let () =
  let module TestHW = Make(Hw04) in
  run_test_tt_main TestHW.tests
