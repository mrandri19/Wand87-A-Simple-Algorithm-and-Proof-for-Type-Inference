type term = TmVar of string | TmApp of term * term | TmLam of string * term

let rec string_of_term term = match term with
  | TmVar(x) -> x
  | TmApp(m,n) -> string_of_term m ^ "" ^ string_of_term n
  | TmLam(x,m) -> "λ" ^ x ^"." ^ string_of_term m

type typ = TyVar of int | TyArr of typ * typ

let rec string_of_typ ty = match ty with
  | TyVar(id) -> Printf.sprintf "t%d" id
  | TyArr(ty1,ty2) -> (string_of_typ ty1) ^ " -> " ^ (string_of_typ ty2)

type equation = Equals of typ * typ

let string_of_equation equation = match equation with
  Equals(ty1,ty2) -> (string_of_typ ty1) ^ " = " ^ (string_of_typ ty2)

type env = (string * typ) list
let emptyEnv = []

let string_of_env env =
    env
    |> List.map (fun (str,ty) -> str ^ ": " ^ string_of_typ ty)
    |> String.concat ", "

(* Global counter which is increased every time a fresh type variable is requested *)
let x: int ref = ref (-1);;
let fresh (): typ =
  incr x;
  TyVar (!x)

let infer (term: term): (equation list* typ) =
  let rec infer_rec (equations: equation list) (goals: (env * term * typ) list): equation list =
  match goals with
    | [] -> equations
    | (env, term, ty)::tl ->
      match term with
      | TmVar(x) ->
        let new_equations = Equals(List.assoc x env, ty)::equations in
        infer_rec new_equations tl
      | TmApp(m,n) ->
        let ty1 = fresh () in
        let new_goals = (env,m,TyArr(ty1,ty))::(env,n,ty1)::tl in
          infer_rec equations new_goals
      | TmLam(x,m) ->
        let ty1 = fresh () in
        let ty2 = fresh () in
        let new_env = (x,ty1)::env in
        let new_goals = (new_env,m,ty2)::tl in
        let new_equations = Equals(ty,TyArr(ty1,ty2))::equations in
          infer_rec new_equations new_goals

  in
    let ty = fresh () in
      (infer_rec [] [(emptyEnv,term,ty)]), ty



let test term =
  let (equations, typ) = infer term in
    print_endline ("("^string_of_term term^")" ^ " has type: " ^ string_of_typ typ);
    print_endline "The generated equations:";

    equations
    |> List.rev
    |> List.map string_of_equation
    |> List.map (fun s -> "    "^s)
    |> String.concat "\n"
    |> print_endline

(* λx.x *)
let () = test (TmLam ("x", (TmVar "x")))

let () = x := -1
(* λx.λy.x *)
let () = test (TmLam ("x", TmLam("y", TmVar("x"))))

let () = x := -1
(* λx.λy.xy *)
let () = test (TmLam ("x", TmLam("y", TmApp(TmVar("x"),TmVar("y")))))

let () = x := -1
(* S combinator *)
(* λx.λy.λz.(xz)(yz)*)
let () = test (TmLam("x", TmLam("y", TmLam("z",TmApp(TmApp(TmVar("x"),TmVar("z")),TmApp(TmVar("y"),TmVar("z")))))))

let () = x := -1
(* Y combinator, with Identity applied to it *)
(* The generated equations are ununifiable *)
let () = test (TmApp(TmLam("x",TmApp(TmVar("x"),TmVar("x"))),TmLam("x",TmApp(TmVar("x"),TmVar("x")))))

