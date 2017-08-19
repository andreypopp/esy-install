type t = 
  | Exact of NpmVersion.t
  | Tag of string
  | Range of formula

and formula = rel conjuction disjunction

and 'a conjuction = 'a list
and 'a disjunction = 'a list

and rel =
  | LT of NpmVersion.t
  | LTE of NpmVersion.t
  | GT of NpmVersion.t
  | GTE of NpmVersion.t
  | EQ of NpmVersion.t
  | NEQ of NpmVersion.t

let to_string (c : t) =
  let rel_to_string rel = match rel with
    | LT v -> "< " ^ NpmVersion.to_string v
    | LTE v -> "<= " ^ NpmVersion.to_string v
    | GT v -> "> " ^ NpmVersion.to_string v
    | GTE v -> ">= " ^ NpmVersion.to_string v
    | EQ v -> "= " ^ NpmVersion.to_string v
    | NEQ v -> "!= " ^ NpmVersion.to_string v
  in 
  match c with
  | Exact v -> NpmVersion.to_string v
  | Tag v -> v
  | Range disj ->
    disj
    |> List.map (fun cnj -> cnj |> List.map rel_to_string |> String.concat ", ")
    |> String.concat " || "

let negate_rel = function
  | LT v -> GTE v
  | LTE v -> GT v
  | GT v -> LTE v
  | GTE v -> LT v
  | EQ v -> NEQ v
  | NEQ v -> EQ v

let to_cnf formula = match formula with
  | [] | _::[] -> formula
  | a::b::[] -> formula
