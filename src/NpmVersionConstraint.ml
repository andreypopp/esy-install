module Relation = struct

  type t =
    | LT of NpmVersion.t
    | LTE of NpmVersion.t
    | GT of NpmVersion.t
    | GTE of NpmVersion.t
    | EQ of NpmVersion.t
    | NEQ of NpmVersion.t

  let to_string rel = match rel with
    | LT v -> "< " ^ NpmVersion.to_string v
    | LTE v -> "<= " ^ NpmVersion.to_string v
    | GT v -> "> " ^ NpmVersion.to_string v
    | GTE v -> ">= " ^ NpmVersion.to_string v
    | EQ v -> "= " ^ NpmVersion.to_string v
    | NEQ v -> "!= " ^ NpmVersion.to_string v

end

type t = 
  | Exact of NpmVersion.t
  | Tag of string
  | Range of formula

and formula = Relation.t conjuction disjunction

and 'a conjuction = 'a list
and 'a disjunction = 'a list

let to_string (c : t) =
  match c with
  | Exact v -> NpmVersion.to_string v
  | Tag v -> v
  | Range disj ->
    let wrap_cnj cnj = match disj with
      | [] | _::[] -> cnj
      | _ -> "{" ^ cnj ^ "}"
    in
    disj
    |> List.map (fun cnj -> wrap_cnj (cnj |> List.map Relation.to_string |> String.concat ", "))
    |> String.concat " || "

let to_cnf (dnf : formula) =
  let add_conj cnf conj =
    cnf
    |> List.map (fun cnf_conj -> conj |> List.map (fun r -> r::cnf_conj))
    |> List.flatten
  in
  match dnf with 
  | [] ->
    []
  | conj::dnf ->
    let cnf = List.map (fun r -> [r]) conj in
    List.fold_left add_conj cnf dnf
