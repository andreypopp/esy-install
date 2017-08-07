let map v f = match v with
  | Some v -> Some (f v)
  | None -> None

let bind v f = match v with
  | Some v -> f v
  | None -> None

let both a b = match (a, b) with
  | (Some a, Some b) -> Some (a, b)
  | _ -> None

let return v =
  Some v

let or_default default option = match option with
  | Some value -> value
  | None -> default

let is_some v = match v with
  | Some _ -> true
  | None -> false

let is_none v = not (is_some v)

module Let_syntax = struct
  let return = return
  let bind = bind
  let map = map
  let both = both

  module Open_in_body = struct
    let return = return
  end

  module Open_in_rhs = struct
    let return = return
  end
end
