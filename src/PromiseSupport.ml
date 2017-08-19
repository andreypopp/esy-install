module Let_syntax = Ppx_let_promise.Let_syntax

let return = Js.Promise.resolve
let return_some v = Js.Promise.resolve (Some v)
let return_none = Js.Promise.resolve None

let resolve = return
let resolve_some = return_some
let resolve_none = return_none

let bind = Let_syntax.bind
let map = Let_syntax.map

let all promises =
  let module Let_syntax = Ppx_let_promise.Let_syntax in
  let promises = Array.of_list promises in
  let%bind results = Js.Promise.all promises in
  return (Array.to_list results)

