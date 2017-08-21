type client

val create : unit -> client

val get_packument :
  ?timeout : int ->
  ?follow : bool ->
  ?staleOk : bool ->
  ?auth : unit ->
  ?fullMetadata : bool ->
  client ->
  string ->
  NpmPackument.t option Js.Promise.t
