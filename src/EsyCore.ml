module StringMap = Map.Make(String)

module Universe = struct
  let empty = StringMap.empty

  let lookup_package pkg univ =
    let res = try
        Some (StringMap.find pkg univ)
      with Not_found ->
        None
    in Option.or_default StringMap.empty res

  (** A mapping from a package name to a list of versions with manifests *)
  type t = NpmManifest.t StringMap.t StringMap.t
end

