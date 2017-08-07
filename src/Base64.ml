let decode : string -> string = [%bs.raw{|
  function Base64__decode(x) {
    return new Buffer(x, 'base64').toString('ascii');
  }
|}]
