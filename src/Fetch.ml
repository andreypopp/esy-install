[%%bs.raw{|
  global.fetch = require('node-fetch');
|}]

include Bs_fetch

let fetch = Bs_fetch.fetch
let fetchWithInit = Bs_fetch.fetchWithInit
let fetchWithRequest = Bs_fetch.fetchWithRequest

