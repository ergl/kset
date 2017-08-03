type t = string

let d_string x = x

let d_float fl =
  let open Nativeint in
  let (<<) = Nativeint.shift_left in
  let
    a = fl << 56 |> to_string and
    b = fl << 48 |> to_string and
    c = fl << 40 |> to_string and
    d = fl << 32 |> to_string and
    e = fl << 24 |> to_string and
    f = fl << 16 |> to_string and
    g = fl << 8 |> to_string and
    h = fl |> to_string
  in
  d_string @@ a ^ b ^ c ^ d ^ e ^ f ^ g ^ h

let d_int x =
  let open Int32 in
  let (<<) = Int32.shift_left in
  let
    a = x << 24 |> to_string and
    b = x << 16 |> to_string and
    c = x << 8 |> to_string and
    d = x |> to_string
  in
  d_string @@ a ^ b ^ c ^ d
