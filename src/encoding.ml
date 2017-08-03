type t = string

let d_string x = x

let d_float fl =
  Nativeint.to_float fl
  |> string_of_float
  |> d_string

let d_int x =
  Int32.to_int x
  |> string_of_int
  |> d_string
