let first_char string =
  match string.[0] with char -> Some char | exception _ -> None

let rest_string (input : string) =
  try String.sub input 1 (String.length input - 1)
  with Invalid_argument _ -> ""

let result v input = Ok (v, input)
let zero _ = Error ""

let item input =
  Option.fold (first_char input)
    ~some:(fun first -> Ok (first, rest_string input))
    ~none:(Error input)

let bind p f input = Result.bind (p input) f

let seq p1 p2 =
  bind p1
    (fun first ->
      bind p2 (fun second -> result (first, second)))

(*   Option.fold (first_char input) *)
(*     ~some:(fun first -> *)
(*       if char = first then Ok (char, rest_string input) else Error input) *)
(*     ~none:(Error input) *)
