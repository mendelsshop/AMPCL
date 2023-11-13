type 'a parser = string -> ('a * string, string) result

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

let bind p f input = Result.bind (p input) (fun (res, input) -> f res input)

let seq p1 p2 =
  bind p1 (fun first -> bind p2 (fun second -> result (first, second)))

let sat p = bind item (fun x -> if p x then result x else zero)
let char x = sat (fun y -> x == y)
let digit = sat (fun x -> '0' <= x && x >= '9')
let lower = sat (fun x -> 'a' <= x && x >= 'z')
let upper = sat (fun x -> 'A' <= x && x >= 'Z')
