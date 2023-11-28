type 'a parser = char list -> ('a * char list) option

let ( >> ) f g x = g (f x)
let explode str = str |> String.to_seq |> List.of_seq
let result v input = Some (v, input)
let bind f p input = Option.bind (p input) (fun (v, inp) -> f v inp)
let ( >>= ) p q input = Option.bind (p input) (fun (v, inp) -> q v inp)
let zero _ = None
let map f = bind (f >> result)

let seq p q =
  p >>= fun x ->
  q >>= fun y -> result (x, y)

let ( ++ ) p q input =
  Option.fold ~some:(fun x -> Some x) ~none:(q input) (p input)

let item input = match input with [] -> None | s :: rest -> Some (s, rest)
let sat p = item >>= fun x -> if p x then result x else zero
let char x = sat (fun y -> x == y)
let digit = sat (fun x -> '0' <= x && x <= '9')
let lower = sat (fun x -> 'a' <= x && x <= 'z')
let upper = sat (fun x -> 'A' <= x && x <= 'Z')
let letter = upper ++ lower
let alphanum = letter ++ digit

let rec word input =
  let neWord =
    letter >>= fun x ->
    word >>= fun xs -> result (String.make 1 x ^ xs)
  in
  (neWord ++ result "") input

let string str =
  let rec string_i x =
    match x with
    | [] -> result ""
    | x :: xs ->
        char x >>= fun _ ->
        string_i xs >>= fun xs -> result (String.make 1 x ^ xs)
  in
  let exp_str : char list = List.of_seq (String.to_seq str) in
  string_i exp_str

let rec many parser =
  let neMany =
    parser >>= fun x ->
    many parser >>= fun xs -> result (x :: xs)
  in
  neMany ++ result []

let many1 p =
  p >>= fun x ->
  many p >>= fun xs -> result (x :: xs)

let sepby1 p sep =
  p >>= fun x ->
  many (seq sep p >>= fun (_, x) -> result x) >>= fun xs -> result (x :: xs)

let sepby p sep = sepby1 p sep ++ result []
let opt p = map (fun x -> Some x) p ++ result None
