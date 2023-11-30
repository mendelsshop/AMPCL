type ('s, 'a) parser = 's list -> ('a * 's list) option

let ( >> ) f g x = g (f x)
let explode str = str |> String.to_seq |> List.of_seq
let implode cs = cs |> List.to_seq |> String.of_seq

let return v input = Some (v, input)
let ( >>= ) p q input = Option.bind (p input) (fun (v, inp) -> q v inp)
let bind f p input = Option.bind (p input) (fun (v, inp) -> f v inp)
let zero _ = None
let map f = bind (f >> return)
let ( <$> ) p f = map f p

let seq p q =
  p >>= fun x ->
  q >>= fun y -> return (x, y)

let ( << ) p q = p >>= fun _ -> q
let keep_right = ( << )

let ( >> ) p q =
  p >>= fun r ->
  q >>= fun _ -> return r

let keep_left = ( >> )

let ( <|> ) p q input =
  Option.fold ~some:(fun x -> Some x) ~none:(q input) (p input)

let alt = ( <|> )
let between l r p = l << p >> r
let rec choice = function [] -> zero | fst :: rest -> fst <|> choice rest
let item input = match input with [] -> None | s :: rest -> Some (s, rest)
let sat p = item >>= fun x -> if p x then return x else zero
let char x = sat (fun y -> x == y)
let digit = sat (fun x -> '0' <= x && x <= '9')
let lower = sat (fun x -> 'a' <= x && x <= 'z')
let upper = sat (fun x -> 'A' <= x && x <= 'Z')
let letter = upper <|> lower
let alphanum = letter <|> digit

let rec word input =
  let neWord =
    letter >>= fun x ->
    word >>= fun xs -> return (String.make 1 x ^ xs)
  in
  (neWord <|> return "") input

let string str =
  let rec string_i x =
    match x with
    | [] -> return ""
    | x :: xs ->
        char x >>= fun _ ->
        string_i xs >>= fun xs -> return (String.make 1 x ^ xs)
  in
  let exp_str : char list = List.of_seq (String.to_seq str) in
  string_i exp_str

let rec many parser =
  let neMany =
    parser >>= fun x ->
    many parser >>= fun xs -> return (x :: xs)
  in
  neMany <|> return []

let many1 p =
  p >>= fun x ->
  many p >>= fun xs -> return (x :: xs)

let sepby1 p sep =
  p >>= fun x ->
  many (seq sep p >>= fun (_, x) -> return x) >>= fun xs -> return (x :: xs)

let sepby p sep = sepby1 p sep <|> return []
let opt p = p <$> (fun x -> Some x) <|> return None
