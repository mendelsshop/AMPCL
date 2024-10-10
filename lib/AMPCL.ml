(*TODO: have expected part*)
type 'e error = { default : string; custom : 'e option }
type ('s, 'a, 'e) parser = 's list -> ('a * 's list, 'e error) result

let ( & ) f g x = g (f x)

(*let ( >> ) f g x = g (f x)*)
let explode str = str |> String.to_seq |> List.of_seq
let implode cs = cs |> List.to_seq |> String.of_seq

let run p str =
  (fun (parsed, str') ->
    if List.length str' = 0 then Ok parsed
    else Error { default = "eof"; custom = None })
  |> Result.bind (str |> explode |> p)

let return v input = Ok (v, input)
let ( >>= ) p q input = Result.bind (p input) (fun (v, inp) -> q v inp)
let bind f p input = Result.bind (p input) (fun (v, inp) -> f v inp)
let zero _ = Error { default = "fail"; custom = None }
let map f = bind (f & return)
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
let map_error f p = p & Result.map_error f

let internal_label e =
  map_error (fun { custom; default } -> { custom; default = e default })

let label e =
  map_error (fun { custom = _; default } -> { custom = Some e; default })

let ( <|> ) p q input =
  Result.fold
    ~ok:(fun x -> Ok x)
    ~error:(fun { default = e'; _ } ->
      (q |> internal_label (fun e -> e' ^ " or " ^ e)) input)
    (p input)

let alt = ( <|> )
let between l r p = l << p >> r

(*TODO: make error not contain fail*)
let rec choice = function [] -> zero | fst :: rest -> fst <|> choice rest

let item input =
  match input with
  | [] -> Error { default = "no input"; custom = None }
  | s :: rest -> Ok (s, rest)

let sat p = item >>= fun x -> if p x then return x else zero
let char x = sat (fun y -> x == y) |> internal_label (fun _ -> String.make 1 x)

let digit =
  sat (fun x -> '0' <= x && x <= '9') |> internal_label (fun _ -> "digit")

let lower =
  sat (fun x -> 'a' <= x && x <= 'z')
  |> internal_label (fun _ -> "lower case letter")

let upper =
  sat (fun x -> 'A' <= x && x <= 'Z')
  |> internal_label (fun _ -> "upper case letter")

let letter = upper <|> lower |> internal_label (fun _ -> "letter")
let alphanum = letter <|> digit |> internal_label (fun _ -> " digit")

let string str =
  let rec string_i x =
    match x with
    | [] -> return ""
    | x :: xs ->
        char x >>= fun _ ->
        string_i xs >>= fun xs -> return (String.make 1 x ^ xs)
  in
  let exp_str : char list = List.of_seq (String.to_seq str) in
  string_i exp_str |> internal_label (fun _ -> str)

let rec many parser =
  let neMany =
    parser >>= fun x ->
    many parser >>= fun xs -> return (x :: xs)
  in
  neMany <|> return []

let many1 p =
  p >>= fun x ->
  many p >>= fun xs -> return (x :: xs)

let word = many letter <$> implode
let word1 = many1 letter <$> implode

let sepby1 p sep =
  p >>= fun x ->
  many (seq sep p >>= fun (_, x) -> return x) >>= fun xs -> return (x :: xs)

let sepby p sep = sepby1 p sep <|> return []
let opt p = p <$> (fun x -> Some x) <|> return None

let rec count n p =
  if n = 0 then return []
  else
    p >>= fun x ->
    count (n - 1) p >>= fun xs -> return (x :: xs)

let check predicate p = p >>= fun x -> if predicate x then return x else zero
