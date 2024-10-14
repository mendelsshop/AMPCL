(*TODO: have expected part*)
type 'e error = { default : string; custom : 'e option }

(*We have to error generics, to make map_error work properly*)
type ('s, 'a, 'e) parser =
  | Parser of {
      unParse :
        'b 'ee.
        's list ->
        ('a -> 's list -> 's list * ('b, 'ee error) result) ->
        ('e error -> 's list -> 's list * ('b, 'ee error) result) ->
        's list * ('b, 'ee error) result;
    }

let ( & ) f g x = g (f x)

(*let ( >> ) f g x = g (f x)*)
let explode str = str |> String.to_seq |> List.of_seq
let implode cs = cs |> List.to_seq |> String.of_seq

let run' (Parser { unParse }) str : char list * ('a, 'e error) result =
  unParse (str |> explode) (fun a s -> (s, Ok a)) (fun e s -> (s, Error e))

let run p str = run' p str |> snd
let return v = Parser { unParse = (fun s ok _ -> ok v s) }

let ( >>= ) (Parser { unParse = unParseP }) q =
  Parser
    {
      unParse =
        (fun s ok err ->
          let mok x s' =
            let (Parser { unParse = unParseQ }) = q x in
            unParseQ s' ok err
          in
          unParseP s mok err);
    }

let bind f p = p >>= f

let zero =
  Parser
    { unParse = (fun s _ err -> err { default = "fail"; custom = None } s) }

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

(*This does not work because, it takes a parser of 'e and returns a parser of 'ee*)
(*But, we also take a new error ('ee) for the return parse (along with the input) which is fed into the orginal parser which expects 'e (this doesn't work for  base combinators, see item)*)
(*Maybe just refactor parser definiton*)
let map_error f (Parser { unParse = p }) =
  Parser
    {
      unParse =
        (fun s ok err ->
          let err' e s' = err (f e) s' in
          let ok' e s' = ok e s' in
          p s ok' err');
    }

let internal_label e =
  map_error (fun { custom; default } -> { custom; default = e default })

let label e =
  map_error (fun { custom = _; default } -> { custom = Some e; default })

let ( <|> ) (Parser { unParse = p }) q =
  Parser
    {
      unParse =
        (fun s ok err ->
          let error { default = e; _ } s =
            let (Parser { unParse = q' }) =
              q |> internal_label (fun e' -> e ^ " or " ^ e')
            in
            q' s ok err
          in
          p s ok error);
    }

let alt = ( <|> )
let between l r p = l << p >> r

(*TODO: make error not contain fail*)
let rec choice = function [] -> zero | fst :: rest -> fst <|> choice rest

(*We cannot make this, be (..., 'e, 'e) parser, because then you cannot map error over it, but since we are plugging in the input error in*)
(*TODO: token parser might be better as it is more primitive / better errors*)
let item =
  Parser
    {
      unParse =
        (fun input ok err ->
          match input with
          | [] -> err { default = "eof"; custom = None } input
          | s :: rest -> ok s rest);
    }

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

(*TODO: better error*)
let rec many parser =
  let neMany =
    parser >>= fun x ->
    many parser >>= fun xs -> return (x :: xs)
  in
  neMany <|> return [] |> internal_label (fun e -> "many " ^ e)

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
