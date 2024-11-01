module StringSet = Set.Make (String)

(* TODO: paramaterize over input state and error
   for error it is to be able to constrain it to be able to be used in a set
   for state so that we can get line numbers
   by parameterize I think I mean make this into a module paramaterized over those things
*)
type ('s, 'e) error = Label of StringSet.t * 's option | Custom of 'e

(*We have to error generics, to make map_error work properly*)
type ('s, 'a, 'e) parser =
  | Parser of {
      unParse :
        'b 'ee.
        's list ->
        ('a -> 's list -> 's list * ('b, ('s, 'ee) error) result) ->
        (('s, 'e) error -> 's list -> 's list * ('b, ('s, 'ee) error) result) ->
        's list * ('b, ('s, 'ee) error) result;
    }

let ( & ) f g x = g (f x)

(*let ( >> ) f g x = g (f x)*)
let explode str = str |> String.to_seq |> List.of_seq
let implode cs = cs |> List.to_seq |> String.of_seq

let run' (Parser { unParse }) str : char list * ('a, (char, 'e) error) result =
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
  Parser { unParse = (fun s _ err -> err (Label (StringSet.empty, None)) s) }

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

(* let map_label f = map_error (function Label l -> Label (f l) | e -> e) *)

let label e =
  map_error (function
    | Label (_, a) -> Label (StringSet.singleton e, a)
    | Custom e -> Custom e)

let alt_error : ('s, 'e) error -> ('s, 'e) error -> ('s, 'e) error =
 fun e1 e2 ->
  match (e1, e2) with
  (* TODO: merge custom errors
     This requires changing custom to list of customs
     maybe we should have list of errors, or split like megaparsec into custom or not custom each one should be a list of errors of that type
     type default = | Label of string | Fail | ...
     type 'e error = Custom of 'e list | Default of default list
  *)
  | Custom c, _ | _, Custom c -> Custom c
  | Label (l1, a1), Label (l2, a2) ->
      (* TODO: find the error that absorbed the most input *)
      Label (StringSet.union l1 l2, Option.fold ~none:a2 ~some:(fun _ -> a1) a1)

let ( <|> ) (Parser { unParse = p }) (Parser { unParse = q }) =
  Parser
    {
      unParse =
        (fun s ok err ->
          let error e _ms =
            let nerror e' s' = err (alt_error e e') s' in
            q s ok nerror
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
          | [] -> err (Label (StringSet.singleton "eof", None)) input
          | s :: rest -> ok s rest);
    }

let token f =
  Parser
    {
      unParse =
        (fun input ok err ->
          match input with
          | [] -> err (Label (StringSet.singleton "eof", None)) input
          | s :: rest -> (
              match f s with
              | Some e -> err (Label (e, Some s)) input
              | _ -> ok s rest));
    }

let sat p = token (fun s -> if p s then None else Some StringSet.empty)
let char x = sat (fun y -> x == y) |> label (String.make 1 x)
let digit = sat (fun x -> '0' <= x && x <= '9') |> label "digit"
let lower = sat (fun x -> 'a' <= x && x <= 'z') |> label "lower case letter"
let upper = sat (fun x -> 'A' <= x && x <= 'Z') |> label "upper case letter"
let letter = upper <|> lower |> label "letter"
let alphanum = letter <|> digit |> label " digit"

let string str =
  let rec string_i x =
    match x with
    | [] -> return ""
    | x :: xs ->
        char x >>= fun _ ->
        string_i xs >>= fun xs -> return (String.make 1 x ^ xs)
  in
  let exp_str : char list = List.of_seq (String.to_seq str) in
  string_i exp_str |> label str

(*TODO: better error*)
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
let word1 = many1 letter <$> implode |> label "word"

let sepby1 p sep =
  p >>= fun x ->
  many (sep << p) >>= fun xs -> return (x :: xs)

let sepby p sep = sepby1 p sep <|> return []
let opt p = p <$> (fun x -> Some x) <|> return None

let rec count n p =
  if n = 0 then return []
  else
    p >>= fun x ->
    count (n - 1) p >>= fun xs -> return (x :: xs)

let check predicate p = p >>= fun x -> if predicate x then return x else zero
