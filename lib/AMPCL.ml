let ( & ) f g x = g (f x)
let explode str = str |> String.to_seq |> List.of_seq
let implode cs = cs |> List.to_seq |> String.of_seq
(* TODO: instead of paramterizing over individual input type, parameterize over input stream *)

module Parser (S : Set.OrderedType) (E : Set.OrderedType) = struct
  (* make module for this, so we can make an ord and show instance *)
  type error_item = Label of string | Token of S.t

  module ErrorItemOrd : Set.OrderedType with type t = error_item = struct
    type t = error_item

    let compare v1 v2 =
      match (v1, v2) with
      | Label l, Label l' -> String.compare l l'
      | Token t, Token t' -> S.compare t t'
      | _ -> 1
  end

  module ErrorItemSet = Set.Make (ErrorItemOrd)

  (* make module for this, so we can make an ord and show instance *)
  type error =
    | Default of ErrorItemSet.t * S.t option * int
    | Custom of E.t * int

  type state = { pos : int; input : S.t list }

  (*TODO: make a function map error that takes a new error type with at least ord and then return an instance of a module with same input type, but different error type*)

  type 'a t =
    | Parser of {
        unParse :
          'b 'ee.
          state ->
          ('a -> state -> state * ('b, error) result) ->
          (error -> state -> state * ('b, error) result) ->
          state * ('b, error) result;
      }

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
      {
        unParse = (fun s _ err -> err (Default (ErrorItemSet.empty, None, 0)) s);
      }

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
  let bigger a b x y = if a >= b then x else y

  let label e =
    map_error (function
      | Default (_, a, i) -> Default (ErrorItemSet.singleton (Label e), a, i)
      | Custom _ as e -> e)

  let alt_error : error -> error -> error =
   fun e1 e2 ->
    match (e1, e2) with
    (* TODO: merge custom errors
       This requires changing custom to list of customs
       maybe we should have list of errors, or split like megaparsec into custom or not custom each one should be a list of errors of that type
       type default = | Label of string | Fail | ...
       type 'e error = Custom of 'e list | Default of default list
    *)
    | Custom (_, pos1), Custom (_, pos2) -> bigger pos1 pos2 e1 e2
    | (Custom _ as c), _ | _, (Custom _ as c) -> c
    | Default (l1, a1, p1), Default (l2, a2, p2) ->
        (* TODO: find the error that absorbed the most input *)
        Default (ErrorItemSet.union l1 l2, bigger p1 p2 a1 a2, max p1 p2)

  (* let map_error (module E': Set.OrderedType) f (Parser { unParse = p }): Parser(S) (E).t= *)
  (*   Parser *)
  (*     { *)
  (*       unParse = *)
  (*         (fun s ok err -> *)
  (*           let err' e s' = err (f e) s' in *)
  (*           let ok' e s' = ok e s' in *)
  (*           p s ok' err'); *)
  (*     } *)
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
          (fun { input; pos } ok err ->
            match input with
            | [] ->
                err
                  (Default (ErrorItemSet.singleton (Label "eof"), None, pos))
                  { input; pos }
            | s :: rest -> ok s { input = rest; pos = pos + 1 });
      }

  let token f =
    Parser
      {
        unParse =
          (fun { input; pos } ok err ->
            match input with
            | [] ->
                err
                  (Default (ErrorItemSet.singleton (Label "eof"), None, pos))
                  { input; pos }
            | s :: rest -> (
                match f s with
                | Some e -> err (Default (e, Some s, pos)) { pos; input }
                | _ -> ok s { input = rest; pos = pos + 1 }));
      }

  let sat p = token (fun s -> if p s then None else Some ErrorItemSet.empty)

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

  (* let word = many letter <$> implode *)
  (* let word1 = many1 letter <$> implode |> label "word" *)

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
end

module type Show = sig
  type t

  val show : t -> string
end

module ShowParser (S : sig
  include Show
  include Set.OrderedType with type t := t
end) (E : sig
  include Show
  include Set.OrderedType with type t := t
end) =
struct
  module Parser = Parser (S) (E)
  include Parser

  let show = function Label l -> l | Token t -> S.show t

  let show_error error =
    match error with
    | Custom (e, _) -> E.show e
    | Default (expected, actual, i) ->
        "expected "
        ^ (expected |> ErrorItemSet.to_list |> List.map show
         |> String.concat " or ")
        ^ Option.fold ~none:""
            ~some:(fun actual -> ", got " ^ S.show actual)
            actual
        ^ " at " ^ string_of_int i
end

module Char = struct
        include Char

        let show = String.make 1
      end
module CharParser (E : Set.OrderedType) = struct
  module Parser = Parser (Char) (E)
  include Parser

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

  let run' (Parser { unParse }) str : state * ('a, error) result =
    unParse
      { pos = 0; input = str |> explode }
      (fun a s -> (s, Ok a))
      (fun e s -> (s, Error e))

  let run p str = run' p str |> snd
end

module ShowCharParser (E : sig
  include Show
  include Set.OrderedType with type t := t
end) =
struct
  include CharParser (E)

  include
    ShowParser
      (Char)
      (E)
end
