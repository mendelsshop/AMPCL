let ( & ) f g x = g (f x)
let explode str = str |> String.to_seq |> List.of_seq
let implode cs = cs |> List.to_seq |> String.of_seq

module type Show = sig
  type t

  val show : t -> string
end

module type Stream = sig
  type token
  type t

  val take1 : t -> (token * t) option
end

(* TODO: instead of paramterizing over individual input type, parameterize over input stream *)
module Parser = struct
  module type T = sig
    module Stream : Stream

    type e
    type error_item = Label of string | Token of Stream.token

    module ErrorItemSet : Set.S with type elt = error_item

    type state = { pos : int; input : Stream.t }

    type error =
      | Default of ErrorItemSet.t * Stream.token option * int
      | Custom of e * int

    type 'a t =
      | Parser of {
          unParse :
            'b 'ee.
            state ->
            ('a -> state -> state * ('b, error) result) ->
            (error -> state -> state * ('b, error) result) ->
            state * ('b, error) result;
        }

    val label : string -> 'a t -> 'a t
    val sat : (Stream.token -> bool) -> Stream.token t
    val return : 'a -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val ( <$> ) : 'a t -> ('a -> 'b) -> 'b t
    val zero : 'a t
    val item : Stream.token t
    val bind : ('a -> 'b t) -> 'a t -> 'b t
    val ( <|> ) : 'a t -> 'a t -> 'a t
    val alt : 'a t -> 'a t -> 'a t
    val choice : 'a t list -> 'a t
    val seq : 'a t -> 'b t -> ('a * 'b) t
    val ( << ) : 'a t -> 'b t -> 'b t
    val keep_right : 'a t -> 'b t -> 'b t
    val ( >> ) : 'a t -> 'b t -> 'a t
    val keep_left : 'a t -> 'b t -> 'a t
    val between : 'l t -> 'r t -> 'a t -> 'a t
    val sepby : 'a t -> 'b t -> 'a list t
    val sepby1 : 'a t -> 'b t -> 'a list t
    val opt : 'a t -> 'a option t
    val count : int -> 'a t -> 'a list t
    val check : ('a -> bool) -> 'a t -> 'a t
    val many : 'a t -> 'a list t
    val many1 : 'a t -> 'a list t
    val run' : 'a t -> Stream.t -> state * ('a, error) result
    val run : 'a t -> Stream.t -> ('a, error) result
  end

  module Make
      (Stream : Stream)
      (S : Set.OrderedType with type t = Stream.token)
      (E : Set.OrderedType) =
  struct
    module Stream = Stream

    type e = E.t
    type error_item = Label of string | Token of Stream.token

    module ErrorItemSet = Set.Make (struct
      type t = error_item

      let compare v1 v2 =
        match (v1, v2) with
        | Label l, Label l' -> String.compare l l'
        | Token t, Token t' -> S.compare t t'
        | _ -> 1
    end)

    type state = { pos : int; input : Stream.t }

    type error =
      | Default of ErrorItemSet.t * Stream.token option * int
      | Custom of e * int

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
          unParse =
            (fun s _ err -> err (Default (ErrorItemSet.empty, None, 0)) s);
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
              match Stream.take1 input with
              | None ->
                  err
                    (Default (ErrorItemSet.singleton (Label "eof"), None, pos))
                    { input; pos }
              | Some (s, rest) -> ok s { input = rest; pos = pos + 1 });
        }

    let token f =
      Parser
        {
          unParse =
            (fun { input; pos } ok err ->
              match Stream.take1 input with
              | None ->
                  err
                    (Default (ErrorItemSet.singleton (Label "eof"), None, pos))
                    { input; pos }
              | Some (s, rest) -> (
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

    let check predicate p =
      p >>= fun x -> if predicate x then return x else zero

    let run' (Parser { unParse }) input =
      unParse { pos = 0; input }
        (fun a s -> (s, Ok a))
        (fun e s -> (s, Error e))

    let run p str = run' p str |> snd
  end

  module Show = struct
    module type TShow = sig
      include T

      val show_error : error -> string
    end

    module From
        (T : T)
        (S : sig
          include Show with type t = T.Stream.token
          include Set.OrderedType with type t := t
        end)
        (E : sig
          include Show with type t = T.e
          include Set.OrderedType with type t := t
        end) =
    struct
      include T

      let show_error error =
        let show = function Label l -> l | Token t -> S.show t in
        match error with
        | Custom (e, _) -> E.show e
        | Default (expected, actual, i) ->
            "expected "
            ^ (expected |> ErrorItemSet.to_list |> List.map show
             |> String.concat " or ")
            ^ Option.fold ~none:", got eof"
                ~some:(fun actual -> ", got " ^ S.show actual)
                actual
            ^ " at " ^ string_of_int i
    end

    module Make
        (Stream : Stream)
        (S : sig
          include Show with type t = Stream.token
          include Set.OrderedType with type t := t
        end)
        (E : sig
          include Show
          include Set.OrderedType with type t := t
        end) =
    struct
      module T = Make (Stream) (S) (E)
      include From (T) (S) (E)
      include T
    end
  end

  module Char = struct
    module Char = struct
      include Char

      let show = String.make 1
    end

    module StringStream = struct
      type token = char
      type t = string

      let take1 i =
        try Some (String.get i 0, String.sub i 1 (String.length i - 1))
        with Invalid_argument _ -> None
    end

    module CharListStream = struct
      type token = char
      type t = char list

      let take1 = function s :: rest -> Some (s, rest) | [] -> None
    end

    module type CharStream = Stream with type token = char

    module type TChar = sig
      module Stream : CharStream
      include T with module Stream := Stream

      val letter : Stream.token t
      val digit : Stream.token t
      val lower : Stream.token t
      val upper : Stream.token t
      val alphanum : Stream.token t
      val word : string t
      val word1 : string t
      val string : string -> string t
      val char : Stream.token -> Stream.token t
    end

    module Make (Stream : CharStream) (E : Set.OrderedType) = struct
      include Make (Stream) (Char) (E)

      let char x = sat (fun y -> x == y) |> label (String.make 1 x)
      let digit = sat (fun x -> '0' <= x && x <= '9') |> label "digit"

      let lower =
        sat (fun x -> 'a' <= x && x <= 'z') |> label "lower case letter"

      let upper =
        sat (fun x -> 'A' <= x && x <= 'Z') |> label "upper case letter"

      let letter = upper <|> lower |> label "letter"
      let alphanum = letter <|> digit |> label " digit"
      let word = many letter <$> implode
      let word1 = many1 letter <$> implode |> label "word"

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
    end

    module Show = struct
      module type TCharShow = sig
        include TChar

        include
          Show.TShow
            with module Stream := Stream
             and type e := e
             and type error_item := error_item
             and module ErrorItemSet := ErrorItemSet
             and type state := state
             and type error := error
             and type 'a t := 'a t
      end

      module Make
          (Stream : CharStream)
          (E : sig
            include Show
            include Set.OrderedType with type t := t
          end) =
      struct
        module T = Make (Stream) (E)
        include Show.From (T) (Char) (E)
        include T
      end
    end

    module String = struct
      module Make = Make (StringStream)

      module Show = struct
        module Make = Show.Make (StringStream)
      end
    end

    module CharList = struct
      module Make = Make (CharListStream)

      module Show = struct
        module Make = Show.Make (CharListStream)
      end
    end
  end
end
