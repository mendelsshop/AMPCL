let ( & ) f g x = g (f x)
let explode str = str |> String.to_seq |> List.of_seq
let implode cs = cs |> List.to_seq |> String.of_seq

module type Show = sig
  type t

  val show : t -> string
end

type pos = { column : int; line : int }

module Stream = struct
  module type T = sig
    (* TODO: constrain to ord *)
    type token

    (* TODO: constrain to ord *)
    type tokens
    type t

    val take1 : t -> (token * t) option
    val taken : int -> t -> (tokens * t) option

    (* TODO: make takewhile1  *)
    val takeWhile : (token -> bool) -> t -> tokens * t
    val len : tokens -> int
    val toTokens : tokens -> token list
  end

  module Show = struct
    module type TShow = sig
      include T

      val reach_offset : int -> t -> pos * string option
    end
  end

  module CharList = struct
    type token = char
    type t = char list
    type tokens = char list

    let toTokens = Fun.id
    let take1 = function s :: rest -> Some (s, rest) | [] -> None

    let taken n =
      let split n = function
        | s when n <= 0 -> ([], s)
        | s ->
            let rec split n = function
              | [] -> ([], [])
              | s :: xs when n = 1 -> ([ s ], xs)
              | s :: xs ->
                  let s', xs' = split (n - 1) xs in
                  (s :: s', xs')
            in
            split n s
      in
      function
      | s when n <= 0 -> Some ([], s)
      | [] -> None
      | i -> Some (split n i)

    let takeWhile p xs =
      let rec go f xs =
        match xs with
        | x :: xs' when p x -> go (fun rest -> f (x :: rest)) xs'
        | _ -> (f [], xs)
      in
      go Fun.id xs

    let len = List.length

    let reach_offset n input =
      let input = List.to_seq input in
      let line, column, substring, _ =
        Seq.fold_lefti
          (fun (line, column, substring, state) index char ->
            if state = `Done then (line, column, substring, state)
            else if char = '\n' && (state = `Line || n = index) then
              (line, column, substring, `Done)
            else
              (* TODO: is n 0 indexed or not *)
              let state = if n = index then `Line else state in
              let line = if char = '\n' then line + 1 else line in
              let column =
                match char with
                | _ when state = `Line -> column
                | '\n' -> 1
                | '\t' -> column + 4
                | _ -> column + 1
              in
              let substring =
                match char with
                | '\n' -> ""
                | '\t' -> substring ^ "    "
                | _ -> substring ^ String.make 1 char
              in
              (line, column, substring, state))
          (1, 1, "", `Looking) input
      in

      ( { column; line },
        Some (if substring = "" then "<empty line>" else substring) )
  end

  module String = struct
    type token = char
    type t = string
    type tokens = string

    let toTokens = explode

    let take1 : t -> (token * t) option =
     fun i ->
      try Some (String.get i 0, String.sub i 1 (String.length i - 1))
      with Invalid_argument _ -> None

    let taken : int -> t -> (tokens * t) option =
     fun n i ->
      let len = String.length i in
      match n with
      | _ when n <= 0 -> Some (String.empty, i)
      | _ when len = 0 -> None
      | _ ->
          (* TODO: make sure this math is correct *)
          let n, rest = if n > len then (len, 0) else (n, len - n) in
          Some (String.sub i 0 n, String.sub i n rest)

    let takeWhile p i =
      let rec go index =
        match String.get i index with
        | x when p x -> go (index + 1)
        | _ ->
            let len = String.length i in
            (String.sub i 0 index, String.sub i index (len - index))
        | exception Invalid_argument _ -> (i, "")
      in
      go 0

    let reach_offset n i =
      let len = String.length i in
      let line_number =
        let rec inner line start =
          try
            let newline = String.index_from i start '\n' in
            if newline >= n then line else inner (line + 1) (start + newline + 1)
          with Not_found | Invalid_argument _ -> line
        in
        inner 1 0
      in

      let start =
        try
          String.rindex_from_opt i (n - 1) '\n'
          |> Option.fold ~some:(( + ) 1) ~none:0
        with Invalid_argument _ -> 0
      in
      let end_index =
        String.index_from_opt i n '\n' |> Option.fold ~some:Fun.id ~none:len
      in
      let colum_index = n - start in
      let substr = String.sub i start (end_index - start) in
      let substr = substr |> explode in
      let substr = substr |> List.mapi (fun a b -> (a, b)) in
      let colum_index, substr =
        substr
        |> List.fold_left
             (fun (c : int * char list) (i : int * char) ->
               ( (if fst i >= colum_index then fst c
                  else if snd i = '\t' then fst c + 4
                  else fst c + 1),
                 if snd i = '\t' then
                   snd c @ (' ' :: ' ' :: ' ' :: ' ' :: [ ' ' ])
                 else snd c @ [ snd i ] ))
             (0, [])
      in
      let substr = substr |> implode in
      ( { line = line_number; column = colum_index + 1 },
        Some (if substr = "" then "<empty line>" else substr) )

    let len = String.length
  end
end

(* TODO: instead of paramterizing over individual input type, parameterize over input stream *)
module Parser = struct
  module type T = sig
    module Stream : Stream.T

    type e
    type error_item = Label of string | Tokens of Stream.token list | EOF

    module ErrorItemSet : Set.S with type elt = error_item

    type state = { pos : int; input : Stream.t }

    type error =
      | Default of ErrorItemSet.t * error_item option * int
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
    val ( <?> ) : 'a t -> string -> 'a t
    val chunk : Stream.tokens -> Stream.tokens t
    val sat : (Stream.token -> bool) -> Stream.token t
    val return : 'a -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val ( <$> ) : 'a t -> ('a -> 'b) -> 'b t
    val zero : 'a t
    val item : Stream.token t
    val takeWhile : (Stream.token -> bool) -> Stream.tokens t
    val token : (Stream.token -> ErrorItemSet.t option) -> Stream.token t

    val tokens :
      (Stream.tokens -> Stream.tokens -> bool) ->
      Stream.tokens ->
      Stream.tokens t

    val bind : ('a -> 'b t) -> 'a t -> 'b t
    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
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
    val makeRecParser : ('a t -> 'a t) -> 'a t
    val eof : unit t
  end

  module Make
      (Stream : Stream.T)
      (S : Set.OrderedType with type t = Stream.token)
      (E : Set.OrderedType) =
  struct
    module rec Implementation :
      (T with type e = E.t and module Stream = Stream) = struct
      module Stream = Stream

      type e = E.t
      type error_item = Label of string | Tokens of Stream.token list | EOF

      module ErrorItemSet = Set.Make (struct
        type t = error_item

        let compare v1 v2 =
          match (v1, v2) with
          | Label l, Label l' -> String.compare l l'
          | Tokens t, Tokens t' -> List.compare S.compare t t'
          | _ -> 1
      end)

      type state = { pos : int; input : Stream.t }

      type error =
        | Default of ErrorItemSet.t * error_item option * int
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

      let ( <*> ) (Parser { unParse = unParseF })
          (Parser { unParse = unParseP }) =
        Parser
          {
            unParse =
              (fun s ok err ->
                let ok x s' = unParseP s' (x & ok) err in
                unParseF s ok err);
          }

      let bind f p = p >>= f

      let eof =
        Parser
          {
            unParse =
              (fun ({ input; pos } as s) ok err ->
                match Stream.take1 input with
                | None -> ok () s
                | Some (x, _) ->
                    err
                      (Default
                         (ErrorItemSet.singleton EOF, Some (Tokens [ x ]), pos))
                      s);
          }

      let zero =
        Parser
          {
            unParse =
              (fun s _ err -> err (Default (ErrorItemSet.empty, None, 0)) s);
          }

      let tokens f tts =
        Parser
          {
            unParse =
              (fun ({ input; pos } as s) ok err ->
                let unexpected pos' u =
                  let ps =
                    Tokens (tts |> Stream.toTokens) |> ErrorItemSet.singleton
                  in
                  Default (ps, Some u, pos')
                in
                let len = Stream.len tts in
                match Stream.taken len input with
                | None -> err (unexpected pos EOF) s
                | Some (tts', input') ->
                    if f tts tts' then
                      ok tts' { input = input'; pos = pos + len }
                    else
                      let ps = Tokens (tts' |> Stream.toTokens) in
                      err (unexpected pos ps) s);
          }

      let takeWhile p =
        Parser
          {
            unParse =
              (fun { input; pos } ok _ ->
                let tokens, input' = Stream.takeWhile p input in
                let absorbed_len = Stream.len tokens in
                ok tokens { input = input'; pos = pos + absorbed_len });
          }

      let chunk = tokens ( = )
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

      (*TODO: syntax for this*)
      let rec makeRecParser p =
        Parser
          {
            unParse =
              (fun s ok err ->
                let (Parser { unParse }) = p (makeRecParser p) in
                unParse s ok err);
          }

      (*This does not work because, it takes a parser of 'e and returns a parser of 'ee*)
      (*But, we also take a new error ('ee) for the return parse (along with the input) which is fed into the orginal parser which expects 'e (this doesn't work for  base combinators, see item)*)
      (*Maybe just refactor parser definiton*)
      let map_error (f : error -> error) (Parser { unParse = p } : 'a t) =
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

      let ( <?> ) p l = label l p

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
      let rec choice = function
        | [] -> zero
        | fst :: rest -> fst <|> choice rest

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
                    | Some e ->
                        err
                          (Default (e, Some (Tokens [ s ]), pos))
                          { pos; input }
                    | _ -> ok s { input = rest; pos = pos + 1 }));
          }

      let sat p = token (fun s -> if p s then None else Some ErrorItemSet.empty)

      (* TODO: make atleast  *)
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

    include Implementation
  end

  module Show = struct
    module type TShow = sig
      module Stream : Stream.Show.TShow
      include T with module Stream := Stream

      val show_error : error -> string
      val show_input : int -> state -> string
      val run_show : 'a t -> Stream.t -> ('a, string) result
    end

    module From
        (T : T)
        (Stream :
          Stream.Show.TShow
            with type t = T.Stream.t
             and type token = T.Stream.token
             and type tokens = T.Stream.tokens)
        (S : sig
          include Show with type t = T.Stream.token
          include Set.OrderedType with type t := t
        end)
        (E : sig
          include Show with type t = T.e
          include Set.OrderedType with type t := t
        end) =
    struct
      include Stream
      include T

      let get_index = function Default (_, _, i) -> i | Custom (_, i) -> i

      let show_input i input =
        let pos = reach_offset i input in
        string_of_int (fst pos).line
        ^ ":"
        ^ string_of_int (fst pos).column
        ^ ":\n"
        ^ Option.fold
            ~some:(fun s ->
              "  |\n"
              ^ string_of_int (fst pos).line
              ^ " | " ^ s ^ "\n  |"
              ^ String.make (fst pos).column ' '
              ^ "^\n")
            ~none:"" (snd pos)

      let show_error error =
        let show = function
          | Label l -> l
          | Tokens t -> t |> List.map S.show |> String.concat ""
          | EOF -> "eof"
        in
        match error with
        | Custom (e, _) -> E.show e
        | Default (expected, actual, _i) ->
            "expected "
            ^ (expected |> ErrorItemSet.to_list |> List.map show
             |> String.concat " or ")
            ^ Option.fold ~none:"\ngot eof"
                ~some:(fun actual -> "\ngot " ^ show actual)
                actual

      let run_show p input =
        run p input
        |> Result.map_error (fun e ->
               show_input (get_index e) input ^ show_error e)
    end

    module Make
        (Stream : Stream.Show.TShow)
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
      include From (T) (Stream) (S) (E)
      include T
    end
  end

  module Char = struct
    module Char = struct
      include Char

      let show c =
        match c with
        | '\t' -> "<tab>"
        | '\n' -> "<newline>"
        | ' ' -> "<space>"
        | _ -> String.make 1 c
    end

    module type CharStream = Stream.T with type token = char

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

      let char x =
        token (fun y ->
            if x = y then None else Some (ErrorItemSet.singleton (Tokens [ x ])))

      let digit = sat (fun x -> '0' <= x && x <= '9') |> label "digit"

      let lower =
        sat (fun x -> 'a' <= x && x <= 'z') |> label "lower case letter"

      let upper =
        sat (fun x -> 'A' <= x && x <= 'Z') |> label "upper case letter"

      let letter = upper <|> lower |> label "letter"
      let alphanum = letter <|> digit |> label " digit"

      let word =
        takeWhile (fun x -> ('a' <= x && x <= 'z') || ('A' <= x && x <= 'Z'))
        |> label "word"

      let word1 = check (fun w -> Stream.len w > 0) word |> label "word"
      let string = chunk
    end

    module Show = struct
      module type CharStreamShow = Stream.Show.TShow with type token = char

      module type TCharShow = sig
        module Stream : CharStreamShow
        include TChar with module Stream := Stream

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
          (Stream : CharStreamShow)
          (E : sig
            include Show
            include Set.OrderedType with type t := t
          end) =
      struct
        module T = Make (Stream) (E)
        include Show.From (T) (Stream) (Char) (E)
        include T
      end
    end

    module String = struct
      module Make = Make (Stream.String)

      module Show = struct
        module Make = Show.Make (Stream.String)
      end
    end

    module CharList = struct
      module Make = Make (Stream.CharList)

      module Show = struct
        module Make = Show.Make (Stream.CharList)
      end
    end
  end
end
