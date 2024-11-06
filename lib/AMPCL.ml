let ( & ) f g x = g (f x)
let explode str = str |> String.to_seq |> List.of_seq
let implode cs = cs |> List.to_seq |> String.of_seq

(* TODO: instead of paramterizing over individual input type, parameterize over input stream *)
module type Show = sig
  type t

  val show : t -> string
end

module ErrorItem = struct
  module type T = sig
    type s
    type t = Label of string | Token of s
  end

  module Make (S : sig
    type t
  end) : T with type s = S.t = struct
    type s = S.t
    type t = Label of string | Token of S.t
  end

  module Ord = struct
    module type TOrd = sig
      include T
      include Set.OrderedType with type t := t
    end

    module From (T : T) (S : Set.OrderedType with type t = T.s) :
      TOrd with type s = S.t and type t = T.t = struct
      include T

      let compare v1 v2 =
        match (v1, v2) with
        | Label l, Label l' -> String.compare l l'
        | Token t, Token t' -> S.compare t t'
        | _ -> 1
    end

    module Make (S : Set.OrderedType) = From (Make (S)) (S)
  end

  module Show = struct
    module type TShow = sig
      include T
      include Show with type t := t
    end

    module From (T : T) (S : Show with type t = T.s) :
      TShow with type s = S.t and type t = T.t = struct
      include T

      let show = function Label l -> l | Token t -> S.show t
    end

    module Make (S : Show) = From (Make (S)) (S)
  end
end

(* module Error = struct *)
(*   module type T = sig *)
(*     type s *)
(*     type te = Label of string | Token of s *)
(*     type ts *)
(**)
(*     include ErrorItem.Ord.TOrd with type s = s and type t = te *)
(*     include Set.S with type elt = te and type t = ts *)
(**)
(*     type e *)
(*     type t = Default of ts * te option * int | Custom of e * int *)
(*   end *)
(**)
(*   module Make (S : sig *)
(*     type t *)
(*   end) : T with type s = S.t = struct *)
(*     type s = S.t *)
(*     type t = Label of string | Token of S.t *)
(*   end *)
(**)
(*   module Ord = struct *)
(*     module type TOrd = sig *)
(*       type s *)
(*       type t = Label of string | Token of s *)
(**)
(*       val compare : t -> t -> int *)
(*     end *)
(**)
(*     module From (T : T) (S : Set.OrderedType with type t = T.s) : *)
(*       TOrd with type s = S.t = struct *)
(*       include T *)
(**)
(*       let compare v1 v2 = *)
(*         match (v1, v2) with *)
(*         | Label l, Label l' -> String.compare l l' *)
(*         | Token t, Token t' -> S.compare t t' *)
(*         | _ -> 1 *)
(*     end *)
(**)
(*     module Make (S : Set.OrderedType) : TOrd with type s = S.t = *)
(*       From (Make (S)) (S) *)
(*   end *)
(**)
(*   module Show = struct *)
(*     module type TShow = sig *)
(*       type s *)
(*       type t = Label of string | Token of s *)
(**)
(*       val show : t -> string *)
(*     end *)
(**)
(*     module From (T : T) (S : Show with type t = T.s) : TShow with type s = S.t = *)
(*     struct *)
(*       include T *)
(**)
(*       let show = function Label l -> l | Token t -> S.show t *)
(*     end *)
(**)
(*     module Make (S : Show) : TShow with type s = S.t = From (Make (S)) (S) *)
(*   end *)
(* end *)

module Parser = struct
  module type T = sig
    module S : Set.OrderedType
    module E : Set.OrderedType
    module ErrorItemT : ErrorItem.T with type s = S.t
    module ErrorItemSet : Set.S with type elt = ErrorItemT.t

    type state = { pos : int; input : ErrorItemT.s list }

    type error =
      | Default of ErrorItemSet.t * ErrorItemT.s option * int
      | Custom of E.t * int

    type 'a t =
      | Parser of {
          unParse :
            'b 'ee.
            state ->
            ('a -> state -> state * ('b, error) result) ->
            (error -> state -> state * ('b, error) result) ->
            state * ('b, error) result;
        }
  end

  module type TFull = sig
    include T

    val label : string -> 'a t -> 'a t
    val sat : (ErrorItemT.s -> bool) -> ErrorItemT.s t
    val return : 'a -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val ( <$> ) : 'a t -> ('a -> 'b) -> 'b t
    val zero : 'a t
    val item : S.t t
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
  end

  module MakeFull (T : T) : TFull with module S = T.S = struct
    include T

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

    let check predicate p =
      p >>= fun x -> if predicate x then return x else zero
  end

  module Make
      (S : Set.OrderedType)
      (E : Set.OrderedType)
  (* : TFull with module S = S  *) =
  struct
    (* make module for this, so we can make an ord and show instance *)

    module T : T with module S = S = struct
      module S = S
      module E = E

      module ErrorItemT = struct
        include ErrorItem.Make (S)
        include ErrorItem.Ord.Make (S)
      end

      module ErrorItemSet = Set.Make (ErrorItemT)

      (* make module for this, so we can make an ord and show instance *)

      type state = { pos : int; input : S.t list }

      type error =
        | Default of ErrorItemSet.t * S.t option * int
        | Custom of E.t * int

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
    end

    include MakeFull (T)
  end

  module Show = struct
    module type T = sig
      module S : sig
        include Show
        include Set.OrderedType with type t := t
      end

      module E : sig
        include Show
        include Set.OrderedType with type t := t
      end

      module ErrorItem : sig
        include ErrorItem.Ord.TOrd with type s = S.t
        include Show with type t := t
      end

      (* include *)
      (*   T with module S := S and module E := E and module ErrorItem := ErrorItem *)
    end

    module Make (S : sig
      include Show
      include Set.OrderedType with type t := t
    end) (E : sig
      include Show
      include Set.OrderedType with type t := t
    end) =
    struct
      module S = S
      module E = E

      module ErrorItem : sig
        include ErrorItem.T with type s := S.t
        include Show with type t := t
        include Set.OrderedType with type t := t
      end = struct
        module E = ErrorItem.Make (S)
        include ErrorItem.Ord.From (E) (S)
        include ErrorItem.Show.From (E) (S)
      end

      module ErrorItemSet = Set.Make (ErrorItem)
      include ErrorItem

      (* make module for this, so we can make an ord and show instance *)

      type state = { pos : int; input : S.t list }

      type error =
        | Default of ErrorItemSet.t * S.t option * int
        | Custom of E.t * int

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

      (* include Parser.ErrorItemOrd *)

      (* module ErrorItemOrdShow = struct *)
      (*   module ErrorItemOrd = ErrorItem.Ord.Make (S) *)
      (*   include ErrorItem.Show.From (ErrorItemOrd) (S) *)
      (*   include ErrorItemOrd *)
      (* end *)
      (**)
      (* include Parser *)
      (* module ErrorItemOrdShowSet = Set.Make (ErrorItemOrdShow) *)

      (*   let show_error error = *)
      (*     match error with *)
      (*     | Custom (e, _) -> E.show e *)
      (*     | Default (expected, actual, i) -> *)
      (*         "expected " *)
      (*         ^ (expected |> ErrorItemOrdShowSet.to_list *)
      (*           |> List.map ErrorItemOrdShow.show *)
      (*           |> String.concat " or ") *)
      (*         ^ Option.fold ~none:"" *)
      (*             ~some:(fun actual -> ", got " ^ S.show actual) *)
      (*             actual *)
      (*         ^ " at " ^ string_of_int i *)
    end
  end

  module Char = struct
    include Char

    let show = String.make 1
  end

  module CharParser (E : Set.OrderedType) = struct
    module Parser : TFull with module S := Char
    (* and module ErrorItemT = ErrorItem.Ord.Make(Char) = *) =
      Make (Char) (E)

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
    (* include Show.Make (Char) (E) *)
  end
end
