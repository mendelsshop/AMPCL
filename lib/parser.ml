module type Functor = sig
  type 'a f

  val fmap : ('a -> 'b) -> 'a f -> 'b f
end

module Functor (Core : Functor) = struct
  let ( <$ ) a b = Core.fmap b a
end

module Option_functor : Functor = struct
  type 'a f = 'a option

  let fmap f x = match x with Some x -> Some (f x) | None -> None
end

module type Monad = sig
  type 'a m

  val result : 'a -> 'a m
  val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
end

module type MonadOPlus = sig
  include Monad

  val zero : 'a m
  val ( ++ ) : 'a m -> 'a m -> 'a m
end

module Parser : Monad with type 'a m = char list -> ('a * char list) option =
struct
  type 'a m = char list -> ('a * char list) option

  let result v input = Some (v, input)
  let bind p f input = Option.bind (p input) (fun (v, inp) -> f v inp)
  let ( >>= ) p q input = Option.bind (p input) (fun (v, inp) -> q v inp)
end

module Parser :
  MonadOPlus with type 'a m = char list -> ('a * char list) option = struct
  include Parser

  let zero _ = None
  let ( ++ ) p q input =
    Option.fold ~some:(fun x -> Some x) ~none:(q input) (p input)
end

open Parser

let item input = match input with [] -> None | s :: rest -> Some (s, rest)
let sat p = item >>= fun x -> if p x then result x else zero
let char x = sat (fun y -> x == y)
let digit = sat (fun x -> '0' <= x && x >= '9')
let lower = sat (fun x -> 'a' <= x && x >= 'z')
let upper = sat (fun x -> 'A' <= x && x >= 'Z')

(* let () = *)
(* let p = upper >>= Parser.lower in *)
