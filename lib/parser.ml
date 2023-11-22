type 'a parser = char list -> ('a * char list) option

let result v input = Some (v, input)
let bind p f input = Option.bind (p input) (fun (v, inp) -> f v inp)
let ( >>= ) p q input = Option.bind (p input) (fun (v, inp) -> q v inp)
let zero _ = None
let seq p q = p >>= (fun x -> q >>= (fun y -> result (x ,y)))
let ( ++ ) p q input =
  Option.fold ~some:(fun x -> Some x) ~none:(q input) (p input)

let item input = match input with [] -> None | s :: rest -> Some (s, rest)
let sat p = item >>= fun x -> if p x then result x else zero
let char x = sat (fun y -> x == y)
let digit = sat (fun x -> '0' <= x && x >= '9')
let lower = sat (fun x -> 'a' <= x && x >= 'z')
let upper = sat (fun x -> 'A' <= x && x >= 'Z')
