type 'a parser = char list -> ('a * char list) option

val explode : string -> char list
val result : 'a -> 'a parser
val zero : 'a parser
val item : char parser
val bind : ('a -> 'b parser) ->  'a parser -> 'b parser
val (>>=) : 'a parser -> ('a -> 'b parser) -> 'b parser
val (++) : 'a parser -> 'a parser -> 'a parser
val map :  ('a -> 'b) ->'a parser -> 'b parser
val seq : 'a parser -> 'b parser -> ('a * 'b) parser
val sat : (char -> bool) -> char parser
val char : char -> char parser
val digit : char parser
val lower : char parser
val upper : char parser
val word : string parser
val alphanum : char parser
val string : string -> string parser
val many : 'a parser -> 'a list parser
val many1 : 'a parser -> 'a list parser
val sepby : 'a parser -> 'b parser -> 'a list parser
val sepby1 : 'a parser -> 'b parser -> 'a list parser
val opt : 'a parser -> 'a option parser
