type 'a parser = char list -> ('a * char list) option

val result : 'a -> 'a parser
val zero : 'a parser
val item : char parser
val bind : 'a parser -> ('a -> 'b parser) -> 'b parser
val seq : 'a parser -> 'b parser -> ('a * 'b) parser
val sat : (char -> bool) -> char parser
val char : char -> char parser
val digit : char parser
val lower : char parser
val upper : char parser
val word :  string parser
val alphanum : char parser
val string : string -> string parser
val many: 'a parser -> 'a list parser
