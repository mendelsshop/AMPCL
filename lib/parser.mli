type 'a parser = string -> ('a * string, string) result

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
