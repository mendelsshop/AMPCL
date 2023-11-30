type ('s, 'a) parser = 's list -> ('a * 's list) option

val explode : string -> char list
val return : 'a -> ('s, 'a) parser
val zero : ('s, 'a) parser
val item : ('a, 'a) parser
val bind : ('a -> ('s, 'b) parser) -> ('s, 'a) parser -> ('s, 'b) parser
val ( >>= ) : ('s, 'a) parser -> ('a -> ('s, 'b) parser) -> ('s, 'b) parser
val ( <|> ) : ('s, 'a) parser -> ('s, 'a) parser -> ('s, 'a) parser
val alt : ('s, 'a) parser -> ('s, 'a) parser -> ('s, 'a) parser
val choice : ('s, 'a) parser list -> ('s, 'a) parser
val map : ('a -> 'b) -> ('s, 'a) parser -> ('s, 'b) parser
val ( <$> ) : ('s, 'a) parser -> ('a -> 'b) -> ('s, 'b) parser
val seq : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'a * 'b) parser
val ( << ) : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'b) parser
val keep_right : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'b) parser
val ( >> ) : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'a) parser
val keep_left : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'a) parser

val between :
  ('s, 'l) parser -> ('s, 'r) parser -> ('s, 'r) parser -> ('s, 'r) parser

val sat : ('a -> bool) -> ('a, 'a) parser
val char : char -> (char, char) parser
val digit : (char, char) parser
val lower : (char, char) parser
val upper : (char, char) parser
val word : (char, string) parser
val alphanum : (char, char) parser
val string : string -> (char, string) parser
val many : ('s, 'a) parser -> ('s, 'a list) parser
val many1 : ('s, 'a) parser -> ('s, 'a list) parser
val sepby : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'a list) parser
val sepby1 : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'a list) parser
val opt : ('s, 'a) parser -> ('s, 'a option) parser
