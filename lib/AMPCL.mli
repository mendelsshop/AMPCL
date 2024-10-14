type 'e error = { default : string; custom : 'e option }

type ('s, 'a, 'e, 'ee) parser =
  's list -> 'e error -> ('a * 's list, 'ee error) result

val explode : string -> char list
val implode : char list -> string
val run : (char, 'a, 'e, 'ee) parser -> string -> ('a, 'ee error) result

val ( >>= ) :
  ('s, 'a, 'e, 'ee) parser ->
  ('a -> ('s, 'b, 'e, 'ee) parser) ->
  ('s, 'b, 'e, 'ee) parser

val return : 'a -> ('s, 'a, 'e, 'ee) parser
val label : 'ee -> ('s, 'a, 'e, 'ee) parser -> ('s, 'a, 'e, 'ee) parser
val zero : ('s, 'a, 'e, 'ee) parser
val item : ('a, 'a, 'e, 'ee) parser

val bind :
  ('a -> ('s, 'b, 'e, 'ee) parser) ->
  ('s, 'a, 'e, 'ee) parser ->
  ('s, 'b, 'e, 'ee) parser

val ( <|> ) :
  ('s, 'a, 'e, 'ee) parser ->
  ('s, 'a, 'e, 'ee) parser ->
  ('s, 'a, 'e, 'ee) parser

val alt :
  ('s, 'a, 'e, 'ee) parser ->
  ('s, 'a, 'e, 'ee) parser ->
  ('s, 'a, 'e, 'ee) parser

val choice : ('s, 'a, 'e, 'ee) parser list -> ('s, 'a, 'e, 'ee) parser
val map : ('a -> 'b) -> ('s, 'a, 'e, 'ee) parser -> ('s, 'b, 'e, 'ee) parser
val ( <$> ) : ('s, 'a, 'e, 'ee) parser -> ('a -> 'b) -> ('s, 'b, 'e, 'ee) parser

val seq :
  ('s, 'a, 'e, 'ee) parser ->
  ('s, 'b, 'e, 'ee) parser ->
  ('s, 'a * 'b, 'e, 'ee) parser

val ( << ) :
  ('s, 'a, 'e, 'ee) parser ->
  ('s, 'b, 'e, 'ee) parser ->
  ('s, 'b, 'e, 'ee) parser

val keep_right :
  ('s, 'a, 'e, 'ee) parser ->
  ('s, 'b, 'e, 'ee) parser ->
  ('s, 'b, 'e, 'ee) parser

val ( >> ) :
  ('s, 'a, 'e, 'ee) parser ->
  ('s, 'b, 'e, 'ee) parser ->
  ('s, 'a, 'e, 'ee) parser

val keep_left :
  ('s, 'a, 'e, 'ee) parser ->
  ('s, 'b, 'e, 'ee) parser ->
  ('s, 'a, 'e, 'ee) parser

val between :
  ('s, 'l, 'e, 'ee) parser ->
  ('s, 'r, 'e, 'ee) parser ->
  ('s, 'a, 'e, 'ee) parser ->
  ('s, 'a, 'e, 'ee) parser

val sat : ('a -> bool) -> ('a, 'a, 'e, 'ee) parser
val char : char -> (char, char, 'e, 'ee) parser
val many : ('s, 'a, 'e, 'ee) parser -> ('s, 'a list, 'e, 'ee) parser
val many1 : ('s, 'a, 'e, 'ee) parser -> ('s, 'a list, 'e, 'ee) parser
val string : string -> (char, string, 'e, 'ee) parser
val sepby :
  ('s, 'a, 'e, 'ee) parser ->
  ('s, 'b, 'e, 'ee) parser ->
  ('s, 'a list, 'e, 'ee) parser

val sepby1 :
  ('s, 'a, 'e, 'ee) parser ->
  ('s, 'b, 'e, 'ee) parser ->
  ('s, 'a list, 'e, 'ee) parser

val opt : ('s, 'a, 'e, 'ee) parser -> ('s, 'a option, 'e, 'ee) parser
val count : int -> ('s, 'a, 'e, 'ee) parser -> ('s, 'a list, 'e, 'ee) parser
val check : ('a -> bool) -> ('s, 'a, 'e, 'ee) parser -> ('s, 'a, 'e, 'ee) parser
val letter : (char, char, 'e, 'e) parser
val digit : (char, char, 'e, 'e) parser
val lower : (char, char, 'e, 'e) parser
val upper : (char, char, 'e, 'e) parser

val alphanum : (char, char, 'e, 'ee) parser
val word : (char, string, 'e, 'e) parser

val word1 : (char, string, 'e, 'e) parser
