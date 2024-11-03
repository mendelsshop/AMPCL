module StringSet : Set.S with type elt = string

type ('s, 'e) error =
  | Label of StringSet.t * 's option * int
  | Custom of 'e * int

type 's state = { pos : int; input : 's list }

type ('s, 'a, 'e) parser =
  | Parser of {
      unParse :
        'b 'ee.
        's state ->
        ('a -> 's state -> 's state * ('b, ('s, 'ee) error) result) ->
        (('s, 'e) error -> 's state -> 's state * ('b, ('s, 'ee) error) result) ->
        's state * ('b, ('s, 'ee) error) result;
    }

val explode : string -> char list
val implode : char list -> string

val run' :
  (char, 'a, 'e) parser -> string -> char state * ('a, (char, 'e) error) result

val run : (char, 'a, 'e) parser -> string -> ('a, (char, 'e) error) result

val ( >>= ) :
  ('s, 'a, 'e) parser -> ('a -> ('s, 'b, 'e) parser) -> ('s, 'b, 'e) parser

val map : ('a -> 'b) -> ('s, 'a, 'e) parser -> ('s, 'b, 'e) parser
val ( <$> ) : ('s, 'a, 'e) parser -> ('a -> 'b) -> ('s, 'b, 'e) parser
val label : string -> ('s, 'a, 'e) parser -> ('s, 'a, 'e) parser
val zero : ('s, 'a, 'e) parser
val item : ('a, 'a, 'e) parser

val bind :
  ('a -> ('s, 'b, 'e) parser) -> ('s, 'a, 'e) parser -> ('s, 'b, 'e) parser

val ( <|> ) : ('s, 'a, 'e) parser -> ('s, 'a, 'e) parser -> ('s, 'a, 'e) parser
val alt : ('s, 'a, 'e) parser -> ('s, 'a, 'e) parser -> ('s, 'a, 'e) parser
val choice : ('s, 'a, 'e) parser list -> ('s, 'a, 'e) parser
val seq : ('s, 'a, 'e) parser -> ('s, 'b, 'e) parser -> ('s, 'a * 'b, 'e) parser
val ( << ) : ('s, 'a, 'e) parser -> ('s, 'b, 'e) parser -> ('s, 'b, 'e) parser

val keep_right :
  ('s, 'a, 'e) parser -> ('s, 'b, 'e) parser -> ('s, 'b, 'e) parser

val ( >> ) : ('s, 'a, 'e) parser -> ('s, 'b, 'e) parser -> ('s, 'a, 'e) parser

val keep_left :
  ('s, 'a, 'e) parser -> ('s, 'b, 'e) parser -> ('s, 'a, 'e) parser

val between :
  ('s, 'l, 'e) parser ->
  ('s, 'r, 'e) parser ->
  ('s, 'a, 'e) parser ->
  ('s, 'a, 'e) parser

val sat : ('a -> bool) -> ('a, 'a, 'e) parser
val char : char -> (char, char, 'e) parser
val many : ('s, 'a, 'e) parser -> ('s, 'a list, 'e) parser
val many1 : ('s, 'a, 'e) parser -> ('s, 'a list, 'e) parser
val string : string -> (char, string, 'e) parser

val sepby :
  ('s, 'a, 'e) parser -> ('s, 'b, 'e) parser -> ('s, 'a list, 'e) parser

val sepby1 :
  ('s, 'a, 'e) parser -> ('s, 'b, 'e) parser -> ('s, 'a list, 'e) parser

val opt : ('s, 'a, 'e) parser -> ('s, 'a option, 'e) parser
val count : int -> ('s, 'a, 'e) parser -> ('s, 'a list, 'e) parser
val check : ('a -> bool) -> ('s, 'a, 'e) parser -> ('s, 'a, 'e) parser
val letter : (char, char, 'e) parser
val digit : (char, char, 'e) parser
val lower : (char, char, 'e) parser
val upper : (char, char, 'e) parser
val alphanum : (char, char, 'e) parser
val word : (char, string, 'e) parser
val word1 : (char, string, 'e) parser
