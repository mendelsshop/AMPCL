module Unit = struct
  include Unit

  let show _ = ""
end

module Parser = AMPCL.Parser.Char.String.Show.Make (Unit)
open Parser

let error =
  let pp f t = Fmt.pf f "@[default=%s]" (show_error t) in
  let equal = ( = ) in
  Alcotest.testable pp equal

let test_char x () =
  Alcotest.(check (result char error))
    ("char " ^ String.make 1 x)
    (Ok x)
    (run (char x) (String.make 1 x))

let test_digit x is_digit () =
  Alcotest.(check (result char error))
    ("digit " ^ String.make 1 x)
    (if is_digit then Ok x
     else
       Error
         (Default
            (ErrorItemSet.singleton (Label "digit"), Some (Tokens [ x ]), 0)))
    (run digit (String.make 1 x))

let test_string x () =
  Alcotest.(check (result string error))
    ("string " ^ x) (Ok x)
    (run (string x) x)

let integer_opt = many digit
let integer = many1 digit
let decimal = char '.'

let number =
  let number_opt_dot_number = seq integer_opt (seq decimal integer)
  and number_dot_number_opt = seq integer (seq decimal integer_opt) in
  map
    (fun (f, ((_ : char), s)) ->
      Float.of_string (String.of_seq (List.to_seq (f @ ('.' :: s)))))
    (number_dot_number_opt <|> number_opt_dot_number)
  <?> "float"

let number_test name expected string () =
  Alcotest.(check (result (float 0.0001) error))
    ("float " ^ name) expected (run number string)

let check_test () =
  Alcotest.(check (result string error))
    "check if"
    (Error (Default (ErrorItemSet.empty, None, 0)))
    (run (check (fun x -> x <> "if") word) "if")

let () =
  let open Alcotest in
  run "Parsers"
    [
      ( "digit",
        [
          test_case "1" `Quick (test_digit '1' true);
          test_case "t" `Quick (test_digit 't' false);
        ] );
      ( "char",
        [
          test_case "x" `Quick (test_char 'x');
          test_case "y" `Quick (test_char 'y');
        ] );
      ("string", [ test_case "hi" `Quick (test_string "hi") ]);
      ("check", [ test_case "if" `Quick check_test ]);
      ( "float",
        [
          test_case "5.6" `Quick (number_test "5.6" (Ok 5.6) "5.6");
          test_case "." `Quick
            (number_test "invalid no actual number"
               (Error
                  (Default (ErrorItemSet.singleton (Label "float"), None, 1)))
               ".");
          test_case "6.7 d  " `Quick
            (number_test "number with trailing stuff" (Ok 6.7) "6.7 d  ");
        ] );
    ]
