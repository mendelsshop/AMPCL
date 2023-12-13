open AMPCL

let test_char x () =
  Alcotest.(check (option (pair char (list char))))
    ("char " ^ String.make 1 x)
    (Some (x, []))
    ((char x) (x :: []))

let test_digit x is_digit () =
  Alcotest.(check (option (pair char (list char))))
    ("digit " ^ String.make 1 x)
    (if is_digit then Some (x, []) else None)
    (digit (x :: []))

let test_string x () =
  Alcotest.(check (option (pair string (list char))))
    ("string " ^ x)
    (Some (x, []))
    ((string x) (AMPCL.explode x))

let integer_opt = many AMPCL.digit
let integer = many1 AMPCL.digit
let decimal = char '.'

let number =
  let number_opt_dot_number = seq integer_opt (AMPCL.seq decimal integer)
  and number_dot_number_opt = seq integer (AMPCL.seq decimal integer_opt) in
  map
    (fun (f, ((_ : char), s)) ->
      Float.of_string (String.of_seq (List.to_seq (f @ ('.' :: s)))))
    (number_dot_number_opt <|> number_opt_dot_number)

let number_test name expected string () =
  Alcotest.(check (option (pair (float 0.0001) (list char))))
    ("float " ^ name) expected
    (number (explode string))

let check_test () =
  Alcotest.(check (option (pair string (list char))))
    "check if" None
    (check (fun x -> x <> "if") word (explode "if"))

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
          test_case "5.6" `Quick (number_test "5.6" (Some (5.6, [])) "5.6");
          test_case "." `Quick (number_test "invalid no actual number" None ".");
          test_case "6.7 d  " `Quick
            (number_test "number with trailing stuff"
               (Some (6.7, [ ' '; 'd'; ' '; ' ' ]))
               "6.7 d  ");
        ] );
    ]
