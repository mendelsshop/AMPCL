let test_char x () =
  Alcotest.(check (option (pair char (list char))))
    ("char " ^ String.make 1 x)
    (Some (x, []))
    ((Parser.char x) (x :: []))

let test_digit x is_digit () =
  Alcotest.(check (option (pair char (list char))))
    ("digit " ^ String.make 1 x)
    (if is_digit then Some (x, []) else None)
    (Parser.digit (x :: []))

let test_string x () =
  Alcotest.(check (option (pair string (list char))))
    ("string " ^ x)
    (Some (x, []))
    ((Parser.string x) (Parser.explode x))

let integer = Parser.many Parser.digit

let number =
  Parser.map
    (fun (f, (_, s)) ->
      Float.of_string (String.of_seq (List.to_seq (f @ ('.' :: s)))))
    (Parser.seq integer (Parser.seq (Parser.char '.') integer))

let number_test () =
  Alcotest.(check (option (pair (float 0.0001) (list char))))
    "float 5.6"
    (Some (5.6, []))
    (number (Parser.explode "5.6"))

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
      ("float", [ test_case "5.6" `Quick number_test ]);
    ]
