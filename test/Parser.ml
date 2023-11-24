let test_char x () =
  Alcotest.(check (option (pair char (list char))))
    ("char " ^ String.make 1 x)
    (Some (x, []))
    ((Parser.char x) (x :: []))

let explode string = List.of_seq (String.to_seq string)

let test_string x () =
  Alcotest.(check (option (pair string (list char))))
    ("string " ^ x)
    (Some (x, []))
    ((Parser.string x) (explode x))

let () =
  let open Alcotest in
  run "Parsers"
    [
      ( "char",
        [
          test_case "x" `Quick (test_char 'x');
          test_case "y" `Quick (test_char 'y');
        ] );
      ("string", [ test_case "hi" `Quick (test_string "hi") ]);
    ]
