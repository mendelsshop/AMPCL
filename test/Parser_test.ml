open AMPCL

let error _e =
  let pp f t= Fmt.pf f "@[default=%s]" t.default  in
  let equal { custom; default } { custom = custom'; default = default' } =
    custom = custom' && default = default'
  in
  Alcotest.testable pp equal

let test_char x () =
  Alcotest.(check (result (pair char (list char)) (error string)))
    ("char " ^ String.make 1 x)
    (Ok (x, []))
    ((char x) (x :: []))

let test_digit x is_digit () =
  Alcotest.(check (result (pair char (list char)) (error string)))
    ("digit " ^ String.make 1 x)
    (if is_digit then Ok (x, []) else Error { custom = None; default = "fail" })
    (digit (x :: []))

let test_string x () =
  Alcotest.(check (result (pair string (list char)) (error string)))
    ("string " ^ x)
    (Ok (x, []))
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
  Alcotest.(check (result (pair (float 0.0001) (list char)) (error string)))
    ("float " ^ name) expected
    (number (explode string))

let check_test () =
  Alcotest.(check (result (pair string (list char)) (error string)))
    "check if"
    (Error { custom = None; default = "fail" })
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
          test_case "5.6" `Quick (number_test "5.6" (Ok (5.6, [])) "5.6");
          test_case "." `Quick
            (number_test "invalid no actual number"
               (Error { default = "no input"; custom = None })
               ".");
          test_case "6.7 d  " `Quick
            (number_test "number with trailing stuff"
               (Ok (6.7, [ ' '; 'd'; ' '; ' ' ]))
               "6.7 d  ");
        ] );
    ]
