(* open Parser *)

let test_char x () =
  Alcotest.(check  (option (pair char (list char))) )
    ("char " ^ String.make 1 x)
    (Some (x, []))
    ((Parser.char x) (x :: []))

let () = 
    let open Alcotest in 
    run "Parsers" [
        "char", [
            test_case "x" `Quick (test_char 'x');
            test_case "y" `Quick (test_char 'y');
        ]
    ]
