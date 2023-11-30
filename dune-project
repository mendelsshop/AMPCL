(lang dune 3.11)

(name AMPCL)

(generate_opam_files true)

(source
 (github mendelsshop/AMPCL))

(authors "mendelsshop")

(maintainers "mendelsshop")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name AMPCL)
 (synopsis "A Monadic Parser Combinator Library")
 (description "A longer description")
 (depends ocaml dune (alcotest :with-test))
 (tags
  (parser parsing "parser combinator" "monadic parser combinator")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
