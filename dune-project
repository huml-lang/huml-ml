(lang dune 3.18)
(using menhir 3.0)

(name huml)

(generate_opam_files true)

(source
 (github nikochiko/huml-ml))

(authors "Kaustubh M <ktvm42@gmail.com>")

(maintainers "Kaustubh M <ktvm42@gmail.com>")

(license MIT)

(documentation https://github.com/nikochiko/huml-ml)

(package
 (name huml)
 (synopsis "OCaml parser for the Huml markup language")
 (description "OCaml parser for the Huml markup language")
 (depends
   ocaml
   (alcotest :with-test)
   (yojson :with-test)))

(package
  (name huml-cli)
  (synopsis "Command line interface for Huml parser")
  (description "Command line interface for Huml parser")
  (depends
    ocaml
    huml
    yojson))
