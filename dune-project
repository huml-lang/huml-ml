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
   (yojson (>= 2.0))
   (alcotest :with-test)))
