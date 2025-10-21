(lang dune 3.18)
(using menhir 3.0)

(name huml)
(version 0.1.1)

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
   (menhir (>= 20180523))
   (alcotest :with-test)
   (yojson :with-test)))

(package
  (name huml-cli)
  (synopsis "Command line interface for Huml parser")
  (description "Command line interface for Huml parser")
  (depends
    ocaml
    (huml (= :version))
    yojson))

(package
  (name huml-js)
  (synopsis "OCaml-to-JS compilation of Huml with Js_of_ocaml")
  (description "OCaml-to-JS compilation of Huml with Js_of_ocaml")
  (depends
    ocaml
    huml
    js_of_ocaml
    js_of_ocaml-ppx
    yojson))
