# huml-ml

OCaml parser for [HUML](https://huml.io) (Human Markup Language).

## Playground

This parser has been compiled with [`Js_of_ocaml`](https://github.com/ocsigen/Js_of_ocaml)
and deployed here: [kaustubh.page/huml](https://kaustubh.page/huml).

## CLI Usage

Install the `huml-cli` package to use the command-line interface.

```bash
# Parse HUML file and output JSON to stdout
huml input.huml

# Parse HUML file and save JSON to file
huml input.huml -o output.json
huml input.huml --output output.json

# Show help
huml -h
huml --help

# Show version
huml -v
huml --version
```

## Library Usage

Install the `huml` package to use just the library (without the CLI).

```ocaml
open Huml

let parse_huml_string content =
  let lexbuf = Lexing.from_string content in
  match parse lexbuf with
  | Ok ast -> ast
  | Error msg -> failwith msg

(* Example *)
let huml_content = {|
name: "John Doe"
age: 30
active: true
|} in
let ast = parse_huml_string huml_content in
(* ast is of type Types.Ast.t, compatible with Yojson.Safe.t *)
```

The parsed AST is compatible with [`Yojson.Safe.t`](https://ocaml-doc.github.io/odoc-examples/yojson/Yojson/Safe/index.html).

## Installation

```bash
# Build from source
dune build

# Install locally
dune install
```

## License

MIT.
