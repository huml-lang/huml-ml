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

## BNF

```
(*
tokens:
  - NEWLINE
  - EOF
  - COMMA
  - INDENT
  - DEDENT
  - <string> IDENT
  - <string> STRING
  - <float> FLOAT
  - <int> INT
  - <bool> BOOL
  - NULL
  - EMPTY_LIST
  - EMPTY_DICT
  - SCALAR_START
  - INLINE_VECTOR_START
  - MULTILINE_VECTOR_START
*)

main:
  | NEWLINE root_value NEWLINE EOF
  ;

root_value:
  (* root value needs disambiguating between inline & multiline vectors *)
  | scalar_key_value, ( COMMA scalar_key_value )+  (* inline dict *)
  | scalar, ( COMMA scalar )+  (* inline list *)
  | multiline_vector
  | scalar
  | EMPTY_LIST
  | EMPTY_DICT
  ; 

scalar:
  | STRING
  | FLOAT
  | INT
  | BOOL
  | NULL
  ;

(* vectors *)
vector:
  | inline_vector
  | multiline_vector
  ;

inline_vector:
  | inline_list
  | inline_dict
  ;

multiline_vector:
  | multiline_list
  | multiline_dict
  ;

(* lists *)

inline_list:
  | EMPTY_LIST
  | scalar, ( COMMA scalar )*
  ;

multiline_list:
  (* preceded(NEWLINE, ...) will error on hanging newline at end *)
  | multiline_list_item, multiline_list_items
  ;

multiline_list_items:
  | NEWLINE, multiline_list_item, multiline_list_items
  | NEWLINE?
  ;

multiline_list_item:
  | DASH ( scalar | vector_value )
  ;

(* dicts *)

inline_dict:
  | EMPTY_DICT
  | scalar_key_value, ( COMMA scalar_key_value )*
  ;

multiline_dict:
  | multiline_dict_item, multiline_dict_items

multiline_dict_items:
  | NEWLINE, multiline_dict_item, multiline_dict_items
  | NEWLINE?
  ;

multiline_dict_item:
  | scalar_key_value
  | dict_key, vector_value
  ;

scalar_key_value:
  | dict_key, SCALAR_START, scalar
  ;

dict_key:
  | IDENT | STRING ;

vector_value:
  | INLINE_VECTOR_START, inline_vector
  | MULTILINE_VECTOR_START, NEWLINE, INDENT, multiline_vector, DEDENT
  ;
```

## License

MIT.
