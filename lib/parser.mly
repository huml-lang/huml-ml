%{
open Types

let make_assoc lst =
  let rec check_duplicates acc = function
  (* TODO: would it be nicer to use `Seq` instead of `List`? *)
  | [] -> List.rev acc
  | ((k, _), pos) :: _ when List.exists (fun (k', _) -> k = k') acc ->
      let msg = Printf.sprintf "duplicate key: %S" k in
      raise (ParseError (msg, pos))
  | (kv, _) :: tl -> check_duplicates (kv :: acc) tl
  in
  `Assoc (check_duplicates [] lst)
%}

%token <string> STRING
%token <float> FLOAT
%token <int> INT
%token <bool> BOOL
%token <string> IDENT
%token NEWLINE
%token NULL
%token LIST_EMPTY
%token DICT_EMPTY
%token COMMA
%token DASH
%token INDENT
%token DEDENT
%token SCALAR_START
%token INLINE_VECTOR_START
%token MULTILINE_VECTOR_START
%token EOF

%start <Types.Ast.t> main
%%

main:
  | NEWLINE*; v = root_value; NEWLINE*; EOF { v }
  | EOF { raise (ParseError ("empty input", $startpos)) }
  ;

root_value:
  | kv_hd = scalar_key_value; COMMA; kv_tl = separated_nonempty_list(COMMA, scalar_key_value)
      { make_assoc (kv_hd :: kv_tl) }
  | lst_hd = scalar; COMMA; lst_tl = separated_nonempty_list(COMMA, scalar)
      { `List (lst_hd :: lst_tl) }
  | v = multiline_dict
  | v = multiline_list
  | v = empty_list
  | v = empty_dict
  | v = scalar
      { v }

scalar:
  | STRING { `String $1 }
  | FLOAT { `Float $1 }
  | INT { `Int $1 }
  | BOOL { `Bool $1 }
  | NULL { `Null }
  ;

inline_vector:
  | inline_list
  | inline_dict { $1 }
  ;

multiline_vector:
  | multiline_list
  | multiline_dict { $1 }
  ;

inline_list: 
  | empty_list { $1 }
  | lv = separated_nonempty_list(COMMA, scalar) { `List lv }
  ;

empty_list:
  | LIST_EMPTY { `List [] }
  ;

multiline_list:
  | lv_hd = preceded(DASH, scalar); lv_tl = multiline_list_values { `List (lv_hd :: lv_tl) }
  ;

multiline_list_values:
  | NEWLINE; DASH; v_hd = scalar; v_tl = multiline_list_values { v_hd :: v_tl }
  | NEWLINE; MULTILINE_VECTOR_START; NEWLINE; INDENT; v = multiline_vector; DEDENT { [v] }
  | NEWLINE | { [] }
  ;

inline_dict:
  | empty_dict { $1 }
  | kv_lst = separated_nonempty_list(COMMA, scalar_key_value) { make_assoc kv_lst }
  ;

empty_dict:
  | DICT_EMPTY { `Assoc [] }
  ;

scalar_key_value:
  | k = dict_key; SCALAR_START; v = scalar { ((k, v), $startpos) }
  ;

dict_key:
  | IDENT | STRING { $1 }
  ;

multiline_dict:
  | kv_hd = multiline_dict_value; kv_tl = multiline_dict_values { make_assoc (kv_hd :: kv_tl) }
  ;

multiline_dict_values:
  | NEWLINE; hd = multiline_dict_value; tl = multiline_dict_values { hd :: tl }
  | NEWLINE | { [] }
  ;

multiline_dict_value:
  | k = dict_key; SCALAR_START; v = scalar { ((k, v), $startpos) }
  | k = dict_key; INLINE_VECTOR_START; v = inline_vector { ((k, v), $startpos) }
  | k = dict_key; MULTILINE_VECTOR_START; NEWLINE; INDENT; v = multiline_vector; DEDENT { ((k, v), $startpos) }
  ;