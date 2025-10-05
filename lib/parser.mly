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
%token <string> INT_LIT
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
  | v = multiline_vector
  | v = empty_list
  | v = empty_dict
  | v = scalar
      { v }

scalar:
  | STRING { `String $1 }
  | FLOAT { `Float $1 }
  | INT { `Int $1 }
  | INT_LIT { `Intlit $1 }
  | BOOL { `Bool $1 }
  | NULL { `Null }
  | IDENT {
      let msg = Printf.sprintf "strings must be quoted: %s\nHint: try %S instead?" $1 $1 in
      raise (ParseError (msg, $startpos))
    }
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
  | lv_hd = multiline_list_item; lv_tl = multiline_list_items
      { `List (lv_hd :: lv_tl) }
  ;

multiline_list_items:
  | NEWLINE; v_hd = multiline_list_item; v_tl = multiline_list_items { v_hd :: v_tl }
  | NEWLINE? { [] }
  ;

multiline_list_item:
  | v = preceded(DASH, scalar)
  | v = preceded(DASH, vector_value)
    { v }
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
  | kv_hd = multiline_dict_item; kv_tl = multiline_dict_items { make_assoc (kv_hd :: kv_tl) }
  ;

multiline_dict_items:
  | NEWLINE; hd = multiline_dict_item; tl = multiline_dict_items { hd :: tl }
  | NEWLINE? { [] }
  ;

multiline_dict_item:
  | scalar_key_value { $1 }
  | k = dict_key; v = vector_value { ((k, v), $startpos) }
  ;

vector_value:
  | INLINE_VECTOR_START; v = inline_vector
  | MULTILINE_VECTOR_START; NEWLINE; INDENT; v = multiline_vector; DEDENT { v }
  ;
