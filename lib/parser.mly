%{
  open Ast

  let rec normalize_inlines = function
    | [] -> []
    | Text a :: Text b :: rest -> normalize_inlines (Text (a ^ b) :: rest)
    | x :: rest -> x :: normalize_inlines rest

  let merge_softbreaks parts =
    let rec aux acc = function
      | [] -> List.rev acc
      | `Break :: `Break :: rest -> aux acc rest
      | `Break :: `Inline i :: rest ->
          (match i with
           | Text s when String.length s > 0 && s.[0] = ' ' ->
               aux (i :: acc) rest
           | _ -> aux (Text " " :: i :: acc) rest)
      | `Inline i :: rest -> aux (i :: acc) rest
      | `Break :: rest -> aux (Text " " :: acc) rest
    in
    aux [] parts
%}

%token <string> TEXT
%token <int * string> HEADING
%token <string option> CODE_BLOCK_FENCE
%token <string> CODE_BLOCK
%token <string> CODE_INLINE
%token UNORDERED_LIST_ITEM
%token ORDERED_LIST_ITEM
%token BLOCKQUOTE_START
%token LINK_START
%token IMAGE_START
%token LINK_END
%token LPAREN
%token RPAREN
%token DOUBLE_STAR
%token DOUBLE_UNDERSCORE
%token STAR
%token UNDERSCORE
%token HARD_BREAK
%token NEWLINE
%token SPACED_NEWLINE
%token THEMATIC_BREAK
%token EOF

%start <Ast.doc> doc

%%

let doc :=
  | blocks = list(block); EOF;
    { blocks }

let block :=
  | b = paragraph; NEWLINE?; { b }
  | b = heading; NEWLINE?; { b }
  | b = code_block; NEWLINE?; { b }
  | b = thematic_break; NEWLINE?; { b }
  | b = blockquote; NEWLINE?; { b }
  | b = list_block; NEWLINE?; { b }

let heading :=
  | h = HEADING;
    { let (level, content) = h in
      Heading { level; content = [Text content] } }

let paragraph :=
  | parts = nonempty_list(inline_or_softbreak);
    { let inlines = merge_softbreaks parts in
      Paragraph (normalize_inlines inlines) }

let inline_or_softbreak :=
  | SPACED_NEWLINE; { `Break }
  | i = inline; { `Inline i }

let inline :=
  | t = TEXT; { Text t }
  | t = CODE_INLINE; { Code t }
  | DOUBLE_STAR; content = nonempty_list(plain_inline); DOUBLE_STAR;
    { Bold (normalize_inlines content) }
  | DOUBLE_UNDERSCORE; content = nonempty_list(plain_inline); DOUBLE_UNDERSCORE;
    { Bold (normalize_inlines content) }
  | STAR; content = nonempty_list(plain_inline); STAR;
    { Italic (normalize_inlines content) }
  | UNDERSCORE; content = nonempty_list(plain_inline); UNDERSCORE;
    { Italic (normalize_inlines content) }
  | LINK_START; text = link_text; LINK_END; LPAREN; url = TEXT; RPAREN;
    { Link { text = normalize_inlines text; url = String.trim url } }
  | HARD_BREAK;
    { Break }

let plain_inline :=
  | t = TEXT; { Text t }
  | t = CODE_INLINE; { Code t }
  | LINK_START; text = link_text; LINK_END; LPAREN; url = TEXT; RPAREN;
    { Link { text = normalize_inlines text; url = String.trim url } }
  | HARD_BREAK;
    { Break }

let link_text :=
  | parts = list(link_text_part);
    { List.flatten parts }

let link_text_part :=
  | t = TEXT; { [Text t] }
  | t = CODE_INLINE; { [Code t] }

let code_block :=
  | lang = CODE_BLOCK_FENCE; code = CODE_BLOCK;
    { CodeBlock { language = lang; code } }

let thematic_break :=
  | THEMATIC_BREAK;
    { ThematicBreak }

let blockquote :=
  | items = nonempty_list(blockquote_item);
    { Blockquote (List.flatten items) }

let blockquote_item :=
  | BLOCKQUOTE_START; b = block;
    { [b] }

let list_block :=
  | items = nonempty_list(unordered_item);
    { List { ordered = false; items } }
  | items = nonempty_list(ordered_item);
    { List { ordered = true; items } }

let unordered_item :=
  | UNORDERED_LIST_ITEM; content = list_item_content;
    { content }

let ordered_item :=
  | ORDERED_LIST_ITEM; content = list_item_content;
    { content }

let list_item_content :=
  | parts = list(list_item_inline);
    { [Paragraph (normalize_inlines (List.flatten parts))] }

let list_item_inline :=
  | SPACED_NEWLINE; { [] }
  | NEWLINE; { [] }
  | i = inline; { [i] }
