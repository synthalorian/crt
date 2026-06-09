{
  open Parser

  let count_hash s = String.length s

  let trim_left s =
    let len = String.length s in
    let rec aux i =
      if i < len && (s.[i] = ' ' || s.[i] = '\t') then aux (i + 1)
      else i
    in
    let start = aux 0 in
    if start = 0 then s
    else String.sub s start (len - start)

  let strip_trailing_nl s =
    let len = String.length s in
    let rec aux i =
      if i > 0 && (s.[i - 1] = '\n' || s.[i - 1] = '\r') then aux (i - 1)
      else i
    in
    let n = aux len in
    if n = len then s else String.sub s 0 n

  let buf = Buffer.create 256
  let in_code_block = ref false
}

let whitespace = [' ' '\t']
let newline = '\n' | '\r' '\n'
let nonnewline = [^ '\n' '\r']
let textchar = [^ '\n' '\r' '*' '_' '`' '[' ']' '!' '\\' '#' '(' ')' '-' '+' '>']

rule token = parse
  | newline whitespace* newline
      { NEWLINE }
  | newline
      { SPACED_NEWLINE }
  | whitespace* "---" whitespace* newline
      { THEMATIC_BREAK }
  | whitespace* "***" whitespace* newline
      { THEMATIC_BREAK }
  | whitespace* "___" whitespace* newline
      { THEMATIC_BREAK }
  | whitespace* ("#"+) whitespace+ nonnewline*
      { let s = Lexing.lexeme lexbuf in
        let trimmed = trim_left s in
        let hash_part =
          let trimmed2 = String.trim trimmed in
          let sp = try String.index trimmed2 ' ' with Not_found -> String.length trimmed2 in
          String.sub trimmed2 0 sp
        in
        let level = count_hash hash_part in
        let sp = String.index trimmed ' ' in
        let content = String.sub trimmed (sp + 1) (String.length trimmed - sp - 1) in
        HEADING (level, content) }
  | whitespace* ">" whitespace?
      { BLOCKQUOTE_START }
  | whitespace* ("-" | "*" | "+") whitespace+
      { UNORDERED_LIST_ITEM }
  | whitespace* (['0'-'9']+) "." whitespace+
      { ORDERED_LIST_ITEM }
  | whitespace* "```" nonnewline*
      { let s = Lexing.lexeme lexbuf in
        let info = trim_left s in
        let lang =
          if String.length info <= 3 then None
          else Some (String.sub info 3 (String.length info - 3))
        in
        Buffer.clear buf;
        in_code_block := true;
        CODE_BLOCK_FENCE lang }
  | "`"
      { Buffer.clear buf;
        let content = read_code_inline 1 lexbuf in
        CODE_INLINE content }
  | "`" "`"+ as ticks
      { let n = String.length ticks in
        Buffer.clear buf;
        let content = read_code_inline n lexbuf in
        CODE_INLINE content }
  | "**"
      { DOUBLE_STAR }
  | "__"
      { DOUBLE_UNDERSCORE }
  | "*"
      { STAR }
  | "_"
      { UNDERSCORE }
  | "!["
      { IMAGE_START }
  | "["
      { LINK_START }
  | "]"
      { LINK_END }
  | "("
      { LPAREN }
  | ")"
      { RPAREN }
  | "\\" newline
      { HARD_BREAK }
  | "\\" _ as escaped
      { TEXT (String.make 1 escaped.[1]) }
  | textchar+
      { TEXT (Lexing.lexeme lexbuf) }
  | whitespace+
      { TEXT (Lexing.lexeme lexbuf) }
  | eof
      { EOF }
  | _ as c
      { TEXT (String.make 1 c) }

and read_code_inline n = parse
  | "`" "`"* as ticks
      { let m = String.length ticks in
        if m >= n then
          let s = Buffer.contents buf in
          Buffer.clear buf;
          s
        else begin
          Buffer.add_string buf ticks;
          read_code_inline n lexbuf
        end }
  | [^ '`']+
      { Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_code_inline n lexbuf }
  | eof
      { Buffer.contents buf }

and code_block = parse
  | newline
      { code_block_body lexbuf }
  | [^ '\n' '\r']+
      { Buffer.add_string buf (Lexing.lexeme lexbuf);
        code_block lexbuf }
  | eof
      { let content = Buffer.contents buf in
        Buffer.clear buf;
        in_code_block := false;
        CODE_BLOCK content }

and code_block_body = parse
  | "```" whitespace* newline
      { let content = Buffer.contents buf in
        Buffer.clear buf;
        in_code_block := false;
        CODE_BLOCK (strip_trailing_nl content) }
  | "```" whitespace* eof
      { let content = Buffer.contents buf in
        Buffer.clear buf;
        in_code_block := false;
        CODE_BLOCK (strip_trailing_nl content) }
  | newline
      { Buffer.add_char buf '\n';
        code_block_body lexbuf }
  | [^ '\n' '\r']+
      { Buffer.add_string buf (Lexing.lexeme lexbuf);
        code_block_body lexbuf }
  | eof
      { let content = Buffer.contents buf in
        Buffer.clear buf;
        in_code_block := false;
        CODE_BLOCK content }

{
  let main_token lexbuf =
    if !in_code_block then code_block lexbuf
    else token lexbuf
}
