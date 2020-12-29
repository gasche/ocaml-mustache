(*{{{ The MIT License (MIT)

   Copyright (c) 2015 Rudi Grinberg

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to
   deal in the Software without restriction, including without limitation the
   rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
   sell copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
   IN THE SOFTWARE.  }}}*)
{
  open Lexing
  open Mustache_parser
  open Mustache_types

  exception Error of string

  let tok_arg lexbuf f =
    let start_p = lexbuf.Lexing.lex_start_p in
    let x = f lexbuf in
    lexbuf.Lexing.lex_start_p <- start_p;
    x

  let lex_tag lexbuf space ident tag_end =
    tok_arg lexbuf (fun lexbuf ->
      let () = space lexbuf in
      let name = ident lexbuf in
      let () = space lexbuf in
      let () = tag_end lexbuf in
      name
    )

  let split_ident ident =
    if ident = "." then []
    else String.split_on_char '.' ident

  let check_mustaches ~expected ~lexed =
    if expected <> lexed then
      raise (Error (Printf.sprintf "'%s' expected" expected))

  let is_blank s =
    let blank = ref true in
    let i = ref 0 in
    let len = String.length s in
    while !blank && !i < len do
      begin match s.[!i] with
      | ' ' | '\t' -> ()
      | _ -> blank := false
      end;
      incr i
    done;
    !blank

  let string_type s =
    if s = "\n" || s = "\r\n" then Newline
    else if is_blank s then Blank
    else Visible

  (* take a user-provided string, which may contain newlines,
     and split it into raw chunks following the structure
     expected by the rendering pass *)
  let process_string (f : string_type -> string -> 'a) s : 'a list =
    let len = String.length s in
    let chunk_type chunk = if is_blank chunk then Blank else Visible in
    let rec process acc start_pos =
      if start_pos >= len then List.rev acc
      else match String.index_from_opt s start_pos '\n' with
        | None ->
          let chunk = String.sub s start_pos (len - start_pos) in
          process (f (chunk_type chunk) chunk :: acc) len
        | Some newline_pos ->
          let newline_start =
            if newline_pos > 0 && s.[newline_pos - 1] = '\r'
            then newline_pos - 1
            else newline_pos
          in
          let newline = String.sub s newline_start (newline_pos - newline_start + 1) in
          if newline_start = start_pos then
            process (f Newline newline :: acc) (newline_pos + 1)
          else
            let chunk = String.sub s start_pos (newline_start - start_pos) in
            process (f Newline newline :: f (chunk_type chunk) chunk :: acc) (newline_pos + 1)
    in
    process [] 0

}

let blank = [' ' '\t']*
let newline = ('\n' | "\r\n")
let raw = [^ '{' '}' '\n']*
let id = ['a'-'z' 'A'-'Z' '-' '_' '/'] ['a'-'z' 'A'-'Z' '0'-'9' '-' '_' '/']*
let ident = ('.' | id ('.' id)*)

(* The grammar of partials is very relaxed compared to normal
   identifiers: we want to allow dots anywhere to express relative
   paths such as ../foo (this is consistent with other implementations
   such as the 'mustache' binary provided by the Ruby implementation),
   and in general we don't know what is going to be used, given that
   partials are controlled programmatically.

   We forbid spaces, to ensure that the behavior of trimming spaces
   around the partial name is consistent with the other tag, and we
   forbid newlines and mustaches to avoid simple delimiter mistakes
   ({{> foo } ... {{bar}}) being parsed as valid partial names.

   (Note: if one wishes to interpret partials using lambdas placed
   within the data (foo.bar interpreted as looking up 'foo' then 'bar'
   in the input data and hoping to find a user-decided representation
   of a function, it is of course possible to restrict the valid names
   and split on dots on the user side.) *)
let partial_name = [^ ' ' '\t' '\n' '{' '}']*

rule space = parse
  | blank newline { new_line lexbuf; space lexbuf }
  | blank { () }

and ident = parse
  | ident { lexeme lexbuf }
  | ""    { raise (Error "ident expected") }

and partial_name = parse
  | partial_name { lexeme lexbuf }

and end_on expected = parse
  | ("}}" | "}}}" | "") as lexed { check_mustaches ~expected ~lexed }

and comment acc = parse
  | "}}"        { String.concat "" (List.rev acc) }
  | ['{' '}']   { comment ((lexeme lexbuf) :: acc) lexbuf }
  | raw         { comment ((lexeme lexbuf) :: acc) lexbuf }
  | newline     { new_line lexbuf; comment ((lexeme lexbuf) :: acc) lexbuf }
  | eof         { raise (Error "non-terminated comment") }

and mustache = parse
  | "{{"         { ESCAPE (lex_tag lexbuf space ident (end_on "}}") |> split_ident) }
  | "{{{"        { UNESCAPE (lex_tag lexbuf space ident (end_on "}}}") |> split_ident) }
  | "{{&"        { UNESCAPE (lex_tag lexbuf space ident (end_on "}}") |> split_ident) }
  | "{{#"        { OPEN_SECTION (lex_tag lexbuf space ident (end_on "}}") |> split_ident) }
  | "{{^"        { OPEN_INVERTED_SECTION (lex_tag lexbuf space ident (end_on "}}") |> split_ident) }
  | "{{/"        { CLOSE_SECTION (lex_tag lexbuf space ident (end_on "}}") |> split_ident) }
  | "{{>"        { PARTIAL (lex_tag lexbuf space partial_name (end_on "}}")) }
  | "{{!"        { COMMENT (tok_arg lexbuf (comment [])) }
  | ['{' '}']    { RAW (Visible, lexeme lexbuf) }
  | raw          { let raw = lexeme lexbuf in
                   let ty = if is_blank raw then Blank else Visible in
                   RAW (ty, raw) }
  | newline      { new_line lexbuf; RAW (Newline, lexeme lexbuf) }
  | eof          { EOF }
