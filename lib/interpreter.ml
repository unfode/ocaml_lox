type position_t = {
  line: int;
}

type token_kind_e =
| Left_parenthesis
| Right_parenthesis
| Left_brace
| Right_brace
| Comma
| Dot
| Minus
| Plus
| Semicolon
| Slash
| Star
| Bang
| Bang_equal
| Equal
| Equal_equal
| Greater
| Greater_equal
| Less
| Less_equal
| Identifier of string
| String of string
| Number of float
| And
| Class
| Else
| False
| Fun
| For
| If 
| Nil 
| Or 
| Print
| Return
| Super 
| This 
| True 
| Var 
| While 

type token_t = {
  position: position_t;
  kind: token_kind_e;
}

let token_kind_to_string (token_kind: token_kind_e) : string = (
  match token_kind with
  | Left_parenthesis -> "("
  | Right_parenthesis -> ")"
  | Left_brace -> "{"
  | Right_brace -> "}"
  | Comma -> ","
  | Dot -> "."
  | Minus -> "-"
  | Plus -> "+"
  | Semicolon -> ";"
  | Slash -> "/"
  | Star -> "*"
  | Bang -> "!"
  | Bang_equal -> "!="
  | Equal -> "="
  | Equal_equal -> "=="
  | Greater -> ">"
  | Greater_equal -> ">="
  | Less -> "<"
  | Less_equal -> "<="
  | Identifier name -> "$" ^ name
  | String value -> "\"" ^ value ^ "\""
  | Number value -> string_of_float value
  | And -> "and"
  | Class -> "class"
  | Else -> "else"
  | False -> "false"
  | Fun -> "fun"
  | For -> "for"
  | If  -> "if"
  | Nil  -> "nil"
  | Or  -> "or"
  | Print  -> "print"
  | Return -> "return"
  | Super  -> "super"
  | This  -> "this"
  | True  -> "true"
  | Var  -> "var"
  | While  -> "while"
)

let token_to_string (token: token_t) : string = token_kind_to_string token.kind

let rec print_token_list (tokens: token_t list) = (
  match tokens with
  | [] -> ()
  | token :: rest_tokens ->
    print_string (token_to_string token);
    print_string " ";
    print_token_list rest_tokens
)

type scan_result_e =
| Error
| Correct of token_t list (* tokens we get *)

let string_to_char_list (str: string) : char list = List.of_seq (String.to_seq str)
let char_list_to_string (characters: char list) : string = String.of_seq (List.to_seq characters)
let is_digit (character: char) : bool = character >= '0' && character <= '9'
let is_alphabet (character: char) : bool =
  (character >= 'a' && character <= 'z') || (character >= 'A' && character <= 'Z')

let rec skip_comment (source: char list) : char list = (
  match source with
  | [] -> []
  | character :: rest_source -> (
    match character with
    | '\n' -> source
    | _ -> skip_comment rest_source
  )
)

type scan_string_literal_result_e =
| No_closing_quotation_mark
| Reach_closing_quotation_mark of char list * char list * int (* string literal, rest source, current line *)
let rec scan_string_literal (source: char list) (line: int) : scan_string_literal_result_e = (
  match source with
  | [] -> No_closing_quotation_mark
  | character :: rest_source -> (
    match character with
    | '"' -> Reach_closing_quotation_mark ([], rest_source, line)
    | '\n' -> (
      match scan_string_literal rest_source (line + 1) with
      | No_closing_quotation_mark -> No_closing_quotation_mark
      | Reach_closing_quotation_mark (string_literal, rest_source, line) ->
        Reach_closing_quotation_mark ('\n' :: string_literal, rest_source, line)
    )
    | _ -> (
      match scan_string_literal rest_source line with
      | No_closing_quotation_mark -> No_closing_quotation_mark
      | Reach_closing_quotation_mark (string_literal, rest_source, line) ->
        Reach_closing_quotation_mark (character :: string_literal, rest_source, line)
    )
  )
)

let rec scan_number_sequence (source: char list) : char list * char list = (
  match source with
  | [] -> ([], [])
  | character :: rest_source ->
    if is_digit character then
      let (rest_sequence, rest_rest_source) = scan_number_sequence rest_source in
        (character :: rest_sequence, rest_rest_source)
    else
      ([], source)
)
let rec scan_number_literal (source: char list) : char list * char list = (
  match source with
  | [] -> ([], [])
  | character :: rest_source ->
    if is_digit character then
      let (rest_literal, rest_rest_source) = scan_number_literal rest_source in
        (character :: rest_literal, rest_rest_source)
    else
      if character == '.' then
        let (rest_sequence, rest_rest_source) = scan_number_sequence rest_source in
          (character :: rest_sequence, rest_rest_source)
      else
        ([], source)
)

let rec scan_word (source: char list) : char list * char list = (
  match source with
  | [] -> ([], [])
  | character :: rest_source -> (
    if (is_alphabet character) || (is_digit character) || (character == '_') then
      let (rest_characters, rest_rest_source) = scan_word rest_source in
        (character :: rest_characters, rest_rest_source)
    else
      ([], source)
  )
)

let rec scan_helper (source: char list) (line: int) : scan_result_e =
  match source with
  | [] -> Correct []
  | character :: rest_source -> (
    match character with
    | '(' -> (
      match scan_helper rest_source line with
      | Error -> Error
      | Correct rest_tokens -> Correct ({position = {line = line}; kind = Left_parenthesis} :: rest_tokens)
    )
    | ')' -> (
      match scan_helper rest_source line with
      | Error -> Error
      | Correct rest_tokens -> Correct ({position = {line = line}; kind = Right_parenthesis} :: rest_tokens)
    )
    | '{' -> (
      match scan_helper rest_source line with
      | Error -> Error
      | Correct rest_tokens -> Correct ({position = {line = line}; kind = Left_brace} :: rest_tokens)
    )
    | '}' -> (
      match scan_helper rest_source line with
      | Error -> Error
      | Correct rest_tokens -> Correct ({position = {line = line}; kind = Right_brace} :: rest_tokens)
    )
    | ',' -> (
      match scan_helper rest_source line with
      | Error -> Error
      | Correct rest_tokens -> Correct ({position = {line = line}; kind = Comma} :: rest_tokens)
    )
    | '.' -> (
      match scan_helper rest_source line with
      | Error -> Error
      | Correct rest_tokens -> Correct ({position = {line = line}; kind = Dot} :: rest_tokens)
    )
    | '-' -> (
      match scan_helper rest_source line with
      | Error -> Error
      | Correct rest_tokens -> Correct ({position = {line = line}; kind = Minus} :: rest_tokens)
    )
    | '+' -> (
      match scan_helper rest_source line with
      | Error -> Error
      | Correct rest_tokens -> Correct ({position = {line = line}; kind = Plus} :: rest_tokens)
    )
    | ';' -> (
      match scan_helper rest_source line with
      | Error -> Error
      | Correct rest_tokens -> Correct ({position = {line = line}; kind = Semicolon} :: rest_tokens)
    )
    | '*' -> (
      match scan_helper rest_source line with
      | Error -> Error
      | Correct rest_tokens -> Correct ({position = {line = line}; kind = Star} :: rest_tokens)
    )
    | '!' -> (
      match rest_source with
      | [] -> Correct [{position = {line = line}; kind = Bang}]
      | character2 :: rest_rest_source -> (
        match character2 with
        | '=' -> (
          match scan_helper rest_rest_source line with
          | Error -> Error
          | Correct rest_tokens -> Correct ({position = {line = line}; kind = Bang_equal} :: rest_tokens)
        )
        | _ -> (
          match scan_helper rest_source line with
          | Error -> Error
          | Correct rest_tokens -> Correct ({position = {line = line}; kind = Bang} :: rest_tokens)
        )
      )
    )
    | '=' -> (
      match rest_source with
      | [] -> Correct [{position = {line = line}; kind = Equal}]
      | character2 :: rest_rest_source -> (
        match character2 with
        | '=' -> (
          match scan_helper rest_rest_source line with
          | Error -> Error
          | Correct rest_tokens -> Correct ({position = {line = line}; kind = Equal_equal} :: rest_tokens)
        )
        | _ -> (
          match scan_helper rest_source line with
          | Error -> Error
          | Correct rest_tokens -> Correct ({position = {line = line}; kind = Equal} :: rest_tokens)
        )
      )
    )
    | '<' -> (
      match rest_source with
      | [] -> Correct [{position = {line = line}; kind = Less}]
      | character2 :: rest_rest_source -> (
        match character2 with
        | '=' -> (
          match scan_helper rest_rest_source line with
          | Error -> Error
          | Correct rest_tokens -> Correct ({position = {line = line}; kind = Less_equal} :: rest_tokens)
        )
        | _ -> (
          match scan_helper rest_source line with
          | Error -> Error
          | Correct rest_tokens -> Correct ({position = {line = line}; kind = Less} :: rest_tokens)
        )
      )
    )
    | '>' -> (
      match rest_source with
      | [] -> Correct [{position = {line = line}; kind = Greater}]
      | character2 :: rest_rest_source -> (
        match character2 with
        | '=' -> (
          match scan_helper rest_rest_source line with
          | Error -> Error
          | Correct rest_tokens -> Correct ({position = {line = line}; kind = Greater_equal} :: rest_tokens)
        )
        | _ -> (
          match scan_helper rest_source line with
          | Error -> Error
          | Correct rest_tokens -> Correct ({position = {line = line}; kind = Greater} :: rest_tokens)
        )
      )
    )
    | '/' -> (
      match rest_source with
      | [] -> Correct [{position = {line = line}; kind = Slash}]
      | character2 :: rest_rest_source -> (
        match character2 with
        | '/' -> scan_helper (skip_comment rest_rest_source) line
        | _ -> (
          match scan_helper rest_source line with
          | Error -> Error
          | Correct rest_tokens -> Correct ({position = {line = line}; kind = Slash} :: rest_tokens)
        )
      )
    )
    | ' ' | '\r' | '\t' -> scan_helper rest_source line
    | '\n' -> scan_helper rest_source (line + 1)
    | '"' -> (
      match scan_string_literal rest_source line with
      | No_closing_quotation_mark -> Error
      | Reach_closing_quotation_mark (string_literal, rest_source, current_line) -> (
        match scan_helper rest_source current_line with
        | Error -> Error
        | Correct rest_tokens -> Correct (
          {
            position = {line = line};
            kind = String (char_list_to_string string_literal)
          } :: rest_tokens
        )
      )
    )
    | _ -> (
      if is_digit character then (
        let (rest_number_literal, rest_source) = scan_number_literal rest_source in (
          match scan_helper rest_source line with
          | Error -> Error
          | Correct rest_tokens -> Correct (
            {
              position = {line = line;};
              kind = Number (float_of_string (char_list_to_string (character :: rest_number_literal)));
            } :: rest_tokens
          )
        )
      )
      else (
        if (is_alphabet character) || (character == '_') then
          let (rest_word, rest_source) = scan_word rest_source in (
            let word = char_list_to_string (character :: rest_word) in (
              match scan_helper rest_source line with
              | Error -> Error
              | Correct rest_tokens -> (
                match word with
                | "and" -> Correct (
                  {
                    position = {line = line;};
                    kind = And;
                  } :: rest_tokens
                )
                | "class" -> Correct (
                  {
                    position = {line = line;};
                    kind = Class;
                  } :: rest_tokens
                )
                | "else" -> Correct (
                  {
                    position = {line = line;};
                    kind = Else;
                  } :: rest_tokens
                )
                | "false" -> Correct (
                  {
                    position = {line = line;};
                    kind = False;
                  } :: rest_tokens
                )
                | "fun" -> Correct (
                  {
                    position = {line = line;};
                    kind = Fun;
                  } :: rest_tokens
                )
                | "for" -> Correct (
                  {
                    position = {line = line;};
                    kind = For;
                  } :: rest_tokens
                )
                | "if" -> Correct (
                  {
                    position = {line = line;};
                    kind = If;
                  } :: rest_tokens
                )
                | "nil" -> Correct (
                  {
                    position = {line = line;};
                    kind = Nil;
                  } :: rest_tokens
                )
                | "or" -> Correct (
                  {
                    position = {line = line;};
                    kind = Or;
                  } :: rest_tokens
                )
                | "print" -> Correct (
                  {
                    position = {line = line;};
                    kind = Print;
                  } :: rest_tokens
                )
                | "return" -> Correct (
                  {
                    position = {line = line;};
                    kind = Return;
                  } :: rest_tokens
                )
                | "super" -> Correct (
                  {
                    position = {line = line;};
                    kind = Super;
                  } :: rest_tokens
                )
                | "this" -> Correct (
                  {
                    position = {line = line;};
                    kind = This;
                  } :: rest_tokens
                )
                | "true" -> Correct (
                  {
                    position = {line = line;};
                    kind = True;
                  } :: rest_tokens
                )
                | "var" -> Correct (
                  {
                    position = {line = line;};
                    kind = Var;
                  } :: rest_tokens
                )
                | "while" -> Correct (
                  {
                    position = {line = line;};
                    kind = While;
                  } :: rest_tokens
                )
                | _ -> Correct (
                  {
                    position = {line = line;};
                    kind = Identifier word;
                  } :: rest_tokens
                )
              )
            )
          )
        else Error
      )
    )
  )
let scan (source: string) : scan_result_e = scan_helper (string_to_char_list source) 1

type identifier_t = {
  position: position_t;
  name: string;
}

type binary_expression_kind_e = 
| Plus
| Minus
| Divide
| Multiply
| Greater
| Greater_equal
| Less
| Less_equal
| Not_equal
| Equal

type logical_expression_kind_e =
| And
| Or

type unary_expression_kind_e =
| Not 
| Negate

type binary_expression_t = {
  left: expression_t;
  right: expression_t;
  kind: binary_expression_kind_e;
}
and logical_expression_t = {
  left: expression_t;
  right: expression_t;
  kind: logical_expression_kind_e;
}
and unary_expression_t = {
  operand: expression_t;
  kind: unary_expression_kind_e;
}
and expression_kind_e =
| Binary of binary_expression_t
| Logical of logical_expression_t
| Unary of unary_expression_t
| Number of float
| String of string
| True
| False
| Nil
| Grouping of expression_t
| Variable of string
| Assign of identifier_t * expression_t (* variable, assigned expression *)
| Call of expression_t * expression_t list (* callee, arguments *)
and expression_t = {
  position: position_t;
  kind: expression_kind_e;
}

type parse_expression_result_e =
| Correct of expression_t * token_t list (* expression we get, rest tokens *)
| Error

type statement_kind_e =
| Block of statement_t list (* statements *)
| Expression of expression_t (* expression *)
| Function of identifier_t * identifier_t list * statement_t list (* function name, parameters, body *)
| If of expression_t * statement_t * statement_t option (* condition, then branch, else branch *)
| Print of expression_t (* expression to print *)
| Return of expression_t option (* expression to return *)
| Var of identifier_t * expression_t option (* identifier, initializer *)
| While of expression_t * statement_t (* condition, body *)
and statement_t = {
  position: position_t;
  kind: statement_kind_e;
}

type argument_list_result_e =
| Error
| Correct of expression_t list * token_t list (* list of arguments, rest tokens *)

let rec primary (tokens: token_t list) : parse_expression_result_e = (
  match tokens with
  | [] -> Error
  | token :: rest_tokens -> (
    match token.kind with
    | Number n -> Correct ({position = token.position; kind = Number n}, rest_tokens)
    | String s -> Correct ({position = token.position; kind = String s}, rest_tokens)
    | True -> Correct ({position = token.position; kind = True}, rest_tokens)
    | False -> Correct ({position = token.position; kind = False}, rest_tokens)
    | Nil -> Correct ({position = token.position; kind = Nil}, rest_tokens)
    | Identifier name -> Correct ({position = token.position; kind = Variable name}, rest_tokens)
    | Left_parenthesis -> (
      match expression rest_tokens with
      | Error -> Error
      | Correct (expression, rest_tokens) -> (
        match rest_tokens with
        | [] -> Error
        | token2 :: rest_tokens -> (
          match token2.kind with
          | Right_parenthesis -> Correct ({position = token.position; kind = Grouping expression}, rest_tokens)
          | Left_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|Plus|Semicolon|
            Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|Less|
            Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|Var|
            While|Identifier _|String _|Number _ -> Error
        )
      )
    )
    | Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|Plus|Semicolon|
      Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|Less|
      Less_equal|And|Class|Else|Fun|For|If|Or|Print|Return|Super|This|Var|While -> Error
  )
)
and argument_list (tokens: token_t list) : argument_list_result_e = (
  match expression tokens with
  | Error -> Error
  | Correct (argument, tokens) -> (
    match tokens with
    | [] -> Error
    | token :: tokens -> (
      match token.kind with
      | Right_parenthesis -> Correct ([argument], tokens)
      | Comma -> (
        match argument_list tokens with
        | Error -> Error
        | Correct (arguments, tokens) -> Correct(argument :: arguments, tokens)
      )
      | Left_parenthesis|Left_brace|Right_brace|Dot|Minus|Plus|Semicolon|Slash|Star|
        Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|Less|Less_equal|And|
        Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|Var|While|
        Identifier _|String _|Number _ -> Error
    )
  )
)
and call_loop (tokens: token_t list) (callee: expression_t) : parse_expression_result_e = (
  match tokens with
  | [] -> Correct (callee, [])
  | token :: rest_tokens -> (
    match token.kind with
    | Left_parenthesis -> (
      let left_parenthesis_token = token in (
        match rest_tokens with
        | [] -> Error
        | token :: rest_tokens -> (
          match token.kind with
          | Right_parenthesis -> call_loop rest_tokens {
            position = left_parenthesis_token.position;
            kind = Call (callee, [])
          }
          | Left_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|Plus|Semicolon|
            Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|Less|
            Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|Var|
            While|Identifier _|String _|Number _ -> (
              match argument_list (token :: rest_tokens) with
              | Error -> Error
              | Correct (arguments, rest_tokens) -> call_loop rest_tokens {
                position = left_parenthesis_token.position;
                kind = Call (callee, arguments)
              }
            )
        )
      )
    )
    | Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|Plus|Semicolon|
      Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|Less|
      Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|Var|
      While|Identifier _|String _|Number _ -> Correct (callee, tokens)
  )
)
and call (tokens: token_t list) : parse_expression_result_e = (
  match primary tokens with
  | Error -> Error
  | Correct (expression, tokens) -> call_loop tokens expression
)
and unary (tokens: token_t list) : parse_expression_result_e = (
  match tokens with
  | [] -> Error
  | token :: rest_tokens -> (
    match token.kind with
    | Bang -> (
      match unary rest_tokens with
      | Error -> Error
      | Correct (operand_expression, rest_rest_tokens) -> Correct(
        {
          position = token.position;
          kind = Unary {
            operand = operand_expression;
            kind = Not
          }
        },
        rest_rest_tokens
      )
    )
    | Minus -> (
      match unary rest_tokens with
      | Error -> Error
      | Correct (operand_expression, rest_rest_tokens) -> Correct (
        {
          position = token.position;
          kind = Unary {
            operand = operand_expression;
            kind = Negate
          }
        },
        rest_rest_tokens
      )
    )
    | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Plus|
      Semicolon|Slash|Star|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|Less|
      Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|Var|
      While|Identifier _|String _|Number _ -> call tokens
  )
)
and factor_loop (tokens: token_t list) (left_expression: expression_t) : parse_expression_result_e = (
  match tokens with
  | [] -> Correct (left_expression, [])
  | token :: rest_tokens -> (
    match token.kind with
    | Slash -> (
      match unary rest_tokens with
      | Error -> Error
      | Correct (right_expression, rest_rest_tokens) ->
        factor_loop
          rest_rest_tokens
          {
            position = token.position;
            kind = Binary {
              left = left_expression;
              right = right_expression;
              kind = Divide
            }
          }
    )
    | Star -> (
      match unary rest_tokens with
      | Error -> Error
      | Correct (right_expression, rest_rest_tokens) ->
        factor_loop
          rest_rest_tokens
          {
            position = token.position;
            kind = Binary {
              left = left_expression;
              right = right_expression;
              kind = Multiply
            }
          }
    )
    | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
      Plus|Semicolon|Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|Less|
      Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|Var|
      While|Identifier _|String _|Number _ -> Correct (left_expression, tokens)
  )
)
and factor (tokens: token_t list) : parse_expression_result_e = (
  match unary tokens with
  | Error -> Error
  | Correct (expression, rest_tokens) ->
    factor_loop rest_tokens expression
)
and term_loop (tokens: token_t list) (left_expression: expression_t) : parse_expression_result_e = (
  match tokens with
  | [] -> Correct (left_expression, [])
  | token :: rest_tokens -> (
    match token.kind with
    | Plus -> (
      match factor rest_tokens with
      | Error -> Error
      | Correct (right_expression, rest_rest_tokens) ->
        term_loop
          rest_rest_tokens
          {
            position = token.position;
            kind = Binary {
              left = left_expression;
              right = right_expression;
              kind = Plus
            }
          }
    )
    | Minus -> (
      match factor rest_tokens with
      | Error -> Error
      | Correct (right_expression, rest_rest_tokens) ->
        term_loop
          rest_rest_tokens
          {
            position = token.position;
            kind = Binary {
              left = left_expression;
              right = right_expression;
              kind = Minus
            }
          }
    )
    | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|
      Semicolon|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|
      Less|Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|
      Var|While|Identifier _|String _|Number _ -> Correct (left_expression, tokens)
  )
)
and term (tokens: token_t list) : parse_expression_result_e = (
  match factor tokens with
  | Error -> Error
  | Correct (expression, rest_tokens) ->
    term_loop rest_tokens expression
)
and comparison_loop (tokens: token_t list) (left_expression: expression_t) : parse_expression_result_e = (
  match tokens with
  | [] -> Correct (left_expression, [])
  | token :: rest_tokens -> (
    match token.kind with
    | Greater -> (
      match term rest_tokens with
      | Error -> Error
      | Correct (right_expression, rest_rest_tokens) ->
        comparison_loop
          rest_rest_tokens
          {
            position = token.position;
            kind = Binary {
              left = left_expression;
              right = right_expression;
              kind = Greater
            }
          }
    )
    | Greater_equal -> (
      match term rest_tokens with
      | Error -> Error
      | Correct (right_expression, rest_rest_tokens) ->
        comparison_loop
          rest_rest_tokens
          {
            position = token.position;
            kind = Binary {
              left = left_expression;
              right = right_expression;
              kind = Greater_equal
            }
          }
    )
    | Less -> (
      match term rest_tokens with
      | Error -> Error
      | Correct(right_expression, rest_rest_tokens) ->
        comparison_loop
          rest_rest_tokens
          {
            position = token.position;
            kind = Binary{
              left = left_expression;
              right = right_expression;
              kind = Less
            }
          }
    )
    | Less_equal -> (
      match term rest_tokens with
      | Error -> Error
      | Correct(right_expression, rest_rest_tokens) ->
        comparison_loop
          rest_rest_tokens
          {
            position = token.position;
            kind = Binary{
              left = left_expression;
              right = right_expression;
              kind = Less_equal
            }
          }
    )
    | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
      Plus|Semicolon|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|And|Class|Else|
      False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|Var|While|Identifier _|
      String _|Number _ -> Correct(left_expression, tokens)
  )
)
and comparison (tokens: token_t list) : parse_expression_result_e = (
  match term tokens with
  | Error -> Error
  | Correct (expression, rest_tokens) ->
    comparison_loop rest_tokens expression
)
and equality_loop (tokens: token_t list) (left_expression: expression_t) : parse_expression_result_e = (
  match tokens with
  | [] -> Correct (left_expression, [])
  | token :: tokens -> (
    match token.kind with
    | Bang_equal -> (
      match comparison tokens with
      | Error -> Error
      | Correct (right_expression, tokens) ->
        equality_loop tokens {position = token.position; kind = Binary {left = left_expression; right = right_expression; kind = Not_equal}}
    )
    | Equal_equal -> (
      match comparison tokens with
      | Error -> Error
      | Correct(right_expression, tokens) ->
        equality_loop tokens {position = token.position; kind = Binary {left = left_expression; right = right_expression; kind = Equal}}
    )
    | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
      Plus|Semicolon|Slash|Star|Bang|Equal|Greater|Greater_equal|Less|Less_equal|
      And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|Var|While|
      Identifier _|String _|Number _ -> Correct (left_expression, token :: tokens)
  )
)
and equality (tokens: token_t list) : parse_expression_result_e = (
  match comparison tokens with
  | Error -> Error
  | Correct (expression, rest_tokens) -> 
    equality_loop rest_tokens expression
)
and logic_and_loop (tokens: token_t list) (left_expression: expression_t) : parse_expression_result_e = (
  match tokens with
  | [] -> Correct (left_expression, [])
  | token :: tokens -> (
    match token.kind with
    | And -> (
      let and_token = token in (
        match equality tokens with
        | Error -> Error
        | Correct (right_expression, tokens) ->
          equality_loop tokens {position = and_token.position; kind = Logical {left = left_expression; right = right_expression; kind = And}}
      )
    )
    | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
      Plus|Semicolon|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|
      Greater_equal|Less|Less_equal|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|
      This|True|Var|While|Identifier _|String _|Number _ -> Correct (left_expression, token :: tokens)
  )
)
and logic_and (tokens: token_t list) : parse_expression_result_e = (
  match equality tokens with
  | Error -> Error
  | Correct (left_expression, tokens) -> logic_and_loop tokens left_expression
)
and logic_or_loop (tokens: token_t list) (left_expression: expression_t) : parse_expression_result_e = (
  match tokens with
  | [] -> Correct (left_expression, [])
  | token :: tokens -> (
    match token.kind with
    | Or -> (
      let or_token = token in (
        match logic_and tokens with
        | Error -> Error
        | Correct (right_expression, tokens) ->
          equality_loop tokens {position = or_token.position; kind = Logical {left = left_expression; right = right_expression; kind = Or}}
      )
    )
    | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
      Plus|Semicolon|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|
      Greater_equal|Less|Less_equal|And|Class|Else|False|Fun|For|If|Nil|Print|Return|
      Super|This|True|Var|While|Identifier _|String _|Number _ -> Correct (left_expression, token :: tokens)
  )
)
and logic_or (tokens: token_t list) : parse_expression_result_e = (
  match logic_and tokens with
  | Error -> Error
  | Correct (left_expression, tokens) -> logic_or_loop tokens left_expression
)
and assignment (tokens: token_t list) : parse_expression_result_e = (
  match tokens with
  | [] -> Error
  | token :: rest_tokens -> (
    match token.kind with
    | Identifier identifier_name -> (
      let identifier_token = token in (
        match rest_tokens with
        | [] -> Correct ({position = identifier_token.position; kind = Variable identifier_name}, [])
        | token :: rest_tokens -> (
          match token.kind with
          | Equal -> (
            let equal_token = token in (
              match assignment rest_tokens with
              | Error -> Error
              | Correct (assigned_expression, rest_tokens) -> Correct (
                {
                  position = equal_token.position;
                  kind = Assign (
                    {position = identifier_token.position; name = identifier_name;},
                    assigned_expression
                  );
                },
                rest_tokens
              )
            )
          )
          | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
            Plus|Semicolon|Slash|Star|Bang|Bang_equal|Equal_equal|Greater|Greater_equal|
            Less|Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|
            Var|While|Identifier _|String _|Number _ -> logic_or tokens
        )
      )
    )
    | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
      Plus|Semicolon|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|
      Greater_equal|Less|Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|
      Super|This|True|Var|While|String _|Number _ -> logic_or tokens
  )
)
and expression (tokens: token_t list) : parse_expression_result_e = assignment tokens

type parse_statement_result_e =
| Correct of statement_t * token_t list (* statement we get, rest tokens *)
| Error

type parse_statements_result_e =
| Correct of statement_t list (* statements we get *)
| Error


let expression_statement (tokens: token_t list) : parse_statement_result_e = (
  match expression tokens with
  | Error -> Error
  | Correct (expression, tokens) -> (
    match tokens with
    | [] -> Error
    | semicolon_token :: rest_tokens -> (
      match semicolon_token.kind with
      | Semicolon -> Correct (
        {
          position = expression.position;
          kind = Expression expression;
        },
        rest_tokens
      )
      | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
        Plus|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|Less|
        Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|Var|
        While|Identifier _|String _|Number _ -> Error
    )
  )
)

let variable_declaration (tokens: token_t list) : parse_statement_result_e = (
  match tokens with
  | [] -> failwith "expects var keyword"
  | var_token :: tokens -> (
    match var_token.kind with
    | Var -> (
      match tokens with
      | [] -> Error
      | identifier_token :: tokens -> (
        match identifier_token.kind with
        | Identifier variable_name -> (
          match tokens with
          | [] -> Error
          | token :: tokens -> (
            match token.kind with
            | Semicolon -> Correct (
              {
                position = var_token.position;
                kind = Var ({position = identifier_token.position; name = variable_name;}, None)
              },
              tokens
            )
            | Equal -> (
              match expression tokens with
              | Error -> Error
              | Correct (initializer_expression, tokens) -> (
                match tokens with
                | [] -> Error
                | semicolon_token :: rest_tokens -> (
                  match semicolon_token.kind with
                  | Semicolon -> Correct (
                    {
                      position = var_token.position;
                      kind = Var (
                        {position = identifier_token.position; name = variable_name;},
                        Some initializer_expression
                      )
                    },
                    rest_tokens
                  )
                  | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
                    Plus|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|Less|
                    Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|Var|
                    While|Identifier _|String _|Number _ -> Error
                )
              )
            )
            | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
              Plus|Slash|Star|Bang|Bang_equal|Equal_equal|Greater|Greater_equal|Less|
              Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|Var|
              While|Identifier _|String _|Number _ -> Error
          )
        )
        | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
          Plus|Semicolon|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|
          Greater_equal|Less|Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|
          Super|This|True|Var|While|String _|Number _ -> Error
      )
    )
    | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
      Plus|Semicolon|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|
      Greater_equal|Less|Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|
      Super|This|True|While|Identifier _|String _|Number _ -> failwith "expects var keyword"
  )
)

type block_statement_loop_result_e =
| Error
| Correct of statement_t list * token_t list (* statements we get, rest tokens*)

type parameter_list_result_e =
| Error
| Correct of identifier_t list * token_t list (* list of parameters, rest tokens *)

let rec parameter_list_loop (tokens: token_t list) : parameter_list_result_e = (
  match tokens with
  | [] -> Correct ([], [])
  | token :: tokens -> (
    match token.kind with
    | Right_parenthesis -> Correct ([], tokens)
    | Comma -> (
      match tokens with
      | [] -> Error
      | token :: tokens -> (
        match token.kind with
        | Identifier parameter_name -> (
          match parameter_list_loop tokens with
          | Error -> Error
          | Correct (identifiers, rest_tokens) -> Correct (
            {
              position = token.position;
              name = parameter_name;
            } :: identifiers,
            rest_tokens
          )
        )
        | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
          Plus|Semicolon|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|
          Greater_equal|Less|Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|
          Super|This|True|Var|While|String _|Number _ -> Error
      )
    )
    | Left_parenthesis|Left_brace|Right_brace|Dot|Minus|Plus|Semicolon|Slash|Star|
      Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|Less|Less_equal|And|
      Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|Var|While|
      Identifier _|String _|Number _ -> Error
  )
)

let parameter_list (tokens: token_t list) : parameter_list_result_e = (
  match tokens with
  | [] -> Error
  | token :: tokens -> (
    match token.kind with
    | Left_parenthesis -> (
      match tokens with
      | [] -> Error
      | token :: tokens -> (
        match token.kind with
        | Right_parenthesis -> Correct([], tokens)
        | Identifier parameter_name -> (
          match parameter_list_loop tokens with
          | Error -> Error
          | Correct (identifiers, rest_tokens) -> Correct (
            {
              position = token.position;
              name = parameter_name;
            } :: identifiers,
            rest_tokens
          )
        )
        | Left_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|Plus|Semicolon|
          Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|Less|
          Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|Var|
          While|String _|Number _ -> Error
      )
    )
    | Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|Plus|Semicolon|
      Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|Less|
      Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|Var|
      While|Identifier _|String _|Number _ -> Error
  )
)

type condition_result_e =
| Error
| Correct of expression_t * token_t list (* condition expression, rest tokens *)

let condition (tokens: token_t list) : condition_result_e = (
  match tokens with
  | [] -> Error
  | token :: tokens -> (
    match token.kind with
    | Left_parenthesis -> (
      match expression tokens with
      | Error -> Error
      | Correct (expression, tokens) -> (
        match tokens with
        | [] -> Error
        | token :: tokens -> (
          match token.kind with
          | Right_parenthesis -> Correct (expression, tokens)
          | Left_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|Plus|Semicolon|
            Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|Less|
            Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|Var|
            While|Identifier _|String _|Number _ -> Error
        )
      )
    )
    | Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|Plus|Semicolon|
      Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|Less|
      Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|Var|
      While|Identifier _|String _|Number _ -> Error
  )
)

let rec block_statement_loop (tokens: token_t list) : block_statement_loop_result_e = (
  match tokens with
  | [] -> Error
  | token :: rest_tokens -> (
    match token.kind with
    | Right_brace -> Correct ([], rest_tokens)
    | Left_parenthesis|Right_parenthesis|Left_brace|Comma|Dot|Minus|Plus|
      Semicolon|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|
      Less|Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|
      Var|While|Identifier _|String _|Number _ -> (
        match declaration tokens with
        | Error -> Error
        | Correct (statement, rest_tokens) -> (
          match block_statement_loop rest_tokens with
          | Error -> Error
          | Correct (statements, rest_tokens) -> Correct (statement :: statements, rest_tokens)
        )
      )
  )
)
and function_declaration (tokens: token_t list) : parse_statement_result_e = (
  match tokens with
  | [] -> failwith "expect fun keyword"
  | fun_token :: tokens -> (
    match fun_token.kind with
    | Fun -> (
      match tokens with
      | [] -> Error
      | identifier_token :: tokens -> (
        match identifier_token.kind with
        | Identifier function_name -> (
          match parameter_list tokens with
          | Error -> Error
          | Correct (parameter_names, tokens) -> (
            match tokens with
            | [] -> Error
            | token :: tokens -> (
              match token.kind with
              | Left_brace -> (
                match block_statement_loop tokens with
                | Error -> Error
                | Correct (statements, rest_tokens) -> Correct (
                  {
                    position = fun_token.position;
                    kind = Function (
                      {position = identifier_token.position; name = function_name;},
                      parameter_names,
                      statements
                    )
                  },
                  rest_tokens
                )
              )
              | Left_parenthesis|Right_parenthesis|Right_brace|Comma|Dot|Minus|Plus|
                Semicolon|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|
                Less|Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|
                Var|While|Identifier _|String _|Number _ -> Error
            )
          )
        )
        | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
          Plus|Semicolon|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|
          Greater_equal|Less|Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|
          Super|This|True|Var|While|String _|Number _ -> Error
      )
    )
    | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
      Plus|Semicolon|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|
      Greater_equal|Less|Less_equal|And|Class|Else|False|For|If|Nil|Or|Print|Return|Super|
      This|True|Var|While|Identifier _|String _|Number _ -> failwith "expect fun keyword"
  )
)
and block_statement (tokens: token_t list) : parse_statement_result_e = (
  match tokens with
  | [] -> failwith "expect {"
  | left_brace_token :: tokens -> (
    match left_brace_token.kind with
    | Left_brace -> (
      match block_statement_loop tokens with
      | Error -> Error
      | Correct (statements, rest_tokens) -> Correct (
        {
          position = left_brace_token.position;
          kind = Block statements;
        },
        rest_tokens
      )
    )
    | Left_parenthesis|Right_parenthesis|Right_brace|Comma|Dot|Minus|Plus|
      Semicolon|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|
      Less|Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|
      Var|While|Identifier _|String _|Number _ -> failwith "expect {"
  )
)
and if_statement (tokens: token_t list) : parse_statement_result_e = (
  match tokens with
  | [] -> failwith "expects if keyword"
  | token :: tokens -> (
    match token.kind with
    | If -> (
      let if_token = token in (
        match condition tokens with
        | Error -> Error
        | Correct (condition, tokens) -> (
          match statement tokens with
          | Error -> Error
          | Correct (then_statement, tokens) -> (
            match tokens with
            | [] -> Correct (
              {
                position = if_token.position;
                kind = If (condition, then_statement, None)
              },
              tokens
            )
            | token :: tokens -> (
              match token.kind with
              | Else -> (
                match statement tokens with
                | Error -> Error
                | Correct (else_statement, tokens) -> Correct (
                  {
                    position = if_token.position;
                    kind = If (condition, then_statement, Some else_statement)
                  },
                  tokens
                )
              )
              | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
                Plus|Semicolon|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|
                Greater_equal|Less|Less_equal|And|Class|False|Fun|For|If|Nil|Or|Print|Return|Super|
                This|True|Var|While|Identifier _|String _|Number _ -> Correct (
                  {
                    position = if_token.position;
                    kind = If (condition, then_statement, None)
                  },
                  token :: tokens
                )
            )
          )
        )
      )
    )
    | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
      Plus|Semicolon|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|
      Greater_equal|Less|Less_equal|And|Class|Else|False|Fun|For|Nil|Or|Print|Return|
      Super|This|True|Var|While|Identifier _|String _|Number _ -> failwith "expects if keyword"
  )
)
and while_statement (tokens: token_t list) : parse_statement_result_e = (
  match tokens with
  | [] -> failwith "expect while keyword"
  | token :: tokens -> (
    match token.kind with
    | While -> (
      let while_token = token in (
        match condition tokens with
        | Error -> Error
        | Correct (condition, tokens) -> (
          match statement tokens with
          | Error -> Error
          | Correct (loop_statement, tokens) -> Correct (
            {
              position = while_token.position;
              kind = While (condition, loop_statement);
            },
            tokens
          )
        )
      )
    )
    | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
      Plus|Semicolon|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|
      Greater_equal|Less|Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|
      Super|This|True|Var|Identifier _|String _|Number _ -> failwith "expect while keyword"
  )
)
and return_statement (tokens: token_t list) : parse_statement_result_e = (
  match tokens with
  | [] -> failwith "expect return keyword"
  | token :: tokens -> (
    match token.kind with
    | Return -> (
      let return_token = token in (
        match tokens with
        | [] -> Error
        | token :: tokens -> (
          match token.kind with
          | Semicolon -> Correct (
            {position = return_token.position; kind = Return None;},
            tokens
          )
          | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
            Plus|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|Less|
            Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|
            True|Var|While|Identifier _|String _|Number _ -> (
              match expression (token :: tokens) with
              | Error -> Error
              | Correct (returned_expression, tokens) -> (
                match tokens with
                | [] -> Error
                | token :: tokens -> (
                  match token.kind with
                  | Semicolon -> Correct (
                    {
                      position = return_token.position;
                      kind = Return (Some returned_expression);
                    },
                    tokens
                  )
                  | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
                    Plus|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|Less|
                    Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|
                    True|Var|While|Identifier _|String _|Number _ -> Error
                )
              )
            )
        )
      )
    )
    | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
      Plus|Semicolon|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|
      Greater_equal|Less|Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|
      Super|This|True|Var|While|Identifier _|String _|Number _ -> failwith "expect return keyword"
  )
)
and statement (tokens: token_t list) : parse_statement_result_e = (
  match tokens with
  | [] -> failwith "expect one or more tokens"
  | token :: rest_tokens -> (
    match token.kind with
    | Print -> (
      match expression rest_tokens with
      | Error -> Error
      | Correct (expression, tokens) -> (
        match tokens with
        | [] -> Error
        | semicolon_token :: rest_tokens -> (
          match semicolon_token.kind with
          | Semicolon -> Correct (
            {
              position = token.position;
              kind = Print(expression);
            },
            rest_tokens
          )
          | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
            Plus|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|Less|
            Less_equal|And|Class|Else|False|Fun|For|If|Nil|Or|Print|Return|Super|This|True|Var|
            While|Identifier _|String _|Number _ -> Error
        )
      )
    )
    | If -> if_statement tokens
    | While -> while_statement tokens
    | Left_brace -> block_statement tokens
    | Return -> return_statement tokens
    | Fun | Var -> failwith "fun and var should have been handled"
    | Left_parenthesis|Right_parenthesis|Right_brace|Comma|Dot|Minus|Plus|
      Semicolon|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|Greater_equal|
      Less|Less_equal|And|Class|Else|False|For|Nil|Or|Super|This|True|Identifier _|
      String _|Number _ -> expression_statement tokens
  )
)
and declaration (tokens: token_t list) : parse_statement_result_e = (
  match tokens with
  | [] -> failwith "declaration should be called with a non-empty list of tokens"
  | token :: _ -> (
    match token.kind with
    | Fun -> function_declaration tokens
    | Var -> variable_declaration tokens
    | Left_parenthesis|Right_parenthesis|Left_brace|Right_brace|Comma|Dot|Minus|
      Plus|Semicolon|Slash|Star|Bang|Bang_equal|Equal|Equal_equal|Greater|
      Greater_equal|Less|Less_equal|And|Class|Else|False|For|If|Nil|Or|Print|Return|Super|
      This|True|While|Identifier _|String _|Number _ -> statement tokens
  )
)

let rec parse_loop (tokens: token_t list) : parse_statements_result_e = (
  match tokens with
  | [] -> Correct []
  | _ :: _ -> (
    match declaration tokens with
    | Error -> Error
    | Correct (statement, rest_tokens) -> (
      match parse_loop rest_tokens with
      | Error -> Error
      | Correct rest_statements -> Correct (statement :: rest_statements)
    )
  )
)
let parse (tokens: token_t list) : parse_statements_result_e = parse_loop tokens

type environment_t = {
  enclosing: environment_t option;
  map: (string, value_e option) Hashtbl.t;
}
and value_e =
| Number of float
| String of string
| Bool of bool
| Nil
| Function of string list * statement_t list * environment_t (* parameters, body, closure *)

let value_to_string (value: value_e) : string = (
  match value with
  | Number value -> string_of_float value
  | String value -> value
  | Bool value -> string_of_bool value
  | Nil -> "nil"
  | Function _ -> "#function"
)



let create_new_environment (enclosing: environment_t option) = {
  enclosing = enclosing;
  map = Hashtbl.create 10;
}

type evaluate_expression_result_e =
| Error
| Correct of value_e

type evaluate_expressions_result_e =
| Error
| Correct of value_e list

type execute_statement_result_e =
| Error
| Correct of environment_t (* new environment *)

type execute_statement_in_function_result_e =
| Error
| Non_return of environment_t (* new environment *)
| Return of value_e

type while_loop_result_e =
| Error
| Correct

type while_loop_in_function_result_e =
| Error
| Non_return
| Return of value_e

let rec get_variable_value (environment: environment_t) (name: string) : value_e option = (
  match Hashtbl.find_opt environment.map name with
  | Some value_option -> value_option
  | None -> (
    match environment.enclosing with
    | None -> None
    | Some encloing_environment -> get_variable_value encloing_environment name
  )
)

let rec set_variable_value (environment: environment_t) (name: string) (value: value_e) : bool = (
  if Hashtbl.mem environment.map name then (
    Hashtbl.replace environment.map name (Some value);
    true
  ) else (
    match environment.enclosing with
    | None -> false
    | Some enclosing_environment -> set_variable_value enclosing_environment name value
  )
)

let rec bind (name_value_pairs: (string * value_e) list) (map: (string, value_e option) Hashtbl.t) = (
  match name_value_pairs with
  | [] -> ()
  | (name, value) :: rest_pairs -> (
    Hashtbl.add map name (Some value);
    bind rest_pairs map
  )
)

let rec evaluate_expression (expression: expression_t) (environment: environment_t) : evaluate_expression_result_e = (
  match expression.kind with
  | Binary binary_expression -> (
    match evaluate_expression binary_expression.left environment with
    | Error -> Error
    | Correct left_operand -> (
      match evaluate_expression binary_expression.right environment with
      | Error -> Error
      | Correct right_operand -> (
        match binary_expression.kind with
        | Plus -> (
          match left_operand with
          | Number left_value -> (
            match right_operand with
            | Number right_value -> Correct (Number (left_value +. right_value))
            | Nil|String _|Bool _|Function _ -> Error
          )
          | Nil|String _|Bool _|Function _ -> Error
        )
        | Minus -> (
          match left_operand with
          | Number left_value -> (
            match right_operand with
            | Number right_value -> Correct (Number (left_value -. right_value))
            | Nil|String _|Bool _|Function _ -> Error
          )
          | Nil|String _|Bool _|Function _ -> Error
        )
        | Divide -> (
          match left_operand with
          | Number left_value -> (
            match right_operand with
            | Number right_value -> (
              if right_value != 0. then Correct (Number (left_value /. right_value))
              else Error
            )
            | Nil|String _|Bool _|Function _ -> Error
          )
          | Nil|String _|Bool _|Function _ -> Error
        )
        | Multiply -> (
          match left_operand with
          | Number left_value -> (
            match right_operand with
            | Number right_value -> Correct (Number (left_value *. right_value))
            | Nil|String _|Bool _|Function _ -> Error
          )
          | Nil|String _|Bool _|Function _ -> Error
        )
        | Greater -> (
          match left_operand with
          | Number left_value -> (
            match right_operand with
            | Number right_value -> Correct (Bool (left_value > right_value))
            | Nil|String _|Bool _|Function _ -> Error
          )
          | Nil|String _|Bool _|Function _ -> Error
        )
        | Greater_equal -> (
          match left_operand with
          | Number left_value -> (
            match right_operand with
            | Number right_value -> Correct (Bool (left_value >= right_value))
            | Nil|String _|Bool _|Function _ -> Error
          )
          | Nil|String _|Bool _|Function _ -> Error
        )
        | Less -> (
          match left_operand with
          | Number left_value -> (
            match right_operand with
            | Number right_value -> Correct (Bool (left_value < right_value))
            | Nil|String _|Bool _|Function _ -> Error
          )
          | Nil|String _|Bool _|Function _ -> Error
        )
        | Less_equal -> (
          match left_operand with
          | Number left_value -> (
            match right_operand with
            | Number right_value -> Correct (Bool (left_value <= right_value))
            | Nil|String _|Bool _|Function _ -> Error
          )
          | Nil|String _|Bool _|Function _ -> Error
        )
        | Not_equal -> (
          match left_operand with
          | Number left_value -> (
            match right_operand with
            | Number right_value -> Correct (Bool (left_value <> right_value))
            | Nil|String _|Bool _|Function _ -> Error
          )
          | Nil|String _|Bool _|Function _ -> Error
        )
        | Equal -> (
          match left_operand with
          | Number left_value -> (
            match right_operand with
            | Number right_value -> Correct (Bool (left_value = right_value))
            | Nil|String _|Bool _|Function _ -> Error
          )
          | Nil|String _|Bool _|Function _ -> Error
        )
      )
    )
  )
  | Logical logical_expression -> (
    match evaluate_expression logical_expression.left environment with
    | Error -> Error
    | Correct left_operand -> (
      match left_operand with
      | Bool left_value -> (
        match logical_expression.kind with
        | And -> (
          if not left_value then Correct (Bool false)
          else (
            match evaluate_expression logical_expression.right environment with
            | Error -> Error
            | Correct right_operand -> (
              match right_operand with
              | Bool right_value -> Correct (Bool right_value)
              | Nil|Number _|String _|Function _ -> Error
            )
          )
        )
        | Or -> (
          if left_value then Correct (Bool true)
          else (
            match evaluate_expression logical_expression.right environment with
            | Error -> Error
            | Correct right_operand -> (
              match right_operand with
              | Bool right_value -> Correct (Bool right_value)
              | Nil|Number _|String _|Function _ -> Error
            )
          )
        )
      )
      | Nil|Number _|String _|Function _ -> Error
    )
  )
  | Unary unary_expression -> (
    match evaluate_expression unary_expression.operand environment with
    | Error -> Error
    | Correct operand -> (
      match operand with
      | Number value -> (
        match unary_expression.kind with
        | Negate -> Correct (Number (-. value))
        | Not -> Error
      )
      | Bool value -> (
        match unary_expression.kind with
        | Not -> Correct (Bool (not value))
        | Negate -> Error
      )
      | Nil|String _|Function _ -> Error
    )
  )
  | Number value -> Correct (Number value)
  | String value -> Correct (String value)
  | True -> Correct (Bool true)
  | False -> Correct (Bool false)
  | Nil -> Correct Nil
  | Grouping expression -> evaluate_expression expression environment
  | Variable name -> (
    match get_variable_value environment name with
    | None -> Error
    | Some value -> Correct value
  )
  | Assign (identifier, assigned_expression) -> (
    match evaluate_expression assigned_expression environment with
    | Error -> Error
    | Correct value -> (
      if set_variable_value environment identifier.name value then
        Correct value
      else Error
    )
  )
  | Call (callee, arguments) -> (
    match evaluate_expression callee environment with
    | Error -> Error
    | Correct callee_value -> (
      match callee_value with
      | Function (parameters, body, closure) -> (
        if (List.length arguments) <> (List.length parameters) then Error
        else (
          match evaluate_expressions arguments environment with
          | Error -> Error
          | Correct argument_values -> (
            let local_environment = create_new_environment (Some closure) in (
              bind (List.combine parameters argument_values) local_environment.map;
              match execute_statements_in_function body local_environment with
              | Error -> Error
              | Non_return _ -> Correct Nil
              | Return return_value -> Correct return_value
            )
          )
        )
      )
      | Nil|Number _|String _|Bool _ -> Error
    )
  )
)
and execute_statements_in_function (statements: statement_t list) (environment: environment_t) : execute_statement_in_function_result_e = (
  match statements with
  | [] -> Non_return environment
  | statement :: statements -> (
    match execute_statement_in_function statement environment with
    | Error -> Error
    | Non_return new_environment -> execute_statements_in_function statements new_environment
    | Return return_value -> Return return_value
  )
)
and execute_statement_in_function (statement: statement_t) (environment: environment_t) : execute_statement_in_function_result_e = (
  match statement.kind with
  | Block statements -> (
    match execute_statements_in_function statements (create_new_environment (Some environment)) with
    | Error -> Error
    | Non_return _ -> Non_return environment
    | Return return_value -> Return return_value
  )
  | Expression expression -> (
    match evaluate_expression expression environment with
    | Error -> Error
    | Correct _ -> Non_return environment
  )
  | Function (identifier, parameters, body) -> (
    let new_environment = create_new_environment(Some environment) in (
      Hashtbl.add new_environment.map identifier.name (Some (Function (
        List.map (fun (identifier: identifier_t) : string -> identifier.name) parameters,
        body,
        environment
      )));
      Non_return new_environment
    )
  )
  | If (condition, then_branch, else_branch_option) -> (
    match evaluate_expression condition environment with
    | Error -> Error
    | Correct value -> (
      match value with
      | Bool value -> (
        if value then (
          match execute_statement_in_function then_branch environment with
          | Error -> Error
          | Non_return _ -> Non_return environment
          | Return return_value -> Return return_value
        ) else (
          match else_branch_option with
          | None -> Non_return environment
          | Some else_branch -> (
            match execute_statement_in_function else_branch environment with
            | Error -> Error
            | Non_return _ -> Non_return environment
            | Return return_value -> Return return_value
          )
        )
      )
      | Nil|Number _|String _|Function _ -> Error
    )
  )
  | Print expression -> (
    match evaluate_expression expression environment with
    | Error -> Error
    | Correct value -> (
      print_endline (value_to_string value);
      Non_return environment
    )
  )
  | Return expression_option -> (
    match expression_option with
    | None -> Return Nil
    | Some expression -> (
      match evaluate_expression expression environment with
      | Error -> Error
      | Correct value -> Return value
    )
  )
  | Var (identifier, initializer_expression_option) -> (
    let new_environment = create_new_environment(Some environment) in (
      match initializer_expression_option with
      | None -> (
        Hashtbl.add new_environment.map identifier.name None;
        Non_return new_environment
      )
      | Some initializer_expression -> (
        match evaluate_expression initializer_expression environment with
        | Error -> Error
        | Correct value -> (
          Hashtbl.add new_environment.map identifier.name (Some value);
          Non_return new_environment
        )
      )
    )
  )
  | While (condition, body) -> (
    match while_loop_in_function condition body environment with
    | Error -> Error
    | Non_return -> Non_return environment
    | Return return_value -> Return return_value
  )
)
and while_loop_in_function (condition: expression_t) (body: statement_t) (environment: environment_t) : while_loop_in_function_result_e = (
  match evaluate_expression condition environment with
  | Error -> Error
  | Correct value -> (
    match value with
    | Bool value -> (
      if value then (
        match execute_statement_in_function body environment with
        | Error -> Error
        | Non_return _ -> while_loop_in_function condition body environment
        | Return return_value -> Return return_value
      ) else Non_return
    )
    | Nil|Number _|String _|Function _ -> Error
  )
)
and evaluate_expressions (expressions: expression_t list) (environment: environment_t) : evaluate_expressions_result_e = (
  match expressions with
  | [] -> Correct []
  | expression :: rest_expressions -> (
    match evaluate_expression expression environment with
    | Error -> Error
    | Correct value -> (
      match evaluate_expressions rest_expressions environment with
      | Error -> Error
      | Correct values -> Correct (value :: values)
    )
  )
)
and while_loop (condition: expression_t) (body: statement_t) (environment: environment_t) : while_loop_result_e = (
  match evaluate_expression condition environment with
  | Error -> Error
  | Correct value -> (
    match value with
    | Bool value -> (
      if value then (
        match execute_statement body environment with
        | Error -> Error
        | Correct _ -> while_loop condition body environment
      ) else Correct
    )
    | Nil|Number _|String _|Function _ -> Error
  )
)
and execute_statement (statement: statement_t) (environment: environment_t) : execute_statement_result_e = (
  match statement.kind with
  | Block statements -> (
    match execute_statements_helper statements (create_new_environment (Some environment)) with
    | Error -> Error
    | Correct _ -> Correct environment
  )
  | Expression expression -> (
    match evaluate_expression expression environment with
    | Error -> Error
    | Correct _ -> Correct environment
  )
  | Function (identifier, parameters, body) -> (
    (* let new_environment = create_new_environment(Some environment) in (
      Hashtbl.add new_environment.map identifier.name (Some (Function (
        List.map (fun (identifier: identifier_t) : string -> identifier.name) parameters,
        body,
        environment
      )));
      Correct new_environment
    ) *)
    Hashtbl.add environment.map identifier.name (Some (Function (
      List.map (fun (identifier: identifier_t) : string -> identifier.name) parameters,
      body,
      environment
    )));
    Correct environment
  )
  | If (condition, then_branch, else_branch_option) -> (
    match evaluate_expression condition environment with
    | Error -> Error
    | Correct value -> (
      match value with
      | Bool value -> (
        if value then (
          match execute_statement then_branch environment with
          | Error -> Error
          | Correct _ -> Correct environment
        ) else (
          match else_branch_option with
          | None -> Correct environment
          | Some else_branch -> (
            match execute_statement else_branch environment with
            | Error -> Error
            | Correct _ -> Correct environment
          )
        )
      )
      | Nil|Number _|String _|Function _ -> Error
    )
  )
  | Print expression -> (
    match evaluate_expression expression environment with
    | Error -> Error
    | Correct value -> (
      print_endline (value_to_string value);
      Correct environment
    )
  )
  | Return _ -> Error
  | Var (identifier, initializer_expression_option) -> (
    let new_environment = create_new_environment(Some environment) in (
      match initializer_expression_option with
      | None -> (
        Hashtbl.add new_environment.map identifier.name None;
        Correct new_environment
      )
      | Some initializer_expression -> (
        match evaluate_expression initializer_expression environment with
        | Error -> Error
        | Correct value -> (
          Hashtbl.add new_environment.map identifier.name (Some value);
          Correct new_environment
        )
      )
    )
  )
  | While (condition, body) -> (
    match while_loop condition body environment with
    | Error -> Error
    | Correct -> Correct environment
  )
)
and execute_statements_helper (statements: statement_t list) (environment: environment_t) : execute_statement_result_e = (
  match statements with
  | [] -> Correct environment
  | statement :: statements -> (
    match execute_statement statement environment with
    | Error -> Error
    | Correct new_environment -> execute_statements_helper statements new_environment
  )
)

type execute_statements_result_e =
| Error
| Correct
let execute_statements (statements: statement_t list) : execute_statements_result_e = (
  match execute_statements_helper statements (create_new_environment None) with
  | Error -> Error
  | Correct _ -> Correct
)