type token_t
val token_to_string : token_t -> string
val print_token_list : token_t list -> unit

type scan_result_e =
| Error
| Correct of token_t list

val scan : string -> scan_result_e

type statement_t

type parse_statements_result_e =
| Correct of statement_t list
| Error

val parse : token_t list -> parse_statements_result_e

type execute_statements_result_e =
| Error
| Correct

val execute_statements : statement_t list -> execute_statements_result_e