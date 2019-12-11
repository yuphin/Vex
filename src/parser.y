%skeleton "lalr1.cc" /* -*- C++ -*- */
%require "3.4"
%defines

%define api.token.constructor
%define api.value.type variant
%define parse.assert

%code requires {
  #include <string>

  class driver;
}

// The parsing context.
%param { driver& drv }

%locations

%define parse.trace
%define parse.error verbose
%code {

    #include "driver.h"
}
// Token prefix
%define api.token.prefix {TOK_}

%token
  END  0  "EOF"
  ASSIGN  ":="
  MINUS   "-"
  PLUS    "+"
  STAR    "*"
  SLASH   "/"
  LPAREN  "("
  RPAREN  ")"
  LBRACKET "["
  RBRACKET "]"
  EQ "="
  LT "<"
  GT ">"
  LTE "<="
  GTE ">="
  AND "and"
  OR "or"
  NOT "not"
  MOD "mod"
  DIV "div"
  FUNC "func"
  ENDFUNC "endfunc"
  INT "int"
  REAL "real"
  RETURN "return"
  TO "to"
  BY "by"
  IF "if"
  THEN "then"
  ELSE "else"
  ENDIF "endif"
  ENDFOR "endfor"
  DO "do"
  PRINT "print"
  READ "read"
  WHILE "while"
  ENDWHILE "endwhile"
  FOR "for"
  VAR "var"
  COLON ":"
  SEMICOLON ";"
  COMMA ","
  QUOTE "\""
;

%token <std::string> ID "identifier"
%token <double> NUM "number"

%printer {yyo << $$;} <*>

%left AND OR NOT

%%
  %start program;

  program : declaration_list function_list {} ;
  
  function_list : function {}
                | function_list function {} ;

  function : basic_type FUNC ID "(" parameter_list ")" function_body ENDFUNC {} ;

  function_body : declaration_list statement_list {} ;

  declaration_list : %empty {} 
                   | declaration_list declaration ";" {} ; 
                 

  declaration : VAR variable_list {};                

  parameter_list : %empty {} 
                 | variable_list {} ;

  variable_list : ID ":" type {}               
                | variable_list "," ID ":" type {} ;

  type : basic_type vector_extension {} ;                

  basic_type : INT {}
             | REAL {} ;

  vector_extension : "[" NUM "]" {}            
                   | "[" "]" {} 
                   | %empty {} ; 

  statement_list : statement ";" {}
                 | statement_list statement ";" {} ;

  statement : assignment_statement {}
            | return_statement {}
            | print_statement {}
            | read_statement {}
            | for_statement {}
            | if_statement {}
            | while_statement {} ;
  
  assignment_statement : variable ":=" expression {} ;

  variable : ID {}
           | ID "[" expression "]" {} ; 

  lexpression : expression {}  ;
              | expression EQ  expression {}
              | expression LT  expression {}
              | expression GT  expression {}
              | expression LTE  expression {}
              | expression GTE expression {} 
              | lexpression AND lexpression {} 
              | lexpression OR lexpression {} 
              | lexpression NOT lexpression {}
              | NOT lexpression {} 
             

  expression : term {}
             | expression PLUS term {}
             | expression MINUS term {} ;

  term : unary {}
       | term STAR unary {}
       | term SLASH unary {}
       | term MOD unary {}
       | term DIV unary {} ;

  unary : factor {} 
        | MINUS factor {} ;

  factor : variable | ID "(" argument_list ")" {}
         | NUM {}
         | "(" expression ")" {};

  argument_list : expression_list {}
                | %empty {} ;

  expression_list : expression {}
                  | expression_list "," expression {} ;

  return_statement : RETURN expression {} ;

  for_statement : FOR variable ASSIGN expression TO expression BY expression statement_list ENDFOR {}
                | FOR variable ASSIGN expression TO expression statement_list ENDFOR {} ;

  if_statement : IF lexpression THEN statement_list ENDIF {}
               | IF lexpression THEN statement_list ELSE statement_list ENDIF {} ;

  while_statement : WHILE lexpression DO statement_list ENDWHILE {} ;

  print_expression : expression {}
                   | print_expression "," expression {}
                  
  read_expression : variable {}
                  | read_expression "," variable {} ;

  print_statement : PRINT print_expression {} ;       

  read_statement : READ  read_expression {} ;

%%


void yy::parser::error (const location_type& l, const std::string& m){
  // TODO : Replace this with custom logger later on
  std::cerr << l << ": " << m << '\n';
}