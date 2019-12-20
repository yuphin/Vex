%skeleton "lalr1.cc" /* -*- C++ -*- */
%require "3.4"
%defines

%define api.token.constructor
%define api.value.type variant
%define parse.assert

%code requires {
  #include <string>
  #include <memory>
  #include "AST.h"
  class driver;
}

// The parsing context.
%param { driver& drv }

%locations

%define parse.trace
%define parse.error verbose
%code {

    #include "Driver.h"
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
%token <double> DBL_NUM "fp_number"
%token <int> INT_NUM "int_number"



%type <std::unique_ptr<Type>> type
%type <obj_type> basic_type
%type <std::unique_ptr<unsigned int>> vector_extension
%type <std::unique_ptr<StatementBlockAST>> statement_list
%type <std::unique_ptr<TopAST>> unit program
%type <std::unique_ptr<std::vector<std::unique_ptr<FunctionAST>>>> function_list
%type <std::unique_ptr<FunctionAST>> function
%type <std::unique_ptr<FunctionDeclAST>> func_prototype
%type <std::unique_ptr<FunctionBodyAST>> function_body
%type <std::unique_ptr<std::vector<std::unique_ptr<VariableDeclAST>>>> declaration_list declaration variable_list parameter_list
%type <std::unique_ptr<StatementAST>> statement
%type <std::unique_ptr<AssignmentStatementAST>> assignment_statement
%type <std::unique_ptr<ReturnStatementAST>> return_statement
%type <std::unique_ptr<PrintStatementAST>> print_statement
%type <std::unique_ptr<ReadStatementAST>> read_statement
%type <std::unique_ptr<ForStatementAST>> for_statement
%type <std::unique_ptr<IfStatementAST>> if_statement
%type <std::unique_ptr<WhileStatementAST>> while_statement
%type <std::unique_ptr<VariableAST>> variable
%type <std::unique_ptr<ExprAST>> lexpression expression term unary factor 
%type <std::unique_ptr<std::vector<std::unique_ptr<ExprAST>>>> expression_list argument_list 
%type <std::unique_ptr<std::vector<std::unique_ptr<ExprAST>>>> print_expression
%type <std::unique_ptr<std::vector<std::unique_ptr<VariableAST>>>> read_expression

%printer {yyo << $$;} <*>

%left AND OR NOT

%%
  %start unit;

  unit : program { drv.root = std::move($1); }

  program: declaration_list function_list {
                        $$ = std::make_unique<TopAST>(std::move(*$1),std::move(*$2)) ;
  } ;
  
  function_list: function { 
                        $$ = std::make_unique<std::vector<std::unique_ptr<FunctionAST>>>();
                        $$->emplace_back(std::move($1));
                }
                | function_list function {
                        $$ = std::move($1);
                        $$->emplace_back(std::move($2));
  } ;
  
  function: func_prototype function_body ENDFUNC {
                        $$ = std::make_unique<FunctionAST>(std::move($1), std::move($2));
  } ;


  func_prototype: basic_type FUNC ID "(" parameter_list ")" {
                        $$ = std::make_unique<FunctionDeclAST>(std::move($3),drv.location,$1,std::move(*$5));
  } ;

  function_body: declaration_list statement_list {
                        $$ = std::make_unique<FunctionBodyAST>(std::move(*$1), std::move($2));
  } ;

  declaration_list: %empty { 
                        $$ = std::make_unique<std::vector<std::unique_ptr<VariableDeclAST>>>();
                    } 
                    | declaration_list declaration ";" {
                        $$ = std::move($1);
                        // std::cout << "Contents of $2 is : " << $2->size() << std::endl; 
                        $$->insert($$->end(), std::make_move_iterator($2->begin()),
                            std::make_move_iterator($2->end()));
                        // std::cout << "Contents of $$ is : " << $$->size() << std::endl; 
  } ; 
                 

  declaration: VAR variable_list {
      $$ = std::move($2);

  } ;                

  parameter_list: %empty {
                       //   $$ = nullptr;
                        $$ = std::make_unique<std::vector<std::unique_ptr<VariableDeclAST>>>();
                 } 
                 | variable_list {
                        $$ = std::move($1);

  } ;

  variable_list: ID ":" type {
                        // std::cout << "Reading: " << $1 << std::endl; 
                        $$ = std::make_unique<std::vector<std::unique_ptr<VariableDeclAST>>>();
                        $$->emplace_back(std::make_unique<VariableDeclAST>(std::move($1),drv.location,std::move($3)));
                }               
                | variable_list "," ID ":" type {
                        // std::cout << "Reading: " << $3 << std::endl; 
                        $$ = std::move($1); 
                        $$->emplace_back(std::make_unique<VariableDeclAST>(std::move($3),drv.location,std::move($5)));
                        // std::cout << "Contents(2) of $$ is : " << $$->size() << std::endl; 

  } ;

  type: basic_type vector_extension {
                      if($2){
                        $$ = std::make_unique<Type>($1,std::move($2));
                      }else{
                        $$ = std::make_unique<Type>($1);
                      }
                        

  } ;

  basic_type: INT {
                        $$ = INT;
             }
             | REAL {
                        $$ = REAL;
                        
             } ;

  vector_extension: "[" INT_NUM "]" {
                        $$ = std::make_unique<unsigned int>($2); 
                   }            
                   | "[" "]" {
                        $$ = std::make_unique<unsigned int>(0);
                   } 
                   | %empty {
                        $$ = nullptr;
                   } ; 

  statement_list: statement ";" {
                       $$ = std::make_unique<StatementBlockAST>(); 
                       $$->statement_list.emplace_back(std::move($1));

                 }
                 | statement_list statement ";" {
                       $$ = std::move($1);
                       $$->statement_list.emplace_back(std::move($2));

  } ;

  statement: assignment_statement {
                       $$ = std::move($1);
            }
            | return_statement {
                       $$ = std::move($1);
            }
            | print_statement {
                       $$ = std::move($1);
            }
            | read_statement {
                       $$ = std::move($1);
            }
            | for_statement {
                       $$ = std::move($1);
            }
            | if_statement {
                       $$ = std::move($1);
            }
            | while_statement {
                       $$ = std::move($1);
  } ;
  
  assignment_statement: variable ":=" expression {
                       $$ = std::make_unique<AssignmentStatementAST>(std::move($1),std::move($3));
                      
  } ;

  variable: ID {
                    $$ = std::make_unique<VariableAST>(std::move($1),drv.location);
           }
           | ID "[" expression "]" {
                    $$ = std::make_unique<VariableAST>(std::move($1),drv.location,std::move($3));
  } ; 

  lexpression: expression {
                       $$ = std::move($1);
              }
              | expression EQ  expression {
                       $$ = std::make_unique<BinaryExprAST>(std::move($1),EQ,std::move($3),drv.location);
              }
              | expression LT  expression {
                       $$ = std::make_unique<BinaryExprAST>(std::move($1),LT,std::move($3),drv.location);
              }
              | expression GT  expression {
                       $$ = std::make_unique<BinaryExprAST>(std::move($1),GT,std::move($3),drv.location);
              }
              | expression LTE  expression {
                       $$ = std::make_unique<BinaryExprAST>(std::move($1),LTE,std::move($3),drv.location);
              }
              | expression GTE expression {
                       $$ = std::make_unique<BinaryExprAST>(std::move($1),GTE,std::move($3),drv.location);
              } 
              | lexpression AND lexpression {
                       $$ = std::make_unique<BinaryExprAST>(std::move($1),AND,std::move($3),drv.location);
              } 
              | lexpression OR lexpression {
                       $$ = std::make_unique<BinaryExprAST>(std::move($1),OR,std::move($3),drv.location);
              } 
              | NOT lexpression {
                       $$ = std::make_unique<UnaryExprAST>(UNOT,std::move($2),drv.location);
  } ; 
             

  expression: term {
                       $$ = std::move($1);           
             }
             | expression PLUS term {
                       $$ = std::make_unique<BinaryExprAST>(std::move($1),ADD,std::move($3),drv.location);
             }
             | expression MINUS term {
                       $$ = std::make_unique<BinaryExprAST>(std::move($1),SUB,std::move($3),drv.location);
  } ;

  term: unary {
                       $$ = std::move($1);
       }
       | term STAR unary {
                       $$ = std::make_unique<BinaryExprAST>(std::move($1),MULT,std::move($3),drv.location);
       }
       | term SLASH unary {
                       $$ = std::make_unique<BinaryExprAST>(std::move($1),DIV,std::move($3),drv.location);
       }
       | term MOD unary {
                       $$ = std::make_unique<BinaryExprAST>(std::move($1),MOD,std::move($3),drv.location);
       }
       | term DIV unary {
                       $$ = std::make_unique<BinaryExprAST>(std::move($1),IDIV,std::move($3),drv.location);
  } ;

  unary: factor {
                       $$ = std::move($1);
        } 
        | MINUS factor {
                       $$ = std::make_unique<UnaryExprAST>(MINUS,std::move($2),drv.location);

  } ;

  factor: variable {
                       $$ = std::move($1);
         } 
         | ID "(" argument_list ")" {
                       $$ = std::make_unique<InvocationAST>(std::move($1),std::move(*$3));
         }
         | INT_NUM {
                       $$ = std::make_unique<IntNumAST>($1,drv.location);
         }
         | DBL_NUM {
                       $$ = std::make_unique<FloatingNumAST>($1,drv.location);
         }
         | "(" expression ")" {
                       $$ = std::move($2);

  };

  argument_list: expression_list {
                       $$ = std::move($1);
                }
                | %empty {
                       $$ = std::make_unique<std::vector<std::unique_ptr<ExprAST>>>();
  } ;

  expression_list: expression {
                        $$ = std::make_unique<std::vector<std::unique_ptr<ExprAST>>>();
                        $$->emplace_back(std::move($1));
                  }
                  | expression_list "," expression {
                        $$ = std::move($1);
                        $$->emplace_back(std::move($3));
  } ;

  return_statement: RETURN expression {
                        $$ = std::make_unique<ReturnStatementAST>(std::move($2));
  } ;

  for_statement: FOR assignment_statement TO expression BY expression statement_list ENDFOR {
                        $$ = std::make_unique<ForStatementAST>(std::move($2),
                            std::move($4),std::move($6),std::move($7)); 
                }
                | FOR assignment_statement TO expression statement_list ENDFOR {
                        $$ = std::make_unique<ForStatementAST>(std::move($2),
                            std::move($4),std::move($5)); 

  } ;

  if_statement: IF lexpression THEN statement_list ENDIF {
                        $$ = std::make_unique<IfStatementAST>(std::move($2),std::move($4));
               }
               | IF lexpression THEN statement_list ELSE statement_list ENDIF {
                        $$ = std::make_unique<IfStatementAST>(std::move($2),std::move($4),std::move($6));

  } ;

  while_statement: WHILE lexpression DO statement_list ENDWHILE {
                        $$ = std::make_unique<WhileStatementAST>(std::move($2),std::move($4));
  } ;

  print_expression: expression {
                        $$ = std::make_unique<std::vector<std::unique_ptr<ExprAST>>>();
                        $$->emplace_back(std::move($1));
                   }
                   | print_expression "," expression {
                        $$ = std::move($1);
                        $$->emplace_back(std::move($3));

  } ;
                  
  read_expression: variable {
                        $$ = std::make_unique<std::vector<std::unique_ptr<VariableAST>>>();
                        $$->emplace_back(std::move($1));
  
                  }
                  | read_expression "," variable {
                        $$ = std::move($1);
                        $$->emplace_back(std::move($3));

  } ;

  print_statement: PRINT print_expression {
                        $$ =  std::make_unique<PrintStatementAST>(std::move(*$2));

  } ;       

  read_statement: READ  read_expression {
                        $$ =  std::make_unique<ReadStatementAST>(std::move(*$2));
  } ;

%%


void yy::parser::error (const location_type& l, const std::string& m){
  // TODO : Replace this with custom logger later on
  std::cerr << l << ": " << m << '\n';
}
