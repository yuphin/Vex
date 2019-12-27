%{ /* -*- C++ -*- */

#if defined(_WIN32) || defined(_WIN64)
    #include <io.h>
#else
    #include <unistd.h>
#endif

# include <cerrno>
# include <climits>
# include <cfloat>
# include <cstdlib>
# include <stdlib.h>
# include <cstring> // strerror
# include <string>
# include "Driver.h"
# include "Parser.h"
using namespace Vex;

%}

%option noyywrap nounput noinput batch debug
%option nounistd

%{
  // A number symbol corresponding to the value in S.
  yy::parser::symbol_type make_INTNUMBER (const std::string &s, const yy::parser::location_type& loc);
  yy::parser::symbol_type make_DBLNUMBER (const std::string &s, const yy::parser::location_type& loc);
%}

id    [a-zA-Z][a-zA-Z_0-9]*
dbl_num   [0-9]+([.][0-9]+)?([e]([+-])?[0-9]+)?
int_num   [0-9]+
blank [ \t\r]

%{
  // Code run each time a pattern is matched.
  #define YY_USER_ACTION loc.columns(yyleng);
%}

%%

%{
       yy::location& loc = drv.location;
       // Code run each time yylex is called.
       loc.step();
%}
{blank}+   loc.step();
\n+        loc.lines(yyleng); loc.step();
"-"        return yy::parser::make_MINUS(loc);
"+"        return yy::parser::make_PLUS(loc);
"*"        return yy::parser::make_STAR(loc);
"/"        return yy::parser::make_SLASH(loc);
"("        return yy::parser::make_LPAREN(loc);
")"        return yy::parser::make_RPAREN(loc);
":="       return yy::parser::make_ASSIGN(loc);
"["        return yy::parser::make_LBRACKET(loc);
"]"        return yy::parser::make_RBRACKET(loc);
"="        return yy::parser::make_EQ(loc);
"<"        return yy::parser::make_LT(loc); 
">"        return yy::parser::make_GT(loc); 
"<="       return yy::parser::make_LTE(loc); 
">="       return yy::parser::make_GTE(loc); 
"and"      return yy::parser::make_AND(loc); 
"or"       return yy::parser::make_OR(loc); 
"not"      return yy::parser::make_NOT(loc); 
"mod"      return yy::parser::make_MOD(loc); 
"div"      return yy::parser::make_DIV(loc); 
"func"     return yy::parser::make_FUNC(loc); 
"endfunc"  return yy::parser::make_ENDFUNC(loc); 
"int"      return yy::parser::make_INT(loc); 
"real"     return yy::parser::make_REAL(loc); 
"return"   return yy::parser::make_RETURN(loc); 
"to"       return yy::parser::make_TO(loc); 
"by"       return yy::parser::make_BY(loc); 
"if"       return yy::parser::make_IF(loc); 
"then"     return yy::parser::make_THEN(loc); 
"else"     return yy::parser::make_ELSE(loc); 
"endif"    return yy::parser::make_ENDIF(loc); 
"endfor"   return yy::parser::make_ENDFOR(loc); 
"do"       return yy::parser::make_DO(loc); 
"print"    return yy::parser::make_PRINT(loc); 
"read"     return yy::parser::make_READ(loc); 
"while"    return yy::parser::make_WHILE(loc); 
"endwhile" return yy::parser::make_ENDWHILE(loc); 
"for"      return yy::parser::make_FOR(loc);
"var"      return yy::parser::make_VAR(loc);
":"        return yy::parser::make_COLON(loc);  
";"        return yy::parser::make_SEMICOLON(loc);  
","        return yy::parser::make_COMMA(loc);  
"\""       return yy::parser::make_QUOTE(loc);  
{int_num}  return make_INTNUMBER(yytext,loc);
{dbl_num}  return make_DBLNUMBER(yytext,loc);
{id}       return yy::parser::make_ID(yytext,loc);
"%".*      { /* Do nothing */}
.          { throw yy::parser::syntax_error(loc,
               "invalid character: " + std::string(yytext)); }
<<EOF>>    return yy::parser::make_END (loc);
%%

yy::parser::symbol_type make_INTNUMBER (const std::string &s, const yy::parser::location_type& loc) {
  errno = 0;
  unsigned long val = strtoul(s.c_str(),nullptr,0);
  if (!(val <= INT_MAX && errno != ERANGE)){
    throw yy::parser::syntax_error (loc, "number is out of range: " + s);
  }
  return yy::parser::make_INT_NUM ((int) val, loc);
}

yy::parser::symbol_type make_DBLNUMBER (const std::string &s, const yy::parser::location_type& loc) {
  errno = 0;
  double val = strtod(s.c_str(),nullptr);
  if (!(val <= DBL_MAX && errno != ERANGE)){
    throw yy::parser::syntax_error (loc, "number is out of range: " + s);
  }
  return yy::parser::make_DBL_NUM ((double) val, loc);
}

void driver::scan_begin () {
  yy_flex_debug = trace_scanning;
  if (file.empty () || file == "-")
    yyin = stdin;
  else if (!(yyin = fopen (file.c_str (), "r")))
    {
      std::cerr << "cannot open " << file << ": " << strerror(errno) << '\n';
      exit (EXIT_FAILURE);
    }
}

void driver::scan_end () {
  fclose (yyin);
}

