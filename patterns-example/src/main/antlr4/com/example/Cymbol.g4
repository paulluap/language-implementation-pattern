/** Simple statically-typed programming language with functions and variables
 *  taken from "Language Implementation Patterns" book.
 */
grammar Cymbol;

file:   (structDecl | functionDecl | varDecl)+ ;

structDecl
    :  'struct' ID '{' structMember+ '}' ';'
    ;

structMember
    :  type ID ';'   #FieldDecl
    | structDecl     #NestedStructDecl 
    ;

varDecl
    :   type ID ('=' expr)? ';'
    ;

type:   'float' | 'int' | 'void' | ID ; // user-defined types

functionDecl
    :   type ID '(' formalParameters? ')' block // "void f(int x) {...}"
    ;

formalParameters
    :   formalParameter (',' formalParameter)*
    ;
formalParameter
    :   type ID
    ;

block:  '{' stat* '}' ;   // possibly empty statement block

stat:   block
    |   varDecl
    |   structDecl
    |   'if' expr 'then' stat ('else' stat)?
    |   'return' expr? ';' 
    |   expr '=' expr ';' // assignment
    |   expr ';'          // func call
    ;

/* expr below becomes the following non-left recursive rule:
expr[int _p]
    :   ( '-' expr[6]
        | '!' expr[5]
        | ID
        | INT
        | '(' expr ')'
        )
        ( {8 >= $_p}? '*' expr[9]
        | {7 >= $_p}? ('+'|'-') expr[8]
        | {4 >= $_p}? '==' expr[5]
        | {10 >= $_p}? '[' expr ']'
        | {9 >= $_p}? '(' exprList? ')'
        )*
    ;
*/

expr:   ID '(' exprList? ')'    # Call
    |   ID ('.' ID)+            # MemberAccess
    |   expr '[' expr ']'       # Index
    |   '-' expr                # Negate
    |   '!' expr                # Not
    |   expr '*' expr           # Mult
    |   expr ('+'|'-') expr     # AddSub
    |   expr '==' expr          # Equal
    |   INT                     # Int
    |   BOOLEAN                 # Bool
    |   ID                      # Var
    |   '(' expr ')'            # Parens

    ;

exprList : expr (',' expr)* ;   // arg list

K_FLOAT : 'float';
K_INT   : 'int';
K_VOID  : 'void';
fragment
LETTER : [a-zA-Z] ;

INT :   [0-9]+ ;

BOOLEAN: 'true' | 'false';

ID  :   LETTER (LETTER | [0-9])* ; //specific rule should come before this

WS  :   [ \t\n\r]+ -> skip ;

SL_COMMENT
    :   '//' .*? '\n' -> skip
    ;
