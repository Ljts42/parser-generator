{
module Parser where

import Grammar
import Lexer
}

%name               parseGrammar StartR
%name               parseLexer LexR
%name               parseParser SyntR
%error              { parseError }

%tokentype          { TokenLex }

%token CODET        { CODEt $$ }
%token MODULET      { MODULEt $$ }
%token REGEXT       { REGEXt $$ }
%token TYPET        { TYPEt $$ }
%token NAMET        { NAMEt $$ }
%token COLONT       { COLONt }
%token SEMICOLONT   { SEMICOLONt }
%token VERTBART     { VERTBARt }
%%

StartR
  : LexR SyntR  { ($1, $2) }

LexR
  : MODULET TYPET TYPET TYPET TYPET TYPET CODET DescR   { GramLexer $1 $2 $3 $4 $5 $6 $7 $8 }

DescR
  : REGEXT CodeR DescR                                  { if $2 == ";"
                                                            then Skip $1 : $3
                                                            else GetToken $1 $2 : $3 }
  | {- empty -}                                         { [] }

CodeR
  : CODET                                               { $1 }
  | SEMICOLONT                                          { ";" }


SyntR
  : MODULET TYPET TYPET TermsR TermsR CODET GramR       { GramParser $1 $2 $3 $4 $5 $6 $7 }

TermsR
  : NAMET TermsR  { $1 : $2 }
  | SEMICOLONT    { [] }

GramR
  : NAMET CODET CODET COLONT TYPET RulesR GramR         { (NonTerm $1 $2 $3 $5 $6) : $7 }
  | {- empty -}                                         { [] }

RulesR
  : VERTBART ChildrenR CODET RulesR                     { (RuleP $2 $3) : $4 }
  | {- empty -}                                         { [] }

ChildrenR
  : NAMET TYPET ChildrenR                               { ($1, $2) : $3 }
  | {- empty -}                                         { [] }

{
parseError = error "Parse error"
}
