grammar silver:translation:java:concrete_syntax:copper;

import silver:definition:env;
import silver:util;

synthesized attribute isLexerClassDeclaration :: Boolean;
attribute isLexerClassDeclaration occurs on EnvItem;
attribute submitsTo occurs on EnvItem;
attribute termDominates occurs on EnvItem;

function lexerClassEnvItem
Decorated EnvItem ::= n::String s::[String] d::[String]
{
  return decorate i_lexerClassEnvItem(n, s, d) with {};
}

abstract production i_lexerClassEnvItem
top::EnvItem ::= n::String s::[String] d::[String]
{
  top.unparse = "lexer_class('" ++ n ++ "', [" ++ (if null(s) then "" else "'" ++ folds("','", s) ++ "'") ++ "], [" ++ (if null(d) then "" else "'" ++ folds("','", d) ++ "'") ++ "])";

  top.submitsTo = s;
  top.termDominates = d;
  -- required to be defined.
  top.itemName = n;

  top.isLexerClassDeclaration = true;

  forwards to i_defaultEnvItem();
}

function getLexerClassDclOne
[Decorated EnvItem] ::= search::String e::Decorated Env
{
  return searchDclsOne(search, e.restTree);
}

function getLexerClassDcl
[Decorated EnvItem] ::= search::String e::Decorated Env
{
  return searchDcls(search, e.restTree);
}

function addLexerClassDcl
Decorated Defs ::= n::String s::[String] d::[String] e::Decorated Defs
{
  return consDefs(lexerClassEnvItem(n, s, d), e);
}

synthesized attribute isDisambiguationDeclaration :: Boolean;
attribute isDisambiguationDeclaration occurs on EnvItem;


function disambiguationEnvItem
Decorated EnvItem ::= n::String
{
  return decorate i_disambiguationEnvItem(n) with {};
}

abstract production i_disambiguationEnvItem
top::EnvItem ::= n::String
{
  top.unparse = "lexer_class('" ++ n ++ "')";

  -- required to be defined.
  top.itemName = n;

  top.isDisambiguationDeclaration = true;

  forwards to i_defaultEnvItem();
}

function getDisambiguationDclOne
[Decorated EnvItem] ::= search::String e::Decorated Env
{
  return searchDclsOne(search, e.restTree);
}

function getDisambiguationDcl
[Decorated EnvItem] ::= search::String e::Decorated Env
{
  return searchDcls(search, e.restTree);
}

function addDisambiguationDcl
Decorated Defs ::= n::String e::Decorated Defs
{
  return consDefs(disambiguationEnvItem(n), e);
}


aspect production i_defaultEnvItem
top::EnvItem ::= 
{
  top.isLexerClassDeclaration = false;
  top.isDisambiguationDeclaration = false;
}