grammar silver:definition:concrete_syntax;
import silver:definition:core;

aspect production globalValueDclConcrete
top::AGDcl ::= 'global' id::Name '=' e::Expr ';'
{
  top.parserDcls = [];
  top.nonTerminalDcls = [];
  top.terminalDcls = [];
  top.ruleDcls = [];
}