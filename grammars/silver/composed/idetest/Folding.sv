grammar silver:composed:idetest;

import silver:definition:core;
import silver:definition:concrete_syntax; --concreteProductionDcl
import silver:definition:type; --TypeExpr
import silver:modification:defaultattr; --aspectDefaultProduction
import silver:extension:patternmatching; --caseExpr

synthesized attribute foldableRanges :: [Location];
attribute foldableRanges occurs on AGDcl, AGDcls, Root, ProductionBody,
  ProductionStmts, ProductionStmt, Expr, AppExprs, AppExpr, AnnoAppExprs,
  AnnoExpr;

-- Root
aspect production root
top::Root ::= gdcl::GrammarDcl ms::ModuleStmts ims::ImportStmts ags::AGDcls
{
  -- TODO: we need a space function or something.
  local span :: Location =
    loc(gdcl.location.filename,
      gdcl.location.line, gdcl.location.column,
      ims.location.endLine, ims.location.endColumn,
      gdcl.location.index, ims.location.endIndex);
  top.foldableRanges = span :: ags.foldableRanges;
}

-- AGDcls
aspect production nilAGDcls
top::AGDcls ::=
{
  top.foldableRanges = [];
}

aspect production consAGDcls
top::AGDcls ::= h::AGDcl t::AGDcls
{
  top.foldableRanges = h.foldableRanges ++ t.foldableRanges;
}

-- AGDcl
aspect production emptyAGDcl
top::AGDcl ::=
{
  top.foldableRanges = [];
}

aspect production appendAGDcl
top::AGDcl ::= h::AGDcl t::AGDcl
{
  top.foldableRanges = h.foldableRanges ++ t.foldableRanges;
}

aspect default production
top::AGDcl ::=
{
  top.foldableRanges = [];
}

aspect production functionDcl
top::AGDcl ::= 'function' id::Name ns::FunctionSignature body::ProductionBody 
{
  top.foldableRanges = body.location :: body.foldableRanges;
}

aspect production productionDcl
top::AGDcl ::= 'abstract' 'production' id::Name ns::ProductionSignature body::ProductionBody
{
  top.foldableRanges = body.location :: body.foldableRanges;
}

aspect production concreteProductionDcl
top::AGDcl ::= 'concrete' 'production' id::Name ns::ProductionSignature pm::ProductionModifiers body::ProductionBody
{
  top.foldableRanges = body.location :: body.foldableRanges;
}

aspect production aspectProductionDcl
top::AGDcl ::= 'aspect' 'production' id::QName ns::AspectProductionSignature body::ProductionBody 
{
  top.foldableRanges = body.location :: body.foldableRanges;
}

aspect production aspectFunctionDcl
top::AGDcl ::= 'aspect' 'function' id::QName ns::AspectFunctionSignature body::ProductionBody 
{
  top.foldableRanges = body.location :: body.foldableRanges;
}

aspect production aspectDefaultProduction
top::AGDcl ::= 'aspect' 'default' 'production' lhs::Name '::' _ '::=' body::ProductionBody 
{
  top.foldableRanges = body.location :: body.foldableRanges;
}

aspect default production
top::ProductionBody ::=
{
  top.foldableRanges = [];
}

aspect production productionBody
top::ProductionBody ::= '{' stmts::ProductionStmts '}'
{
  top.foldableRanges = stmts.foldableRanges;
}

aspect default production
top::ProductionStmts ::=
{
  top.foldableRanges = [];
}

aspect production productionStmtsSnoc
top::ProductionStmts ::= h::ProductionStmts t::ProductionStmt
{
  top.foldableRanges = h.foldableRanges ++ t.foldableRanges;
}

aspect default production
top::ProductionStmt ::=
{
  top.foldableRanges = [];
}

aspect production productionStmtAppend
top::ProductionStmt ::= h::ProductionStmt t::ProductionStmt
{
  top.foldableRanges = h.foldableRanges ++ t.foldableRanges;
}

aspect production returnDef
top::ProductionStmt ::= 'return' e::Expr ';'
{
  top.foldableRanges = e.foldableRanges;
}

aspect production forwardsTo
top::ProductionStmt ::= 'forwards' 'to' e::Expr ';'
{
  top.foldableRanges = e.foldableRanges;
}

aspect production attributeDef
top::ProductionStmt ::= dl::DefLHS '.' attr::QNameAttrOccur '=' e::Expr ';'
{
  top.foldableRanges = e.foldableRanges;
}

aspect production errorAttributeDef
top::ProductionStmt ::= dl::Decorated DefLHS  attr::Decorated QNameAttrOccur  e::Expr
{
  top.foldableRanges = e.foldableRanges;
}

aspect production synthesizedAttributeDef
top::ProductionStmt ::= dl::Decorated DefLHS  attr::Decorated QNameAttrOccur  e::Expr
{
  top.foldableRanges = e.foldableRanges;
}

aspect production inheritedAttributeDef
top::ProductionStmt ::= dl::Decorated DefLHS  attr::Decorated QNameAttrOccur  e::Expr
{
  top.foldableRanges = e.foldableRanges;
}

aspect production valueEq
top::ProductionStmt ::= val::QName '=' e::Expr ';'
{
  top.foldableRanges = e.foldableRanges;
}

aspect production errorValueDef
top::ProductionStmt ::= val::Decorated QName  e::Expr
{
  top.foldableRanges = e.foldableRanges;
}

aspect production localValueDef
top::ProductionStmt ::= val::Decorated QName  e::Expr
{
  top.foldableRanges = e.foldableRanges;
}

aspect default production
top::Expr ::=
{
  top.foldableRanges = [];
}

aspect production nestedExpr
top::Expr ::= '(' e::Expr ')'
{
  top.foldableRanges = e.foldableRanges;
}

aspect production application
top::Expr ::= e::Expr '(' es::AppExprs ',' anns::AnnoAppExprs ')'
{
  top.foldableRanges = e.foldableRanges ++ es.foldableRanges ++ anns.foldableRanges;
}

aspect production applicationAnno
top::Expr ::= e::Expr '(' anns::AnnoAppExprs ')'
{
  top.foldableRanges = e.foldableRanges ++ anns.foldableRanges;
}

aspect production applicationExpr
top::Expr ::= e::Expr '(' es::AppExprs ')'
{
  top.foldableRanges = e.foldableRanges ++ es.foldableRanges;
}

aspect production applicationEmpty
top::Expr ::= e::Expr '(' ')'
{
  top.foldableRanges = e.foldableRanges;
}

aspect production errorApplication
top::Expr ::= e::Decorated Expr  es::AppExprs  anns::AnnoAppExprs
{
  top.foldableRanges = e.foldableRanges ++ es.foldableRanges ++ anns.foldableRanges;
}

aspect production functionApplication
top::Expr ::= e::Decorated Expr  es::AppExprs  anns::AnnoAppExprs
{
  top.foldableRanges = e.foldableRanges ++ es.foldableRanges ++ anns.foldableRanges;
}

aspect production functionInvocation
top::Expr ::= e::Decorated Expr  es::Decorated AppExprs  anns::Decorated AnnoAppExprs
{
  top.foldableRanges = e.foldableRanges ++ es.foldableRanges ++ anns.foldableRanges;
}

aspect production partialApplication
top::Expr ::= e::Decorated Expr  es::Decorated AppExprs  anns::Decorated AnnoAppExprs
{
  top.foldableRanges = e.foldableRanges ++ es.foldableRanges ++ anns.foldableRanges;
}

aspect production forwardAccess
top::Expr ::= e::Expr '.' 'forward'
{
  top.foldableRanges = e.foldableRanges;
}

aspect production access
top::Expr ::= e::Expr '.' q::QNameAttrOccur
{
  top.foldableRanges = e.foldableRanges;
}

aspect production errorAccessHandler
top::Expr ::= e::Decorated Expr  q::Decorated QNameAttrOccur
{
  top.foldableRanges = e.foldableRanges;
}

aspect production annoAccessHandler
top::Expr ::= e::Decorated Expr  q::Decorated QNameAttrOccur
{
  top.foldableRanges = e.foldableRanges;
}

aspect production terminalAccessHandler
top::Expr ::= e::Decorated Expr  q::Decorated QNameAttrOccur
{
  top.foldableRanges = e.foldableRanges;
}

aspect production undecoratedAccessHandler
top::Expr ::= e::Decorated Expr  q::Decorated QNameAttrOccur
{
  top.foldableRanges = e.foldableRanges;
}

aspect production accessBouncer
top::Expr ::= target::(Expr ::= Decorated Expr  Decorated QNameAttrOccur Location)
              e::Expr  q::Decorated QNameAttrOccur
{
  top.foldableRanges = e.foldableRanges;
}

aspect production decoratedAccessHandler
top::Expr ::= e::Decorated Expr  q::Decorated QNameAttrOccur
{
  top.foldableRanges = e.foldableRanges;
}

aspect production synDecoratedAccessHandler
top::Expr ::= e::Decorated Expr  q::Decorated QNameAttrOccur
{
  top.foldableRanges = e.foldableRanges;
}

aspect production inhDecoratedAccessHandler
top::Expr ::= e::Decorated Expr  q::Decorated QNameAttrOccur
{
  top.foldableRanges = e.foldableRanges;
}

aspect production errorDecoratedAccessHandler
top::Expr ::= e::Decorated Expr  q::Decorated QNameAttrOccur
{
  top.foldableRanges = e.foldableRanges;
}

aspect production decorateExprWithEmpty
top::Expr ::= 'decorate' e::Expr 'with' '{' '}'
{
  top.foldableRanges = e.foldableRanges;
}

aspect production decorateExprWith
top::Expr ::= 'decorate' e::Expr 'with' '{' inh::ExprInhs '}'
{
  top.foldableRanges = e.foldableRanges;
}

aspect production and
top::Expr ::= e1::Expr '&&' e2::Expr
{
  top.foldableRanges = e1.foldableRanges ++ e2.foldableRanges;
}

aspect production or
top::Expr ::= e1::Expr '||' e2::Expr
{
  top.foldableRanges = e1.foldableRanges ++ e2.foldableRanges;
}

aspect production not
top::Expr ::= '!' e::Expr
{
  top.foldableRanges = e.foldableRanges;
}

aspect production gt
top::Expr ::= e1::Expr '>' e2::Expr
{
  top.foldableRanges = e1.foldableRanges ++ e2.foldableRanges;
}

aspect production lt
top::Expr ::= e1::Expr '<' e2::Expr
{
  top.foldableRanges = e1.foldableRanges ++ e2.foldableRanges;
}

aspect production gteq
top::Expr ::= e1::Expr '>=' e2::Expr
{
  top.foldableRanges = e1.foldableRanges ++ e2.foldableRanges;
}

aspect production lteq
top::Expr ::= e1::Expr '<=' e2::Expr
{
  top.foldableRanges = e1.foldableRanges ++ e2.foldableRanges;
}

aspect production eqeq
top::Expr ::= e1::Expr '==' e2::Expr
{
  top.foldableRanges = e1.foldableRanges ++ e2.foldableRanges;
}

aspect production neq
top::Expr ::= e1::Expr '!=' e2::Expr
{
  top.foldableRanges = e1.foldableRanges ++ e2.foldableRanges;
}

aspect production ifThenElse
top::Expr ::= 'if' e1::Expr 'then' e2::Expr 'else' e3::Expr
{
  top.foldableRanges = [e1.location, e2.location, e3.location] ++ e1.foldableRanges ++
                       e2.foldableRanges ++ e3.foldableRanges;
}

aspect production plus
top::Expr ::= e1::Expr '+' e2::Expr
{
  top.foldableRanges = e1.foldableRanges ++ e2.foldableRanges;
}

aspect production minus
top::Expr ::= e1::Expr '-' e2::Expr
{
  top.foldableRanges = e1.foldableRanges ++ e2.foldableRanges;
}

aspect production multiply
top::Expr ::= e1::Expr '*' e2::Expr
{
  top.foldableRanges = e1.foldableRanges ++ e2.foldableRanges;
}

aspect production divide
top::Expr ::= e1::Expr '/' e2::Expr
{
  top.foldableRanges = e1.foldableRanges ++ e2.foldableRanges;
}

aspect production modulus
top::Expr ::= e1::Expr '%' e2::Expr
{
  top.foldableRanges = e1.foldableRanges ++ e2.foldableRanges;
}

aspect production neg
top::Expr ::= '-' e::Expr
{
  top.foldableRanges = e.foldableRanges;
}

aspect production plusPlus
top::Expr ::= e1::Expr '++' e2::Expr
{
  top.foldableRanges = e1.foldableRanges ++ e2.foldableRanges;
}

aspect production stringPlusPlus
top::Expr ::= e1::Decorated Expr  e2::Decorated Expr
{
  top.foldableRanges = e1.foldableRanges ++ e2.foldableRanges;
}

aspect production errorPlusPlus
top::Expr ::= e1::Decorated Expr  e2::Decorated Expr
{
  top.foldableRanges = e1.foldableRanges ++ e2.foldableRanges;
}

aspect production caseExpr
top::Expr ::= es::[Expr] ml::[AbstractMatchRule] failExpr::Expr retType::TypeExp
{
  top.foldableRanges = [top.location];
}

aspect production exprRef
top::Expr ::= e::Decorated Expr
{
  top.foldableRanges = e.foldableRanges;
}

aspect default production
top::AppExprs ::=
{
  top.foldableRanges = [];
}

aspect production snocAppExprs
top::AppExprs ::= es::AppExprs ',' e::AppExpr
{
  top.foldableRanges = es.foldableRanges ++ e.foldableRanges;
}

aspect production oneAppExprs
top::AppExprs ::= e::AppExpr
{
  top.foldableRanges = e.foldableRanges;
}

aspect default production
top::AppExpr ::=
{
  top.foldableRanges = [];
}

aspect production presentAppExpr
top::AppExpr ::= e::Expr
{
  top.foldableRanges = e.foldableRanges;
}

aspect default production
top::AnnoAppExprs ::=
{
  top.foldableRanges = [];
}

aspect production snocAnnoAppExprs
top::AnnoAppExprs ::= es::AnnoAppExprs ',' e::AnnoExpr
{
  top.foldableRanges = es.foldableRanges ++ e.foldableRanges;
}

aspect production oneAnnoAppExprs
top::AnnoAppExprs ::= e::AnnoExpr
{
  top.foldableRanges = e.foldableRanges;
}

aspect default production
top::AnnoExpr ::=
{
  top.foldableRanges = [];
}

aspect production annoExpr
top::AnnoExpr ::= qn::QName '=' e::AppExpr
{
  top.foldableRanges = e.foldableRanges;
}

