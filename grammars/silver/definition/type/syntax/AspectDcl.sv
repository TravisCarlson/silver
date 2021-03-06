grammar silver:definition:type:syntax;

attribute lexicalTypeVariables occurs on AspectProductionSignature, AspectProductionLHS, AspectRHS, AspectRHSElem, AspectFunctionSignature, AspectFunctionLHS;

function addNewLexicalTyVars_ActuallyVariables
[Def] ::= gn::String sl::Location l::[String]
{
  return if null(l) then []
         else lexTyVarDef(gn, sl, head(l), freshType()) ::
                  addNewLexicalTyVars_ActuallyVariables(gn, sl, tail(l));
}

-- This binds variables that appear in the signature to type variables, rather than skolem constants
-- as in productions declarations.  They will be unified with the "real" type, and therefore
-- will become those skolem constants.

aspect production aspectProductionDcl
top::AGDcl ::= 'aspect' 'production' id::QName ns::AspectProductionSignature body::ProductionBody 
{
  production attribute allLexicalTyVars :: [String];
  allLexicalTyVars = makeSet(ns.lexicalTypeVariables);
  
  sigDefs <- addNewLexicalTyVars_ActuallyVariables(top.grammarName, top.location, allLexicalTyVars);
}

aspect production aspectFunctionDcl
top::AGDcl ::= 'aspect' 'function' id::QName ns::AspectFunctionSignature body::ProductionBody 
{
  production attribute allLexicalTyVars :: [String];
  allLexicalTyVars = makeSet(ns.lexicalTypeVariables);
  
  sigDefs <- addNewLexicalTyVars_ActuallyVariables(top.grammarName, top.location, allLexicalTyVars);
}

aspect production aspectProductionSignature
top::AspectProductionSignature ::= lhs::AspectProductionLHS '::=' rhs::AspectRHS
{
  top.lexicalTypeVariables = makeSet(lhs.lexicalTypeVariables ++ rhs.lexicalTypeVariables);
}

aspect production aspectProductionLHSTyped
top::AspectProductionLHS ::= id::Name '::' t::Type
{
  top.lexicalTypeVariables = t.lexicalTypeVariables;
}
aspect production aspectProductionLHSFull
top::AspectProductionLHS ::= id::Name t::TypeExp
{
  top.lexicalTypeVariables = []; -- The above overrides this
}

aspect production aspectRHSElemNil
top::AspectRHS ::= 
{
  top.lexicalTypeVariables = [];
}

aspect production aspectRHSElemCons
top::AspectRHS ::= h::AspectRHSElem t::AspectRHS
{
  top.lexicalTypeVariables = makeSet(h.lexicalTypeVariables ++ t.lexicalTypeVariables);
}

aspect production aspectRHSElemTyped
top::AspectRHSElem ::= id::Name '::' t::Type
{
  top.lexicalTypeVariables = t.lexicalTypeVariables;
}
aspect production aspectRHSElemFull
top::AspectRHSElem ::= id::Name t::TypeExp
{
  top.lexicalTypeVariables = []; -- The above overrides this
}

aspect production aspectFunctionSignature
top::AspectFunctionSignature ::= lhs::AspectFunctionLHS '::=' rhs::AspectRHS 
{
  top.lexicalTypeVariables = makeSet(lhs.lexicalTypeVariables ++ rhs.lexicalTypeVariables);
}

aspect production functionLHSType
top::AspectFunctionLHS ::= t::Type
{
  top.lexicalTypeVariables = t.lexicalTypeVariables;
}
