grammar silver:definition:env:env_parser;

import silver:definition:env;
import silver:definition:regex hiding RegexRBrack_t, RegexLBrack_t, RegexLParen_t, RegexRParen_t; -- TODO: a bit of a hack?
import silver:definition:type;

import silver:definition:core only grammarName, env;

lexer class C_0;
lexer class C_1 dominates C_0;

ignore terminal WS /[\ \r\n\t]+/ lexer classes {C_0};

terminal LB_t    '[';
terminal RB_t    ']';
terminal Comma_t ',';
terminal LParent_t    '(';
terminal RParent_t    ')';
terminal RegExprDelim '/' lexer classes {C_0};

terminal Sigturnstile '::=' ;

terminal Id_t /[\']([^\'\\]|[\\][\']|[\\][\\]|[\\]n|[\\]r|[\\]t)*[\']/ lexer classes {C_0};
terminal Num_t /\-?[0-9]+/ lexer classes {C_0};
terminal EscapedStringTerm /"([^\"\\]|\\.)*"/ lexer classes {C_1};

terminal T_t 't';
terminal F_t 'f';

terminal DefaultTerm  'default' lexer classes {C_1};

-- Dcls
terminal LocalTerm       'loc'  lexer classes {C_1};
terminal InheritedTerm   'inh'  lexer classes {C_1};
terminal SynthesizedTerm 'syn'  lexer classes {C_1};
terminal OccursTerm      '@'    lexer classes {C_1};
terminal ProdAttrTerm    'p@'   lexer classes {C_1};
terminal ForwardTerm     'fwd'  lexer classes {C_1};
terminal GlobTerm        'glob' lexer classes {C_1};
terminal Anno_t          'anno' lexer classes {C_1};
terminal AnnoAt_t       'anno@' lexer classes {C_1};

--shared dcl/type
terminal ProductionTerm  'prod' lexer classes {C_1};
terminal FunctionTerm    'fun'  lexer classes {C_1};
terminal TerminalTerm    'term' lexer classes {C_1};
terminal NonterminalTerm 'nt'   lexer classes {C_1};

-- Types
terminal IntegerTerm     'int'       lexer classes {C_1};
terminal FloatTerm       'float'     lexer classes {C_1};
terminal StringTerm      'string'    lexer classes {C_1};
terminal BooleanTerm     'bool'      lexer classes {C_1};
terminal DecoratedTerm   'decorated' lexer classes {C_1};
terminal IOTerm          'io'        lexer classes {C_1};
terminal ITyVar /[A-Za-z]+/ lexer classes {C_0}; 

-- signatures
terminal SignatureTerm      'signature' lexer classes {C_1};
terminal SignatureElementTerm 'element' lexer classes {C_1};

-- top level, root spec parts
terminal DeclaredNameTerm     'declaredName'     lexer classes {C_1};
terminal GrammarTimeTerm      'grammarTime'      lexer classes {C_1};
terminal ModuleNamesTerm      'moduleNames'      lexer classes {C_1};
terminal AllDepsTerm          'allDeps'          lexer classes {C_1};
terminal DefsTerm             'defs'             lexer classes {C_1};
terminal ExportedGrammarsTerm 'exportedGrammars' lexer classes {C_1};
terminal OptionalGrammarsTerm 'optionalGrammars' lexer classes {C_1};
terminal CondBuildTerm        'condBuild'        lexer classes {C_1};
terminal GrammarSourceTerm    'grammarSource'    lexer classes {C_1};


synthesized attribute signature :: NamedSignature;
synthesized attribute elements :: [NamedSignatureElement];
synthesized attribute element :: NamedSignatureElement;
synthesized attribute typereps :: [TypeExp];
synthesized attribute tyvars :: [TyVar];

{- The "uninteresting" plumbing of interface files: -}

nonterminal IRoot with defs, exportedGrammars, optionalGrammars, condBuild, declaredName, grammarSource, moduleNames, grammarName, allGrammarDependencies, grammarTime;
nonterminal IDefs with defs, env, grammarName; -- including square brackets
nonterminal IDefsInner with defs, env, grammarName; -- inside square brackets
nonterminal ITypeReps with env, typereps, grammarName; -- including square brackets
nonterminal ITypeRepsInner with env, typereps, grammarName; -- inside square brackets

{- Extension points! -}

{- Top-level elements of the interface file -}
closed nonterminal IRootPart with defs, exportedGrammars, optionalGrammars, condBuild, declaredName, grammarSource, moduleNames, grammarName, allGrammarDependencies, grammarTime;
{- A DclInfo record -}
closed nonterminal IDclInfo with defs, env, grammarName;
{- A TypeExp record -}
closed nonterminal ITypeRep with env, typerep, grammarName;

{- Utilities -}
nonterminal ITyVarDcls with defs, tyvars;
 nonterminal ITyVarDclsInner with defs, tyvars;
nonterminal INamedSignature with signature, env, grammarName;
 nonterminal INamedSignatureElement with element, env, grammarName;
 nonterminal INamedSignatureElements with elements, env, grammarName;
 nonterminal INamedSignatureElementsInner with elements, env, grammarName;

-- a few simple utilities

nonterminal IName with aname;
nonterminal ILocation with alocation;
nonterminal IBool with bval;
nonterminal INames with names;
nonterminal INamesInner with names;
nonterminal IString with str;

synthesized attribute bval :: Boolean;
synthesized attribute names :: [String];
synthesized attribute aname :: String;
synthesized attribute str :: String;
synthesized attribute alocation :: Location;

concrete production aTrue
top::IBool ::= 't'
{
  top.bval = true;
}
concrete production aFalse
top::IBool ::= 'f'
{
  top.bval = false;
}

concrete production aLocationInfo
top::ILocation ::= filename::IName ',' line::Num_t ',' column::Num_t
{
  top.alocation = loc(filename.aname, toInt(line.lexeme), toInt(column.lexeme), -1, -1, -1, -1);
}

concrete production aString
top::IString ::= s::EscapedStringTerm
{
  top.str = unescapeString(substring(1,length(s.lexeme)-1,s.lexeme));
}

concrete production aName
top::IName ::= i::Id_t
{
  top.aname = substring(1, length(i.lexeme)-1, i.lexeme);
}

concrete production aNamesNone
top::INames ::= '[' ']'
{
  top.names = [];
}
concrete production aNamesOne
top::INames ::= '[' d::INamesInner ']'
{
  top.names = d.names;
}
concrete production aNamesInnerOne
top::INamesInner ::= d::IName
{
  top.names = [d.aname];
}
concrete production aNamesInnerCons
top::INamesInner ::= d1::IName ',' d2::INamesInner
{
  top.names = [d1.aname] ++ d2.names;
}


--The Grammar 

concrete production aRoot1
top::IRoot ::= r::IRootPart
{
  top.declaredName = r.declaredName;
  top.grammarTime = r.grammarTime;
  top.grammarSource = r.grammarSource;
  top.defs = r.defs;
  top.moduleNames = r.moduleNames;
  top.allGrammarDependencies = r.allGrammarDependencies;
  top.exportedGrammars = r.exportedGrammars;
  top.optionalGrammars = r.optionalGrammars;
  top.condBuild = r.condBuild;  
}

concrete production aRoot2
top::IRoot ::= r1::IRootPart r2::IRoot
{
  top.declaredName = if r1.declaredName == "" then r2.declaredName else r1.declaredName;
  top.grammarTime = if r1.grammarTime == -1 then r2.grammarTime else r1.grammarTime;
  top.grammarSource = if r1.grammarSource == "" then r2.grammarSource else r1.grammarSource;
  top.defs = r1.defs ++ r2.defs;
  top.moduleNames = r1.moduleNames ++ r2.moduleNames;
  top.allGrammarDependencies = r1.allGrammarDependencies ++ r2.allGrammarDependencies;
  top.exportedGrammars = r1.exportedGrammars ++ r2.exportedGrammars;
  top.optionalGrammars = r1.optionalGrammars ++ r2.optionalGrammars;
  top.condBuild = r1.condBuild ++ r2.condBuild;
}

-- The pieces

aspect default production
top::IRootPart ::=
{
  top.declaredName = "";
  top.grammarTime = -1;
  top.grammarSource = "";
  top.moduleNames = [];
  top.allGrammarDependencies = [];
  top.defs = [];
  top.exportedGrammars = [];
  top.optionalGrammars = [];
  top.condBuild = [];
}

concrete production aRootDeclaredName
top::IRootPart ::= 'declaredName' i::IName
{
  top.declaredName = i.aname;
}

concrete production aRootGrammarTime
top::IRootPart ::= 'grammarTime' i::Num_t
{
  top.grammarTime = toInt(i.lexeme);
}

concrete production aRootGrammarSource
top::IRootPart ::= 'grammarSource' s::IString
{
  top.grammarSource = s.str;
}

concrete production aRootModuleNames
top::IRootPart ::= 'moduleNames' i::INames
{
  top.moduleNames = i.names;
}

concrete production aRootAllDeps
top::IRootPart ::= 'allDeps' i::INames
{
  top.allGrammarDependencies = i.names;
}

concrete production aRootDefs
top::IRootPart ::= 'defs' i::IDefs
{
  top.defs = i.defs;
  i.env = emptyEnv();
}

concrete production aRootExportedGrammars
top::IRootPart ::= 'exportedGrammars' i::INames
{
  top.exportedGrammars = i.names;
}

concrete production aRootOptionalGrammars
top::IRootPart ::= 'optionalGrammars' i::INames
{
  top.optionalGrammars = i.names;
}

concrete production aRootCondBuilds
top::IRootPart ::= 'condBuild' i::INames
{
  top.condBuild = unfoldCB(i.names);
}

function unfoldCB
[[String]] ::= lst::[String]
{
  return if null(lst) then [] else cons( [head(lst), head(tail(lst))], unfoldCB(tail(tail(lst))));
}

--The lists
concrete production aDefsNone
top::IDefs ::= '[' ']'
{
  top.defs = [];
}

concrete production aDefsOne
top::IDefs ::= '[' d::IDefsInner ']'
{
  top.defs = d.defs;
}

concrete production aDefsInnerOne
top::IDefsInner ::= d::IDclInfo
{
  top.defs = d.defs;
}

concrete production aDefsInnerCons
top::IDefsInner ::= d1::IDclInfo ',' d2::IDefsInner
{
  top.defs = d1.defs ++ d2.defs;
}

concrete production aTypeRepsNone
top::ITypeReps ::= '[' ']'
{
  top.typereps = [];
}

concrete production aTypeRepsOne
top::ITypeReps ::= '[' t::ITypeRepsInner ']'
{
  top.typereps = t.typereps;
}

concrete production aTypeRepsInnerOne
top::ITypeRepsInner ::= t::ITypeRep
{
  top.typereps = [t.typerep];
}

concrete production aTypeRepsInnerCons
top::ITypeRepsInner ::= t1::ITypeRep ',' t2::ITypeRepsInner
{
  top.typereps = [t1.typerep] ++ t2.typereps;
}

concrete production aNamedSignatureElementsNone
top::INamedSignatureElements ::= '['']'
{
  top.elements = [];
}

concrete production aNamedSignatureElementsOne
top::INamedSignatureElements ::= '[' t::INamedSignatureElementsInner ']'
{
  top.elements = t.elements;
}

concrete production aNamedSignatureElementsInnerOne
top::INamedSignatureElementsInner ::= t::INamedSignatureElement
{
  top.elements = [t.element];
}

concrete production aNamedSignatureElementsInnerCons
top::INamedSignatureElementsInner ::= t1::INamedSignatureElement ',' t2::INamedSignatureElementsInner
{
  top.elements = [t1.element] ++ t2.elements;
}

concrete production aTyVarDclsOne
top::ITyVarDcls ::= '[' t::ITyVarDclsInner ']'
{
  top.defs = t.defs;
  top.tyvars = t.tyvars;
}
concrete production aTyVarDclsNone
top::ITyVarDcls ::= '[' ']'
{
  top.defs = [];
  top.tyvars = [];
}

concrete production aTyVarDclsInnerOne
top::ITyVarDclsInner ::= t1::ITyVar
{
  local attribute tv :: TyVar;
  tv = freshTyVar();
  
  top.defs = [lexTyVarDef("IFACE", bogusLocation(), t1.lexeme, skolemTypeExp(tv))];
  top.tyvars = [tv];
}

concrete production aTyVarDclsInnerCons
top::ITyVarDclsInner ::= t1::ITyVar ',' t2::ITyVarDclsInner
{
  local attribute tv :: TyVar;
  tv = freshTyVar();
  
  top.defs = lexTyVarDef("IFACE", bogusLocation(), t1.lexeme, skolemTypeExp(tv)) :: t2.defs;
  top.tyvars = [tv] ++ t2.tyvars;
}

--The DclInfos

concrete production aDclInfoLocal
top::IDclInfo ::= 'loc' '(' l::ILocation ',' fn::IName ',' t::ITypeRep ')'
{
  top.defs = [localDef(top.grammarName, l.alocation, fn.aname, t.typerep)];
}

concrete production aDclInfoProduction
top::IDclInfo ::= 'prod' '(' l::ILocation ',' td::ITyVarDcls ',' s::INamedSignature ')'
{
  s.env = newScopeEnv(td.defs, top.env);
  
  top.defs = [prodDef(top.grammarName, l.alocation, s.signature)];
}

concrete production aDclInfoFunction
top::IDclInfo ::= 'fun' '(' l::ILocation ',' td::ITyVarDcls ',' s::INamedSignature ')'
{
  s.env = newScopeEnv(td.defs, top.env);
  
  top.defs = [funDef(top.grammarName, l.alocation, s.signature)];
}

concrete production aDclInfoGlobalValue
top::IDclInfo ::= 'glob' '(' l::ILocation ',' fn::IName ',' t::ITypeRep ')'
{
  top.defs = [globalDef(top.grammarName, l.alocation, fn.aname, t.typerep)];
}

concrete production aDclInfoNonterminal
top::IDclInfo ::= 'nt' '(' l::ILocation ',' s::IName ',' td::ITyVarDcls ',' t::ITypeRep ',' cl::IBool ')'
{
  t.env = newScopeEnv(td.defs, top.env);
  
  top.defs = if cl.bval
             then [closedNtDef(top.grammarName, l.alocation, s.aname, td.tyvars, t.typerep)]
             else [ntDef(top.grammarName, l.alocation, s.aname, td.tyvars, t.typerep)];
}

concrete production aDclInfoTerminal
top::IDclInfo ::= 'term' '(' l::ILocation ',' n::IName ',' '/' r::Regex_R '/' ')'
{
  top.defs = [termDef(top.grammarName, l.alocation, n.aname, r)];
}

concrete production aDclInfoSynthesized
top::IDclInfo ::= 'syn' '(' l::ILocation ',' fn::IName ',' td::ITyVarDcls ',' t::ITypeRep ')'
{
  t.env = newScopeEnv(td.defs, top.env);
  
  top.defs = [synDef(top.grammarName, l.alocation, fn.aname, td.tyvars, t.typerep)];
}

concrete production aDclInfoInherited
top::IDclInfo ::= 'inh' '(' l::ILocation ',' fn::IName ',' td::ITyVarDcls ',' t::ITypeRep ')'
{
  t.env = newScopeEnv(td.defs, top.env);
  
  top.defs = [inhDef(top.grammarName, l.alocation, fn.aname, td.tyvars, t.typerep)];
}

concrete production aDclInfoProdAttr
top::IDclInfo ::= 'p@' '(' l::ILocation ',' td::ITyVarDcls ',' s::INamedSignature ',' t::IDefs ')'
{
  s.env = newScopeEnv(td.defs, top.env);

  top.defs = [prodOccursDef(top.grammarName, l.alocation, s.signature, t.defs)];
}

concrete production aDclInfoForward
top::IDclInfo ::= 'fwd' '(' l::ILocation ',' t::ITypeRep ')'
{
  top.defs = [forwardDef(top.grammarName, l.alocation, t.typerep)];
}

concrete production aDclInfoOccurs
top::IDclInfo ::= '@' '(' l::ILocation ',' fnnt::IName ',' fnat::IName ',' td::ITyVarDcls ',' ntt::ITypeRep ',' att::ITypeRep ')'
{
  ntt.env = newScopeEnv(td.defs, top.env);
  att.env = ntt.env;
  
  local attribute fresh :: [TyVar];
  fresh = freshTyVars(length(td.tyvars));

  -- Recall that constraint on occurs DclInfos: the types need to be tyvars, not skolem constants.
  
  top.defs = [oDef(occursDcl(top.grammarName, l.alocation, fnnt.aname, fnat.aname, 
                        freshenTypeExpWith(ntt.typerep, td.tyvars, fresh),
                        freshenTypeExpWith(att.typerep, td.tyvars, fresh)))];
}

concrete production aDclInfoAnno
top::IDclInfo ::= 'anno' '(' l::ILocation ',' fn::IName ',' td::ITyVarDcls ',' t::ITypeRep ')'
{
  t.env = newScopeEnv(td.defs, top.env);
  
  top.defs = [annoDef(top.grammarName, l.alocation, fn.aname, td.tyvars, t.typerep)];
}

concrete production aDclInfoAnnoInstance
top::IDclInfo ::= 'anno@' '(' l::ILocation ',' fnnt::IName ',' fnat::IName ',' td::ITyVarDcls ',' ntt::ITypeRep ',' att::ITypeRep ')'
{
  ntt.env = newScopeEnv(td.defs, top.env);
  att.env = ntt.env;
  
  local attribute fresh :: [TyVar];
  fresh = freshTyVars(length(td.tyvars));

  -- Recall that constraint on occurs DclInfos: the types need to be tyvars, not skolem constants.
  
  top.defs = [
    annoInstanceDef(top.grammarName, l.alocation, fnnt.aname, fnat.aname, 
      freshenTypeExpWith(ntt.typerep, td.tyvars, fresh),
      freshenTypeExpWith(att.typerep, td.tyvars, fresh))];
}

--The TypeReps
concrete production aTypeRepInteger
top::ITypeRep ::= 'int'
{
  top.typerep = intTypeExp();
}

concrete production aTypeRepFloat
top::ITypeRep ::= 'float'
{
  top.typerep = floatTypeExp();
}

concrete production aTypeRepString
top::ITypeRep ::= 'string'
{
  top.typerep = stringTypeExp();
}

concrete production aTypeRepBoolean
top::ITypeRep ::= 'bool'
{
  top.typerep = boolTypeExp();
}

concrete production aTypeRepTerminal
top::ITypeRep ::= 'term' '(' n::IName ')'
{
  top.typerep = terminalTypeExp(n.aname);
}

concrete production aTypeRepNonterminal
top::ITypeRep ::= 'nt' '(' n::IName ',' ty::ITypeReps ')'
{
  top.typerep = nonterminalTypeExp(n.aname, ty.typereps);
}

concrete production aTypeRepDecorated
top::ITypeRep ::= 'decorated' '(' t::ITypeRep ')'
{
  top.typerep = decoratedTypeExp(t.typerep);
}

concrete production aTypeRepFunction
top::ITypeRep ::= 'fun' '(' it::ITypeReps ','  ot::ITypeRep na::INamedArgTypes ')'
{
  top.typerep = functionTypeExp(ot.typerep, it.typereps, na.aNamedArgs);
}

terminal Semi ';';
terminal Eq '=';
nonterminal INamedArgTypes with grammarName, env, aNamedArgs;

synthesized attribute aNamedArgs :: [NamedArgType];

concrete production aNamedArgTypeNil
top::INamedArgTypes ::=
{
  top.aNamedArgs = [];
}

concrete production aNamedArgTypeCons
top::INamedArgTypes ::= ';' n::IName '=' ty::ITypeRep rest::INamedArgTypes
{
  top.aNamedArgs = namedArgType(n.aname, ty.typerep) :: rest.aNamedArgs;
}


concrete production aTypeRepVar
top::ITypeRep ::= t::ITyVar
{
  local attribute res :: [DclInfo];
  res = getTypeDcl(t.lexeme, top.env);
  
  top.typerep = if null(res)
                then error("INTERNAL ERROR: interface file for " ++ top.grammarName ++
                           " lacks type for tyvar " ++ t.lexeme ++
                           " on line " ++ toString(t.line) ++
                           " column " ++ toString(t.column))
                else head(res).typerep;
}

--The NamedSignatures
concrete production aNamedSignatureDcl
top::INamedSignature ::= 'signature' '(' fn::IName ',' i::INamedSignatureElements ',' o::INamedSignatureElement ',' n::INamedSignatureElements ')'
{
  top.signature = namedSignature(fn.aname, i.elements, o.element, n.elements);
}

concrete production aNamedSignatureElementDcl
top::INamedSignatureElement ::= 'element' '(' n::IName ',' t::ITypeRep ')'
{
  top.element = namedSignatureElement(n.aname, t.typerep);
}

