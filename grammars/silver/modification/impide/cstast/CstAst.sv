grammar silver:modification:impide:cstast;

imports silver:definition:concrete_syntax:ast;
imports silver:definition:regex;
imports silver:definition:type;
imports silver:definition:env;

imports silver:translation:java:core only makeIdName, makeClassName, makeNTClassName;
imports silver:translation:java:type only transType;
imports silver:modification:impide:spec;

-- The attribute into which the copper parser in new XML skin is written
synthesized attribute nxmlCopper :: String occurs on SyntaxRoot, Syntax, SyntaxDcl;

-- The attribute carries the name of IDE plugin packge, which is ultimately 
-- derived from the name of grammar where IDE declaration block is defined.
inherited attribute jPkgName :: String occurs on SyntaxRoot;
-- The attribute is the name of generated Java parser.
inherited attribute jParserName :: String occurs on SyntaxRoot;

aspect default production
top::SyntaxRoot ::=
{
  top.nxmlCopper = error("This should only ever be demanded from cstRoot.");
  top.fontList = error("This should only ever be demanded from cstRoot.");
  top.termFontPairList = error("This should only ever be demanded from cstRoot.");
}

aspect production cstRoot
top::SyntaxRoot ::= parsername::String  startnt::String  s::Syntax  terminalPrefixes::[Pair<String String>]
{

  -- 1) font information
  top.fontList = s2.fontList;
  top.termFontPairList = s2.termFontPairList;

  -- 2) The copper parser
  top.nxmlCopper =
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\n" ++

"<CopperSpec xmlns=\"http://melt.cs.umn.edu/copper/xmlns\">\n" ++
"  <Parser id=\"" ++ makeCopperName(parsername) ++ "\" isUnitary=\"true\">\n" ++
"    <PP>" ++ parsername ++ "</PP>\n" ++
"    <Grammars><GrammarRef id=\"" ++ s2.containingGrammar ++ "\"/></Grammars>\n" ++
"    <StartSymbol>" ++ xmlCopperRef(head(startFound)) ++ "</StartSymbol>\n" ++
"    <StartLayout>" ++ univLayout ++ "</StartLayout>\n" ++
-- DIFFERENCES ****************************************************************
"    <ClassAuxiliaryCode>\n" ++
"      <Code><![CDATA[\n" ++ 

"\t//IDE Extension START\n\n" ++

  getParseTreeCode(top.jPkgName ++ ".imp.coloring." ++ top.jParserName ++ "_TokenClassifier") ++

"\n\t//IDE Extension END\n" ++

"      ]]></Code>\n" ++
"    </ClassAuxiliaryCode>\n" ++
"    <ParserInitCode>\n" ++
"      <Code><![CDATA[\n" ++
"        reset();\n" ++
"      ]]></Code>\n" ++
"    </ParserInitCode>\n" ++
"    <Preamble>\n" ++
"<Code><![CDATA[\n" ++
"import java.util.ArrayList;\n" ++
"import java.util.Iterator;\n" ++
"import java.util.List;\n" ++
"import java.util.HashMap;\n" ++
"import java.util.Map;\n" ++
"import edu.umn.cs.melt.copper.runtime.engines.single.scanner.SingleDFAMatchData;\n" ++
"import edu.umn.cs.melt.ide.copper.*;\n" ++
"import edu.umn.cs.melt.ide.copper.coloring.*;\n" ++
"]]></Code>\n" ++
"    </Preamble>\n" ++
--"    <SemanticActionAuxiliaryCode>\n" ++
--"    </SemanticActionAuxiliaryCode>\n" ++
-- END DIFFERENCES ************************************************************
"  </Parser>\n\n" ++

"  <Grammar id=\"" ++ s2.containingGrammar ++ "\">\n\n" ++
"    <PP>" ++ s2.containingGrammar ++ "</PP>\n\n" ++
"    <Layout>" ++ univLayout ++ "</Layout>\n\n" ++
"    <Declarations>\n" ++
"      <ParserAttribute id=\"context\">\n" ++
"        <Type><![CDATA[common.DecoratedNode]]></Type>\n" ++
"        <Code><![CDATA[context = common.TopNode.singleton;]]></Code>\n" ++
"      </ParserAttribute>\n" ++
       s2.nxmlCopper ++
"    </Declarations>\n" ++
"  </Grammar>\n" ++
"</CopperSpec>\n";
}


{-
Assumptions we make about initial Syntax:

1. All type parameter lists are the appropriate length. (Silver type checking)
-}

function getParseTreeCode
String ::= tokenClassifierClass::String
{
return 
"\tprotected List<CopperToken> tokenList = null;\n" ++
"\n" ++

"\tprivate void addToken(SingleDFAMatchData _terminal, int start, int end){\n" ++
"\t\tif(start == end) return;\n" ++
"\t\tCopperToken t = new CopperToken(getKind(_terminal), start, end);\n" ++
"\t\ttokenList.add(t);\n" ++
"\t}\n" ++
"\n" ++

"\t/**\n" ++
"\t * Called at start of parsing\n" ++
"\t*/\n" ++
"\tpublic void reset(){\n" ++
"\t\t\ttokenList = new ArrayList<CopperToken>();\n" ++
"\t}\n" ++
"\t\n" ++

"\tpublic List<CopperToken> getTokens() {\n" ++
"\t\treturn tokenList; // The way we reset this iterator when parsing again is to create a new list, so this is defacto immutable\n" ++
"\t}\n" ++
"\t\n" ++

"\tprotected int getKind(SingleDFAMatchData scanResult){\n" ++
"\t\tString term = getSymbolNames()[scanResult.firstTerm];\n" ++
"\t\treturn " ++ tokenClassifierClass ++ ".getKind(term);\n" ++
"\t}\n" ++
"\t\n";
}


