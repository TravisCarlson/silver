grammar silver:extension:templating:syntax;

import silver:definition:core only Expr, RCurly_t, LITERAL;

terminal TripleQuote /\"\"\"/ lexer classes {LITERAL};
terminal DoubleDollar '$$' lexer classes {LITERAL};
terminal QuoteWater /[^$\r\n\t\"\\]+/ lexer classes {LITERAL};
terminal SingleLineQuoteWater /([^$\r\n\t\"\\]|[\\][\"]|[\\][\\]|[\\]n|[\\]r|[\\]t)+/ lexer classes {LITERAL};
terminal LiteralNewline /(\n|\r\n)/ lexer classes {LITERAL};
terminal LiteralTab /\t/ lexer classes {LITERAL};
terminal LiteralQuote /\"/ lexer classes {LITERAL};
terminal LiteralBackslash /\\/ lexer classes {LITERAL};

terminal OpenEscape '${';

{-- A string without the first triple quote, with escaped expressions within -}
nonterminal TemplateString with location;
{-- A single-line string without the first quote, with escaped expressions within -}
nonterminal SingleLineTemplateString with location;
{-- A list of alternating String/Exprs -}
nonterminal TemplateStringBody with location;
{-- A single-line list of alternating String/Exprs -}
nonterminal SingleLineTemplateStringBody with location;
{-- Either a String or an Expr -}
nonterminal TemplateStringBodyItem with location;
{-- Either a single-line String or an Expr -}
nonterminal SingleLineTemplateStringBodyItem with location;
{-- An escape -}
nonterminal NonWater with location;
{-- List that yields a string -}
nonterminal Water with location, waterString;
{-- List that yields a single-line string -}
nonterminal SingleLineWater with location, waterString;
{-- Components that yield a string -}
nonterminal WaterItem with location, waterString;
{-- Components that yield a single-line string -}
nonterminal SingleLineWaterItem with location, waterString;

{-- The string corresponding to the water -}
synthesized attribute waterString :: String;

concrete production templateString
top::TemplateString ::= b::TemplateStringBody TripleQuote
layout {}
{
}

concrete production templateStringEmpty
top::TemplateString ::= TripleQuote
layout {}
{
}

concrete production singleLineTemplateString
top::SingleLineTemplateString ::= b::SingleLineTemplateStringBody LiteralQuote
layout {}
{
}

concrete production singleLineTemplateStringEmpty
top::SingleLineTemplateString ::= LiteralQuote
layout {}
{
}

concrete production bodyCons
top::TemplateStringBody ::= h::TemplateStringBodyItem  t::TemplateStringBody
layout {}
{
}

concrete production bodyOne
top::TemplateStringBody ::= h::TemplateStringBodyItem
layout {}
{
}

concrete production bodyOneWater
top::TemplateStringBody ::= h::Water
layout {}
{
}

concrete production singleLineBodyCons
top::SingleLineTemplateStringBody ::= h::SingleLineTemplateStringBodyItem  t::SingleLineTemplateStringBody
layout {}
{
}

concrete production singleLineBodyOne
top::SingleLineTemplateStringBody ::= h::SingleLineTemplateStringBodyItem
layout {}
{
}

concrete production singleLineBodyOneWater
top::SingleLineTemplateStringBody ::= h::SingleLineWater
layout {}
{
}

concrete production itemWaterEscape
top::TemplateStringBodyItem ::= w::Water nw::NonWater
layout {}
{
}

concrete production itemEscape
top::TemplateStringBodyItem ::= nw::NonWater
layout {}
{
}

concrete production singleLineItemWaterEscape
top::SingleLineTemplateStringBodyItem ::= w::SingleLineWater nw::NonWater
layout {}
{
}

concrete production singleLineItemEscape
top::SingleLineTemplateStringBodyItem ::= nw::NonWater
layout {}
{
}

concrete production nonwater
top::NonWater ::= '${' e::Expr '}'
--layout {} -- TODO: need to control layout better... But this should allow it here.
{
}

concrete production waterCons
top::Water ::= h::Water  t::WaterItem
layout {}
{
  top.waterString = h.waterString ++ t.waterString;
}

concrete production waterOne
top::Water ::= h::WaterItem
layout {}
{
  top.waterString = h.waterString;
}

concrete production water
top::WaterItem ::= w::QuoteWater
layout {}
{
  top.waterString = w.lexeme;
}

concrete production waterDollar
top::WaterItem ::= '$$'
layout {}
{
  top.waterString = "$";
}

concrete production waterBackSlash
top::WaterItem ::= LiteralBackslash
layout {}
{
  -- The reason I decided to make backslashes not "work" is due to
  -- dealing with \"  Originally, this turned into \\" in the string
  -- because the quote got escaped... this of course, was disaster."
  top.waterString = "\\\\";
}

concrete production waterNewline
top::WaterItem ::= LiteralNewline
layout {}
{
  -- We always interpret newlines as just \n, even if the source file was \r\n.
  top.waterString = "\\n";
}

concrete production waterTab
top::WaterItem ::= LiteralTab
layout {}
{
  top.waterString = "\\t";
}

concrete production waterQuote
top::WaterItem ::= LiteralQuote
layout {}
{
  top.waterString = "\\\"";
}

concrete production singleLineWaterCons
top::SingleLineWater ::= h::SingleLineWater  t::SingleLineWaterItem
layout {}
{
  top.waterString = h.waterString ++ t.waterString;
}

concrete production singleLineWaterOne
top::SingleLineWater ::= h::SingleLineWaterItem
layout {}
{
  top.waterString = h.waterString;
}

concrete production singleLineWater
top::SingleLineWaterItem ::= w::SingleLineQuoteWater
layout {}
{
  top.waterString = w.lexeme;
}

concrete production singleLineWaterDollar
top::SingleLineWaterItem ::= '$$'
layout {}
{
  top.waterString = "$";
}

concrete production singleLineWaterBackSlash
top::SingleLineWaterItem ::= LiteralBackslash
layout {}
{
  -- Same as waterBackSlash
  top.waterString = "\\\\";
}
