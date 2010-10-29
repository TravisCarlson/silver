grammar silver:extension:list:env_parser;

import silver:extension:list hiding LSqr_t, RSqr_t;
import silver:definition:env;
import silver:definition:env:parser;


concrete production aTypeRepList
top::ITypeRep ::= '[' tr::ITypeRep ']' {
  top.typerep = listTypeExp(tr.typerep);
}

