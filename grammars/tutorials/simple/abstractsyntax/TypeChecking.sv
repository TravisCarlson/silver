grammar tutorials:simple:abstractsyntax ;

{- This file performs type checking on Simple programs by defining the
   attribute type::Type for all expressions (Expr).  If an error is
   detected then the an error message is added (via the <- assignment
   operator) to the collection attribute errors, which is declared and
   initialized (via the := assignment operator) in the file Expr.sv.
-}

attribute type occurs on Expr ;

-- Constants
------------
{- These constructs do not create any errors and define their type
   attribute directly.  
-}
aspect production intLit   e::Expr ::= n::IntegerLiteral_t
{ e.type = integerType() ; }

aspect production floatLit e::Expr ::= x::FloatLiteral_t
{ e.type = floatType() ; }

aspect production boolLit  e::Expr ::= b::BooleanLiteral_t
{ e.type = booleanType() ; }


-- Variable Reference
---------------------
{- In the varRef production in Expr.sv, we defined declTypeExpr to be
   the optional decorated tree (a reference to) of the TypeExpr in the
   variable declaraion.  Since declTypeExpr is a production attribute
   it can be referenced in the aspect production below.  We check to
   see that it is defined, and if so, extract the "type" attribute
   from the referenced TypeExpr to assign to the variables "type"
   attribute. If it is not defined (it a "nothing()") we generate the
   error type.  

   There is no error to generate.  If it is not defined that error is
   generated in the production that defines declTypeExpr.  
-}

aspect production varRef  e::Expr ::= id::Id_t
{ e.type = case declTypeExpr of
             just(dte) -> dte.type 
           | nothing() -> errorType() 
           end ;
}


-- Arithmetic Operations
------------------------
{- We could further simply arithmetic productions if they forwarded to
   a generic numeric binary operation.  But we have not done that on
   this simple language.
-}
aspect production add e::Expr ::= l::Expr r::Expr 
{ e.type = resolveNumericTypes (l.type, r.type) ;
  e.errors <- (if isNumeric(l.type) then [ ]
               else [ "Expression \"" ++ l.pp ++ 
                      "\" must be of type Integer or Float.\n" ]) ++
              (if isNumeric(r.type) then [ ]
               else [ "Expression \"" ++ r.pp ++ 
                      "\" must be of type Integer or Float.\n" ]);
}
aspect production sub e::Expr ::= l::Expr r::Expr 
{ e.type = resolveNumericTypes (l.type, r.type) ;
  e.errors <- (if isNumeric(l.type) then [ ]
               else [ "Expression \"" ++ l.pp ++ 
                      "\" must be of type Integer or Float.\n" ]) ++
              (if isNumeric(r.type) then [ ]
               else [ "Expression \"" ++ r.pp ++ 
                      "\" must be of type Integer or Float.\n" ]);
}
aspect production mul e::Expr ::= l::Expr r::Expr 
{ e.type = resolveNumericTypes (l.type, r.type) ;
  e.errors <- (if isNumeric(l.type) then [ ]
               else [ "Expression \"" ++ l.pp ++ 
                      "\" must be of type Integer or Float.\n" ]) ++
              (if isNumeric(r.type) then [ ]
               else [ "Expression \"" ++ r.pp ++ 
                      "\" must be of type Integer or Float.\n" ]);
}
aspect production div e::Expr ::= l::Expr r::Expr 
{ e.type = resolveNumericTypes (l.type, r.type) ;
  e.errors <- (if isNumeric(l.type) then [ ]
               else [ "Expression \"" ++ l.pp ++ 
                      "\" must be of type Integer or Float.\n" ]) ++
              (if isNumeric(r.type) then [ ]
               else [ "Expression \"" ++ r.pp ++ 
                      "\" must be of type Integer or Float.\n" ]);
}

function resolveNumericTypes  Type ::= lType::Type rType::Type
{ return
    case lType of
      integerType() -> case rType of
                         integerType() -> integerType()
                       | floatType()   -> floatType()
                       | _             -> errorType()
                       end
    | floatType() -> case rType of
                       integerType() -> floatType()
                     | floatType()   -> floatType()
                     | _             -> errorType()
                     end 
    | _ -> errorType()
    end ;
}

function isNumeric  Boolean ::= t::Type
{ return case t of
           integerType() -> true
         | floatType() -> true
         | _ -> false
         end ;
}


-- Relational and Logical Operations
------------------------------------ 
{- Because of forwarding in Expr.sv, we do not need to write aspect
   productions for "or", "neq", "lte", "gt", or "gte", only these below.
-}
aspect production eq e::Expr ::= l::Expr r::Expr 
{ e.type = booleanType() ;
  e.errors <- (if isNumeric(l.type) then [ ]
               else [ "Expression \"" ++ l.pp ++ 
                      "\" must be of type Integer or Float.\n" ]) ++
              (if isNumeric(r.type) then [ ]
               else [ "Expression \"" ++ r.pp ++ 
                      "\" must be of type Integer or Float.\n" ]);
}
aspect production lt e::Expr ::= l::Expr r::Expr 
{ e.type = booleanType() ;
  e.errors <- (if isNumeric(l.type) then [ ]
               else [ "Expression \"" ++ l.pp ++ 
                      "\" must be of type Integer or Float.\n" ]) ++
              (if isNumeric(r.type) then [ ]
               else [ "Expression \"" ++ r.pp ++ 
                      "\" must be of type Integer or Float.\n" ]);
}

aspect production and e::Expr ::= l::Expr r::Expr 
{ e.type = booleanType() ;
  e.errors <- (if isBoolean(l.type) then [ ]
               else [ "Expression \"" ++ l.pp ++ 
                      "\" must be of type Boolean.\n" ]) ++
              (if isBoolean(r.type) then [ ]
               else [ "Expression \"" ++ r.pp ++ 
                      "\" must be of type Boolean.\n" ]);
}

aspect production not e::Expr ::= ne::Expr 
{ e.type = booleanType() ;
  e.errors <- if isBoolean(ne.type) then [ ]
              else [ "Expression \"" ++ ne.pp ++ 
                     "\" must be of type Boolean.\n" ] ;
}

function isBoolean  Boolean ::= t::Type
{ return case t of booleanType() -> true  | _ -> false  end ;  }



