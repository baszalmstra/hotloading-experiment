# Hotloading Experiment 

An experiment to:
   
* Parse Mun
* Generate machine code
* Hotload Mun

## Used resourced

- Difference between and AST and a CST: https://eli.thegreenplace.net/2009/02/16/abstract-vs-concrete-syntax-trees
- Rust-analyzer is a great source of inspiration: https://github.com/rust-analyzer/rust-analyzer

## Choices

This is a list of choices I made during the development of the language. None of these are in any way final and are open for discussion. I just needed to make a decision to be able to progress. I documented my choices and the reason I did so here:

### `function`

I picked the `function` keyword to specify a function because this is what is it called in Lua and Typescript. However, I myself dislike the keyword because it's verbose and easy to misspell. In would prefer either:

- `fun`
- `fn`

Also contrary to Lua I use braces instead of `function`, `end` because I feel that's way less verbose and much more familiar for many users not comming from Lua. 

```mun
function foo() {}
```

### `export`

Different from Lua, I want to explicitly state what is exported from a *module* (e.g. a source file). I added the `export` keyword to signify which functions (and later other items) can be accessed from outside the module. 

This includes what can be called from the runtime by name.

```mun
function internalFunction() {}
export function exportedFunction() {}
```

## EBNF

Grammar of the language so far. This is likely outdated and definitely not proper EBNF but it paints a picture.

```
SourceFile ::= Declaration*
Declaration ::= [Visibility] DeclarationElement
DeclarationElement ::= FunctionDefinition
Visibility ::= "export"
FunctionDefinition ::= "function" Name ParamList Block
ParamList ::= "(" ParamListElements )"
ParamListElements ::=
    Param
  | Param "," ParamListElement 
Param ::= Name TypeAscription
TypeAscription ::= ":" Type
Type ::= NameRef
Name ::= Identifier
Block ::= Statement*
Statement ::= 
    LetStatement
  | ExprStatement
LetStatement ::= "let" Name ["=" Expr] [";"]
ExprStatement ::= Expr [";"]
Expr ::= ExprLhs [Op Expr]
ExprLhs ::= 
    PrefixExpr
  | AtomExpr
PrefixExpr ::= "-"|"not" Expr
AtomExpr ::= 
    Literal
Literal = 
    STRING
  | INT_NUMBER
  | FLOAT_NUMBER
  | "true"
  | "false"
```