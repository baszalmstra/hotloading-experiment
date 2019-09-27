# Mun

This is the main source code repository for [Mun](https://mun-lang.org). It contains the compiler and runtime.

## How to compile (MacOS / Linux)

1. Install [llvmenv](https://github.com/termoshtt/llvmenv)
2. Run `llvmenv init`
3. Open the `$XDG_CONFIG_HOME/llvmenv/entry.toml` file, and add the following settings (On MacOS
   `$XDG_CONFIG_HOME` is `~/Library/Preferences`):

```toml
[release_70]
url = "https://github.com/llvm-mirror/llvm#release_70"
build_type = "Release"
```

4. Build LLVM 7.0 `llvmenv build-entry release_70`

## How to compile (Windows)

1. Install the prerequisites for [llvmenv](https://github.com/termoshtt/llvmenv)
2. Clone *llvmenv* `git clone https://github.com/termoshtt/llvmenv.git`
3. Change directories `cd llvmenv`
4. Run `cargo build`
5. Execute `cargo run init`
6. Open the `%appdata%\llvmenv\entry.toml` file, and add the following settings:

```toml
[release_70]
url = "https://github.com/llvm-mirror/llvm#release_70"
build_type="Release"
builder="VisualStudioWin64"
tools=[{ name = "lld", url = "https://github.com/llvm-mirror/lld.git#release_70" }]
```

7. Build LLVM 7.0 `cargo run build-entry release_70`
8. Set the `LLVM_SYS_70_PREFIX` environment variable to `%appdata%\llvmenv\release_70`

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
NameRef ::= Identifier
Block ::= Statement*
Statement ::= 
    LetStatement
  | ExprStatement
LetStatement ::= "let" Name ["=" Expr] [";"]
ExprStatement ::= Expr [";"]
Expr ::= ExprLhs [Op Expr]
ExprLhs ::= 
    PrefixExpr
  | PostfixExpr
PrefixExpr ::= "-"|"not" Expr
PostfixExpr ::= 
    AtomExpr
  | CallExpr
CallExpr ::= ExprLhs ArgList
ArgList ::= "(" ArgListElements ")"
ArgListElements ::= 
  | Expr
  | Expr "," ArgListElements
AtomExpr ::= 
    Literal
  | NameRef
  | ParenExpr
ParenExpr ::= "(" Expr ")" 
Literal = 
    STRING
  | INT_NUMBER
  | FLOAT_NUMBER
  | "true"
  | "false"
```

## Whats missing?

A lot, but notably:

### `if`/`while`/`for`/`loop`/...

I've not implemented any form of control flow yet.

### `import`/`use`

Using stuff from other modules. I mean we do have `export`, but you can't `import` anything from other modules. 

### `class`

Any mention of classes or objects or anything with field or method access.

### Type checking

There is no type checking whatsoever yet

### Code generation

There is not code generation as of yet.  

## Large todo's

### Generate code

Now that we got actual parsing we need to be able to generate some form of code.

### Execute mun

Basically be able to run Mun code

#### Type inferencing

We'll have to add type inferencing as soon as we can. 

### Embed API

Write and implement the API

## Installing LLVM on Windows

Installing LLVM on windows is not a very pleasant experience. Especially if you want to try out multiple versions. Here is a short explanation on how I do this:

1. Clone llvmenv from [this repo](https://github.com/baszalmstra/llvmenv)
2. Run `cargo run init` 
3. Add the following entry to `%appdata%/llvmenv/entry.toml`
   ```
   [7_0_0]
   url    = "https://github.com/llvm-mirror/llvm#release_70"
   build_type = "Release"
   builder = "VisualStudioWin64"
   ```
4. Run `cargo run build-entry 7_0_0`. This will build the entire LLVM 7.0 toolchain.
5. Add an environment variable:
   ```
   LLVM_SYS_70_PREFIX=%appdata%\llvmenv\7_0_0
   ```

## Installing on Linux

For `ubuntu 18.04` use the following:

```bash
sudo apt install llvm-7 llvm-7-* lld-7
```
