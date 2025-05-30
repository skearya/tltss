# tltss

a type level type script script!

## about
tltss is a basic proof of concept programming language implemented entirely in typescript types.

```ts
type Input = `
	let x = 1;

	let f = x -> x + 1;
	print f(x);

	x = 2;

	let a = a -> b -> a * b;
	print a(x)(x);
`;

type Tokens = Tokenizer<Input>;
type Statements = Parser<Tokens>;
type Output = Evaluate<Statements>; // ["2", "4"]
```

[playground](https://skearya.github.io/tltss/)

## features
- first class functions and unsigned integers as datatypes
- scopes
- closures
- mutable variables
- \+ \- \* \/

## grammar
```
program = (declaration)*

declaration = var | assignment | statement

var = "let" identifier "=" expr ";"
assignment = identifier "=" expr ";"
statement = print | block | expr-statement

print = "print" expr ";"
block = "{" (declaration)* "}"
expr-statement = expr ";"

expr = term
term = factor (("+" | "-") factor)*
factor = call (("*" | "/") call)*
call = primary ("(" expr? ")")*
primary = literal | identifier ("->" expr)? | "(" expr ")"
```

## references
- [Crafting Interpreters](https://craftinginterpreters.com/)
