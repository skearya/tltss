type Prettify<T> = {
	[K in keyof T]: T[K];
} & {};

type Numbers = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
// prettier-ignore
type LowercaseAlphabet = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z";
// prettier-ignore
type UppercaseAlphabet = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z";

type ValidIdentifier = Numbers | LowercaseAlphabet | UppercaseAlphabet | "_";

type Token =
	| { type: "semicolon" }
	| { type: "let" }
	| { type: "print" }
	| { type: "equals" }
	| { type: "arrow" }
	| { type: "left-paren" }
	| { type: "right-paren" }
	| { type: "left-brace" }
	| { type: "right-brace" }
	| { type: "plus" }
	| { type: "minus" }
	| { type: "star" }
	| { type: "slash" }
	| { type: "num"; num: number }
	| { type: "identifier"; name: string };

type Tokenizer<Input extends string, Tokens extends Token[] = []> = Input extends `${" " | "\n" | "\t" | "\r"}${infer Rest}`
	? Tokenizer<Rest, Tokens>
	: Input extends `;${infer Rest}`
	? Tokenizer<Rest, [...Tokens, { type: "semicolon" }]>
	: Input extends `let${infer Rest}`
	? Tokenizer<Rest, [...Tokens, { type: "let" }]>
	: Input extends `print${infer Rest}`
	? Tokenizer<Rest, [...Tokens, { type: "print" }]>
	: Input extends `=${infer Rest}`
	? Tokenizer<Rest, [...Tokens, { type: "equals" }]>
	: Input extends `->${infer Rest}`
	? Tokenizer<Rest, [...Tokens, { type: "arrow" }]>
	: Input extends `(${infer Rest}`
	? Tokenizer<Rest, [...Tokens, { type: "left-paren" }]>
	: Input extends `)${infer Rest}`
	? Tokenizer<Rest, [...Tokens, { type: "right-paren" }]>
	: Input extends `{${infer Rest}`
	? Tokenizer<Rest, [...Tokens, { type: "left-brace" }]>
	: Input extends `}${infer Rest}`
	? Tokenizer<Rest, [...Tokens, { type: "right-brace" }]>
	: Input extends `+${infer Rest}`
	? Tokenizer<Rest, [...Tokens, { type: "plus" }]>
	: Input extends `-${infer Rest}`
	? Tokenizer<Rest, [...Tokens, { type: "minus" }]>
	: Input extends `*${infer Rest}`
	? Tokenizer<Rest, [...Tokens, { type: "star" }]>
	: Input extends `/${infer Rest}`
	? Tokenizer<Rest, [...Tokens, { type: "slash" }]>
	: Input extends `${infer Num extends number}${infer Rest}`
	? Tokenizer<Rest, [...Tokens, { type: "num"; num: Num }]>
	: Input extends `${infer Ident extends ValidIdentifier}${infer Rest}`
	? Tokenizer<Rest, [...Tokens, { type: "identifier"; name: Ident }]>
	: Tokens;

type Expr =
	| { type: "grouping"; expr: Expr }
	| { type: "add"; left: Expr; right: Expr }
	| { type: "sub"; left: Expr; right: Expr }
	| { type: "mul"; left: Expr; right: Expr }
	| { type: "div"; left: Expr; right: Expr }
	| { type: "literal"; num: number }
	| { type: "identifier"; name: string }
	| { type: "function"; argument: string; expr: Expr }
	| { type: "call"; callee: Expr; argument: Expr };

type Stmt =
	| { type: "declaration"; name: string; value: Expr }
	| { type: "assign"; name: string; value: Expr }
	| { type: "print"; expr: Expr }
	| { type: "expression"; expr: Expr }
	| { type: "block"; stmts: Stmt[] };

type Parser<Tokens extends Token[]> = Program<Tokens>;

// (declaration)*
type Program<Tokens extends Token[], Stmts extends Stmt[] = []> = Tokens["length"] extends 0
	? Stmts
	: Declaration<Tokens> extends [infer S extends Stmt, infer Tokens extends Token[]]
	? Program<Tokens, [...Stmts, S]>
	: never;

// var-declaration | assignment-declaration | statement
type Declaration<Tokens extends Token[]> = Tokens extends [{ type: "let" }, ...infer _]
	? VarDeclaration<Tokens>
	: Tokens extends [{ type: "identifier" }, { type: "equals" }, ...infer _]
	? AssignmentDeclaration<Tokens>
	: Statement<Tokens>;

// print-statement | block-statement | expression
type Statement<Tokens extends Token[]> = Tokens extends [{ type: "print" }, ...infer _]
	? PrintStatement<Tokens>
	: Tokens extends [{ type: "left-brace" }, ...infer _]
	? BlockStatement<Tokens>
	: ExpressionStatement<Tokens>;

// "let" identifier "=" expr ";"
type VarDeclaration<Tokens extends Token[]> = Tokens extends [{ type: "let" }, ...infer Tokens extends Token[]]
	? Tokens extends [{ type: "identifier"; name: infer Ident }, ...infer Tokens extends Token[]]
		? Tokens extends [{ type: "equals" }, ...infer Tokens extends Token[]]
			? Term<Tokens> extends [infer Value extends Expr, infer Tokens extends Token[]]
				? Tokens extends [{ type: "semicolon" }, ...infer Tokens extends Token[]]
					? [{ type: "declaration"; name: Ident; value: Value }, Tokens]
					: never
				: never
			: never
		: never
	: never;

// identifier "=" expr ";"
type AssignmentDeclaration<Tokens extends Token[]> = Tokens extends [{ type: "identifier"; name: infer Ident }, ...infer Tokens extends Token[]]
	? Tokens extends [{ type: "equals" }, ...infer Tokens extends Token[]]
		? Term<Tokens> extends [infer Value extends Expr, infer Tokens extends Token[]]
			? Tokens extends [{ type: "semicolon" }, ...infer Tokens extends Token[]]
				? [{ type: "assign"; name: Ident; value: Value }, Tokens]
				: never
			: never
		: never
	: never;

// "print" expr ";"
type PrintStatement<Tokens extends Token[]> = Tokens extends [{ type: "print" }, ...infer Tokens extends Token[]]
	? Term<Tokens> extends [infer E extends Expr, infer Tokens extends Token[]]
		? Tokens extends [{ type: "semicolon" }, ...infer Tokens extends Token[]]
			? [{ type: "print"; expr: E }, Tokens]
			: never
		: never
	: never;

// "{" (declaration)* "}"
type BlockStatement<Tokens extends Token[]> = Tokens extends [{ type: "left-brace" }, ...infer Tokens extends Token[]] ? BlockStatementInner<Tokens> : never;

type BlockStatementInner<Tokens extends Token[], Stmts extends Stmt[] = []> = Tokens extends [{ type: "right-brace" }, ...infer Tokens extends Token[]]
	? [{ type: "block"; stmts: Stmts }, Tokens]
	: Declaration<Tokens> extends [infer S extends Stmt, infer Tokens extends Token[]]
	? BlockStatementInner<Tokens, [...Stmts, S]>
	: never;

// expr ";"
type ExpressionStatement<Tokens extends Token[]> = Expression<Tokens> extends [infer E extends Expr, infer Tokens extends Token[]]
	? Tokens extends [{ type: "semicolon" }, ...infer Tokens extends Token[]]
		? [{ type: "expression"; expr: E }, Tokens]
		: never
	: never;

// term
type Expression<Tokens extends Token[]> = Term<Tokens>;

// factor (("+" | "-") factor)*
type Term<Tokens extends Token[]> = Factor<Tokens> extends [infer Left, infer Tokens extends Token[]] ? TermInner<Left, Tokens> : never;

type TermInner<Left, Tokens extends Token[]> = Tokens extends [{ type: infer Op extends "plus" | "minus" }, ...infer Tokens extends Token[]]
	? Factor<Tokens> extends [infer Right, infer Tokens extends Token[]]
		? TermInner<{ type: { plus: "add"; minus: "sub" }[Op]; left: Left; right: Right }, Tokens>
		: never
	: [Left, Tokens];

// call (("*" | "/") call)*
type Factor<Tokens extends Token[]> = Call<Tokens> extends [infer Left, infer Tokens extends Token[]] ? FactorInner<Left, Tokens> : never;

type FactorInner<Left, Tokens extends Token[]> = Tokens extends [{ type: infer Op extends "star" | "slash" }, ...infer Tokens extends Token[]]
	? Call<Tokens> extends [infer Right, infer Tokens extends Token[]]
		? FactorInner<{ type: { star: "mul"; slash: "div" }[Op]; left: Left; right: Right }, Tokens>
		: never
	: [Left, Tokens];

// primary ("(" expr ")")*
type Call<Tokens extends Token[]> = Primary<Tokens> extends [infer Left, infer Tokens extends Token[]] ? CallInner<Left, Tokens> : never;

type CallInner<Left, Tokens extends Token[]> = Tokens extends [{ type: "left-paren" }, ...infer Tokens extends Token[]]
	? Expression<Tokens> extends [infer Expr, infer Tokens extends Token[]]
		? Tokens extends [{ type: "right-paren" }, ...infer Tokens extends Token[]]
			? CallInner<{ type: "call"; callee: Left; argument: Expr }, Tokens>
			: never
		: never
	: [Left, Tokens];

// literal | identifier ("->" expr)? | "(" expr ")"
type Primary<Tokens extends Token[]> = Tokens extends [{ type: "num"; num: infer Num extends number }, ...infer Tokens extends Token[]]
	? [{ type: "literal"; num: Num }, Tokens]
	: Tokens extends [{ type: "identifier"; name: infer Name extends string }, ...infer Tokens extends Token[]]
	? Tokens extends [{ type: "arrow" }, ...infer Tokens extends Token[]]
		? Expression<Tokens> extends [infer Expr, infer Tokens extends Token[]]
			? [{ type: "function"; argument: Name; expr: Expr }, Tokens]
			: never
		: [{ type: "identifier"; name: Name }, Tokens]
	: Tokens extends [{ type: "left-paren" }, ...infer Tokens extends Token[]]
	? Expression<Tokens> extends [infer Expr, infer Tokens extends Token[]]
		? Tokens extends [{ type: "right-paren" }, ...infer Tokens extends Token[]]
			? [{ type: "grouping"; expr: Expr }, Tokens]
			: never
		: never
	: never;

type Arr<N extends number, Carry extends never[] = []> = Carry["length"] extends N ? Carry : Arr<N, [...Carry, never]>;
type ForceNum<T> = T extends infer Num extends number ? Num : never;

type Add<A extends number, B extends number> = [...Arr<A>, ...Arr<B>]["length"];
type Sub<A extends number, B extends number> = Arr<A> extends [...Arr<B>, ...infer R] ? R["length"] : 0;
type Mul<A extends number, B extends number, Carry extends number = 0> = B extends 0 ? Carry : Mul<A, Sub<B, 1>, ForceNum<Add<A, Carry>>>;
type Div<A extends number, B extends number, Carry extends number = 0> = A extends 0 ? Carry : Div<Sub<A, B>, B, ForceNum<Add<Carry, 1>>>;

type Value = number | { argument: string; expr: Expr };

type ValueDisplay<Val extends Value> = Val extends infer Num extends number
	? `${Num}`
	: Val extends { argument: infer Arg extends string }
	? `fn ${Arg} -> <code>`
	: `unknown`;

type Environment = {
	enclosing: Environment | undefined;
	locals: Record<string, Value>;
};

type EnvironmentEnclosing<Env extends Environment> = {
	enclosing: Env;
	locals: {};
};

type EnvironmentDeclare<Env extends Environment, Name extends string, Val extends Value> = {
	enclosing: Env["enclosing"];
	locals: Env["locals"] & Record<Name, Val>;
};

type EnvironmentAssign<Env extends Environment, Name extends string, Val extends Value> = Env["locals"][Name] extends infer _ extends Value
	? { enclosing: Env["enclosing"]; locals: Omit<Env["locals"], Name> & Record<Name, Val> }
	: Env["enclosing"] extends infer Enclosing extends Environment
	? { enclosing: EnvironmentAssign<Enclosing, Name, Val>; locals: Env["locals"] }
	: never;

type EnvironmentLookup<Env extends Environment, Name extends string> = Env["locals"][Name] extends infer Val extends Value
	? Val
	: Env["enclosing"] extends infer Env extends Environment
	? EnvironmentLookup<Env, Name>
	: never;

type EvalExpr<Node extends Expr, Env extends Environment> = Node extends { type: "grouping" }
	? EvalExpr<Node["expr"], Env>
	: Node extends { type: "add" }
	? Add<ForceNum<EvalExpr<Node["left"], Env>>, ForceNum<EvalExpr<Node["right"], Env>>>
	: Node extends { type: "sub" }
	? Sub<ForceNum<EvalExpr<Node["left"], Env>>, ForceNum<EvalExpr<Node["right"], Env>>>
	: Node extends { type: "mul" }
	? Mul<ForceNum<EvalExpr<Node["left"], Env>>, ForceNum<EvalExpr<Node["right"], Env>>>
	: Node extends { type: "div" }
	? Div<ForceNum<EvalExpr<Node["left"], Env>>, ForceNum<EvalExpr<Node["right"], Env>>>
	: Node extends { type: "literal" }
	? Node["num"]
	: Node extends { type: "identifier" }
	? EnvironmentLookup<Env, Node["name"]>
	: Node extends { type: "function" }
	? { argument: Node["argument"]; expr: Node["expr"] }
	: Node extends { type: "call" }
	? EvalExpr<Node["callee"], Env> extends { argument: infer Arg extends string; expr: infer Fn extends Expr }
		? EvalExpr<Fn, EnvironmentDeclare<EnvironmentEnclosing<Env>, Arg, EvalExpr<Node["argument"], Env>>>
		: never
	: never;

type EvalStmt<Node extends Stmt, Env extends Environment, Out extends string[]> = Node extends { type: "declaration" }
	? [EnvironmentDeclare<Env, Node["name"], EvalExpr<Node["value"], Env>>, Out]
	: Node extends { type: "assign" }
	? [EnvironmentAssign<Env, Node["name"], EvalExpr<Node["value"], Env>>, Out]
	: Node extends { type: "print" }
	? [Env, [...Out, ValueDisplay<EvalExpr<Node["expr"], Env>>]]
	: Node extends { type: "expression" }
	? [Env, Out]
	: Node extends { type: "block" }
	? Eval<Node["stmts"], EnvironmentEnclosing<Env>, Out>
	: never;

type Eval<Stmts extends Stmt[], Env extends Environment = { enclosing: undefined; locals: {} }, Out extends string[] = []> = Stmts extends [
	infer S extends Stmt,
	...infer Stmts extends Stmt[]
]
	? EvalStmt<S, Env, Out> extends [infer Env extends Environment, infer Out extends string[]]
		? Eval<Stmts, Env, Out>
		: never
	: [Env["enclosing"], Out];

type Evaluate<Stmts extends Stmt[]> = Eval<Stmts> extends [infer Env extends Environment | undefined, infer Out extends string[]] ? Out : never;

type Input = `
    let x = 1;
	let y = x * 2;
	x = y + 3;

    print x;
	print x * x * x;
`;

type Input2 = `
	let x = 1;

	{
		let y = x * 2;
		x = y;
	}

	print x;
`;

type Input3 = `
	let f = x -> x + 1;
	print f(1);

	let y = x -> (z -> z + 2);
	print y(0)(2);
`;

type Input4 = `
	let x = 1 + 1;
	print x;
`;

type Tokens = Tokenizer<Input3>;
type Statements = Parser<Tokens>;
type Output = Evaluate<Statements>;

type TestEnv = {
	enclosing: {
		enclosing: undefined;
		locals: { x: 1 };
	};
	locals: { y: 2 };
};
