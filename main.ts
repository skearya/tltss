import { Evaluate, Parser, Tokenizer } from "./language";

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
	let x = 1;

	let f = x -> x + 1;
	print f(x);

	x = 2;

	let a = a -> b -> a * b;
	print a(x)(x);
`;

type Input4 = `
	let a = f -> f(1);

	print a(x -> x + 1);
`;

type Tokens = Tokenizer<Input4>;
type Statements = Parser<Tokens>;
type Output = Evaluate<Statements>;
