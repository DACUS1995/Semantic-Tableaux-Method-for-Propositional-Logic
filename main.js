"use strict";
// @ts-check

const Lexer = require("./Lexer");

class Main
{
	constructor()
	{

	}

	static async run()
	{
		const strFileName = process.argv[2] || "input.txt";
		
		const lexer = new Lexer(strFileName);
		lexer.processFile();
	}
}

process.on(
	"unhandledRejection",
	(reason, promise) => {
		console.log("-> unhandledRejection");
		console.log(`Promise: ${promise}, Reason: ${reason}`);

		process.exit(1);
	}
);

process.on(
	"uncaughtException", 
	(error) => {
		console.log("uncaughtException");
		console.error(error);
		
		process.exit(1);
	}
)


Main.run()
	.catch(err => console.log(err));

