// @ts-check

const FileLoader = require("./FileLoader");
const Op = require("./Operators");

class Parser
{
	/**
	 * @param {string} strFileName 
	 */
	constructor(strFileName)
	{
		this._strFileName = strFileName;
		this._arrRawFormulas = [];
		this._arrParsedFormulas = [];
	}

	async processFile()
	{
		const strFileContents = await FileLoader.readFile(this._strFileName);
		this._arrFormulas = this._separateFormulas(strFileContents);

		for(let strFormula of this._arrFormulas)
		{
			this._arrRawFormulas.push(strFormula);
			this._parseFormula(strFormula);
		}
	}

	printAllFormulas()
	{
		for(let objFormula of this._arrParsedFormulas)
		{
			console.log("----------------------------------");
			console.log(JSON.stringify(objFormula, null, 4));
			console.log("----------------------------------\n");
		}
	}

	async _parseFormula(strFormula)
	{
		// First make sure to eliminate all white spaces
		strFormula = strFormula.replace(/ /g,'');
		
		this._arrParsedFormulas.push(this._addExpressionBlock(strFormula));
	}

	_addExpressionBlock(strFormula)
	{
		console.log("Parsing: " + strFormula);

		const objCurrentBlock = {};

		if(strFormula.length === 0)
		{
			throw new Error("Empty string representation of formula given.");
		}

		// Make sure the raw formula is not bounded by redundant parentheses
		if(
			strFormula.charAt(0) === "(" 
			&& strFormula.charAt(strFormula.length - 1) === ")"
		)
		{
			strFormula = strFormula.substring(1, strFormula.length - 1);
		}
		// // Check if there are parentheses
		// if(strFormula.indexOf("(") !== -1)
		
		let nOpenParenthesesCount = 0;
		let nPrincipalOpOffset = null;
		let principalOp = null;

		for(let i = 0; i < strFormula.length; i++)
		{
			let strCurrentChar = strFormula.charAt(i);

			// Only use the operators that are exterior to any parentheses
			if(strCurrentChar === "(")
			{
				nOpenParenthesesCount++;
			}

			if(strCurrentChar === ")")
			{
				nOpenParenthesesCount--;
			}

			if(nOpenParenthesesCount != 0)
			{
				continue;
			}

			if(strCurrentChar === Op.OR.symbol)
			{
				nPrincipalOpOffset = i;
				principalOp = Op.OR;
				break;
			}

			if(strCurrentChar === Op.AND.symbol && principalOp !== Op.AND)
			{
				nPrincipalOpOffset = i;
				principalOp = Op.AND;
			}

			if(strCurrentChar === Op.NOT.symbol && principalOp === null)
			{
				nPrincipalOpOffset = i;
				principalOp = Op.NOT;
			}
		}

		if(principalOp === null || nPrincipalOpOffset === null)
		{
			throw new Error("No valid operator found in expression. Check syntax.");
		}

		if(principalOp.type === "unary")
		{
			if(nPrincipalOpOffset === (strFormula.length - 1))
			{
				throw new Error("No characters left.");
			}
			
			objCurrentBlock.op = principalOp.name;

			if(strFormula.charAt(nPrincipalOpOffset + 1) === "(")
			{
				// If is not primitive then it must be a parantheses block
				objCurrentBlock.expR = this._addExpressionBlock(strFormula.substring(nPrincipalOpOffset + 1, strFormula.length));
			}
			else
			{
				objCurrentBlock.expR = strFormula.charAt(nPrincipalOpOffset + 1);
			}
		}

		if(principalOp.type === "binary")
		{
			objCurrentBlock.op = principalOp.name;

			// Checks for the left expression
			if(nPrincipalOpOffset > 1)
			{
				objCurrentBlock.expL = this._addExpressionBlock(strFormula.substring(0, nPrincipalOpOffset));
			}
			else
			{
				objCurrentBlock.expL = strFormula.charAt(0);
			}

			// Checks for the right expression
			if(strFormula.length - nPrincipalOpOffset > 2)
			{
				objCurrentBlock.expR = this._addExpressionBlock(strFormula.substring(nPrincipalOpOffset + 1, strFormula.length));
			}
			else
			{
				objCurrentBlock.expR = strFormula.charAt(nPrincipalOpOffset + 1);
			}
		}

		return objCurrentBlock;
	}


	_separateFormulas(strFileContents)
	{
		const arrFormulas = strFileContents.split("\n");
		return arrFormulas;
	}
}

module.exports = Parser;
