// @ts-check

const FileLoader = require("./FileLoader");

class Lexer
{
	/**
	 * @param {string} strFileName 
	 */
	constructor(strFileName)
	{
		this._strFileName = strFileName;
	}

	async processFile()
	{
		const strFileContents = await FileLoader.readFile(this._strFileName);
		this._arrFormulas = this._separateFormulas(strFileContents);

		for(let strFormula of this._arrFormulas)
		{
			this._parseFormula(strFormula);
		}
	}

	async _parseFormula(strFormula)
	{
		
	}

	_separateFormulas(strFileContents)
	{
		const arrFormulas = strFileContents.split("\n");
		return arrFormulas;
	}
}

module.exports = Lexer;
