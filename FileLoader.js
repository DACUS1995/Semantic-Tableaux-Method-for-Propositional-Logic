// @ts-check

const fs = require("fs-extra");
const path = require("path");

class FileLoader
{
	constructor()
	{
	}

	/**
	 * @param {string} strFileName 
	 */
	static async readFile(strFileName)
	{
		if(path.extname(strFileName).toLowerCase() !== ".txt")
		{
			throw new Error("Input file must have '.txt' extension.");
		}

		const strInputFilePath = path.join(__dirname, strFileName);

		if(!(await fs.stat(strInputFilePath)).isFile())
		{
			throw new Error(`Input file with the name: ${strFileName} was not found in: ${strInputFilePath}`);
		}

		const strFileContents = await fs.readFile(strInputFilePath, {encoding: "utf8"});

		return strFileContents;
	}
}

module.exports = FileLoader;
