const Op = require("./Operators");

class TableauxMethod
{
	constructor(arrParsedFormulas)
	{
		this._arrParsedFormulas = arrParsedFormulas;
		this._objVariablePool = {};
	}

	applyMethod()
	{
		// Evaluate the formulas in the order in which they were present in the input file
		this._evaluateFormula(0);
	}

	_evaluateFormula(nFormulaIndex)
	{
		const objCurrentFormula = this._arrParsedFormulas[nFormulaIndex];
		this._visitNode(objCurrentFormula);
	}

	_visitNode(objExpressionNode)
	{
		const strCurrentOp = objExpressionNode.op;
		console.log("Op:" + strCurrentOp);

		if(Op[strCurrentOp].type === "unary")
		{
			// Check if the operand is a terminal node
			if(typeof objExpressionNode.expR === "string")
			{
				this._objVariablePool[objExpressionNode.expR] = true;
			}
			else
			{
				this._visitNode(objExpressionNode.expR);
			}
		}

		if(Op[strCurrentOp].type === "binary")
		{
			switch(Op[strCurrentOp].name)
			{
				case "IMP":
					this._Rule_A_IMP_B(objExpressionNode);
					this._visitNode(objExpressionNode);
					return;
				case "EQI":
					this._Rule_A_EQI_B(objExpressionNode);
					this._visitNode(objExpressionNode);
					return;
			}

			// Check if the operands are a terminal node
			if(typeof objExpressionNode.expL === "string")
			{
				this._objVariablePool[objExpressionNode.expL] = true;
			}
			else
			{
				this._visitNode(objExpressionNode.expL);
			}

			// Check if the operand is a terminal node
			if(typeof objExpressionNode.expR === "string")
			{
				this._objVariablePool[objExpressionNode.expR] = true;
			}
			else
			{
				this._visitNode(objExpressionNode.expR);
			}
		}
	}

	_Rule_A_IMP_B(objNode)
	{

	}

	_Rule_A_EQI_B(objNode)
	{

	}

	_Rule_NON_A(objNode)
	{

	}

	_Rule_NON_A_AND_B(objNode)
	{

	}

	_Rule_NON_A_OR_B(objNode)
	{

	}

	_Rule_NON_A_IMP_B(objNode)
	{

	}

	_Rule_NON_A_EQI_B(objNode)
	{

	}

	printResults()
	{
		console.log(Object.keys(this._objVariablePool));
	}
}

module.exports = TableauxMethod;
