const Op = require("./Operators");

class TableauxMethod
{
	constructor(arrParsedFormulas)
	{
		this._arrParsedFormulas = arrParsedFormulas;
		this._arrExpandedFormulas = JSON.parse(JSON.stringify(arrParsedFormulas));
		this._objVariablePool = {};
	}

	applyMethod()
	{
		this._applyRules();

		// Evaluate the formulas in the order in which they were present in the input file
		// this._evaluateFormula(0);
	}

	_evaluateFormula(nFormulaIndex = 0)
	{
		const objCurrentFormula = this._arrParsedFormulas[nFormulaIndex];
		this._visitNode(objCurrentFormula);
	}

	_applyRules(nFormulaIndex = 0)
	{
		const objCurrentFormula = this._arrExpandedFormulas[nFormulaIndex];
		this._visitNode(objCurrentFormula);
	}

	_visitNode(objExpressionNode)
	{
		// Safety check
		if(typeof objExpressionNode === "string")
		{
			return;
		}

		const strCurrentOp = objExpressionNode.op;
		console.log("Op:" + JSON.stringify(objExpressionNode, null, 4));

		if(Op[strCurrentOp].type === "unary")
		{
			// Check if the operand is a terminal node
			if(typeof objExpressionNode.expR === "string")
			{
				this._objVariablePool[objExpressionNode.expR] = true;
			}
			else
			{
				switch (objExpressionNode.expR.op) 
				{
					case "NOT":
						this._Rule_NON_NON_A(objExpressionNode);
						this._visitNode(objExpressionNode);
						return;
					case "AND":
						this._Rule_NON_A_AND_B(objExpressionNode);
						this._visitNode(objExpressionNode);
						return;
					case "OR":
						this._Rule_NON_A_OR_B(objExpressionNode);
						this._visitNode(objExpressionNode);
						return;
					case "IMP":
						this._Rule_NON_A_IMP_B(objExpressionNode);
						this._visitNode(objExpressionNode);
						return;
					case "EQI":
						this._Rule_NON_A_EQI_B(objExpressionNode);
						this._visitNode(objExpressionNode);
						return;
				}
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
		console.log("_Rule_A_IMP_B");
		objNode.op = Op.OR.name;

		objNode.expL = {
			op: Op.NOT.name,
			expR: objnode.expL
		};
	}

	_Rule_A_EQI_B(objNode)
	{
		console.log("_Rule_A_EQI_B");
		const oldExpL = objNode.expL;
		const oldExpR = objNode.expR;

		objNode.op = Op.OR.name;

		objNode.expL = {
			op: Op.AND.name,
			expL: oldExpL,
			expR: oldExpR
		};

		objNode.expR = {
			op: Op.AND.name,
			expL: {
				op: Op.NOT.name,
				expR: oldExpL
			},
			expR: {
				op: Op.NOT.name,
				expR: oldExpR
			}
		};
	}

	_Rule_NON_NON_A(objNode)
	{
		console.log("_Rule_NON_NON_A");
		objNode = objNode.expL.expL;
	}

	_Rule_NON_A_AND_B(objNode)
	{
		console.log("_Rule_NON_A_AND_B");
		const oldExpL = objNode.expR.expL;
		const oldExpR = objNode.expR.expR;

		objNode.op = Op.OR.name;

		objNode.expL = {
			op: Op.NOT.name,
			expR: oldExpL
		};

		objNode.expR = {
			op: Op.NOT.name,
			expR: oldExpR
		};
	}

	_Rule_NON_A_OR_B(objNode)
	{
		console.log("_Rule_NON_A_OR_B");
		const oldExpL = objNode.expR.expL;
		const oldExpR = objNode.expR.expR;

		objNode.op = Op.AND.name;

		objNode.expL = {
			op: Op.NOT.name,
			expR: oldExpL
		};

		objNode.expR = {
			op: Op.NOT.name,
			expR: oldExpR
		};
	}

	_Rule_NON_A_IMP_B(objNode)
	{
		console.log("_Rule_NON_A_IMP_B");
		const oldExpL = objNode.expR.expL;
		const oldExpR = objNode.expR.expR;

		objNode.op = Op.AND.name;

		objNode.expL = oldExpL;

		objNode.expR = {
			op: Op.NOT,
			expR: oldExpR
		};
	}

	_Rule_NON_A_EQI_B(objNode)
	{
		console.log("_Rule_NON_A_EQI_B");
		const oldExpL = objNode.expR.expL;
		const oldExpR = objNode.expR.expR;

		objNode.op = Op.OR.name;

		objNode.expL = {
			op: Op.AND.name,
			expL: oldExpL,
			expR: {
				op: Op.NOT.name,
				expR: oldExpR
			}
		};

		objNode.expR = {
			op: Op.AND.name,
			expL: {
				op: Op.NOT,
				expR: oldExpL
			},
			expR: oldExpR
		};
	}

	printResults()
	{
		console.log("---> After the rules were applied:");
		// console.log(Object.keys(this._objVariablePool));
		for(let objExpandedExpression of this._arrExpandedFormulas)
		{
			console.log("----------------------------------");
			console.log(JSON.stringify(objExpandedExpression, null, 4));
			console.log("----------------------------------\n");
		}
	}
}

module.exports = TableauxMethod;
