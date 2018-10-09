const Op = require("./Operators");

class TableauxMethod
{
	constructor(arrParsedFormulas)
	{
		this._arrParsedFormulas = arrParsedFormulas;
		this._arrExpandedFormulas = JSON.parse(JSON.stringify(arrParsedFormulas));
		this._objVariablePool = {};

		this._bFoundSolution = false;
		this._objSolution = null;
	}

	applyMethod()
	{
		this._applyRules();

		// Evaluate the formulas in the order in which they were present in the input file
		this._evaluateFormula();

		if(this._bFoundSolution)
		{
			console.log("\n!!!!!!Solution found!!!!!!");
			console.log(JSON.stringify(this._objSolution));
			console.log("\n");
		}
		else
		{
			console.log("\n!!!!!!Solution not found!!!!!!\n");
		}
	}

	_evaluateFormula(nFormulaIndex = 0)
	{
		const objCurrentFormula = this._arrExpandedFormulas[nFormulaIndex];
		this._evaluateNode(objCurrentFormula);
	}

	_applyRules(nFormulaIndex = 0)
	{
		const objCurrentFormula = this._arrExpandedFormulas[nFormulaIndex];
		this._visitNode(objCurrentFormula);
	}

	_evaluateNode(objExpressionNode, objVariableCollection = {}, bCanTerminate = true)
	{
		if(this._bFoundSolution)
		{
			return;
		}

		const strCurrentOp = objExpressionNode.op;

		if(Op[strCurrentOp].type === "unary")
		{
			// Check if the operand is a terminal node
			if(typeof objExpressionNode.expR === "string")
			{
				const bVariableValue = !(strCurrentOp === Op.NOT.name);

				// Check if there is allready set a specific variable and if the truth value differs
				if(
					objVariableCollection[objExpressionNode.expR] 
					&& objVariableCollection[objExpressionNode.expR] !== bVariableValue
				)
				{
					return;
				}
				else
				{
					if(bCanTerminate === true)
					{
						objVariableCollection[objExpressionNode.expR] = bVariableValue;
						this._bFoundSolution = true;
						this._objSolution = objVariableCollection;
						return;
					}
					else
					{
						objVariableCollection[objExpressionNode.expR] = bVariableValue;
					}
				}
			}
			else
			{
				this._evaluateNode(objExpressionNode.expR, objVariableCollection);
			}
		}

		if(Op[strCurrentOp].type === "binary")
		{
			let objVarCollForLeftExp = objVariableCollection;
			let objVarCollForRightExp = objVariableCollection;

			if(strCurrentOp !== Op.AND.name)
			{
				objVarCollForLeftExp = JSON.parse(JSON.stringify(objVariableCollection));
				objVarCollForRightExp = JSON.parse(JSON.stringify(objVariableCollection));
			}

			// Check if the operands are a terminal node
			// The terminal expersion must be added to the variable pool first to propagate that specific pool into the recursion
			if(typeof objExpressionNode.expL === "string")
			{
				// Check if there is allready set a specific variable and if the truth value differs
				if(
					objVarCollForLeftExp[objExpressionNode.expL] 
					&& objVarCollForLeftExp[objExpressionNode.expL] !== true
				)
				{
					return;
				}
				else
				{
					objVarCollForLeftExp[objExpressionNode.expL] = true;

					if(strCurrentOp === Op.OR.name && bCanTerminate === true)
					{
						this._bFoundSolution = true;
						this._objSolution = objVarCollForLeftExp;
						return;
					}
				}
			}

			// Check if the operand is a terminal node
			if(typeof objExpressionNode.expR === "string")
			{
				// Check if there is allready set a specific variable and if the truth value differs
				if(
					objVarCollForRightExp[objExpressionNode.expR] 
					&& objVarCollForRightExp[objExpressionNode.expR] !== true
				)
				{
					return;
				}
				else
				{
					objVarCollForRightExp[objExpressionNode.expR] = true;

					if(strCurrentOp === Op.OR.name && bCanTerminate === true)
					{
						this._bFoundSolution = true;
						this._objSolution = objVarCollForRightExp;
						return;
					}

					if(
						typeof objExpressionNode.expL === "string" 
						&& strCurrentOp === Op.AND.name
						&& bCanTerminate === true
					)
					{
						this._bFoundSolution = true;
						this._objSolution = objVarCollForRightExp;
						return;
					}
				}
			}

			if(typeof objExpressionNode.expL !== "string")
			{
				if(strCurrentOp === Op.AND.name)
				{
					this._evaluateNode(objExpressionNode.expL, objVarCollForLeftExp, false);
				}
				else
				{
					this._evaluateNode(objExpressionNode.expL, objVarCollForLeftExp, bCanTerminate);
				}
			}

			if(typeof objExpressionNode.expR !== "string")
			{
				this._evaluateNode(objExpressionNode.expR, objVarCollForRightExp, bCanTerminate);
			}
		}
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
