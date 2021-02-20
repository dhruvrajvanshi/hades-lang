package hadesc.hir

import hadesc.Name
import hadesc.analysis.TypeAnalyzer
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.types.Type
import hadesc.unit
import javax.print.attribute.standard.Destination
import kotlin.contracts.ExperimentalContracts
import kotlin.contracts.contract
import kotlin.math.exp

class HIRChecker() {
    private val typeAnalyzer = TypeAnalyzer()

    fun checkModule(module: HIRModule) {
        module.definitions.forEach { checkDefinition(it) }
    }

    private fun checkDefinition(definition: HIRDefinition): Unit = when(definition) {
        is HIRDefinition.Const -> TODO()
        is HIRDefinition.ExternFunction -> {}
        is HIRDefinition.Function -> checkFunctionDef(definition)
        is HIRDefinition.Implementation -> TODO()
        is HIRDefinition.Struct -> checkStructDef(definition)
    }

    private fun checkStructDef(definition: HIRDefinition.Struct) {
        definition.fields.forEach { checkType(it.second) }
    }

    private fun checkType(second: Type) {

    }

    private fun checkFunctionDef(definition: HIRDefinition.Function) {
        valStatements.clear()
        definition.body.statements.forEach { checkStatement(it) }
    }

    private fun checkStatement(statement: HIRStatement): Unit = when(statement) {
        is HIRStatement.Assignment -> checkAssignment(statement)
        is HIRStatement.Expression -> checkExpression(statement.expression)
        is HIRStatement.If -> TODO()
        is HIRStatement.Return -> TODO()
        is HIRStatement.ReturnVoid -> unit
        is HIRStatement.Store -> TODO()
        is HIRStatement.ValDeclaration -> checkValDeclaration(statement)
        is HIRStatement.While -> TODO()
        is HIRStatement.ValWithInitializer -> TODO()
    }

    private fun checkAssignment(statement: HIRStatement.Assignment) {
        val valDef = valStatements[statement.name]
        if (valDef == null) {
            error(statement, "Assignment to an undeclared variable: ${statement.prettyPrint()}")
            return
        }
        checkTypeAssignable(statement.value, source = statement.value.type, destination = valDef.type)

        checkExpression(statement.value)
    }

    private val valStatements = mutableMapOf<Name, HIRStatement.ValDeclaration>()
    private fun checkValDeclaration(statement: HIRStatement.ValDeclaration) {
        checkType(statement.type)

        val previousStatement = valStatements[statement.name]
        if (previousStatement != null) {
            error(statement, "Duplicate local variable name: ${statement.name}\nPreviously declared at:\n\t${previousStatement.location}: ${previousStatement.prettyPrint()}")
        }

        valStatements[statement.name] = statement
    }

    private fun checkExpression(expression: HIRExpression): Unit = when(expression) {
        is HIRExpression.AddressOf -> TODO()
        is HIRExpression.BinOp -> TODO()
        is HIRExpression.Call -> checkCall(expression)
        is HIRExpression.Closure -> TODO()
        is HIRExpression.Constant -> checkConstant(expression)
        is HIRExpression.GetStructField -> TODO()
        is HIRExpression.GetStructFieldPointer -> TODO()
        is HIRExpression.GlobalRef -> TODO()
        is HIRExpression.InvokeClosure -> TODO()
        is HIRExpression.Load -> TODO()
        is HIRExpression.Not -> TODO()
        is HIRExpression.NullPtr -> TODO()
        is HIRExpression.ParamRef -> TODO()
        is HIRExpression.PointerCast -> TODO()
        is HIRExpression.SizeOf -> TODO()
        is HIRExpression.TraitMethodCall -> TODO()
        is HIRExpression.TypeApplication -> TODO()
        is HIRExpression.UnsafeCast -> TODO()
        is HIRExpression.ValRef -> TODO()
        is HIRExpression.When -> TODO()
    }

    private fun checkConstant(expression: HIRExpression.Constant) {

    }

    private fun checkCall(expression: HIRExpression.Call) {
        val calleeType = expression.callee.type
        val (expectedArgTypes, to) = if (calleeType is Type.Ptr && calleeType.to is Type.Function) {
            calleeType.to.from to calleeType.to.to
        } else if (calleeType is Type.Function) {
            calleeType.from to calleeType.to
        } else {
            TODO()
        }

        if (expression.args.size != expectedArgTypes.size) {
            error(expression, "Argument length mismatch: Expected: ${expectedArgTypes.size}, passed: ${expectedArgTypes.size}")
            return
        }
        expectedArgTypes.zip(expression.args).forEach { (expected, actual) ->
            checkTypeAssignable(actual, source = actual.type, destination = expected)
        }
    }

    private fun checkTypeAssignable(node: HIRNode, source: Type, destination: Type) {
        if (!typeAnalyzer.isTypeAssignableTo(source = source, destination = destination)) {
            error(node, "Type mismatch: Expected ${destination.prettyPrint()}, found ${source.prettyPrint()}")
        }
    }

    fun error(node: HIRNode, message: String) {
        error("${node.location}: $message\n  ${node.prettyPrint()}")
    }
}