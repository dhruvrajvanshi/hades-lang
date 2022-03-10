package hadesc.hir

import hadesc.unit

@Suppress("unused")
interface HIRVisitor : TypeVisitor {
    fun visitBlock(block: HIRBlock) {
        block.statements.forEach {
            visitStatement(it)
        }
    }

    fun visitStatement(statement: HIRStatement) {
        return when (statement) {
            is HIRStatement.Assignment -> visitAssignmentStatement(statement)
            is HIRStatement.Expression -> visitExpressionStatement(statement)
            is HIRStatement.MatchInt -> visitMatchInt(statement)
            is HIRStatement.Return -> visitReturnStatement(statement)
            is HIRStatement.ReturnVoid -> unit
            is HIRStatement.Store -> visitStore(statement)
            is HIRStatement.Alloca -> visitValDeclaration(statement)
            is HIRStatement.While -> visitWhileStatement(statement)
            is HIRStatement.SwitchInt -> visitConditionalBranchStatement(statement)
        }
    }

    fun visitConditionalBranchStatement(statement: HIRStatement.SwitchInt) {
        visitExpression(statement.condition)
    }

    fun visitWhileStatement(statement: HIRStatement.While) {
        visitBlock(statement.conditionBlock)
        visitBlock(statement.body)
    }

    fun visitValDeclaration(statement: HIRStatement.Alloca) {
        visitType(statement.type)
    }


    fun visitStore(statement: HIRStatement.Store) {
        visitExpression(statement.ptr)
        visitExpression(statement.value)
    }

    fun visitReturnStatement(statement: HIRStatement.Return) {
        visitExpression(statement.expression)
    }

    fun visitMatchInt(statement: HIRStatement.MatchInt) {
        visitExpression(statement.value)
        for (arm in statement.arms) {
            visitBlock(arm.block)
        }
        visitBlock(statement.otherwise)
    }

    fun visitExpressionStatement(statement: HIRStatement.Expression) {
        visitExpression(statement.expression)
    }

    fun visitAssignmentStatement(statement: HIRStatement.Assignment) {
        visitExpression(statement.value)
    }

    fun visitExpression(expression: HIRExpression) {
        visitType(expression.type)
        return when (expression) {
            is HIRExpression.AddressOf -> visitAddressOf(expression)
            is HIRExpression.BinOp -> visitBinOp(expression)
            is HIRExpression.Call -> visitCall(expression)
            is HIRExpression.Closure -> visitClosure(expression)
            is HIRExpression.GetStructField -> visitGetStructField(expression)
            is HIRExpression.GetStructFieldPointer -> visitGetStructFieldPointer(expression)
            is HIRExpression.GlobalRef -> visitGlobalRef(expression)
            is HIRExpression.IntegerConvert -> visitIntegerConvert(expression)
            is HIRExpression.InvokeClosure -> visitInvokeClosure(expression)
            is HIRExpression.Load -> visitLoad(expression)
            is HIRExpression.Not -> visitNot(expression)
            is HIRExpression.NullPtr -> visitNullPtr(expression)
            is HIRExpression.ParamRef -> visitParamRef(expression)
            is HIRExpression.PointerCast -> visitPointerCast(expression)
            is HIRExpression.SizeOf -> visitSizeOf(expression)
            is HIRExpression.TraitMethodRef -> visitTraitMethodRef(expression)
            is HIRExpression.TypeApplication -> visitTypeApplication(expression)
            is HIRExpression.UnsafeCast -> visitUnsafeCast(expression)
            is HIRExpression.ValRef -> visitValRef(expression)
            is HIRExpression.When -> visitWhen(expression)
            is HIRExpression.ArrayIndex -> visitArrayIndex(expression)
            is HIRExpression.BlockExpression -> visitBlockExpression(expression)
            is HIRConstant -> visitConstant(expression)
            is HIRExpression.LocalRef -> visitLocalRef(expression)
        }
    }

    fun visitLocalRef(expression: HIRExpression.LocalRef) = unit

    fun visitBlockExpression(expression: HIRExpression.BlockExpression) {
        visitBlock(expression.block)
    }

    fun visitArrayIndex(expression: HIRExpression.ArrayIndex) {
        visitExpression(expression.array)
        visitExpression(expression.index)
    }

    fun visitBinOp(expression: HIRExpression.BinOp) {
        visitExpression(expression.lhs)
        visitExpression(expression.rhs)
    }

    fun visitCall(expression: HIRExpression.Call) {
        visitExpression(expression.callee)
        expression.args.forEach { visitExpression(it) }
    }

    fun visitClosure(expression: HIRExpression.Closure) {
        visitBlock(expression.body)
        expression.params.forEach { visitType(it.type) }
        visitType(expression.returnType)
    }

    fun visitGetStructField(expression: HIRExpression.GetStructField) {
        visitExpression(expression.lhs)
    }

    fun visitConstant(expression: HIRConstant) {

    }

    fun visitGetStructFieldPointer(expression: HIRExpression.GetStructFieldPointer) {
        visitExpression(expression.lhs)
    }

    fun visitGlobalRef(expression: HIRExpression.GlobalRef) {

    }

    fun visitIntegerConvert(expression: HIRExpression.IntegerConvert) {

    }

    fun visitInvokeClosure(expression: HIRExpression.InvokeClosure) {
        visitExpression(expression.closure)
        expression.args.forEach { visitExpression(it) }
    }

    fun visitLoad(expression: HIRExpression.Load) {
        visitExpression(expression.ptr)
    }

    fun visitNot(expression: HIRExpression.Not) {
        visitExpression(expression.expression)
    }

    fun visitNullPtr(expression: HIRExpression.NullPtr) {

    }

    fun visitParamRef(expression: HIRExpression.ParamRef) {

    }

    fun visitPointerCast(expression: HIRExpression.PointerCast) {
        visitExpression(expression.value)
        visitType(expression.toPointerOfType)
    }

    fun visitSizeOf(expression: HIRExpression.SizeOf) {
        visitType(expression.ofType)
    }

    fun visitTraitMethodRef(expression: HIRExpression.TraitMethodRef) {
        expression.traitArgs.forEach {
            visitType(it)
        }
    }

    fun visitTypeApplication(expression: HIRExpression.TypeApplication) {
        visitExpression(expression.expression)
        expression.args.forEach { visitType(it) }
    }

    fun visitUnsafeCast(expression: HIRExpression.UnsafeCast) {
        visitExpression(expression.value)
    }

    fun visitValRef(expression: HIRExpression.ValRef) {
    }

    fun visitWhen(expression: HIRExpression.When) {
        visitExpression(expression.discriminant)
        expression.cases.forEach {
            visitExpression(it.expression)
        }
    }

    fun visitAddressOf(expression: HIRExpression.AddressOf) {
    }
}