package hadesc.hir

import hadesc.unit

@Suppress("unused")
interface HIRBlockVisitor : TypeVisitor {
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
            is HIRStatement.Store -> visitStore(statement)
            is HIRStatement.Alloca -> visitValDeclaration(statement)
            is HIRStatement.While -> visitWhileStatement(statement)
            is HIRStatement.SwitchInt -> visitConditionalBranchStatement(statement)
            is HIRStatement.Call -> visitCallStatement(statement)
            is HIRStatement.Load -> visitLoadStatement(statement)
            is HIRStatement.GetStructField -> visitGetStructField(statement)
            is HIRStatement.GetStructFieldPointer -> visitGetStructFieldPointer(statement)
            is HIRStatement.Not -> visitNot(statement)
            is HIRStatement.Jump -> visitJump(statement)
            is HIRStatement.IntegerConvert -> visitIntegerConvert(statement)
            is HIRStatement.TypeApplication -> visitTypeApplication(statement)
            is HIRStatement.PointerCast -> visitPointerCast(statement)
            is HIRStatement.BinOp -> visitBinOp(statement)
        }
    }

    fun visitJump(statement: HIRStatement.Jump) = unit

    fun visitLoadStatement(statement: HIRStatement.Load) {
        visitExpression(statement.ptr)
    }

    fun visitCallStatement(statement: HIRStatement.Call) {
        visitType(statement.resultType)
        visitExpression(statement.callee)
        for (arg in statement.args) {
            visitExpression(arg)
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
            is HIRExpression.Call -> visitCall(expression)
            is HIRExpression.Closure -> visitClosure(expression)
            is HIRExpression.GlobalRef -> visitGlobalRef(expression)
            is HIRExpression.InvokeClosure -> visitInvokeClosure(expression)
            is HIRExpression.NullPtr -> visitNullPtr(expression)
            is HIRExpression.ParamRef -> visitParamRef(expression)
            is HIRExpression.SizeOf -> visitSizeOf(expression)
            is HIRExpression.TraitMethodRef -> visitTraitMethodRef(expression)
            is HIRExpression.ValRef -> visitValRef(expression)
            is HIRExpression.BlockExpression -> visitBlockExpression(expression)
            is HIRConstant -> visitConstant(expression)
            is HIRExpression.LocalRef -> visitLocalRef(expression)
        }
    }

    fun visitLocalRef(expression: HIRExpression.LocalRef) = unit

    fun visitBlockExpression(expression: HIRExpression.BlockExpression) {
        visitBlock(expression.block)
    }

    fun visitBinOp(statement: HIRStatement.BinOp) {
        visitExpression(statement.lhs)
        visitExpression(statement.rhs)
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

    fun visitGetStructField(expression: HIRStatement.GetStructField) {
        visitType(expression.type)
        visitExpression(expression.lhs)
    }

    fun visitConstant(expression: HIRConstant) {

    }

    fun visitGetStructFieldPointer(expression: HIRStatement.GetStructFieldPointer) {
        visitType(expression.type)
        visitExpression(expression.lhs)
    }

    fun visitGlobalRef(expression: HIRExpression.GlobalRef) {

    }

    fun visitIntegerConvert(statement: HIRStatement.IntegerConvert) {
        visitExpression(statement.value)
        visitType(statement.type)
    }

    fun visitInvokeClosure(expression: HIRExpression.InvokeClosure) {
        visitExpression(expression.closure)
        expression.args.forEach { visitExpression(it) }
    }

    fun visitNot(statement: HIRStatement.Not) {
        visitExpression(statement.expression)
    }

    fun visitNullPtr(expression: HIRExpression.NullPtr) {

    }

    fun visitParamRef(expression: HIRExpression.ParamRef) {

    }

    fun visitPointerCast(statement: HIRStatement.PointerCast) {
        visitExpression(statement.value)
        visitType(statement.toPointerOfType)
    }

    fun visitSizeOf(expression: HIRExpression.SizeOf) {
        visitType(expression.ofType)
    }

    fun visitTraitMethodRef(expression: HIRExpression.TraitMethodRef) {
        expression.traitArgs.forEach {
            visitType(it)
        }
    }

    fun visitTypeApplication(statement: HIRStatement.TypeApplication) {
        visitExpression(statement.expression)
        statement.args.forEach { visitType(it) }
    }

    fun visitValRef(expression: HIRExpression.ValRef) {
    }
}