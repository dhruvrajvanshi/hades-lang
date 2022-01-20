package hadesc.ast

import hadesc.unit


interface SyntaxVisitor {
    fun visitType(type: TypeAnnotation): Unit = when(type) {
        is TypeAnnotation.Application -> visitTypeApplicationType(type)
        is TypeAnnotation.Error -> visitErrorType(type)
        is TypeAnnotation.Function -> visitFunctionType(type)
        is TypeAnnotation.MutPtr -> visitMutPtrType(type)
        is TypeAnnotation.Ptr -> visitPtrType(type)
        is TypeAnnotation.Qualified -> visitQualifiedType(type)
        is TypeAnnotation.Union -> visitUnionType(type)
        is TypeAnnotation.Var -> visitVarType(type)
        is TypeAnnotation.Select -> visitSelectType(type)
    }

    fun visitSelectType(type: TypeAnnotation.Select) {
        visitType(type.lhs)
    }

    fun visitVarType(type: TypeAnnotation.Var) {

    }

    fun visitUnionType(type: TypeAnnotation.Union) {
        type.args.map { visitType(it) }
    }

    fun visitQualifiedType(type: TypeAnnotation.Qualified) {
    }

    fun visitPtrType(type: TypeAnnotation.Ptr) {
        visitType(type.to)
    }

    fun visitMutPtrType(type: TypeAnnotation.MutPtr) {
        visitType(type.to)
    }

    fun visitFunctionType(type: TypeAnnotation.Function) {
        type.from.forEach { visitType(it) }
        visitType(type.to)
    }

    fun visitErrorType(type: TypeAnnotation.Error) {

    }

    fun visitTypeApplicationType(type: TypeAnnotation.Application) {
        visitType(type.callee)
        type.args.forEach { visitType(it) }
    }

    fun visitExpression(expression: Expression): Unit = when(expression) {
        is Expression.AddressOf -> visitAddressOfExpr(expression)
        is Expression.AddressOfMut -> visitAddressOfMutExpr(expression)
        is Expression.BinaryOperation -> visitBinaryOperationExpr(expression)
        is Expression.BoolLiteral -> visitBoolLiteralExpr(expression)
        is Expression.ByteString -> visitByteStringExpr(expression)
        is Expression.Call -> visitCallExpr(expression)
        is Expression.Closure -> visitClosureExpr(expression)
        is Expression.Deref -> visitDerefExpr(expression)
        is Expression.Error -> visitErrorExpr(expression)
        is Expression.If -> visitIfExpr(expression)
        is Expression.IntLiteral -> visitIntLiteralExpr(expression)
        is Expression.Not -> visitNotExpr(expression)
        is Expression.NullPtr -> visitNullPtrExpr(expression)
        is Expression.PointerCast -> visitPointerCast(expression)
        is Expression.Property -> visitPropertyExpr(expression)
        is Expression.SizeOf -> visitSizeOfExpr(expression)
        is Expression.This -> visitThisExpr(expression)
        is Expression.TypeApplication -> visitTypeApplicationExpr(expression)
        is Expression.UnsafeCast -> visitUnsafeCastExpr(expression)
        is Expression.Var -> visitVarExpr(expression)
        is Expression.When -> visitWhenExpr(expression)
        is Expression.As -> visitAsExpression(expression)
        is Expression.ArrayIndex -> visitArrayIndexExpression(expression)
        is Expression.ArrayLiteral -> visitArrayLiteralExpression(expression)
        is Expression.BlockExpression -> visitBlockExpression(expression)
        is Expression.Intrinsic -> visitIntrinsicExpression(expression)
        is Expression.UnaryMinus -> visitUnaryMinusExpression(expression)
        is Expression.ByteCharLiteral -> visitByteCharExpression(expression)
        is Expression.Match -> visitMatchExpression(expression)
    }

    fun visitMatchExpression(expression: Expression.Match) {
        visitExpression(expression)
        for (arm in expression.arms) {
            visitPattern(arm.pattern)
            visitExpression(arm.value)
        }
    }

    fun visitByteCharExpression(expression: Expression.ByteCharLiteral) = unit

    fun visitUnaryMinusExpression(expression: Expression.UnaryMinus) {
        visitExpression(expression.expression)
    }

    fun visitIntrinsicExpression(expression: Expression.Intrinsic) = unit

    fun visitBlockExpression(expression: Expression.BlockExpression) {
        visitBlock(expression.block)
    }

    fun visitArrayLiteralExpression(expression: Expression.ArrayLiteral) {
        visitType(expression.ofType)
        expression.items.forEach { visitExpression(it) }
    }

    fun visitArrayIndexExpression(expression: Expression.ArrayIndex) {
        visitExpression(expression.lhs)
        visitExpression(expression.index)
    }

    fun visitAsExpression(expression: Expression.As) {
        visitExpression(expression.lhs)
        visitType(expression.rhs)
    }

    fun visitByteStringExpr(expression: Expression.ByteString) {

    }

    fun visitCallExpr(expression: Expression.Call) {
        visitExpression(expression.callee)
        expression.args.forEach { visitArg(it) }
    }

    fun visitArg(arg: Arg) {
        visitExpression(arg.expression)
    }

    fun visitClosureExpr(expression: Expression.Closure) {
        for (param in expression.params) {
            visitParam(param)
        }
        expression.returnType?.let { visitType(it) }
        when (expression.body) {
            is ClosureBody.Block -> visitBlock(expression.body.block)
            is ClosureBody.Expression -> visitExpression(expression.body.expression)
        }
    }

    fun visitBlock(block: Block) {
        for (member in block.members) {
            when (member) {
                is Block.Member.Expression -> visitExpression(member.expression)
                is Block.Member.Statement -> visitStatement(member.statement)
            }
        }
    }

    fun visitStatement(statement: Statement): Unit = when(statement) {
        is Statement.Defer -> visitDeferStatement(statement)
        is Statement.Error -> visitErrorStatement(statement)
        is Statement.If -> visitIfStatement(statement)
        is Statement.LocalAssignment -> visitLocalAssignment(statement)
        is Statement.MemberAssignment -> visitMemberAssignment(statement)
        is Statement.PointerAssignment -> visitPointerAssignment(statement)
        is Statement.Return -> visitReturnStatement(statement)
        is Statement.Val -> visitValStatement(statement)
        is Statement.While -> visitWhileStatement(statement)
    }

    fun visitWhileStatement(statement: Statement.While) {
        visitExpression(statement.condition)
        visitBlock(statement.body)
    }

    fun visitValStatement(statement: Statement.Val) {
        statement.typeAnnotation?.let { visitType(it) }
        visitExpression(statement.rhs)
    }

    fun visitReturnStatement(statement: Statement.Return) {
        statement.value?.let { visitExpression(it) }
    }

    fun visitPointerAssignment(statement: Statement.PointerAssignment) {
        visitExpression(statement.lhs)
        visitExpression(statement.value)
    }

    fun visitMemberAssignment(statement: Statement.MemberAssignment) {
        visitExpression(statement.lhs)
        visitExpression(statement.value)
    }

    fun visitLocalAssignment(statement: Statement.LocalAssignment) {

    }

    fun visitIfStatement(statement: Statement.If) {
        visitExpression(statement.condition)
        visitBlock(statement.ifTrue)
        statement.ifFalse?.let { visitBlock(it) }
    }

    fun visitErrorStatement(statement: Statement.Error) {
    }

    fun visitDeferStatement(statement: Statement.Defer) {
        when (statement.blockMember) {
            is Block.Member.Expression -> visitExpression(statement.blockMember.expression)
            is Block.Member.Statement -> visitStatement(statement.blockMember.statement)
        }
    }

    fun visitParam(param: Param) {
        param.annotation?.let {
            visitType(it)
        }
    }

    fun visitDerefExpr(expression: Expression.Deref) {
        visitExpression(expression.expression)
    }

    fun visitErrorExpr(expression: Expression.Error) {

    }

    fun visitIfExpr(expression: Expression.If) {
        visitExpression(expression.condition)

        visitExpression(expression.trueBranch)
        visitExpression(expression.falseBranch)
    }

    fun visitIntLiteralExpr(expression: Expression.IntLiteral) {
    }

    fun visitNotExpr(expression: Expression.Not) {
        visitExpression(expression.expression)
    }

    fun visitNullPtrExpr(expression: Expression.NullPtr) {

    }

    fun visitPointerCast(expression: Expression.PointerCast) {
        visitType(expression.toType)
        visitExpression(expression.arg)
    }

    fun visitPropertyExpr(expression: Expression.Property) {
        visitExpression(expression.lhs)
    }

    fun visitSizeOfExpr(expression: Expression.SizeOf) {
        visitType(expression.type)
    }

    fun visitThisExpr(expression: Expression.This) {
    }

    fun visitTypeApplicationExpr(expression: Expression.TypeApplication) {
        visitExpression(expression.lhs)
        expression.args.forEach {
            visitType(it)
        }
    }

    fun visitUnsafeCastExpr(expression: Expression.UnsafeCast) {
        visitType(expression.toType)
        visitExpression(expression.value)
    }

    fun visitVarExpr(expression: Expression.Var) {
    }

    fun visitWhenExpr(expression: Expression.When) {
        visitExpression(expression.value)
        expression.arms.forEach {
            visitWhenArm(it)
        }
    }

    fun visitWhenArm(arm: Expression.WhenArm) {
        visitExpression(arm.value)
    }

    fun visitBoolLiteralExpr(expression: Expression.BoolLiteral) {
    }

    fun visitBinaryOperationExpr(expression: Expression.BinaryOperation) {
        visitExpression(expression.lhs)
        visitExpression(expression.rhs)
    }

    fun visitAddressOfMutExpr(expression: Expression.AddressOfMut) {
        visitExpression(expression.expression)
    }

    fun visitAddressOfExpr(expression: Expression.AddressOf) {
        visitExpression(expression.expression)
    }

    fun visitPattern(pattern: Pattern): Unit = when(pattern) {
        is Pattern.IntLiteral -> visitIntPattern(pattern)
        is Pattern.Wildcard -> visitWildcardPattern(pattern)
        is Pattern.EnumVariant -> visitEnumVariantPattern(pattern)
    }

    fun visitEnumVariantPattern(pattern: Pattern.EnumVariant) = unit

    fun visitWildcardPattern(pattern: Pattern.Wildcard) = unit

    fun visitIntPattern(pattern: Pattern.IntLiteral) = unit
}
