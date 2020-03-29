package hadesc.codegen

import dev.supergrecko.kllvm.core.types.IntType
import dev.supergrecko.kllvm.core.types.PointerType
import dev.supergrecko.kllvm.core.values.FunctionValue
import dev.supergrecko.kllvm.core.values.InstructionValue
import dev.supergrecko.kllvm.core.values.PointerValue
import hadesc.ast.*
import hadesc.context.Context
import hadesc.logging.logger
import hadesc.qualifiedpath.QualifiedName
import llvm.FunctionType
import llvm.Value
import llvm.VoidType
import org.bytedeco.javacpp.BytePointer
import org.bytedeco.llvm.LLVM.LLVMTargetMachineRef
import org.bytedeco.llvm.global.LLVM

@OptIn(ExperimentalStdlibApi::class)
class LLVMGen(val ctx: Context) : AutoCloseable {
    private val log = logger()
    private val llvmCtx = llvm.Context.create()
    private val llvmModule = llvm.Module.create(ctx.options.main.toString(), llvmCtx)
    private val builder = llvm.Builder.create(llvmCtx)

    fun generate() {
        log.info("Generating LLVM IR")
        lowerSourceFile(ctx.sourceFile(QualifiedName(), ctx.mainPath()))
        llvmModule.dump()

        val buffer = ByteArray(100)
        val len = LLVM.LLVMVerifyModule(llvmModule.getUnderlyingReference(), 100, buffer)
        println(buffer.decodeToString().slice(0 until len))
        writeModuleToFile()
        linkWithRuntime()
    }

    private fun lowerSourceFile(sourceFile: SourceFile) {
        for (declaration in sourceFile.declarations) {
            lowerDeclaration(declaration)
        }
    }

    private fun lowerDeclaration(declaration: Declaration) = when (declaration.kind) {
        is Declaration.Kind.ImportAs -> {
            // Imports don't generate anything
        }
        is Declaration.Kind.FunctionDef -> lowerFunctionDefDeclaration(declaration, declaration.kind)
        Declaration.Kind.Error -> {
        }
        is Declaration.Kind.ExternFunctionDef -> lowerExternDefDeclaration(declaration, declaration.kind)
    }

    private fun lowerExternDefDeclaration(declaration: Declaration, kind: Declaration.Kind.ExternFunctionDef) {
        llvmModule.addFunction(
            kind.externName.name.text, FunctionType.new(
                returns = VoidType.new(llvmCtx),
                types = listOf(PointerType.new(IntType.new(8, llvmCtx))),
                variadic = false
            )
        )
    }

    private fun lowerFunctionDefDeclaration(declaration: Declaration, def: Declaration.Kind.FunctionDef) {
        // TODO: Replace this with actual types of the function
        val type = FunctionType.new(
            VoidType.new(llvmCtx),
            listOf(),
            false
        )
        val func = llvmModule.addFunction(lowerBinder(def.name), type)

        LLVM.LLVMSetLinkage(func.getUnderlyingReference(), LLVM.LLVMExternalLinkage)
        val basicBlock = func.appendBasicBlock("entry")
        builder.positionAtEnd(basicBlock)

        for (member in def.body.members) {
            lowerBlockMember(member)
        }
        builder.buildRetVoid()

        LLVM.LLVMVerifyFunction(func.getUnderlyingReference(), LLVM.LLVMAbortProcessAction)
    }

    private fun lowerBlockMember(member: Block.Member): Unit = when (member) {
        is Block.Member.Expression -> {
            lowerExpression(member.expression)
            Unit
        }
        is Block.Member.Statement -> TODO()
    }

    private fun lowerExpression(expr: Expression): llvm.Value = when (expr.kind) {
        Expression.Kind.Error -> builder.buildRetVoid()
        is Expression.Kind.Var -> lowerVarExpression(expr, expr.kind)
        is Expression.Kind.Call -> {
            lowerCallExpression(expr, expr.kind)
        }
        is Expression.Kind.Property -> TODO("Property")
        is Expression.Kind.ByteString -> lowerByteStringExpression(expr, expr.kind)
    }

    private fun lowerByteStringExpression(expr: Expression, kind: Expression.Kind.ByteString): Value {
        val text = kind.bytes.decodeToString()
        val constStringRef = LLVM.LLVMConstString(text, text.length, 0)
        val globalRef = LLVM.LLVMAddGlobal(
            llvmModule.getUnderlyingReference(),
            LLVM.LLVMTypeOf(constStringRef),
            stringLiteralName()
        )
        LLVM.LLVMSetInitializer(globalRef, constStringRef)
        val ptrRef = LLVM.LLVMConstPointerCast(globalRef, bytePtrTy.getUnderlyingReference())
        return PointerValue(ptrRef)
    }

    private var nextLiteralIndex = 0
    private fun stringLiteralName(): String {
        nextLiteralIndex++
        return "\$string_literal_$nextLiteralIndex"
    }

    private val byteTy = IntType.new(8, llvmCtx)
    private val bytePtrTy = PointerType.new(byteTy)

    private fun lowerVarExpression(expr: Expression, kind: Expression.Kind.Var): Value {
        return FunctionValue(
            LLVM.LLVMGetNamedFunction(
                llvmModule.getUnderlyingReference(),
                BytePointer(kind.name.name.text)
            )
        )
    }


    private fun lowerCallExpression(
        expr: Expression,
        kind: Expression.Kind.Call
    ): InstructionValue {
        return builder.buildCall(
            lowerExpression(kind.callee),
            kind.args.map { lowerExpression(it.expression) }
        )
    }


    private fun lowerBinder(binder: Binder): String {
        // TODO: Use a fully resolved name here
        return if (binder.identifier.name.text == "main") {
            "hades_main"
        } else {
            binder.identifier.name.text
        }
    }

    private fun linkWithRuntime() {
        log.info("Linking using gcc")
        val builder = ProcessBuilder(
            "gcc",
            "-no-pie",
            "-o", ctx.options.output.toString(),
            "runtime.c",
            objectFilePath
        ).inheritIO()
        log.debug(builder.command().joinToString(","))
        val process = builder.start()
        val exitCode = process.waitFor()
        assert(exitCode == 0) {
            "gcc exited with code $exitCode"
        }
    }

    private val objectFilePath get() = ctx.options.output.toString() + ".object"

    private fun writeModuleToFile() {
        log.info("Writing object file")
        LLVM.LLVMInitializeAllTargetInfos()
        LLVM.LLVMInitializeAllTargets()
        LLVM.LLVMInitializeAllTargetMCs()
        LLVM.LLVMInitializeAllAsmParsers()
        LLVM.LLVMInitializeAllAsmPrinters()
        val targetTriple = LLVM.LLVMGetDefaultTargetTriple()
        val cpu = BytePointer("generic")
        val features = BytePointer("")
        val target = LLVM.LLVMGetFirstTarget()
        val targetMachine: LLVMTargetMachineRef = LLVM.LLVMCreateTargetMachine(
            target,
            targetTriple,
            cpu,
            features,
            LLVM.LLVMCodeGenLevelLess,
            LLVM.LLVMRelocDefault,
            LLVM.LLVMCodeModelDefault
        )

        val pass = LLVM.LLVMCreatePassManager()
        LLVM.LLVMTargetMachineEmitToFile(
            targetMachine,
            llvmModule.getUnderlyingReference(),
            BytePointer(objectFilePath),
            LLVM.LLVMObjectFile,
            BytePointer("Message")
        )
        LLVM.LLVMRunPassManager(pass, llvmModule.getUnderlyingReference())

        LLVM.LLVMDisposePassManager(pass)
        LLVM.LLVMDisposeTargetMachine(targetMachine)
    }

    override fun close() {
        llvmCtx.dispose()
    }
}