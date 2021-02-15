package llvm

import com.sun.jna.Pointer
import org.bytedeco.llvm.LLVM.LLVMDIBuilderRef
import org.bytedeco.llvm.LLVM.LLVMMetadataRef
import org.bytedeco.llvm.global.LLVM

fun LLVMDIBuilderRef.createCompileUnit(
    fileRef: LLVMMetadataRef
) =
    LLVM.LLVMDIBuilderCreateCompileUnit(
        this,
        -1,
        fileRef,
        "hadesboot",
        "hadesboot".length.toLong(),
        false.toLLVMBool(),
        null, 0,
        0,
        null, 0,
        LLVM.LLVMDWARFEmissionFull,
        0,
        false.toLLVMBool(),
        false.toLLVMBool()
    )

fun LLVMDIBuilderRef.createLexicalBlock(scope: LLVMMetadataRef, file: LLVMMetadataRef, line: Int, column: Int) =
    LLVM.LLVMDIBuilderCreateLexicalBlock(this, scope, file, line, column)

fun LLVMDIBuilderRef.createFile(name: String, directory: String) =
    LLVM.LLVMDIBuilderCreateFile(this, name, name.length.toLong(), directory, directory.length.toLong())

fun LLVMDIBuilderRef.createParam(
    scope: LLVMMetadataRef,
    name: String,
    argNumber: Int,
    file: LLVMMetadataRef,
    type: LLVMMetadataRef,
    lineNumber: Int,
    llvmDIFlags: Int
) =
    LLVM.LLVMDIBuilderCreateParameterVariable(
        this,
        scope,
        name,
        name.length.toLong(),
        argNumber,
        file,
        lineNumber,
        type,
        true.toLLVMBool(),
        llvmDIFlags
    )

fun LLVMDIBuilderRef.createBasicType(
    name: String,
    sizeInBits: Long,
    llvmDWARFEncoding: Int = 0,
    llvmDIFlags: Int = LLVM.LLVMDIFlagZero
) =
    LLVM.LLVMDIBuilderCreateBasicType(
        this,
        name,
        name.length.toLong(),
        sizeInBits,
        llvmDWARFEncoding,
        llvmDIFlags)

fun LLVMDIBuilderRef.createFunction(
    scope: LLVMMetadataRef,
    name: String,
    file: LLVMMetadataRef,
    lineno: Int,
    ty: LLVMMetadataRef,
    isLocalToUnit: Boolean,
    isDefinition: Boolean,
    scopeLine: Int,
    flags: Int,
) =
    LLVM.LLVMDIBuilderCreateFunction(
        this,
        scope,
        name,
        name.length.toLong(),
        name,
        name.length.toLong(),
        file,
        lineno,
        ty,
        isLocalToUnit.toLLVMBool(),
        isDefinition.toLLVMBool(),
        scopeLine,
        flags,
        false.toLLVMBool(),
    )
