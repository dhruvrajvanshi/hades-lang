package llvm

import org.bytedeco.llvm.LLVM.*
import org.bytedeco.llvm.global.LLVM

fun LLVMDIBuilderRef.createCompileUnit(
    fileRef: LLVMMetadataRef
): LLVMMetadataRef =
    LLVM.LLVMDIBuilderCreateCompileUnit(
        // LLVMDIBuilderRef Builder
        this,
        // @Cast("LLVMDWARFSourceLanguage") int Lang
        LLVM.LLVMDWARFSourceLanguageC,
        // LLVMMetadataRef FileRef,
        fileRef,
        // @Cast("const char*") BytePointer Producer,
        "hadesboot",
        // @Cast("size_t") long ProducerLen,
        "hadesboot".length.toLong(),
        // @Cast("LLVMBool") int isOptimized
        false.toLLVMBool(),
        // @Cast("const char*") BytePointer Flags
        null,
        // @Cast("size_t") long FlagsLen,
        0,
        // @Cast("unsigned") int RuntimeVer,
        1,
        // @Cast("const char*") BytePointer SplitName,
        null,
        // @Cast("size_t") long SplitNameLen
        0,
        // @Cast("LLVMDWARFEmissionKind") int Kind
        LLVM.LLVMDWARFEmissionFull,
        // @Cast("unsigned") int DWOId
        0,
        // @Cast("LLVMBool") int SplitDebugInlining
        false.toLLVMBool(),
        // @Cast("LLVMBool") int DebugInfoForProfiling
        false.toLLVMBool(),
        // @Cast("const char*") BytePointer SysRoot,
        null,
        // @Cast("size_t") long SysRootLen,
        0,
        // @Cast("const char*") BytePointer SDK
        null,
        // @Cast("size_t") long SDKLen
        0
    )
@Suppress("unused")
fun LLVMDIBuilderRef.createLexicalBlock(scope: LLVMMetadataRef, file: LLVMMetadataRef, line: Int, column: Int): LLVMMetadataRef =
    LLVM.LLVMDIBuilderCreateLexicalBlock(this, scope, file, line, column)

fun LLVMDIBuilderRef.createFile(name: String, directory: String): LLVMMetadataRef =
    LLVM.LLVMDIBuilderCreateFile(this, name, name.length.toLong(), directory, directory.length.toLong())

@Suppress("unused")
fun LLVMDIBuilderRef.createParam(
    scope: LLVMMetadataRef,
    name: String,
    argNumber: Int,
    file: LLVMMetadataRef,
    type: LLVMMetadataRef,
    lineNumber: Int,
    llvmDIFlags: Int
): LLVMMetadataRef =
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
): LLVMMetadataRef =
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
    linkageName: String,
    file: LLVMMetadataRef,
    lineno: Int,
    ty: LLVMMetadataRef,
    isLocalToUnit: Boolean,
    isDefinition: Boolean,
    scopeLine: Int,
    flags: Int,
): LLVMMetadataRef =
    LLVM.LLVMDIBuilderCreateFunction(
        this,
        scope,
        name,
        name.length.toLong(),
        linkageName,
        linkageName.length.toLong(),
        file,
        lineno,
        ty,
        isLocalToUnit.toLLVMBool(),
        isDefinition.toLLVMBool(),
        scopeLine,
        flags,
        false.toLLVMBool(),
    )
