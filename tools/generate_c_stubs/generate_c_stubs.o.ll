; ModuleID = 'generate_c_stubs.hds'
source_filename = "generate_c_stubs.hds"

%"hades.Result.Self.$[Int,StubGen.Error.Self]" = type { i1, %StubGen.Error.Self }
%StubGen.Error.Self = type { i32, %StubGen.Flags.ParseError }
%StubGen.Flags.ParseError = type { i8* }
%"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" = type { i1, %StubGen.Flags.t }
%StubGen.Flags.t = type { i8*, i8* }
%libc.File = type {}
%libc.Dir = type {}
%libc.dirent = type { i8* }

@StubGen.Error.KIND_UNIMPLEMENTED = global i32 0
@StubGen.Error.KIND_INVALID_FLAGS = global i32 1
@_hadesboot_string_literal_1 = global [14 x i8] c"UNIMPLEMENTED\00"
@StubGen.Flags.USAGE = global i8* getelementptr inbounds ([80 x i8], [80 x i8]* @_hadesboot_string_literal_2, i32 0, i32 0)
@_hadesboot_string_literal_2 = global [80 x i8] c"\0AUSAGE: generate_c_stubs [...FLAGS] <INPUT_HEADER_PATH> <OUTPUT_HDS_FILE_PATH>\0A\00"
@_hadesboot_string_literal_3 = global [17 x i8] c"Assertion failed\00"
@_hadesboot_string_literal_4 = global [19 x i8] c"Missing arguments.\00"
@hades.Result.TRACE_ON_ERROR = global i1 true

define void @hades_main(i32, i8**) {
entry:
  %"result$ptr" = alloca %"hades.Result.Self.$[Int,StubGen.Error.Self]"
  %"1" = call %"hades.Result.Self.$[Int,StubGen.Error.Self]" @run(i32 %0, i8** %1)
  store %"hades.Result.Self.$[Int,StubGen.Error.Self]" %"1", %"hades.Result.Self.$[Int,StubGen.Error.Self]"* %"result$ptr"
  %result = load %"hades.Result.Self.$[Int,StubGen.Error.Self]", %"hades.Result.Self.$[Int,StubGen.Error.Self]"* %"result$ptr"
  %"5" = call i1 @"hades.Result.is_error.$[Int,StubGen.Error.Self]"(%"hades.Result.Self.$[Int,StubGen.Error.Self]" %result)
  br i1 %"5", label %"2", label %"3"

"2":                                              ; preds = %entry
  %result1 = load %"hades.Result.Self.$[Int,StubGen.Error.Self]", %"hades.Result.Self.$[Int,StubGen.Error.Self]"* %"result$ptr"
  %"7" = call %StubGen.Error.Self @"hades.Result.get_error.$[Int,StubGen.Error.Self]"(%"hades.Result.Self.$[Int,StubGen.Error.Self]" %result1)
  call void @StubGen.Error.dump(%StubGen.Error.Self %"7")
  call void @exit(i32 -1)
  br label %"4"

"3":                                              ; preds = %entry
  call void @exit(i32 0)
  br label %"4"

"4":                                              ; preds = %"3", %"2"
  ret void
}

define %"hades.Result.Self.$[Int,StubGen.Error.Self]" @run(i32, i8**) {
entry:
  %"flags_result$ptr" = alloca %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]"
  %"10" = call %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" @StubGen.Flags.parse(i32 %0, i8** %1)
  store %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" %"10", %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]"* %"flags_result$ptr"
  %flags_result = load %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]", %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]"* %"flags_result$ptr"
  %"14" = call i1 @"hades.Result.is_error.$[StubGen.Flags.t,StubGen.Error.Self]"(%"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" %flags_result)
  br i1 %"14", label %"11", label %"12"

"11":                                             ; preds = %entry
  %flags_result1 = load %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]", %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]"* %"flags_result$ptr"
  %"16" = call %StubGen.Error.Self @"hades.Result.get_error.$[StubGen.Flags.t,StubGen.Error.Self]"(%"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" %flags_result1)
  %"15" = call %"hades.Result.Self.$[Int,StubGen.Error.Self]" @"hades.Result.error.$[Int,StubGen.Error.Self]"(%StubGen.Error.Self %"16")
  ret %"hades.Result.Self.$[Int,StubGen.Error.Self]" %"15"

"12":                                             ; preds = %entry
  br label %"13"

"13":                                             ; preds = %"12"
  %"flags$ptr" = alloca %StubGen.Flags.t
  %flags_result2 = load %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]", %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]"* %"flags_result$ptr"
  %"17" = call %StubGen.Flags.t @"hades.Result.get_value.$[StubGen.Flags.t,StubGen.Error.Self]"(%"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" %flags_result2)
  store %StubGen.Flags.t %"17", %StubGen.Flags.t* %"flags$ptr"
  %"19" = call %StubGen.Error.Self @StubGen.Error.unimplemented()
  %"18" = call %"hades.Result.Self.$[Int,StubGen.Error.Self]" @"hades.Result.error.$[Int,StubGen.Error.Self]"(%StubGen.Error.Self %"19")
  ret %"hades.Result.Self.$[Int,StubGen.Error.Self]" %"18"
}

define i1 @"hades.Result.is_error.$[Int,StubGen.Error.Self]"(%"hades.Result.Self.$[Int,StubGen.Error.Self]") {
entry:
  %"135" = extractvalue %"hades.Result.Self.$[Int,StubGen.Error.Self]" %0, 0
  ret i1 %"135"
}

define %StubGen.Error.Self @"hades.Result.get_error.$[Int,StubGen.Error.Self]"(%"hades.Result.Self.$[Int,StubGen.Error.Self]") {
entry:
  %"63" = call i1 @"hades.Result.is_error.$[Int,StubGen.Error.Self]"(%"hades.Result.Self.$[Int,StubGen.Error.Self]" %0)
  call void @hades.Assert.assert(i1 %"63")
  %"136" = extractvalue %"hades.Result.Self.$[Int,StubGen.Error.Self]" %0, 1
  %"64" = call %StubGen.Error.Self @"hades.UnsafeCast.downcast.$[StubGen.Error.Self,union[Int, StubGen.Error.Self]]"(%StubGen.Error.Self %"136")
  ret %StubGen.Error.Self %"64"
}

define void @StubGen.Error.dump(%StubGen.Error.Self) {
entry:
  %"31" = call i1 @StubGen.Error.is(%StubGen.Error.Self %0, i32 0)
  br i1 %"31", label %"28", label %"29"

"28":                                             ; preds = %entry
  call void @puts(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @_hadesboot_string_literal_1, i32 0, i32 0))
  br label %"30"

"29":                                             ; preds = %entry
  br label %"30"

"30":                                             ; preds = %"29", %"28"
  %"36" = call i1 @StubGen.Error.is(%StubGen.Error.Self %0, i32 1)
  br i1 %"36", label %"33", label %"34"

"33":                                             ; preds = %"30"
  %"38" = call %StubGen.Flags.ParseError @"StubGen.Error._data.$[StubGen.Flags.ParseError]"(%StubGen.Error.Self %0)
  %"134" = extractvalue %StubGen.Flags.ParseError %"38", 0
  call void @puts(i8* %"134")
  call void @puts(i8* getelementptr inbounds ([80 x i8], [80 x i8]* @_hadesboot_string_literal_2, i32 0, i32 0))
  br label %"35"

"34":                                             ; preds = %"30"
  br label %"35"

"35":                                             ; preds = %"34", %"33"
  ret void
}

declare void @exit(i32)

define %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" @StubGen.Flags.parse(i32, i8**) {
entry:
  %"50" = icmp slt i32 %0, 3
  br i1 %"50", label %"47", label %"48"

"47":                                             ; preds = %entry
  %"51" = call %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" @"StubGen.Flags.parse_error.$[StubGen.Flags.t]"(i8* getelementptr inbounds ([19 x i8], [19 x i8]* @_hadesboot_string_literal_4, i32 0, i32 0))
  ret %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" %"51"

"48":                                             ; preds = %entry
  br label %"49"

"49":                                             ; preds = %"48"
  %"OUTPUT_INDEX$ptr" = alloca i64
  %"53" = sub i32 %0, 1
  %"52" = call i64 @"hades.UnsafeCast.upcast.$[Size,Int]"(i32 %"53")
  store i64 %"52", i64* %"OUTPUT_INDEX$ptr"
  %"INPUT_INDEX$ptr" = alloca i64
  %"55" = sub i32 %0, 2
  %"54" = call i64 @"hades.UnsafeCast.upcast.$[Size,Int]"(i32 %"55")
  store i64 %"54", i64* %"INPUT_INDEX$ptr"
  %INPUT_INDEX = load i64, i64* %"INPUT_INDEX$ptr"
  %"59" = call i8** @"hades.Pointer.offset.$[*Byte]"(i8** %1, i64 %INPUT_INDEX)
  %"58" = load i8*, i8** %"59"
  %OUTPUT_INDEX = load i64, i64* %"OUTPUT_INDEX$ptr"
  %"61" = call i8** @"hades.Pointer.offset.$[*Byte]"(i8** %1, i64 %OUTPUT_INDEX)
  %"60" = load i8*, i8** %"61"
  %"57" = call %StubGen.Flags.t @StubGen.Flags.t(i8* %"58", i8* %"60")
  %"56" = call %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" @"hades.Result.ok.$[StubGen.Error.Self,StubGen.Flags.t]"(%StubGen.Flags.t %"57")
  ret %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" %"56"
}

define i1 @"hades.Result.is_error.$[StubGen.Flags.t,StubGen.Error.Self]"(%"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]") {
entry:
  %"137" = extractvalue %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" %0, 0
  ret i1 %"137"
}

define %StubGen.Error.Self @"hades.Result.get_error.$[StubGen.Flags.t,StubGen.Error.Self]"(%"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]") {
entry:
  %"72" = call i1 @"hades.Result.is_error.$[StubGen.Flags.t,StubGen.Error.Self]"(%"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" %0)
  call void @hades.Assert.assert(i1 %"72")
  %"138" = extractvalue %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" %0, 1
  %"73" = call %StubGen.Error.Self @"hades.UnsafeCast.downcast.$[StubGen.Error.Self,union[StubGen.Flags.t, StubGen.Error.Self]]"(%StubGen.Flags.t %"138")
  ret %StubGen.Error.Self %"73"
}

define %"hades.Result.Self.$[Int,StubGen.Error.Self]" @"hades.Result.error.$[Int,StubGen.Error.Self]"(%StubGen.Error.Self) {
entry:
  br i1 true, label %"65", label %"66"

"65":                                             ; preds = %entry
  call void @Hades_Debug_dump_stack_trace()
  br label %"67"

"66":                                             ; preds = %entry
  br label %"67"

"67":                                             ; preds = %"66", %"65"
  %"70" = call %StubGen.Error.Self @"hades.UnsafeCast.upcast.$[union[Int, StubGen.Error.Self],StubGen.Error.Self]"(%StubGen.Error.Self %0)
  %"69" = call %"hades.Result.Self.$[Int,StubGen.Error.Self]" @"hades.Result.Self.$[Int,StubGen.Error.Self]"(i1 true, %StubGen.Error.Self %"70")
  ret %"hades.Result.Self.$[Int,StubGen.Error.Self]" %"69"
}

define %StubGen.Flags.t @"hades.Result.get_value.$[StubGen.Flags.t,StubGen.Error.Self]"(%"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]") {
entry:
  %"75" = call i1 @"hades.Result.is_ok.$[StubGen.Flags.t,StubGen.Error.Self]"(%"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" %0)
  call void @hades.Assert.assert(i1 %"75")
  %"139" = extractvalue %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" %0, 1
  %"76" = call %StubGen.Flags.t @"hades.UnsafeCast.downcast.$[StubGen.Flags.t,union[StubGen.Flags.t, StubGen.Error.Self]]"(%StubGen.Flags.t %"139")
  ret %StubGen.Flags.t %"76"
}

define %StubGen.Error.Self @StubGen.Error.unimplemented() {
entry:
  %"23" = call %StubGen.Flags.ParseError @"hades.UnsafeCast.upcast.$[union[Bool, StubGen.Flags.ParseError],Bool]"(i1 false)
  %"22" = call %StubGen.Error.Self @StubGen.Error.Self(i32 0, %StubGen.Flags.ParseError %"23")
  ret %StubGen.Error.Self %"22"
}

declare void @puts(i8*)

declare void* @malloc(i64)

declare void @free(void*)

declare i64 @strlen(i8*)

declare i1 @strcmp(i8*, i8*)

declare void @memcpy(void*, void*, i64)

declare i32 @memcmp(void*, void*, i64)

declare double @floor(double)

declare i64 @fprintf(%libc.File*, i8*, i8*)

declare i64 @printf(i8*, i8*)

declare i64 @sleep(i64)

declare void @fflush(%libc.File*)

declare void @fputs(i8*, %libc.File*)

define %libc.Dir @libc.Dir() {
entry:
  %this = alloca %libc.Dir
  %instance = load %libc.Dir, %libc.Dir* %this
  ret %libc.Dir %instance
}

define %libc.dirent @libc.dirent(i8*) {
entry:
  %this = alloca %libc.dirent
  %field_0 = getelementptr inbounds %libc.dirent, %libc.dirent* %this, i32 0, i32 0
  store i8* %0, i8** %field_0
  %instance = load %libc.dirent, %libc.dirent* %this
  ret %libc.dirent %instance
}

define %libc.File @libc.File() {
entry:
  %this = alloca %libc.File
  %instance = load %libc.File, %libc.File* %this
  ret %libc.File %instance
}

declare %libc.Dir* @opendir(i8*)

declare void @closedir(%libc.Dir*)

declare %libc.dirent* @readdir(%libc.Dir*)

declare i64 @getline(i8**, i64*, %libc.File*)

declare i8 @fgetc(%libc.File*)

declare i64 @strtoul(i8*, i8**, i32)

declare i8* @hdc_dirent_name(%libc.dirent*)

declare i1 @hdc_dirent_is_directory(%libc.dirent*)

define i8* @libc.name(%libc.dirent*) {
entry:
  %"20" = call i8* @hdc_dirent_name(%libc.dirent* %0)
  ret i8* %"20"
}

define i1 @libc.is_directory(%libc.dirent*) {
entry:
  %"21" = call i1 @hdc_dirent_is_directory(%libc.dirent* %0)
  ret i1 %"21"
}

declare i64 @fread(void*, i64, i64, %libc.File*)

declare void* @clang_createIndex(i1, i1)

declare void @clang_disposeIndex(void*)

define %StubGen.Error.Self @StubGen.Error.Self(i32, %StubGen.Flags.ParseError) {
entry:
  %this = alloca %StubGen.Error.Self
  %field_0 = getelementptr inbounds %StubGen.Error.Self, %StubGen.Error.Self* %this, i32 0, i32 0
  store i32 %0, i32* %field_0
  %field_1 = getelementptr inbounds %StubGen.Error.Self, %StubGen.Error.Self* %this, i32 0, i32 1
  store %StubGen.Flags.ParseError %1, %StubGen.Flags.ParseError* %field_1
  %instance = load %StubGen.Error.Self, %StubGen.Error.Self* %this
  ret %StubGen.Error.Self %instance
}

define %StubGen.Flags.ParseError @"hades.UnsafeCast.upcast.$[union[Bool, StubGen.Flags.ParseError],Bool]"(i1) {
entry:
  call void @hades.Assert.assert(i1 icmp sle (i64 ptrtoint (i1* getelementptr (i1, i1* null, i32 1) to i64), i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64)))
  %"79" = call %StubGen.Flags.ParseError @"hades.UnsafeCast._cast.$[union[Bool, StubGen.Flags.ParseError],Bool]"(i1 %0)
  ret %StubGen.Flags.ParseError %"79"
}

define %StubGen.Error.Self @StubGen.Error.invalid_flags(%StubGen.Flags.ParseError) {
entry:
  %"25" = call %StubGen.Flags.ParseError @"hades.UnsafeCast.upcast.$[union[Bool, StubGen.Flags.ParseError],StubGen.Flags.ParseError]"(%StubGen.Flags.ParseError %0)
  %"24" = call %StubGen.Error.Self @StubGen.Error.Self(i32 1, %StubGen.Flags.ParseError %"25")
  ret %StubGen.Error.Self %"24"
}

define %StubGen.Flags.ParseError @"hades.UnsafeCast.upcast.$[union[Bool, StubGen.Flags.ParseError],StubGen.Flags.ParseError]"(%StubGen.Flags.ParseError) {
entry:
  call void @hades.Assert.assert(i1 true)
  %"82" = call %StubGen.Flags.ParseError @"hades.UnsafeCast._cast.$[union[Bool, StubGen.Flags.ParseError],StubGen.Flags.ParseError]"(%StubGen.Flags.ParseError %0)
  ret %StubGen.Flags.ParseError %"82"
}

define i1 @StubGen.Error.is(%StubGen.Error.Self, i32) {
entry:
  %"26" = call i32 @StubGen.Error.kind(%StubGen.Error.Self %0)
  %"27" = icmp eq i32 %"26", %1
  ret i1 %"27"
}

define i32 @StubGen.Error.kind(%StubGen.Error.Self) {
entry:
  %"133" = extractvalue %StubGen.Error.Self %0, 0
  ret i32 %"133"
}

define %StubGen.Flags.ParseError @"StubGen.Error._data.$[StubGen.Flags.ParseError]"(%StubGen.Error.Self) {
entry:
  %"140" = extractvalue %StubGen.Error.Self %0, 1
  %"83" = call %StubGen.Flags.ParseError @"hades.UnsafeCast.downcast.$[StubGen.Flags.ParseError,union[Bool, StubGen.Flags.ParseError]]"(%StubGen.Flags.ParseError %"140")
  ret %StubGen.Flags.ParseError %"83"
}

define void @hades.Assert.assert(i1) {
entry:
  %"43" = xor i1 %0, true
  br i1 %"43", label %"40", label %"41"

"40":                                             ; preds = %entry
  call void @hades.Assert.panic(i8* getelementptr inbounds ([17 x i8], [17 x i8]* @_hadesboot_string_literal_3, i32 0, i32 0))
  br label %"42"

"41":                                             ; preds = %entry
  br label %"42"

"42":                                             ; preds = %"41", %"40"
  ret void
}

define void @hades.Assert.panic(i8*) {
entry:
  call void @puts(i8* %0)
  call void @exit(i32 1)
  ret void
}

define %StubGen.Flags.t @StubGen.Flags.t(i8*, i8*) {
entry:
  %this = alloca %StubGen.Flags.t
  %field_0 = getelementptr inbounds %StubGen.Flags.t, %StubGen.Flags.t* %this, i32 0, i32 0
  store i8* %0, i8** %field_0
  %field_1 = getelementptr inbounds %StubGen.Flags.t, %StubGen.Flags.t* %this, i32 0, i32 1
  store i8* %1, i8** %field_1
  %instance = load %StubGen.Flags.t, %StubGen.Flags.t* %this
  ret %StubGen.Flags.t %instance
}

define %StubGen.Flags.ParseError @StubGen.Flags.ParseError(i8*) {
entry:
  %this = alloca %StubGen.Flags.ParseError
  %field_0 = getelementptr inbounds %StubGen.Flags.ParseError, %StubGen.Flags.ParseError* %this, i32 0, i32 0
  store i8* %0, i8** %field_0
  %instance = load %StubGen.Flags.ParseError, %StubGen.Flags.ParseError* %this
  ret %StubGen.Flags.ParseError %instance
}

define %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" @"StubGen.Flags.parse_error.$[StubGen.Flags.t]"(i8*) {
entry:
  %"86" = call %StubGen.Flags.ParseError @StubGen.Flags.ParseError(i8* %0)
  %"85" = call %StubGen.Error.Self @StubGen.Error.invalid_flags(%StubGen.Flags.ParseError %"86")
  %"84" = call %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" @"hades.Result.error.$[StubGen.Flags.t,StubGen.Error.Self]"(%StubGen.Error.Self %"85")
  ret %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" %"84"
}

define i64 @"hades.UnsafeCast.upcast.$[Size,Int]"(i32) {
entry:
  call void @hades.Assert.assert(i1 icmp sle (i64 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i64), i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64)))
  %"89" = call i64 @"hades.UnsafeCast._cast.$[Size,Int]"(i32 %0)
  ret i64 %"89"
}

define i8** @"hades.Pointer.offset.$[*Byte]"(i8**, i64) {
entry:
  %"93" = mul i64 %1, ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64)
  %"141" = bitcast i8** %0 to void*
  %"92" = call void* @__hds_pointer_add(void* %"141", i64 %"93")
  %"142" = bitcast void* %"92" to i8**
  ret i8** %"142"
}

define %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" @"hades.Result.ok.$[StubGen.Error.Self,StubGen.Flags.t]"(%StubGen.Flags.t) {
entry:
  %"91" = call %StubGen.Flags.t @"hades.UnsafeCast.upcast.$[union[StubGen.Flags.t, StubGen.Error.Self],StubGen.Flags.t]"(%StubGen.Flags.t %0)
  %"90" = call %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" @"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]"(i1 false, %StubGen.Flags.t %"91")
  ret %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" %"90"
}

declare void @Hades_Debug_dump_int(i32)

declare void @Hades_Debug_dump_string(i8*)

declare void @Hades_Debug_dump_newline()

declare void @Hades_Debug_dump_stack_trace()

declare void* @__hds_pointer_add(void*, i64)

define %"hades.Result.Self.$[Int,StubGen.Error.Self]" @"hades.Result.Self.$[Int,StubGen.Error.Self]"(i1, %StubGen.Error.Self) {
entry:
  %this = alloca %"hades.Result.Self.$[Int,StubGen.Error.Self]"
  %field_0 = getelementptr inbounds %"hades.Result.Self.$[Int,StubGen.Error.Self]", %"hades.Result.Self.$[Int,StubGen.Error.Self]"* %this, i32 0, i32 0
  store i1 %0, i1* %field_0
  %field_1 = getelementptr inbounds %"hades.Result.Self.$[Int,StubGen.Error.Self]", %"hades.Result.Self.$[Int,StubGen.Error.Self]"* %this, i32 0, i32 1
  store %StubGen.Error.Self %1, %StubGen.Error.Self* %field_1
  %instance = load %"hades.Result.Self.$[Int,StubGen.Error.Self]", %"hades.Result.Self.$[Int,StubGen.Error.Self]"* %this
  ret %"hades.Result.Self.$[Int,StubGen.Error.Self]" %instance
}

define %StubGen.Error.Self @"hades.UnsafeCast.downcast.$[StubGen.Error.Self,union[Int, StubGen.Error.Self]]"(%StubGen.Error.Self) {
entry:
  call void @hades.Assert.assert(i1 true)
  %"96" = call %StubGen.Error.Self @"hades.UnsafeCast._cast.$[StubGen.Error.Self,union[Int, StubGen.Error.Self]]"(%StubGen.Error.Self %0)
  ret %StubGen.Error.Self %"96"
}

define %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" @"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]"(i1, %StubGen.Flags.t) {
entry:
  %this = alloca %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]"
  %field_0 = getelementptr inbounds %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]", %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]"* %this, i32 0, i32 0
  store i1 %0, i1* %field_0
  %field_1 = getelementptr inbounds %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]", %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]"* %this, i32 0, i32 1
  store %StubGen.Flags.t %1, %StubGen.Flags.t* %field_1
  %instance = load %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]", %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]"* %this
  ret %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" %instance
}

define %StubGen.Error.Self @"hades.UnsafeCast.upcast.$[union[Int, StubGen.Error.Self],StubGen.Error.Self]"(%StubGen.Error.Self) {
entry:
  call void @hades.Assert.assert(i1 true)
  %"99" = call %StubGen.Error.Self @"hades.UnsafeCast._cast.$[union[Int, StubGen.Error.Self],StubGen.Error.Self]"(%StubGen.Error.Self %0)
  ret %StubGen.Error.Self %"99"
}

define %StubGen.Error.Self @"hades.UnsafeCast.downcast.$[StubGen.Error.Self,union[StubGen.Flags.t, StubGen.Error.Self]]"(%StubGen.Flags.t) {
entry:
  call void @hades.Assert.assert(i1 icmp sge (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2), i64 ptrtoint (%StubGen.Error.Self* getelementptr (%StubGen.Error.Self, %StubGen.Error.Self* null, i32 1) to i64)))
  %"102" = call %StubGen.Error.Self @"hades.UnsafeCast._cast.$[StubGen.Error.Self,union[StubGen.Flags.t, StubGen.Error.Self]]"(%StubGen.Flags.t %0)
  ret %StubGen.Error.Self %"102"
}

define i1 @"hades.Result.is_ok.$[StubGen.Flags.t,StubGen.Error.Self]"(%"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]") {
entry:
  %"104" = call i1 @"hades.Result.is_error.$[StubGen.Flags.t,StubGen.Error.Self]"(%"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" %0)
  %"103" = xor i1 %"104", true
  ret i1 %"103"
}

define %StubGen.Flags.t @"hades.UnsafeCast.downcast.$[StubGen.Flags.t,union[StubGen.Flags.t, StubGen.Error.Self]]"(%StubGen.Flags.t) {
entry:
  call void @hades.Assert.assert(i1 true)
  %"107" = call %StubGen.Flags.t @"hades.UnsafeCast._cast.$[StubGen.Flags.t,union[StubGen.Flags.t, StubGen.Error.Self]]"(%StubGen.Flags.t %0)
  ret %StubGen.Flags.t %"107"
}

define %StubGen.Flags.ParseError @"hades.UnsafeCast._cast.$[union[Bool, StubGen.Flags.ParseError],Bool]"(i1) {
entry:
  %"copy$ptr" = alloca i1
  store i1 %0, i1* %"copy$ptr"
  %"143" = bitcast i1* %"copy$ptr" to %StubGen.Flags.ParseError*
  %"108" = load %StubGen.Flags.ParseError, %StubGen.Flags.ParseError* %"143"
  ret %StubGen.Flags.ParseError %"108"
}

define %StubGen.Flags.ParseError @"hades.UnsafeCast._cast.$[union[Bool, StubGen.Flags.ParseError],StubGen.Flags.ParseError]"(%StubGen.Flags.ParseError) {
entry:
  %"copy$ptr" = alloca %StubGen.Flags.ParseError
  store %StubGen.Flags.ParseError %0, %StubGen.Flags.ParseError* %"copy$ptr"
  %"109" = load %StubGen.Flags.ParseError, %StubGen.Flags.ParseError* %"copy$ptr"
  ret %StubGen.Flags.ParseError %"109"
}

define %StubGen.Flags.ParseError @"hades.UnsafeCast.downcast.$[StubGen.Flags.ParseError,union[Bool, StubGen.Flags.ParseError]]"(%StubGen.Flags.ParseError) {
entry:
  call void @hades.Assert.assert(i1 true)
  %"112" = call %StubGen.Flags.ParseError @"hades.UnsafeCast._cast.$[StubGen.Flags.ParseError,union[Bool, StubGen.Flags.ParseError]]"(%StubGen.Flags.ParseError %0)
  ret %StubGen.Flags.ParseError %"112"
}

define %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" @"hades.Result.error.$[StubGen.Flags.t,StubGen.Error.Self]"(%StubGen.Error.Self) {
entry:
  br i1 true, label %"113", label %"114"

"113":                                            ; preds = %entry
  call void @Hades_Debug_dump_stack_trace()
  br label %"115"

"114":                                            ; preds = %entry
  br label %"115"

"115":                                            ; preds = %"114", %"113"
  %"118" = call %StubGen.Flags.t @"hades.UnsafeCast.upcast.$[union[StubGen.Flags.t, StubGen.Error.Self],StubGen.Error.Self]"(%StubGen.Error.Self %0)
  %"117" = call %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" @"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]"(i1 true, %StubGen.Flags.t %"118")
  ret %"hades.Result.Self.$[StubGen.Flags.t,StubGen.Error.Self]" %"117"
}

define i64 @"hades.UnsafeCast._cast.$[Size,Int]"(i32) {
entry:
  %"copy$ptr" = alloca i32
  store i32 %0, i32* %"copy$ptr"
  %"145" = bitcast i32* %"copy$ptr" to i64*
  %"119" = load i64, i64* %"145"
  ret i64 %"119"
}

define %StubGen.Flags.t @"hades.UnsafeCast.upcast.$[union[StubGen.Flags.t, StubGen.Error.Self],StubGen.Flags.t]"(%StubGen.Flags.t) {
entry:
  call void @hades.Assert.assert(i1 true)
  %"122" = call %StubGen.Flags.t @"hades.UnsafeCast._cast.$[union[StubGen.Flags.t, StubGen.Error.Self],StubGen.Flags.t]"(%StubGen.Flags.t %0)
  ret %StubGen.Flags.t %"122"
}

define %StubGen.Error.Self @"hades.UnsafeCast._cast.$[StubGen.Error.Self,union[Int, StubGen.Error.Self]]"(%StubGen.Error.Self) {
entry:
  %"copy$ptr" = alloca %StubGen.Error.Self
  store %StubGen.Error.Self %0, %StubGen.Error.Self* %"copy$ptr"
  %"123" = load %StubGen.Error.Self, %StubGen.Error.Self* %"copy$ptr"
  ret %StubGen.Error.Self %"123"
}

define %StubGen.Error.Self @"hades.UnsafeCast._cast.$[union[Int, StubGen.Error.Self],StubGen.Error.Self]"(%StubGen.Error.Self) {
entry:
  %"copy$ptr" = alloca %StubGen.Error.Self
  store %StubGen.Error.Self %0, %StubGen.Error.Self* %"copy$ptr"
  %"124" = load %StubGen.Error.Self, %StubGen.Error.Self* %"copy$ptr"
  ret %StubGen.Error.Self %"124"
}

define %StubGen.Error.Self @"hades.UnsafeCast._cast.$[StubGen.Error.Self,union[StubGen.Flags.t, StubGen.Error.Self]]"(%StubGen.Flags.t) {
entry:
  %"copy$ptr" = alloca %StubGen.Flags.t
  store %StubGen.Flags.t %0, %StubGen.Flags.t* %"copy$ptr"
  %"148" = bitcast %StubGen.Flags.t* %"copy$ptr" to %StubGen.Error.Self*
  %"125" = load %StubGen.Error.Self, %StubGen.Error.Self* %"148"
  ret %StubGen.Error.Self %"125"
}

define %StubGen.Flags.t @"hades.UnsafeCast._cast.$[StubGen.Flags.t,union[StubGen.Flags.t, StubGen.Error.Self]]"(%StubGen.Flags.t) {
entry:
  %"copy$ptr" = alloca %StubGen.Flags.t
  store %StubGen.Flags.t %0, %StubGen.Flags.t* %"copy$ptr"
  %"126" = load %StubGen.Flags.t, %StubGen.Flags.t* %"copy$ptr"
  ret %StubGen.Flags.t %"126"
}

define %StubGen.Flags.ParseError @"hades.UnsafeCast._cast.$[StubGen.Flags.ParseError,union[Bool, StubGen.Flags.ParseError]]"(%StubGen.Flags.ParseError) {
entry:
  %"copy$ptr" = alloca %StubGen.Flags.ParseError
  store %StubGen.Flags.ParseError %0, %StubGen.Flags.ParseError* %"copy$ptr"
  %"127" = load %StubGen.Flags.ParseError, %StubGen.Flags.ParseError* %"copy$ptr"
  ret %StubGen.Flags.ParseError %"127"
}

define %StubGen.Flags.t @"hades.UnsafeCast.upcast.$[union[StubGen.Flags.t, StubGen.Error.Self],StubGen.Error.Self]"(%StubGen.Error.Self) {
entry:
  call void @hades.Assert.assert(i1 icmp sle (i64 ptrtoint (%StubGen.Error.Self* getelementptr (%StubGen.Error.Self, %StubGen.Error.Self* null, i32 1) to i64), i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2)))
  %"130" = call %StubGen.Flags.t @"hades.UnsafeCast._cast.$[union[StubGen.Flags.t, StubGen.Error.Self],StubGen.Error.Self]"(%StubGen.Error.Self %0)
  ret %StubGen.Flags.t %"130"
}

define %StubGen.Flags.t @"hades.UnsafeCast._cast.$[union[StubGen.Flags.t, StubGen.Error.Self],StubGen.Flags.t]"(%StubGen.Flags.t) {
entry:
  %"copy$ptr" = alloca %StubGen.Flags.t
  store %StubGen.Flags.t %0, %StubGen.Flags.t* %"copy$ptr"
  %"131" = load %StubGen.Flags.t, %StubGen.Flags.t* %"copy$ptr"
  ret %StubGen.Flags.t %"131"
}

define %StubGen.Flags.t @"hades.UnsafeCast._cast.$[union[StubGen.Flags.t, StubGen.Error.Self],StubGen.Error.Self]"(%StubGen.Error.Self) {
entry:
  %"copy$ptr" = alloca %StubGen.Error.Self
  store %StubGen.Error.Self %0, %StubGen.Error.Self* %"copy$ptr"
  %"152" = bitcast %StubGen.Error.Self* %"copy$ptr" to %StubGen.Flags.t*
  %"132" = load %StubGen.Flags.t, %StubGen.Flags.t* %"152"
  ret %StubGen.Flags.t %"132"
}
