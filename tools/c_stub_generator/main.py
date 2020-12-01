import sys
from pathlib import Path
from clang.cindex import Index, CursorKind, Cursor, Type, TypeKind, SourceLocation


def main():
    out = sys.stdout

    index = Index.create()
    [*in_files] = sys.argv[1:]

    out.write('import c_types as c_types;\n\n')
    for in_file in in_files:
        process_file(in_file, index, out)


def process_file(in_file: str, index: Index, out):
    infile = Path(in_file)
    tu = index.parse(in_file, [])

    for _node in tu.cursor.get_children():
        node: Cursor = _node
        location: SourceLocation = node.location
        filename = Path(str(location.file.name))

        if filename.absolute() != infile.absolute():
            continue

        if node.kind == CursorKind.STRUCT_DECL:
            if node.spelling == '':
                continue
            if node.get_definition() is None:
                out.write(f'opaque type Struct_{node.spelling};\n')
                continue
            out.write(f'struct Struct_{node.spelling} {{\n')
            if node.get_definition() is not None:
                for _field in node.get_definition().get_children():
                    field: Cursor = _field

                    assert field.kind == CursorKind.FIELD_DECL, f"Expected Struct field, found {field.kind}"
                    typ: Type = field.type
                    name = field.spelling
                    out.write(f'  val f{name}: {print_type(typ)};\n')

            out.write(f'}}\n')

        if node.kind == CursorKind.FUNCTION_DECL:
            params = ', '.join([print_type(arg.type) for arg in node.get_arguments()])
            out.write(f'extern def {node.spelling}({params}): {print_type(node.result_type)} = {node.spelling};\n')
            pass
        if node.kind == CursorKind.TYPEDEF_DECL:
            if node.underlying_typedef_type.kind == TypeKind.ELABORATED:
                assert AssertionError()
            else:
                out.write(f'type {node.spelling} = {print_type(node.underlying_typedef_type)};\n')


def print_type(typ: Type) -> str:
    assert isinstance(typ, Type)
    if typ.kind == TypeKind.INT:
        return 'c_types.int'
    elif typ.kind == TypeKind.UINT:
        return 'c_types.uint'
    elif typ.kind == TypeKind.USHORT:
        return 'c_types.ushort'
    elif typ.kind == TypeKind.SHORT:
        return 'c_types.short'
    elif typ.kind == TypeKind.FLOAT:
        return 'c_types.float'
    elif typ.kind == TypeKind.LONG:
        return 'c_types.long'
    elif typ.kind == TypeKind.CHAR_S:
        return 'c_types.char'
    elif typ.kind == TypeKind.SCHAR:
        return 'c_types.schar'
    elif typ.kind == TypeKind.UCHAR:
        return 'c_types.uchar'
    elif typ.kind == TypeKind.DOUBLE:
        return 'c_types.double'
    elif typ.kind == TypeKind.LONGLONG:
        return 'c_types.long_long'
    elif typ.kind == TypeKind.LONGDOUBLE:
        return 'c_types.long_double'
    elif typ.kind == TypeKind.ULONG:
        return 'c_types.ulong'
    elif typ.kind == TypeKind.ULONGLONG:
        return 'c_types.ulonglong'
    elif typ.kind == TypeKind.USHORT:
        return 'c_types.ushort'
    elif typ.kind == TypeKind.VOID:
        return 'Void'
    elif typ.kind == TypeKind.ELABORATED:
        if typ.get_named_type().kind == TypeKind.RECORD:
            return f'Struct_{typ.get_named_type().spelling.replace("struct ", "")}'
        elif typ.get_named_type().kind == TypeKind.ENUM:
            return f'Enum_{typ.get_named_type().spelling}'
        else:
            raise AssertionError()
    elif typ.kind == TypeKind.RECORD:
        return typ.get_declaration().spelling
    elif typ.kind == TypeKind.CONSTANTARRAY:
        size = typ.get_array_size()
        inner = print_type(typ.get_array_element_type())
        return f'Array[{size}, {inner}]'
    elif typ.kind == TypeKind.POINTER:
        inner = print_type(typ.get_pointee())
        if typ.is_const_qualified:
            return f'*{inner}'
        else:
            return f'*mut {inner}'
    elif typ.kind == TypeKind.ENUM:
        return typ.spelling
    elif typ.kind == TypeKind.FUNCTIONPROTO:
        args = ', '.join([print_type(t) for t in typ.argument_types()])
        return f'({args}) -> {print_type(typ.get_result())}'
    elif typ.kind == TypeKind.TYPEDEF:
        return typ.spelling
    else:
        raise AssertionError(f'Unhandled type kind {typ.kind} at {typ}')


if __name__ == '__main__':
    main()
