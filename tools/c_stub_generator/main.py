import sys
from clang.cindex import Index, CursorKind, Cursor, Type, TypeKind


def main():
    out = sys.stdout

    index = Index.create()
    [*_, in_file] = sys.argv
    tu = index.parse(in_file, [])

    out.write('import c_types as c_types;\n')

    for _node in tu.cursor.get_children():
        node: Cursor = _node

        if node.kind == CursorKind.STRUCT_DECL:
            assert node.spelling is not None and node.spelling != ''
            out.write(f'struct {node.spelling} {{\n')
            if node.get_definition() is not None:
                for _field in node.get_definition().get_children():
                    field: Cursor = _field
                    assert field.kind == CursorKind.FIELD_DECL, f"Expected Struct field, found {field.kind}"
                    typ: Type = field.type
                    name = field.spelling
                    out.write(f'  val f{name}: {print_type(typ)};\n')

            out.write(f'}}\n')

        if node.kind == CursorKind.FUNCTION_DECL:
            params = ', '.join([ print_type(arg.type) for arg in node.get_arguments() ])
            out.write(f'extern def {node.spelling}({params}): {print_type(node.result_type)} = {node.spelling};\n')
            pass
        if node.kind == CursorKind.TYPEDEF_DECL:
            pass
            # out.write(f'type {node.spelling} = {print_type(node.type)};\n')


def print_type(_typ: Type) -> str:
    typ = _typ.get_canonical()
    assert isinstance(typ, Type)
    if typ.kind == TypeKind.INT:
        return 'c_types.int'
    elif typ.kind == TypeKind.FLOAT:
        return 'c_types.float'
    elif typ.kind == TypeKind.LONG:
        return 'c_types.long'
    elif typ.kind == TypeKind.CHAR_S:
        return 'c_types.char'
    elif typ.kind == TypeKind.ELABORATED:
        return typ.get_declaration().spelling
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
    else:
        raise AssertionError(f'Unhandled type kind {typ.kind}')


if __name__ == '__main__':
    main()
