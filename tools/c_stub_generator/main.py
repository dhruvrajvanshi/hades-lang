import sys
from pathlib import Path
from clang.cindex import Index, CursorKind, Cursor, Type, TypeKind, SourceLocation, Config


def main():
    out = open('out.hds', 'w')

    index = Index.create()
    [*in_files] = sys.argv[1:]

    out.write('import hades.ffi.c as c\n\n')
    for in_file in in_files:
        process_file(in_file, index, out)


def process_file(in_file: str, index: Index, out):
    print(f'Generating {in_file}')
    tu = index.parse(in_file, [])
    counter = 0
    for _node in tu.cursor.get_children():
        node: Cursor = _node
        after = ''
        if node.kind == CursorKind.STRUCT_DECL:
            if node.spelling == '':
                continue
            if node.get_definition() is None:
                out.write(f'type {node.spelling};\n')
                continue
            out.write(f'struct {node.spelling} {{\n')
            if node.get_definition() is not None:
                for _field in node.get_definition().get_children():
                    field: Cursor = _field

                    if field.kind == CursorKind.FIELD_DECL:
                        typ: Type = field.type
                        name = field.spelling
                        out.write(f'  val f{name}: {print_type(typ)};\n')
                    elif field.kind == CursorKind.STRUCT_DECL:
                        members = []
                        for _member in field.get_children():
                            member: Cursor = _member
                            members.append(f'  val {member.displayname}: ')
                            members.append(print_type(member.type))
                        name = f'__hades_stubs_{counter}'
                        counter += 1
                        fields = '\n'.join(members)
                        decl = f'struct {name} {{\n{fields}\n}}'
                        after += decl

                    elif field.kind == CursorKind.ENUM_DECL:
                        raise Exception('')
                    elif field.kind == CursorKind.UNION_DECL:
                        members = []
                        for _member in field.get_children():
                            member: Cursor = _member
                            members.append(print_type(member.type))
                        
                        name = f'__hades_stubs_{counter}'
                        counter += 1
                        fields = ', '.join(members)
                        union_decl = f'\ntype {name} = Union[{fields}]\n'
                        after += union_decl
                    else:
                        raise Exception(f'Unexpected kind: {field.kind}')

            out.write(f'}}\n')

        elif node.kind == CursorKind.FUNCTION_DECL:
            params = ', '.join([print_type(arg.type) for arg in node.get_arguments()])
            out.write(f'extern def {node.spelling}({params}): {print_type(node.result_type)} = {node.spelling};\n')
            pass
        elif node.kind == CursorKind.TYPEDEF_DECL:
            if node.underlying_typedef_type.kind == TypeKind.ELABORATED:
                assert AssertionError()
            else:
                out.write(f'type {node.spelling} = {print_type(node.underlying_typedef_type)};\n')
        elif node.kind == CursorKind.VAR_DECL:
            out.write(f'extern const {node.mangled_name}: {print_type(node.type)}\n')
        elif node.kind == CursorKind.ENUM_DECL:
            out.write(f'enum {node.spelling} {{\n')

            members = []

            for _member in node.get_children():
                member: Cursor = _member
                members.append(f'  {member.displayname} = {member.enum_value}')

            out.write('\n'.join(members))

            out.write('\n}\n\n')
        else:
            raise Exception(f'Unhandled decl: {node.kind}')

        out.write(after)


def print_type(typ: Type) -> str:
    assert isinstance(typ, Type), f'{typ.__class__}'
    if typ.kind == TypeKind.INT:
        return 'c.int'
    elif typ.kind == TypeKind.UINT:
        return 'c.uint'
    elif typ.kind == TypeKind.USHORT:
        return 'c.ushort'
    elif typ.kind == TypeKind.SHORT:
        return 'c.short'
    elif typ.kind == TypeKind.FLOAT:
        return 'c.float'
    elif typ.kind == TypeKind.LONG:
        return 'c.long'
    elif typ.kind == TypeKind.CHAR_S:
        return 'c.char'
    elif typ.kind == TypeKind.SCHAR:
        return 'c.schar'
    elif typ.kind == TypeKind.UCHAR:
        return 'c.uchar'
    elif typ.kind == TypeKind.DOUBLE:
        return 'c.double'
    elif typ.kind == TypeKind.LONGLONG:
        return 'c.long_long'
    elif typ.kind == TypeKind.LONGDOUBLE:
        return 'c.long_double'
    elif typ.kind == TypeKind.ULONG:
        return 'c.ulong'
    elif typ.kind == TypeKind.ULONGLONG:
        return 'c.ulonglong'
    elif typ.kind == TypeKind.USHORT:
        return 'c.ushort'
    elif typ.kind == TypeKind.VOID:
        return 'c.void'
    elif typ.kind == TypeKind.ELABORATED:
        if typ.get_named_type().kind == TypeKind.RECORD:
            return f'{typ.get_named_type().spelling.replace("struct ", "")}'
        elif typ.get_named_type().kind == TypeKind.ENUM:
            return f'{typ.get_named_type().spelling}'
        else:
            raise AssertionError()
    elif typ.kind == TypeKind.RECORD:
        return typ.get_declaration().spelling
    elif typ.kind == TypeKind.CONSTANTARRAY:
        size = typ.get_array_size()
        inner = print_type(typ.get_array_element_type())
        return f'array[{inner}, {size}]'
    elif typ.kind == TypeKind.POINTER:
        inner = print_type(typ.get_pointee())
        if typ.is_const_qualified:
            return f'*{inner}'
        else:
            return f'*mut {inner}'
    elif typ.kind == TypeKind.ENUM:
        return typ.spelling.replace('enum ', '')
    elif typ.kind == TypeKind.FUNCTIONPROTO:
        args = ', '.join([print_type(t) for t in typ.argument_types()])
        return f'def ({args}) -> {print_type(typ.get_result())}'
    elif typ.kind == TypeKind.TYPEDEF:
        return typ.spelling
    elif typ.kind == TypeKind.INCOMPLETEARRAY:
        return f'{print_type(typ.element_type)}'
    else:
        raise AssertionError(f'Unhandled type kind {typ.kind} at {typ}')


if __name__ == '__main__':
    main()
