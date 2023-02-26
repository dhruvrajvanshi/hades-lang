import sys
from clang.cindex import Index, CursorKind, Cursor, Type, TypeKind
from abc import ABC, abstractmethod
from dataclasses import dataclass


class HadesType(ABC):
    @abstractmethod
    def pretty_print(self) -> str:
        ...


@dataclass
class HadesConst(ABC):
    type: HadesType
    @abstractmethod
    def pretty_print(self) -> str: ...


@dataclass
class HadesIntLiteral(HadesConst):
    type: HadesType
    value: int

    def pretty_print(self) -> str: ...


@dataclass
class NamedType(HadesType):
    name: str

    def pretty_print(self) -> str: ...


@dataclass
class HadesArrayType(HadesType):
    inner: HadesType
    length: int

    def pretty_print(self) -> str: ...


@dataclass
class HadesPointerType(HadesType):
    inner: HadesType

    def pretty_print(self) -> str: ...


@dataclass
class HadesMutPointerType(HadesType):
    inner: HadesType
    def pretty_print(self) -> str: ...


@dataclass
class HadesDefType(HadesType):
    arg_types: list[HadesType]
    return_type: HadesType

    def pretty_print(self) -> str: ...


class HadesDef(ABC):
    @abstractmethod
    def pretty_print(self) -> str:
        ...


@dataclass
class HadesStructDef(HadesDef):
    name: str
    fields: list[tuple[str, HadesType]]

    def pretty_print(self) -> str:
        ...


@dataclass
class HadesExternFunctionDef(HadesDef):
    name: str
    param_types: list[HadesType]
    return_type: HadesType
    extern_name: str

    def pretty_print(self) -> str:
        ...


@dataclass
class HadesExternConstDef(HadesDef):
    name: str
    type: HadesType

    def pretty_print(self) -> str: ...


@dataclass
class HadesConstDef(HadesDef):
    name: str
    type: HadesType
    value: HadesConst

    def pretty_print(self) -> str: ...


@dataclass
class HadesTypeAlias(HadesDef):
    name: str
    rhs: HadesType

    def pretty_print(self) -> str:
        raise Exception("Unimplemented")


class HadesBuilder:
    defs: list[HadesDef] = []

    _unique_names = 0

    def emitDef(self, d: HadesDef):
        self.defs.append(d)

    def unique_name(self) -> str:
        name = f'__hades_stub_generated_{self._unique_names}'
        self._unique_names += 1
        return name


def main():
    out = open("out.hds", "w")

    index = Index.create()
    [*in_files] = sys.argv[1:]

    out.write("import hades.ffi.c as c\n\n")
    for in_file in in_files:
        process_file(in_file, index)


def process_file(in_file: str, index: Index):
    print(f"Generating {in_file}")
    tu = index.parse(in_file, [])
    builder = HadesBuilder()

    def lowerType(ty: Type) -> HadesType:
        result: HadesType
        match ty.kind:
            case TypeKind.UCHAR:
                result = NamedType('u8')
            case TypeKind.CHAR_S:
                result = NamedType('i8')
            case TypeKind.SCHAR:
                result = NamedType('i8')
            case TypeKind.USHORT:
                result = NamedType('u16')
            case TypeKind.SHORT:
                result = NamedType('i16')
            case TypeKind.UINT:
                result = NamedType('u32')
            case TypeKind.INT:
                result = NamedType('i32')
            case TypeKind.ULONG:
                result = NamedType('u64')
            case TypeKind.ULONGLONG:
                result = NamedType('u64')
            case TypeKind.LONG:
                result = NamedType('i64')
            case TypeKind.DOUBLE:
                result = NamedType('i64')
            case TypeKind.VOID:
                result = NamedType('void')
            case TypeKind.TYPEDEF:
                result = NamedType(ty.spelling)
            case TypeKind.CONSTANTARRAY:
                result = HadesArrayType(
                    lowerType(ty.element_type), ty.element_count)
            case TypeKind.POINTER if ty.get_pointee().kind == TypeKind.FUNCTIONPROTO:
                fn_ty = ty.get_pointee()
                result = HadesDefType(
                    arg_types=[lowerType(arg)
                               for arg in fn_ty.argument_types()],
                    return_type=lowerType(fn_ty.get_result())
                )
            case TypeKind.POINTER:
                pointee = lowerType(ty.get_pointee())
                is_const = ty.is_const_qualified()
                if is_const:
                    result = HadesPointerType(pointee)
                else:
                    result = HadesMutPointerType(pointee)
            case TypeKind.ELABORATED | TypeKind.RECORD:
                decl = ty.get_declaration()
                match decl.kind:
                    case CursorKind.STRUCT_DECL:
                        name = visitStructDecl(decl)
                        result = NamedType(name)
                    case CursorKind.ENUM_DECL:
                        name = visitEnumDecl(decl)
                        result = NamedType(name)
                    case _: raise Exception(f'Unhandled elaborated type: {decl.kind}')
            case _:
                raise Exception(f'Unhandled type: {ty.kind}')

        return result

    def visitTypedef(node: Cursor):
        builder.emitDef(HadesTypeAlias(node.displayname,
                        lowerType(node.underlying_typedef_type)))

    def visitStructDecl(node: Cursor) -> str:
        name = node.displayname
        if name == '':
            name = builder.unique_name()
        fields: list[tuple[str, HadesType]] = []
        for field in node.get_children():
            assert field.kind == CursorKind.FIELD_DECL or field.kind == CursorKind.STRUCT_DECL
            name = field.spelling
            assert name is not None
            assert name != ''
            type = lowerType(field.type)
            fields.append((name, type))

        builder.emitDef(HadesStructDef(
            name=name,
            fields=fields
        ))
        return name

    def visitFunctionDecl(node: Cursor):
        param_types: list[HadesType] = []
        for arg in node.get_arguments():
            assert arg.kind == CursorKind.PARM_DECL
            param_types.append(lowerType(arg.type))
        builder.emitDef(
            HadesExternFunctionDef(
                name=node.mangled_name,
                param_types=param_types,
                return_type=lowerType(node.result_type),
                extern_name=node.mangled_name
            )
        )

    def visitVarDecl(node: Cursor):
        builder.emitDef(
            HadesExternConstDef(
                node.mangled_name,
                lowerType(node.type)
            )
        )

    def visitEnumDecl(node: Cursor) -> str:
        name = node.displayname
        if name == '':
            name = builder.unique_name()

        if node.enum_type.kind == TypeKind.INVALID:
            type = NamedType('i32')
        else:
            type = lowerType(node.enum_type)
        builder.emitDef(
            HadesTypeAlias(
                name,
                type
            )
        )

        for member in node.get_children():
            assert member.kind == CursorKind.ENUM_CONSTANT_DECL
            assert isinstance(member.enum_value, int)
            builder.emitDef(
                HadesConstDef(
                    member.mangled_name,
                    type,
                    HadesIntLiteral(
                        type,
                        member.enum_value
                    )
                )
            )

        return name
        # builder.emitDef(
        #     HadesConstDef(
        #         name=node.mangled_name
        #     )
        # )

    for _node in tu.cursor.get_children():
        node: Cursor = _node
        match node.kind:
            case CursorKind.TYPEDEF_DECL:
                visitTypedef(node)
            case CursorKind.STRUCT_DECL:
                visitStructDecl(node)
            case CursorKind.FUNCTION_DECL:
                visitFunctionDecl(node)
            case CursorKind.VAR_DECL:
                visitVarDecl(node)
            case CursorKind.ENUM_DECL:
                visitEnumDecl(node)
            case kind:
                raise Exception(f"Unhandled node kind: {kind}")


if __name__ == "__main__":
    main()
