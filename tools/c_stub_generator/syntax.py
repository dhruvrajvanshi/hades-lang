from abc import ABC, abstractmethod
from dataclasses import dataclass


class HadesType(ABC):
    @abstractmethod
    def pretty_print(self) -> str:
        raise Exception(f'Unimplemented for {self.__class__}')


@dataclass
class HadesConst(ABC):
    type: HadesType

    @abstractmethod
    def pretty_print(self) -> str:
        raise Exception(f'Unimplemented for {self.__class__}')


@dataclass
class HadesIntLiteral(HadesConst):
    type: HadesType
    value: int

    def pretty_print(self) -> str:
        return str(self.value)


@dataclass
class NamedType(HadesType):
    name: str

    def __init__(self, name: str) -> None:
        assert name != ''
        assert ' ' not in name
        self.name = name

    def pretty_print(self) -> str:
        return self.name


@dataclass
class HadesArrayType(HadesType):
    inner: HadesType
    length: int

    def pretty_print(self) -> str:
        return f'array[{self.inner.pretty_print()}, {self.length}]'


@dataclass
class HadesPointerType(HadesType):
    inner: HadesType

    def pretty_print(self) -> str:
        return f'*{self.inner.pretty_print()}'


@dataclass
class HadesMutPointerType(HadesType):
    inner: HadesType

    def pretty_print(self) -> str:
        return f'*mut {self.inner.pretty_print()}'


@dataclass
class HadesDefType(HadesType):
    arg_types: list[HadesType]
    return_type: HadesType

    def pretty_print(self) -> str:
        params = ', '.join(it.pretty_print() for it in self.arg_types)
        return f'def({params}) -> {self.return_type.pretty_print()}'


class HadesDef(ABC):
    @abstractmethod
    def pretty_print(self) -> str:
        raise Exception(f'Unimplemented for {self.__class__}')


@dataclass
class HadesStructDef(HadesDef):
    name: str
    fields: list[tuple[str, HadesType]]

    def pretty_print(self) -> str:
        fields = '\n'.join(
            f'  val {name}: {ty.pretty_print()}' for name, ty in self.fields)
        return f'struct {self.name} {{\n{fields}\n}}'


@dataclass
class HadesExternFunctionDef(HadesDef):
    name: str
    param_types: list[HadesType]
    return_type: HadesType
    extern_name: str

    def pretty_print(self) -> str:
        params = ', '.join(it.pretty_print() for it in self.param_types)
        return f'extern def {self.name}({params}): {self.return_type.pretty_print()} = {self.extern_name}'


@dataclass
class HadesExternConstDef(HadesDef):
    name: str
    type: HadesType

    def pretty_print(self) -> str:
        return f'extern const {self.name}: {self.type.pretty_print()} = {self.name}'


@dataclass
class HadesConstDef(HadesDef):
    name: str
    type: HadesType
    value: HadesConst

    def pretty_print(self) -> str:
        return f'const {self.name}: {self.type.pretty_print()} = {self.value.pretty_print()}'


@dataclass
class HadesTypeAlias(HadesDef):
    name: str
    rhs: HadesType

    def pretty_print(self) -> str:
        return f'type {self.name} = {self.rhs.pretty_print()}'
