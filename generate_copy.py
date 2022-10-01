

def impl(ty):
    append(f"""
implementation Copy[{ty}] {{
    def copy(to: *mut {ty}, from: *{ty}): Void {{
        M.copy(to, from, 1)
    }}
}}
"""
    )

def append(text: str):
    with open(f'./stdlib/hades/copy.hds', 'a') as f:
        f.write(text)


def trait(op_method):
    with open(f'./stdlib/hades/copy.hds', 'w') as f:
        f.write(f""" // Autogenerated file. Do not edit.
import hades.memory as M

trait Copy[Self] {{
    
    def copy(to: *mut Self, from: *Self): Void
}}

""")


integral_types = ['u8', 'i8', 'u16', 'i16', 'u32', 'i32', 'u64', 'i64', 'usize', 'isize']


def main():

    for ty in integral_types:
        impl(ty)

    impl('Bool')

    append("""

implementation[T] for *T {
    def copy(to: *mut *T, from: * *T): Void {
        M.copy(to, from, 1)
    }
}

""")

if __name__ == '__main__':
    main()
