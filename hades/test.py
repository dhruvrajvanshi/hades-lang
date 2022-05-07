import subprocess
from pathlib import Path
import os
import sys
from typing import List, Optional

HADES_HOME=Path('..').joinpath('hadesboot', 'build', 'install', 'hades')

def main():
    test_sources = collect_test_sources()
    
    for test_source in test_sources:
        print(f'Compiling {test_source}')
        output = compile_hades_source(test_source, output=str(Path('build').joinpath(test_source)))
        
        if output == None:
            print(f'Could not compile {test_source}')
            continue
            
        subprocess.call(
            output,
            stderr=sys.stderr,
            stdout=sys.stdout,
        )
    

def compile_hades_source(test_source:str, output:str) -> Optional[str]:
    hades = str(HADES_HOME.joinpath('bin', 'hades.bat' if os.name == 'nt' else 'hades'))
    command = [
        hades,
            '--main', test_source,
            '--output', output,
            '--module-path', 'src'
    ]
    print(' '.join(command))
    status = subprocess.call(
        command,
        stdout=sys.stdout,
        stderr=sys.stderr,
    )
    if status != 0:
        return None
    return output
    

def collect_test_sources() -> List[str]:
    test_sources = []
    for entry in os.walk(Path('test')):
        dir, _, files = entry
        for file in files:
            _, extension = os.path.splitext(file)
            if extension == '.hds':
                test_sources.append(os.path.join(dir, file))
    return test_sources
    

if __name__ == '__main__':
    main()

