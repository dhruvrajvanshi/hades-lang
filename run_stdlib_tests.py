#!env python
import glob
import os
import subprocess as subp
import sys
import xml.etree.ElementTree as ET

def main():
    error_set = set()
    memcheck_errors = {}
    for file in glob.glob('./stdlib/hadesx/**/*_test.hds'):
        output = f'test_build/{file}'
        hades_path = f'./hadesboot/build/install/hades/bin/hades'
        completed_process = subp.run(
            [hades_path, 'build', '-g', '--main', file, '--output', output, '--emit-ide-metadata'],
            stdout=sys.stdout,
            stderr=sys.stderr,
            env={**os.environ, 'HADES_HOME': '.'}
        )
        if completed_process.returncode != 0:
            error_set.add(file)
            continue

        has_valgrind = subp.run(['valgrind', '--version']).returncode == 0

        completed_process = subp.run(
            [
                'valgrind',
                *(
                    [] if 'VALGRIND_SUMMARY' in os.environ
                    else [
                        '--xml=yes',
                        f'--xml-file={output}.memcheck',
                    ]
                ),
                '--leak-check=full',
                output,
            ] if has_valgrind else [output],
            stdout=sys.stdout,
            stderr=sys.stderr,
        )

        if has_valgrind:
            memcheck_file = f'{output}.memcheck'
            etree = ET.parse(memcheck_file)
            errors = []
            for child in etree.getroot():

                if child.tag != 'error':
                    continue
                errors.append(child)
            if len(errors) > 0:
                error_set.add(file)
            memcheck_errors[file] = errors
                

        

    if len(error_set) == 0:
        return
    
    print('The following test cases failed:')
    for error in error_set:
        print('\t' + error)

    exit(1)

if __name__ == '__main__':
    main()