import tarfile
from tarfile import TarFile
import os
from pathlib import Path
import shutil
import subprocess
import sys
import io


def main():
    smoke_test_dir = Path(".distribution_smoke_test")
    if smoke_test_dir.exists():
        shutil.rmtree(smoke_test_dir)

    os.mkdir(smoke_test_dir)

    tar_path = Path(sys.argv[1])
    with tarfile.open(tar_path) as t:
        f: TarFile = t
        f.extractall(smoke_test_dir)

    dir_name, _ = tar_path.name.split('.')

    print(dir_name)

    hades_bin = str(smoke_test_dir.joinpath(dir_name, "bin", "hades"))
    ps = subprocess.run(
        [hades_bin, 'build', '--main', 'test/hello_world.hds', '--output', f'{smoke_test_dir}/hello_world' ],
        stdout=sys.stdout,
        stderr=sys.stderr,
        shell=True,
        env={**os.environ, "HADES_HOME": str(smoke_test_dir.joinpath(dir_name))},
    )
    ps.check_returncode()

    output = io.StringIO()
    ps = subprocess.run([f'{smoke_test_dir}/hello_world'])
    ps.check_returncode()


if __name__ == "__main__":
    main()
