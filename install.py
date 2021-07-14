import argparse
import os
import pathlib
import subprocess
import tarfile
import time
import urllib.request as req
import urllib.parse
import logging
import threading
import shutil
import sys

logging.basicConfig(level=logging.INFO, format='%(message)s')
log = logging.getLogger()


def main():
    args = parse_args(sys.argv[1:])

    version = check_not_none(args.version)
    install_path = check_not_none(pathlib.Path(args.install_path))

    env_file_path = install_path.joinpath('env')

    if install_path.exists() and not install_path.is_dir():
        user_error(f'Installation path {install_path} already exists and is not a directory')

    if not install_path.exists():
        install_path.mkdir(parents=True)

    tmp_directory = install_path.joinpath('tmp')

    if not tmp_directory.exists():
        tmp_directory.mkdir()

    tar_path = tmp_directory.joinpath(f'hades-{version}.tar')

    download_file(download_link(version), tar_path)

    this_version_directory_parent = install_path.joinpath('versions', version)
    if this_version_directory_parent.exists():
        shutil.rmtree(this_version_directory_parent)

    this_version_directory_parent.mkdir(parents=True)

    with tarfile.open(tar_path) as f:
        log.info(f'Extracting to {this_version_directory_parent}')
        f.extractall(path=this_version_directory_parent)

    this_version_directory = this_version_directory_parent.joinpath('hades')

    shutil.rmtree(tmp_directory)

    if env_file_path.exists():
        if env_file_path.is_dir():
            shutil.rmtree(env_file_path)
        else:
            env_file_path.unlink()

    with open(env_file_path, mode='w', encoding='utf-8') as env_file:
        env_file.write(env_file_text(this_version_directory))

    update_shell_profile('.zshrc', env_file_path)
    update_shell_profile('.bashrc', env_file_path)
    update_shell_profile('.bash_profile', env_file_path)
    update_shell_profile('.fishrc', env_file_path)

    log.info('Restart this terminal to complete the installation.')

    log.info('To configure shells other than bash, zsh or fish, add this to your config file')
    log.info(f'source {env_file_path}')

    if os.name == 'nt':

        path_backup_file = install_path.joinpath(f'PATH.{time.time()}.backup.')
        log.info(f"Changing PATH variable, old path is backed up at: {path_backup_file}")
        with open(path_backup_file, mode='w') as f:
            f.write('\n')
            f.write(os.environ['PATH'])

        new_path_list = [path for path in os.environ['PATH'].split(';') if not is_hades_home_path(pathlib.Path(path))]
        new_path_list.insert(0, str(path_to_hades_exe(this_version_directory)))

        setx('PATH', ';'.join(new_path_list))
        setx('HADES_HOME', str(hades_home(this_version_directory)))


def hades_home(this_version_directory: pathlib.Path):
    return this_version_directory


def path_to_hades_exe(this_version_directory: pathlib.Path):
    return this_version_directory.joinpath('bin')


def setx(key: str, value: str):
    subprocess.run(['setx', key, value], capture_output=True)


def is_hades_home_path(path: pathlib.Path):
    return path.joinpath('bin', 'hades').exists()


def update_shell_profile(filename, env_file_path: pathlib.Path):
    home_path = get_home_path()
    if home_path is None:
        return

    shell_profile_path = pathlib.Path(home_path, filename)
    if not shell_profile_path.exists():
        return

    if shell_profile_path.is_dir():
        return

    text = f"source {env_file_path} # HADES_ENV"
    if text in shell_profile_path.read_text().split('\n'):
        log.info(f'{filename} already contains hades env configuration, skipping.')
        return

    log.info(f'Updating {shell_profile_path}')
    with open(shell_profile_path, mode='a') as file:
        file.write('\n')
        file.write(text)
        file.write('\n')
        file.write('\n')


def get_home_path():
    if 'HOME' in os.environ:
        return pathlib.Path(os.environ['HOME'])
    else:
        return None


def env_file_text(this_version_path: pathlib.Path):
    return f"""
export PATH={path_to_hades_exe(this_version_path)}:$PATH
export HADES_HOME={hades_home(this_version_path)}/
"""


class Ref(object):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)


def download_file(url, destination_path: pathlib.Path):
    log.info(f'Downloading {url} to {destination_path}')

    progress = Ref(0)
    megabytes = Ref(0)
    done = Ref(False)

    def do():
        threading.Thread(target=report_progress).start()
        try:
            req.urlretrieve(url, destination_path, reporthook=reporthook)
        finally:
            done.value = True

    def reporthook(block_number: int, read_size: int, total_size: int):
        # print(f'{block_number}, {read_size}, {total_size}')
        megabytes.value = round(total_size / 1024 / 1024, 2)
        progress.value = round(((block_number * read_size) / total_size) * 100, 2)

    def report_progress():
        if megabytes.value == 0:
            time.sleep(0.2)
        while not done.value:

            log.info(
                f'Downloaded {progress}% of '
                f'{megabytes} MB')
            time.sleep(1)

    do()


def download_link(version: str):
    version_part = urllib.parse.quote(version, safe='')
    return f'https://github.com/dhruvrajvanshi/hades-lang/releases/download/{version_part}/hades.tar'


def user_error(message: str):
    log.error(f'Error: {message}')
    log.error(f'Exiting with code 1')
    sys.exit(1)


def check_not_none(value):
    assert value is not None, "Unexpected None value"
    return value


def check(condition, message='Assertion failed'):
    assert condition, message


def parse_args(args):
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '--version',
        dest='version',
        type=str,
        required=False,
        default='latest',
        help='Release version number. '
             'Check https://github.com/dhruvrajvanshi/hades-lang/releases '
             'for release names (defaults to latest)'
    )
    parser.add_argument(
        '--install-path',
        dest='install_path',
        type=str,
        required=False,
        default=os.environ['APPDATA'] + "\\hades" if os.name == 'nt' else f'{find_home_directory()}/.hades',
        help='Directory to use as a base for installing different versions. Defaults to ~/.hades'
    )

    return parser.parse_args(args)


def find_home_directory():
    return os.environ['HOME'] if 'HOME' in os.environ else os.environ['USERPROFILE']


if __name__ == "__main__":
    main()
