
import locale
import calendar
import sys
import os
import urllib.request
import urllib.parse

from contextlib import contextmanager
from typing import Dict


@contextmanager
def localized(code):
    old_code, old_encoding = locale.getlocale()
    locale.setlocale(locale.LC_ALL, code)
    yield
    locale.setlocale(locale.LC_ALL, f"{old_code}.{old_encoding}")


class ProgressBar:

    def __init__(self, total_count: int, status_tml: str, bar_length: int = 70):
        self.actual_count = 0
        self.total_count = total_count
        self.status_tml = status_tml
        self.bar_length = bar_length

    def __refresh__(self):
        filled_len = int(round(self.bar_length * self.actual_count / float(self.total_count)))

        percents = round(100.0 * self.actual_count / float(self.total_count), 1)
        bar = '=' * filled_len + '-' * (self.bar_length - filled_len)

        sys.stdout.write(f'[{bar}] {percents}% ...{self.status_tml}\r')
        sys.stdout.flush()

    def report_advance(self, advance_count: int):
        self.actual_count += advance_count
        self.__refresh__()

    def update_count(self, actual_count: int):
        self.actual_count = actual_count
        self.__refresh__()

    @staticmethod
    def up():
        sys.stdout.write('\x1b[1A')
        sys.stdout.flush()

    @staticmethod
    def down():
        sys.stdout.write('\n')
        sys.stdout.flush()

    @staticmethod
    def clear_line():
        sys.stdout.write("\033[K")
        sys.stdout.flush()

    def open(self):
        self.update_count(0)

    @staticmethod
    def pin_up():
        sys.stdout.write('\n')

    @staticmethod
    def close():
        sys.stdout.write('\n')


class DownloadProgressBar:
    def __init__(self, file_name):
        self.pbar = None
        self.fnme = file_name

    def __call__(self, block_num, block_size, total_size):
        if not self.pbar:
            self.pbar = ProgressBar(total_size, f"Downloading file: {self.fnme}")
            self.pbar.down()

        downloaded = block_num * block_size

        if downloaded > total_size:
            downloaded = total_size

        advance = downloaded - self.pbar.actual_count
        self.pbar.report_advance(advance)

        if downloaded == total_size:
            self.pbar.clear_line()
            self.pbar.up()


class MonthsProcessor:

    with localized("en_US.utf8"):
        months_abbr = list(calendar.month_abbr)

    with localized("en_US.utf8"):
        months_names = list(calendar.month_name)

    @classmethod
    def month_abbr_to_month_num_as_str(cls, month_abbr: str) -> str:
        splitted = month_abbr.split('-')
        if len(splitted) == 1:
            return f"{cls.months_abbr.index(splitted[0])}"
        if len(splitted) == 2:
            return f"{cls.months_abbr.index(splitted[0])}-{cls.months_abbr.index(splitted[1])}"

    @classmethod
    def target_in_next_year(cls, start_month: str, target_month: str) -> bool:
        target_fisrt_month = target_month.split('-')[0]
        return cls.months_abbr.index(start_month) > cls.months_abbr.index(target_fisrt_month)

    @classmethod
    def month_abbr_to_month_name(cls, month_abbr: str) -> str:
        splitted = month_abbr.split('-')
        if len(splitted) == 1:
            m1i = cls.months_abbr.index(splitted[0])
            return f"{cls.months_names[m1i]}"
        if len(splitted) == 2:
            m1i, m3i = cls.months_abbr.index(splitted[0]), cls.months_abbr.index(splitted[1])
            return f"{cls.months_names[m1i]}-{cls.months_names[m3i]}"


class FilesProcessor:

    @staticmethod
    def download_file(download_url: str, file_path: str):
        #
        download_url = urllib.parse.quote(download_url, safe=':/')
        # Create progress bar to track download
        pb = DownloadProgressBar(os.path.basename(file_path))
        # Download file
        f, h = urllib.request.urlretrieve(download_url, file_path, pb)
        # Check file size
        assert os.stat(file_path).st_size != 0


class ConfigProcessor:

    @staticmethod
    def nested_dict_values(d: Dict):
        for v in d.values():
            if isinstance(v, dict):
                yield from ConfigProcessor.nested_dict_values(v)
            else:
                yield v

    @staticmethod
    def count_iterations(d: Dict):
        return sum([len(v)/2 for v in ConfigProcessor.nested_dict_values(d) if isinstance(v, list)])
