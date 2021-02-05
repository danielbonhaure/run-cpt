
import locale
import calendar
import sys

from contextlib import contextmanager


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

    def report_advance(self, advance_count: int):
        self.actual_count += advance_count

        filled_len = int(round(self.bar_length * self.actual_count / float(self.total_count)))

        percents = round(100.0 * self.actual_count / float(self.total_count), 1)
        bar = '=' * filled_len + '-' * (self.bar_length - filled_len)

        sys.stdout.write('[%s] %s%s ...%s\r' % (bar, percents, '%', self.status_tml))
        sys.stdout.flush()

    @staticmethod
    def clear_line():
        sys.stdout.write("\033[K")

    @staticmethod
    def close():
        sys.stdout.write('\n')


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
