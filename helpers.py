
from singleton import Singleton
from errors import ConfigError

import locale
import calendar
import sys
import os
import urllib.request
import urllib.parse
import pandas as pd
import yaml

from contextlib import contextmanager
from typing import Dict, List, Any


@contextmanager
def localized(code):
    old_code, old_encoding = locale.getlocale()
    locale.setlocale(locale.LC_ALL, code)
    yield
    locale.setlocale(locale.LC_ALL, f"{old_code}.{old_encoding}")


class ProgressBar:

    def __init__(self, total_count: int, status_tml: str, bar_length: int = 70):
        self.actual_count: float = 0
        self.total_count: int = total_count
        self.status_tml: str = status_tml
        self.bar_length: int = bar_length

    def __refresh__(self):
        filled_len = int(round(self.bar_length * self.actual_count / float(self.total_count)))

        percents = round(100.0 * self.actual_count / float(self.total_count), 1)
        bar = '=' * filled_len + '-' * (self.bar_length - filled_len)

        sys.stdout.write(f'[{bar}] {percents}% ...{self.status_tml}\r')
        sys.stdout.flush()

    def report_advance(self, advance_count: float):
        self.actual_count += advance_count
        self.__refresh__()

    def update_count(self, actual_count: float):
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
    def __init__(self, file_name, bar_length: int = 70):
        self.pbar = None
        self.fnme = file_name
        self.blen = bar_length

    def __call__(self, block_num, block_size, total_size):
        if not self.pbar:
            self.pbar = ProgressBar(total_size, f"Downloading file: {self.fnme}", self.blen)
            self.pbar.down()

        downloaded = block_num * block_size

        if downloaded > total_size:
            downloaded = total_size

        advance = downloaded - self.pbar.actual_count
        self.pbar.report_advance(advance)

        if downloaded == total_size:
            self.pbar.clear_line()
            self.pbar.up()


class SecondaryProgressBar(ProgressBar):

    def open(self):
        self.down()
        super().open()

    def close(self):
        self.clear_line()
        self.up()


class MonthsProcessor:

    with localized("en_US.utf8"):
        months_abbr = list(calendar.month_abbr)

    with localized("en_US.utf8"):
        months_names = list(calendar.month_name)

    @classmethod
    def month_abbr_to_int(cls, month: str) -> int:
        return cls.months_abbr.index(month)

    @classmethod
    def month_int_to_abbr(cls, month: int) -> str:
        return cls.months_abbr[month]

    @classmethod
    def month_name_to_int(cls, month: str) -> int:
        return cls.months_names.index(month)

    @classmethod
    def month_int_to_name(cls, month: int) -> str:
        return cls.months_names[month]

    @classmethod
    def trgt_month_in_next_year(cls, start_month: str, target_month: str) -> bool:
        return cls.month_abbr_to_int(start_month) > cls.month_abbr_to_int(target_month)

    @classmethod
    def month_abbr_to_months_range(cls, month_abbr: str) -> List[int]:
        splitted = month_abbr.split('-')
        if len(splitted) == 1:
            return [cls.month_abbr_to_int(splitted[0])]
        if len(splitted) == 2:
            return list(range(cls.month_abbr_to_int(splitted[0]), cls.month_abbr_to_int(splitted[1])+1))

    @classmethod
    def month_abbr_to_month_num_as_str(cls, month_abbr: str) -> str:
        splitted = month_abbr.split('-')
        if len(splitted) == 1:
            return f"{cls.month_abbr_to_int(splitted[0])}"
        if len(splitted) == 2:
            return f"{cls.month_abbr_to_int(splitted[0])}-{cls.month_abbr_to_int(splitted[1])}"

    @classmethod
    def month_abbr_to_month_name(cls, month_abbr: str) -> str:
        splitted = month_abbr.split('-')
        if len(splitted) == 1:
            m1i = cls.month_abbr_to_int(splitted[0])
            return f"{cls.month_int_to_name(m1i)}"
        if len(splitted) == 2:
            m1i, m3i = cls.month_abbr_to_int(splitted[0]), cls.month_abbr_to_int(splitted[1])
            return f"{cls.month_int_to_name(m1i)}-{cls.month_int_to_name(m3i)}"


class YearsProcessor:

    @classmethod
    def trng_period_to_years_str(cls, trng_period, trgt_season) -> str:
        years_to_str = f"{trng_period.tini}"
        if trgt_season.type == 'seasonal':
            years_to_str += f"-{trng_period.tend}"
        return years_to_str

    @classmethod
    def fcst_data_to_years_str(cls, fcst_data, trgt_season, fcst_year: int) -> str:
        trgt_years = fcst_data.trgt_years_by_month(trgt_season)
        years_to_str = f"{trgt_years.get(fcst_year).pop(0)}"
        if trgt_season.type == 'seasonal':
            years_to_str += f"-{trgt_years.get(fcst_year).pop(-1)}"
        return years_to_str

    @classmethod
    def years_for_month_range(cls, year: int, trgt_season) -> List[int]:
        years = list()
        for month in trgt_season.trgt_months_range:
            years.append(year+1 if trgt_season.start_month_int > month else year)
        return years


class FilesProcessor:

    @classmethod
    def download_file(cls, download_url: str, file_path: str):
        #
        download_url = urllib.parse.quote(download_url, safe=':/')
        # Create progress bar to track download
        pb = DownloadProgressBar(os.path.basename(file_path))
        # Download file
        f, h = urllib.request.urlretrieve(download_url, file_path, pb)
        # Check file size
        assert os.stat(file_path).st_size != 0


class ConfigProcessor:

    @classmethod
    def nested_dict_values(cls, d: Dict):
        for v in d.values():
            if isinstance(v, dict):
                yield from ConfigProcessor.nested_dict_values(v)
            else:
                yield v

    @classmethod
    def count_iterations(cls, d: Dict):
        return round(sum([len(v)/3 for v in ConfigProcessor.nested_dict_values(d) if isinstance(v, list)]))


class CPTFileProcessor:

    def __init__(self, longitudes: List[float], latitudes: List[float]):
        self.longitudes = longitudes
        self.latitudes = latitudes

    def rename_dataframe_columns_to_cpt_format(self, df_to_rename: pd.DataFrame) -> pd.DataFrame:
        df_output = pd.DataFrame()
        for lat, lon in zip(self.latitudes, self.longitudes):
            df_est = df_to_rename.loc[lat, lon]
            df_est.rename(lambda x: f"E({lon}_{lat})", axis='columns', inplace=True)
            df_output = pd.concat([df_output, df_est], axis=1)
        return df_output

    def dataframe_to_cpt_format_file(self, filename: str, df_to_save: pd.DataFrame):
        # Rename columns to CPT file column format
        df_to_save = self.rename_dataframe_columns_to_cpt_format(df_to_save)

        # Save dataframe
        with open(filename, 'w') as f:
            f.write("Stn\t" + "\t".join(df_to_save.columns.values.tolist()) + "\n")
            f.write("Lat\t" + "\t".join(map(str, self.latitudes)) + "\n")
            f.write("Lon\t" + "\t".join(map(str, self.longitudes)) + "\n")
        df_to_save.to_csv(filename, sep='\t', mode='a', header=False)


@Singleton
class ConfigFile:

    def __init__(self):
        self._file_name: str = 'config.yaml'
        self.cpt_config: dict = self.__load_config()

    def __load_config(self) -> dict:
        if not os.path.exists(self._file_name):
            raise ConfigError(f"Configuration file (i.e. {self._file_name}) not found!")
        with open(self._file_name, 'r') as f:
            return yaml.safe_load(f)

    @property
    def file_name(self):
        return self._file_name

    @file_name.setter
    def file_name(self, value):
        self._file_name = value
        self.cpt_config = self.__load_config()

    def get(self, key, default=None) -> Any:
        return self.cpt_config.get(key, default)


@Singleton
class UpdateControl:

    def __init__(self):
        self._config_file: str = 'config.yaml'
        self._updated_files: set = set()
        self._update_config: dict = dict()
        self.__load_global_update_config()

    def __load_global_update_config(self):
        if not os.path.exists(self._config_file):
            raise ConfigError(f"Configuration file (i.e. {self._config_file}) not found!")
        with open(self._config_file, 'r') as f:
            self._update_config = yaml.safe_load(f).get('update', {})

    @property
    def config_file(self):
        return self._config_file

    @config_file.setter
    def config_file(self, value):
        self._config_file = value
        self.__load_global_update_config()

    def must_be_updated(self, k1: str, k2: str, file_abs_path: str) -> bool:
        # k1 can be "predictors" or "predictands"
        # k2 can be "raw_data", "intermediate_data" or "cpt_input_data"
        if self.file_already_updated(file_abs_path):
            return False
        return self._update_config.get(k1, {}).get(k2, False)

    def report_updated_file(self, file_abs_path: str):
        self._updated_files.add(file_abs_path)

    def file_already_updated(self, file_abs_path: str) -> bool:
        return file_abs_path in self._updated_files
