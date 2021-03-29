
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
import re
import shutil

from contextlib import contextmanager
from typing import Dict, List, Any
from itertools import chain
from collections import namedtuple


@contextmanager
def localized(code):
    old_code, old_encoding = locale.getlocale()
    locale.setlocale(locale.LC_ALL, code)
    yield
    locale.setlocale(locale.LC_ALL, f"{old_code}.{old_encoding}")


def crange(start, stop, modulo):
    if start > stop:
        return chain(range(start, modulo+1), range(1, stop))
    else:
        return range(start, stop)


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
            return [*crange(cls.month_abbr_to_int(splitted[0]), cls.month_abbr_to_int(splitted[1])+1, 12)]

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

    @classmethod
    def cpt_noaa_monthly_file_to_dataframe(cls, file_name: str):

        Info = namedtuple('Info', ['year', 'month', 'field_line', 'last_line'])

        # Open monthly file, to inspect it
        df_info = list()
        with open(file_name) as fp:
            for cnt, line in enumerate(fp):
                date_regex, nrow_regex = re.search('cpt:T=(\d+)-(\d+),', line), re.search('cpt:nrow=(\d+),', line)
                if date_regex and nrow_regex:
                    year, month = int(date_regex.group(1)), int(date_regex.group(2))
                    field_line, last_line = cnt + 1, int(nrow_regex.group(1))
                    df_info.append(Info(year, month, field_line, last_line))

        # Gen dataframe for current file accessing only rows with data
        final_df = pd.DataFrame()
        for info in df_info:
            df = pd.read_csv(file_name, sep='\t', index_col=0, skiprows=info.field_line,
                             nrows=info.last_line, na_values='-999')
            df.insert(0, 'month', info.month)
            df.insert(0, 'year', info.year)
            df.set_index(['year', 'month'], append=True, inplace=True)
            final_df = pd.concat([final_df, df])

        # Return generated dataframe
        return final_df

    @classmethod
    def cpt_file_is_a_locally_created_one(cls, file_name: str) -> bool:
        """Check if file exists and it's a locally created file"""
        if os.path.exists(file_name):
            with open(file_name, 'r') as fp:
                if 'locally_created_file' in fp.readline():
                    return True
        return False

    @classmethod
    def dataframe_to_cpt_noaa_seasonal_hindcast_file(cls, file_name: str, file_data: dict, df: pd.DataFrame,
                                                     debug: bool = False):
        # Set correct file_name according to debug status
        file_name = file_name.replace('.txt', '_debug.txt') if debug else file_name

        # Order df by year
        df = df.sort_index(level=['year', 0], ascending=[True, False])

        # Backup file if there was previously downloaded instead of locally created
        if not cls.cpt_file_is_a_locally_created_one(file_name):
            _ = shutil.copy(file_name, file_name.replace('.txt', '_downloaded_from_noaa.txt'))

        # Create file, open it and add header (2 firsts lines)
        with open(file_name, "w") as fp:
            fp.write('xmlns:cpt=http://iri.columbia.edu/CPT/v10/locally_created_file\n')
            fp.write('cpt:nfields=1\n')

        # Iter df by year and save data to the exit file
        for year in range(file_data['training_original_first_year'], file_data['training_original_last_year']+1):

            # Define target years
            first_target_month_year = year+1 if file_data["first_target_month_in_next_year"] else year
            last_target_month_year = year+1 if file_data["last_target_month_in_next_year"] else year

            # Define cpt inputs
            cpt_field = 'precip' if file_data['seasonal_file_variable'] == 'precip' else '2m Air Temperature'
            cpt_t = f'{first_target_month_year}-{file_data["first_target_month"]:02d}/'
            cpt_t += f'{last_target_month_year}-' if first_target_month_year != last_target_month_year else ''
            cpt_t += f'{file_data["last_target_month"]:02d}'
            cpt_s = f'{year}-{file_data["forecast_init_month"]:02d}-01'
            cpt_units = 'millimeters per day' if file_data['seasonal_file_variable'] == 'precip' else 'Degrees Celcius'

            # Define line that describe year
            y_line = f'cpt:field={cpt_field}, cpt:T={cpt_t}, cpt:S={cpt_s}, cpt:nrow=181, cpt:ncol=360, ' \
                     f'cpt:row=Y, cpt:col=X, cpt:units={cpt_units}, cpt:missing=-999.00000000000000{" "*15}'

            # Add line that describe the year to save
            with open(file_name, "a") as fp:
                fp.write(f'{y_line}\n')

            # Filter data to save
            df_y = df.loc[df.index.get_level_values('year') == year].droplevel(level='year')

            # Add df_y to the exit file
            df_y.sort_index(ascending=False).to_csv(file_name, sep='\t', na_rep='-999.000000', mode='a')

    @classmethod
    def dataframe_to_cpt_noaa_seasonal_forecast_file(cls, file_name: str, file_data: dict, df: pd.DataFrame,
                                                     debug: bool = False):
        # Set correct file_name according to debug status
        file_name = file_name.replace('.txt', '_debug.txt') if debug else file_name

        # Create file, open it and add header (2 firsts lines)
        with open(file_name, "w") as fp:
            fp.write('xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n')
            fp.write('cpt:nfields=1\n')

        # Get forecast year
        year = file_data["forecast_year"]

        # Define target years
        first_target_month_year = year+1 if file_data["first_target_month_in_next_year"] else year
        last_target_month_year = year+1 if file_data["last_target_month_in_next_year"] else year

        # Define cpt inputs
        cpt_field = 'precip' if file_data['seasonal_file_variable'] == 'precip' else '2m Air Temperature'
        cpt_t = f'{first_target_month_year}-{file_data["first_target_month"]:02d}/'
        cpt_t += f'{last_target_month_year}-' if first_target_month_year != last_target_month_year else ''
        cpt_t += f'{file_data["last_target_month"]:02d}'
        cpt_s = f'{year}-{file_data["forecast_init_month"]:02d}-01'
        cpt_units = 'millimeters per day' if file_data['seasonal_file_variable'] == 'precip' else 'Degrees Celcius'

        # Define line that describe year
        y_line = f'cpt:field={cpt_field}, cpt:T={cpt_t}, cpt:S={cpt_s}, cpt:nrow=181, cpt:ncol=360, ' \
                 f'cpt:row=Y, cpt:col=X, cpt:units={cpt_units}, cpt:missing=-999.00000000000000{" "*15}'

        # Add line that describe the year to save
        with open(file_name, "a") as fp:
            fp.write(f'{y_line}\n')

        # Add df_y to the exit file
        df.sort_index(ascending=False).to_csv(file_name, sep='\t', na_rep='-999.000000', mode='a')


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
        # k2 can be "raw_data", ... , "cpt_input_data"
        if self.file_already_updated(file_abs_path):
            return False
        return self._update_config.get(k1, {}).get(k2, False)

    def report_updated_file(self, file_abs_path: str):
        self._updated_files.add(file_abs_path)

    def file_already_updated(self, file_abs_path: str) -> bool:
        return file_abs_path in self._updated_files
