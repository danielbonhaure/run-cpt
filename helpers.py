
import os
import locale
import calendar
import sys
import math
import urllib.request
import urllib.parse
import shutil

from typing import List, Dict, Union, Any
from contextlib import contextmanager
from string import Template


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


class FilesProcessor:

    def __init__(self, file_name: str, file_type: str, url_tmplt: str, dir_paths: Dict[str, Any]):
        self.file_name: str = file_name
        self.file_type: str = file_type  # predictor (x) or predictand (y)
        self.url_tmplt: Template = Template(url_tmplt)
        self.dir_paths: Dict[str, Any] = dir_paths

        self.predictor_keys: List[str] = ["model", "iri_var", "iri_month", "fcst_months", "trng_years", "fcst_years"]
        self.predictand_keys: List[str] = ["variable", "fcst_months", "fcst_years"]

        self.__post_init__()

    def __post_init__(self):
        data_keys: List[str] = self.predictor_keys if self.file_type == "predictor" else self.predictand_keys
        self.file_data = dict(zip(data_keys, self.file_name.replace(".txt", "").split("_")))

        fcst_years_dict: Dict[str, int] = dict(
            zip(['first_year', 'last_year'], map(int, self.file_data.get("fcst_years").split('-')))
        )  # si fcst_year no tiene '-' entonces fcst_years_dict.get('last_year') devuelve None y puedo usar default
        self.file_data.update(
            {"fcst_years_dict": fcst_years_dict}
        )

        fcst_months_dict: Dict[str, int] = dict(
            zip(['first_month', 'last_month'], map(int, self.file_data.get("fcst_months").split('-')))
        )  # si fcst_months no tiene '-' entonces fcst_months_dict.get('last_month') devuelve None y puedo usar default
        self.file_data.update(
            {"fcst_months_dict": fcst_months_dict}
        )

        self.file_data.update(
            {"fcst_type": "monthly" if self.file_data.get("fcst_months").split("-") == 1 else "seasonal"}
        )

        if self.file_type == "predictor":
            self.url_tmplt = Template(self.url_tmplt.safe_substitute(fcst_type=self.file_data.get('fcst_type')))

    def __gen_predictor_files_names(self) -> Dict[str, Union[str, List[str]]]:
        hcst_file_name: str
        fcst_files_name: List[str] = list()
        comb_file_name: str

        var_str = self.file_data.get('iri_var')
        fcst_years_str = self.file_data.get("fcst_years")
        train_years_str = self.file_data.get("train_years")

        hcst_file_name = self.file_name\
            .replace(var_str, f"{var_str}_hcst")\
            .replace(f"_{fcst_years_str}", "")

        first_fcst_year = self.file_data.get("fcst_years_dict").get("first_year")
        last_fcst_year = self.file_data.get("fcst_years_dict").get("last_year")

        if not last_fcst_year:
            fcst_files_name.append(
                self.file_name
                    .replace(var_str, f"{var_str}_fcst")
                    .replace(f"_{train_years_str}", "")
            )
        else:
            for year in range(first_fcst_year, last_fcst_year+1):
                first_fcst_month = self.file_data.get("fcst_months_dict").get("first_month")
                last_fcst_month = self.file_data.get("fcst_months_dict").get("first_month", math.inf)
                year_of_months = f"{year}-{year+1 if first_fcst_month > last_fcst_month else year}"
                fcst_files_name.append(
                    self.file_name
                        .replace(var_str, f"{var_str}_fcst")
                        .replace(f"_{train_years_str}", "")
                        .replace(f"_{fcst_years_str}", f"_{year_of_months}")
                )

        # TODO: definir el nombre del archivo combinado
        comb_file_name = ""

        return {"hcst_file_name": hcst_file_name, "fcst_files_name": fcst_files_name, "comb_file_name": comb_file_name}

    def __gen_predictand_files_names(self):
        pass

    @staticmethod
    def download_file(download_url, filename):
        #
        download_url = urllib.parse.quote(download_url, safe=':/')
        # Download file
        f, h = urllib.request.urlretrieve(download_url, filename)
        # Check file size
        assert os.stat(filename).st_size != 0

    def download_raw_data_files(self):

        if self.file_data == "predictor":
            p_files = self.__gen_predictor_files_names()
            raw_data_folders = self.dir_paths.get('raw_data')

            hcst_file_name = p_files.get('hcst_file_name')
            hcst_file_url = self.url_tmplt.substitute(file_type="hindcast", file_name=hcst_file_name)
            hcst_file_path = f"{raw_data_folders.get('hindcasts')}/{hcst_file_name}".replace("//", "/")
            self.download_file(hcst_file_url, hcst_file_path)

            fcst_files_name = p_files.get('fcst_files_name')
            fcst_files_url = [self.url_tmplt.substitute(file_type="forecast", file_name=file_name)
                              for file_name in fcst_files_name]
            fcst_files_path = [f"{raw_data_folders.get('forecasts')}/{file_name}".replace("//", "/")
                               for file_name in fcst_files_name]
            for file_url, file_path in zip(fcst_files_url, fcst_files_path):
                self.download_file(file_url, file_path)

    def combine_raw_data_files(self):

        if self.file_data == "predictor":
            p_files = self.__gen_predictor_files_names()
            raw_data_folders = self.dir_paths.get('raw_data')

            hcst_file_path = f"{raw_data_folders.get('hindcasts')}/{p_files.get('hcst_file_name')}".replace("//", "/")

            fcst_files_path = [f"{raw_data_folders.get('forecasts')}/{file_name}".replace("//", "/")
                               for file_name in p_files.get('fcst_files_name')]

            comb_file_path = f"{self.dir_paths.get('predictors')}/{p_files.get('comb_file_name')}".replace("//", "/")

            shutil.copy(hcst_file_path, comb_file_path)
            # TODO: agregar a este archivo el contenido de los fcst_files_path

    def setup_files(self):

        if self.file_name and not os.path.exists(self.file_name):
            self.download_raw_data_files()
            self.combine_raw_data_files()





