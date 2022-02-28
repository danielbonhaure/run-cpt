from helpers import YearsProcessor, MonthsProcessor
from helpers import FilesProcessor, CPTFileProcessor
from helpers import UpdateControl, ConfigFile
from helpers import SecondaryProgressBar, crange
from crcsas_grid import CrcSasGrid
from errors import ConfigError

import os
import shutil
import xarray as xr
import pandas as pd
import re

from typing import List, Dict, Union
from dataclasses import dataclass, InitVar, field
from string import Template
from urllib.error import HTTPError
from time import sleep
from datetime import date
from calendar import monthrange


@dataclass
class TargetSeason:
    mons: str  # ,'Feb','Mar','Apr','May','Jun','Jul','Aug']
    tgts: str  # ,'Mar-May','Apr-Jun','May-Jul','Jun-Aug','Jul-Sep']

    @property
    def type(self) -> str:
        return 'monthly' if len(self.tgts.split('-')) == 1 else 'seasonal'

    @property
    def start_month_int(self) -> int:
        return MonthsProcessor.month_abbr_to_int(self.mons)

    @property
    def start_month_str(self) -> str:
        return self.mons

    @property
    def first_trgt_month_abbr(self) -> str:
        return self.tgts.split('-')[0]

    @property
    def last_trgt_month_abbr(self) -> str:
        return self.tgts.split('-')[-1]

    @property
    def first_trgt_month_int(self) -> int:
        return MonthsProcessor.month_abbr_to_int(self.first_trgt_month_abbr)

    @property
    def last_trgt_month_int(self) -> int:
        return MonthsProcessor.month_abbr_to_int(self.last_trgt_month_abbr)

    @property
    def first_trgt_month_in_next_year(self) -> bool:
        return MonthsProcessor.trgt_month_in_next_year(self.mons, self.first_trgt_month_abbr)

    @property
    def last_trgt_month_in_next_year(self) -> bool:
        return MonthsProcessor.trgt_month_in_next_year(self.mons, self.last_trgt_month_abbr)

    @property
    def trgt_months_range(self) -> List[int]:
        return MonthsProcessor.month_abbr_to_months_range(self.tgts)

    def __post_init__(self):
        if not self.last_trgt_month_in_next_year and self.first_trgt_month_in_next_year:
            raise AttributeError("Last target month year can't be prior to first target month year!!")


@dataclass
class ForecastData:
    fyr: int  # Forecast first year
    monf: str  # Initialization month
    nfcsts: int  # Number of forecasts

    # Years for each target month
    def trgt_years_by_month(self, trgt_season: TargetSeason) -> Dict[int, List[int]]:
        trgt_years_by_month = dict()
        for year in range(self.fyr, self.lyr + 1):
            trgt_years_by_month[year] = YearsProcessor.years_for_month_range(year, trgt_season)
        return trgt_years_by_month

    @property
    def lyr(self) -> int:  # Forecast last year
        return self.fyr + self.nfcsts - 1

    @property
    def init_month_int(self) -> int:
        return MonthsProcessor.month_abbr_to_int(self.monf)

    @property
    def init_month_str(self) -> str:
        return self.monf


@dataclass
class TrainingPeriod:
    # Start and end of the training period: (must be >1982 for NMME models. Because of CanSIPSv2, probably end in 2018)
    tini: int = 1982
    tend: int = 2010

    original_tini: int = field(init=False)
    original_tend: int = field(init=False)

    trgt_season: InitVar[TargetSeason] = None

    def __post_init__(self, trgt_season):
        self.original_tini = self.tini
        self.original_tend = self.tend

        if trgt_season:
            if trgt_season.first_trgt_month_in_next_year:
                self.tini += 1
            if trgt_season.last_trgt_month_in_next_year:
                self.tend += 1

    # Years for each target month
    def trng_years_by_month(self, trgt_season: TargetSeason) -> Dict[int, List[int]]:
        trng_years_by_month = dict()
        for year in range(self.original_tini, self.original_tend + 1):
            trng_years_by_month[year] = YearsProcessor.years_for_month_range(year, trgt_season)
        return trng_years_by_month

    @property
    def ntrain(self) -> int:  # Length of training period
        return self.original_tend - self.original_tini + 1


@dataclass
class SpatialDomain:
    nla: int  # Northernmost latitude
    sla: int  # Southernmost latitude
    wlo: int  # Westernmost longitude
    elo: int  # Easternmost longitude


@dataclass
class HindcastFile:
    variable: str

    name: str = field(init=False)
    url: str = field(init=False)
    folder: str = field(init=False)

    # Data of monthly files to be used to create the missing seasonal file
    monthly_files_data: dict = field(init=False, default_factory=dict)
    # Data of seasonal file to be used to create the missing seasonal file
    seasonal_file_data: dict = field(init=False, default_factory=dict)

    # Upload cpt config file (It's a singleton!!)
    config_file: ConfigFile = field(init=False, default=ConfigFile.Instance())
    # Define update control (It's a singleton!!)
    update_ctrl: UpdateControl = field(init=False, default=UpdateControl.Instance())

    model: InitVar[str] = None
    fcst_data: InitVar[ForecastData] = None
    trgt_season: InitVar[TargetSeason] = None
    trng_period: InitVar[TrainingPeriod] = None

    @property
    def abs_path(self):
        return os.path.join(self.folder, self.name) if self.name else ""

    def __post_init__(self, model, fcst_data, trgt_season, trng_period):
        # Define file name
        self.name = self.__define_hindcast_filename(model, fcst_data, trgt_season, trng_period)
        # Define url
        url_template = Template(self.config_file.get('url').get('predictor').get('nmme').get('template'))
        self.url = url_template.substitute(trgt_type=trgt_season.type, file_type="hindcast", file_name=self.name)
        # Define folder
        self.folder = self.config_file.get('folders').get('raw_data').get('hindcasts')
        # Define and save auxiliary data
        if trgt_season.type == 'seasonal':
            self.monthly_files_data = self.__define_monthly_files_data(model, fcst_data, trgt_season, trng_period)
            self.seasonal_file_data = self.__define_seasonal_file_data(fcst_data, trgt_season, trng_period)

    def __define_hindcast_filename(self, model, fcst_data, trgt_season, trng_period) -> str:
        model_name = self.config_file.get('models').get(model).get('hcst_name', model)
        months_indexes = MonthsProcessor.month_abbr_to_month_num_as_str(trgt_season.tgts)
        years_to_str = YearsProcessor.trng_period_to_years_str(trng_period, trgt_season)
        return f"{model_name}_{self.variable}_hcst_{fcst_data.monf}ic_{months_indexes}_{years_to_str}.txt"

    def __define_monthly_files_data(self, model, fcst_data, trgt_season, trng_period) -> dict:
        files: list = list()
        model_name = self.config_file.get('models').get(model).get('hcst_name', model)
        trgt_years = trng_period.trng_years_by_month(trgt_season)
        for i, month in enumerate(trgt_season.trgt_months_range):
            year = trgt_years[trng_period.original_tini][i]
            f_name = f'{model_name}_{self.variable}_hcst_{fcst_data.monf}ic_{month}_{year}.txt'
            url_tpl = Template(self.config_file.get('url').get('predictor').get('nmme').get('template'))
            files.append({'name': os.path.join(self.folder, f_name), 'year': year, 'month': month,
                          'url': url_tpl.substitute(trgt_type='monthly', file_type="hindcast", file_name=f_name)})
        return dict(files=files)

    def __define_seasonal_file_data(self, fcst_data, trgt_season, trng_period) -> dict:
        return dict(forecast_init_month=fcst_data.init_month_int,
                    first_target_month=trgt_season.first_trgt_month_int,
                    first_target_month_in_next_year=trgt_season.first_trgt_month_in_next_year,
                    last_target_month=trgt_season.last_trgt_month_int,
                    last_target_month_in_next_year=trgt_season.last_trgt_month_in_next_year,
                    training_original_first_year=trng_period.original_tini,
                    training_original_last_year=trng_period.original_tend,
                    target_years=trng_period.trng_years_by_month(trgt_season),
                    target_months=trgt_season.trgt_months_range,
                    seasonal_file_variable=self.variable)

    def download_raw_data(self):
        if not os.path.exists(self.abs_path) or \
                self.update_ctrl.must_be_updated('predictors', 'raw_data', self.abs_path):
            try:
                FilesProcessor.download_file_from_url(self.url, self.abs_path, 20000000)
            except HTTPError as e:
                if e.code == 404 and self.monthly_files_data and (
                        not os.path.exists(self.abs_path) or self.update_ctrl.must_be_updated(
                            'predictors', 'locally_created_seasonal_files', self.abs_path)):
                    self.__create_seasonal_file_from_monthly_files()
                else:
                    os.remove(self.abs_path) if os.path.exists(self.abs_path) else None
                    raise
            except AssertionError:
                os.remove(self.abs_path) if os.path.exists(self.abs_path) else None
                raise
            else:
                self.update_ctrl.report_updated_file(self.abs_path)

    def __create_seasonal_file_from_monthly_files(self, debug: bool = False):
        # Dataframe with files content
        df: pd.DataFrame = pd.DataFrame()

        # Generate a dataframe for each monthly file, and concatenate them
        for fd in self.monthly_files_data.get('files'):
            # Download monthly file
            if not os.path.exists(fd.get('name')) or \
                    self.update_ctrl.must_be_updated('predictors', 'raw_data', fd.get('name')):
                try:
                    FilesProcessor.download_file_from_url(fd.get('url'), fd.get('name'), 20000000)
                except (HTTPError, AssertionError):
                    os.remove(fd.get('name')) if os.path.exists(fd.get('name')) else None
                    raise
                else:
                    self.update_ctrl.report_updated_file(fd.get('name'))
            # Read file, get a dataframe that represent it, and concatenate it to df
            df = pd.concat([df, CPTFileProcessor.cpt_noaa_monthly_file_to_dataframe(fd.get('name'))])

        # Create a column over that we can group.
        df.insert(0, 'trng_year', 0)
        for year in range(self.seasonal_file_data['training_original_first_year'],
                          self.seasonal_file_data['training_original_last_year'] + 1):
            years, months = self.seasonal_file_data['target_years'][year], self.seasonal_file_data['target_months']
            for y, m in zip(years, months):
                df.loc[(df.index.get_level_values('year') == y) &
                       (df.index.get_level_values('month') == m), 'trng_year'] = year
        df.set_index('trng_year', append=True, inplace=True)

        # Drop unused levels
        df = df.droplevel(['year', 'month'])

        # Group data by coord and year (excluding month)
        if self.variable == 'precip':
            df = df.groupby(level=[0, 'trng_year']).median()
        elif self.variable == 'tmp2m':
            df = df.groupby(level=[0, 'trng_year']).median()
        else:
            raise AttributeError('Hindcast files only support "precip" and "tmp2m" as variables!')

        # Rename dataframe trng_year level to year
        df.index.set_names('year', level='trng_year', inplace=True)

        # Save generated dataframe as cpt file
        CPTFileProcessor.dataframe_to_cpt_noaa_seasonal_hindcast_file(self.abs_path, self.seasonal_file_data, df, debug)


@dataclass
class ForecastFile:
    variable: str

    name: str = field(init=False)
    url: str = field(init=False)
    folder: str = field(init=False)

    # Data of monthly files to be used to create the missing seasonal file
    monthly_files_data: dict = field(init=False, default_factory=dict)
    # Data of seasonal file to be used to create the missing seasonal file
    seasonal_file_data: dict = field(init=False, default_factory=dict)

    # Upload cpt config file (It's a singleton!!)
    config_file: ConfigFile = field(init=False, default=ConfigFile.Instance())
    # Define update control (It's a singleton!!)
    update_ctrl: UpdateControl = field(init=False, default=UpdateControl.Instance())

    model: InitVar[str] = None
    fcst_data: InitVar[ForecastData] = None
    trgt_season: InitVar[TargetSeason] = None
    fcst_year: InitVar[int] = None

    @property
    def abs_path(self):
        return os.path.join(self.folder, self.name) if self.name else ""

    def __post_init__(self, model, fcst_data, trgt_season, fcst_year):
        # Define file name
        self.name = self.__define_forecast_filename(model, fcst_data, trgt_season, fcst_year)
        # Define url
        url_template = Template(self.config_file.get('url').get('predictor').get('nmme').get('template'))
        self.url = url_template.substitute(trgt_type=trgt_season.type, file_type="forecast", file_name=self.name)
        # Define folder
        self.folder = self.config_file.get('folders').get('raw_data').get('forecasts')
        # Define and save auxiliary data
        if trgt_season.type == 'seasonal':
            self.monthly_files_data = self.__define_monthly_files_data(model, fcst_data, trgt_season, fcst_year)
            self.seasonal_file_data = self.__define_seasonal_file_data(fcst_data, trgt_season, fcst_year)

    def __define_forecast_filename(self, model, fcst_data, trgt_season, fcst_year) -> str:
        model_name = self.config_file.get('models').get(model).get('predictors').get('fcst_name', model)
        months_indexes = MonthsProcessor.month_abbr_to_month_num_as_str(trgt_season.tgts)
        years_to_str = YearsProcessor.fcst_data_to_years_str(fcst_data, trgt_season, fcst_year)
        return f'{model_name}_{self.variable}_fcst_{fcst_data.monf}ic_{months_indexes}_{years_to_str}.txt'

    def __define_monthly_files_data(self, model, fcst_data, trgt_season, fcst_year) -> dict:
        files: list = list()
        model_name = self.config_file.get('models').get(model).get('predictors').get('fcst_name', model)
        trgt_years = fcst_data.trgt_years_by_month(trgt_season)
        for i, month in enumerate(trgt_season.trgt_months_range):
            year = trgt_years[fcst_year][i]
            f_name = f'{model_name}_{self.variable}_fcst_{fcst_data.monf}ic_{month}_{year}.txt'
            url_tpl = Template(self.config_file.get('url').get('predictor').get('nmme').get('template'))
            files.append({'name': os.path.join(self.folder, f_name), 'year': year, 'month': month,
                          'url': url_tpl.substitute(trgt_type='monthly', file_type="forecast", file_name=f_name)})
        return dict(files=files)

    def __define_seasonal_file_data(self, fcst_data, trgt_season, fcst_year) -> dict:
        return dict(forecast_init_month=fcst_data.init_month_int,
                    first_target_month=trgt_season.first_trgt_month_int,
                    first_target_month_in_next_year=trgt_season.first_trgt_month_in_next_year,
                    last_target_month=trgt_season.last_trgt_month_int,
                    last_target_month_in_next_year=trgt_season.last_trgt_month_in_next_year,
                    forecast_year=fcst_year,
                    seasonal_file_variable=self.variable)

    def download_raw_data(self):
        if not os.path.exists(self.abs_path) or \
                self.update_ctrl.must_be_updated('predictors', 'raw_data', self.abs_path):
            try:
                FilesProcessor.download_file_from_url(self.url, self.abs_path, 500000)
            except HTTPError as e:
                if e.code == 404 and self.monthly_files_data and (
                        not os.path.exists(self.abs_path) or self.update_ctrl.must_be_updated(
                            'predictors', 'locally_created_seasonal_files', self.abs_path)):
                    self.__create_seasonal_file_from_monthly_files()
                else:
                    os.remove(self.abs_path) if os.path.exists(self.abs_path) else None
                    raise
            except AssertionError:
                os.remove(self.abs_path) if os.path.exists(self.abs_path) else None
                raise
            else:
                self.update_ctrl.report_updated_file(self.abs_path)

    def __create_seasonal_file_from_monthly_files(self, debug: bool = False):
        # Dataframe with files content
        df: pd.DataFrame = pd.DataFrame()

        # Generate a dataframe for each monthly file, and concatenate them
        for fd in self.monthly_files_data.get('files'):
            # Download monthly file
            if not os.path.exists(fd.get('name')) or \
                    self.update_ctrl.must_be_updated('predictors', 'raw_data', fd.get('name')):
                try:
                    FilesProcessor.download_file_from_url(fd.get('url'), fd.get('name'), 500000)
                except (HTTPError, AssertionError):
                    os.remove(fd.get('name')) if os.path.exists(fd.get('name')) else None
                    raise
                else:
                    self.update_ctrl.report_updated_file(fd.get('name'))
            # Read file, get a dataframe that represent it, and concatenate it to df
            df = pd.concat([df, CPTFileProcessor.cpt_noaa_monthly_file_to_dataframe(fd.get('name'))])

        # Group data by coord and year (excluding month)
        if self.variable == 'precip':
            df = df.groupby(level=0).median()
        elif self.variable == 'tmp2m':
            df = df.groupby(level=0).median()
        else:
            raise AttributeError('Forecast files only support "precip" and "tmp2m" as variables!')

        # Save generated dataframe as cpt file
        CPTFileProcessor.dataframe_to_cpt_noaa_seasonal_forecast_file(self.abs_path, self.seasonal_file_data, df, debug)


@dataclass
class NoaaPredictorFile:
    predictor: str

    name: str = field(init=False)
    folder: str = field(init=False)

    hcst_file: HindcastFile = field(init=False)
    fcst_files: List[ForecastFile] = field(init=False, default_factory=list)

    # Upload cpt config file (It's a singleton!!)
    config_file: ConfigFile = field(init=False, default=ConfigFile.Instance())
    # Define update control (It's a singleton!!)
    update_ctrl: UpdateControl = field(init=False, default=UpdateControl.Instance())

    model: InitVar[str] = None
    fcst_data: InitVar[ForecastData] = None
    trgt_season: InitVar[TargetSeason] = None
    trng_period: InitVar[TrainingPeriod] = None

    @property
    def abs_path(self):
        return os.path.join(self.folder, self.name) if self.name else ""

    def __post_init__(self, model, fcst_data, trgt_season, trng_period):
        # Define file name
        self.name = self.__define_predictor_filename(model, fcst_data, trgt_season, trng_period)
        # Define folder
        self.folder = self.config_file.get('folders').get('predictors')
        # Define raw files
        self.__define_raw_files(model, fcst_data, trgt_season, trng_period)
        # Download raw files
        self.__download_raw_files()
        # Create predictor file (combining raw files)
        self.__create_predictor_file(fcst_data, trgt_season, trng_period)

    def __define_predictor_filename(self, model, fcst_data, trgt_season, trng_period) -> str:
        months_indexes = MonthsProcessor.month_abbr_to_month_num_as_str(trgt_season.tgts)
        years_to_str = YearsProcessor.fcst_data_to_years_str(fcst_data, trgt_season, fcst_data.fyr)
        return f"{model}_{self.predictor}_{fcst_data.monf}ic_{months_indexes}_" \
               f"{trng_period.tini}-{trng_period.tend}_{years_to_str}_{fcst_data.nfcsts}.txt"

    def __define_raw_files(self, model, fcst_data, trgt_season, trng_period):
        # Create hindcast file
        self.hcst_file = HindcastFile(self.predictor, model, fcst_data, trgt_season, trng_period)
        # Create forecast files
        for year in range(fcst_data.fyr, fcst_data.lyr + 1):
            self.fcst_files.append(ForecastFile(self.predictor, model, fcst_data, trgt_season, year))

    def __download_raw_files(self):
        if os.path.exists(self.abs_path) and \
                not self.update_ctrl.must_be_updated('predictors', 'cpt_input_data', self.abs_path):
            return

        # Download raw files
        self.hcst_file.download_raw_data()
        for fcst_file in self.fcst_files:
            fcst_file.download_raw_data()

    def __create_predictor_file(self, fcst_data, trgt_season, trng_period):
        if os.path.exists(self.abs_path) and \
                not self.update_ctrl.must_be_updated('predictors', 'cpt_input_data', self.abs_path):
            return

        # define forecast years data
        fcst_years = list(range(fcst_data.fyr, fcst_data.lyr + 1))
        fcst_years_count = list(range(1, len(fcst_years) + 1))

        # create progress bar to track interpolation
        run_status = f'Creating file {self.abs_path.split("/").pop(-1)} (PID: {os.getpid()})'
        pb = SecondaryProgressBar(len(fcst_years) + 1, run_status)

        # open progress bar
        pb.open()

        # Copiar y renombrar el archivo hindcast
        dst = shutil.copy(self.hcst_file.abs_path, self.abs_path)

        # report status
        pb.report_advance(1)

        # Agregar el contenido de los fcst_files_path
        with open(dst, 'a') as fp_comb:
            for fcst_file, fcst_year, years_to_add in zip(self.fcst_files, fcst_years, fcst_years_count):
                with open(fcst_file.abs_path, 'r') as fp_fcst:
                    for line in fp_fcst.readlines():
                        if all(x not in line for x in ['xmlns:cpt', 'cpt:nfields']):
                            if 'cpt:field=' in line:
                                # Define start_year. It is based on the original training period!
                                start_year = trng_period.original_tend + years_to_add

                                # Set correct start date
                                line = re.sub(rf"(cpt:S=){fcst_year}(-\d{{2}}-\d{{2}})",
                                              rf"\g<1>{start_year}\g<2>", line)

                                # Set year correction
                                yc_1 = 1 if trgt_season.first_trgt_month_in_next_year else 0
                                yc_2 = 1 if trgt_season.last_trgt_month_in_next_year else 0

                                # Set correct target date
                                if trgt_season.type == 'seasonal':
                                    line = re.sub(rf"(cpt:T=){fcst_year + yc_1}(-\d{{2}}/){fcst_year + yc_2}(-\d{{2}})",
                                                  rf"\g<1>{start_year + yc_1}\g<2>{start_year + yc_2}\g<3>", line)
                                else:
                                    line = re.sub(rf"(cpt:T=){fcst_year + yc_1}(-\d{{2}})",
                                                  rf"\g<1>{start_year + yc_1}\g<2>", line)

                            fp_comb.write(line)
                # report status
                pb.report_advance(1)

        # Reportar que el archivo ya fue actualizado
        self.update_ctrl.report_updated_file(self.abs_path)

        # close progress bar
        sleep(0.5)
        pb.close()


@dataclass
class IriDLPredictorFile:
    predictor: str

    name: str = field(init=False)
    folder: str = field(init=False)
    url: str = field(init=False)

    # Upload cpt config file (It's a singleton!!)
    config_file: ConfigFile = field(init=False, default=ConfigFile.Instance())
    # Define update control (It's a singleton!!)
    update_ctrl: UpdateControl = field(init=False, default=UpdateControl.Instance())

    model: InitVar[str] = None
    fcst_data: InitVar[ForecastData] = None
    trgt_season: InitVar[TargetSeason] = None
    trng_period: InitVar[TrainingPeriod] = None

    @property
    def abs_path(self):
        return os.path.join(self.folder, self.name) if self.name else ""

    def __post_init__(self, model, fcst_data, trgt_season, trng_period):
        # Define file name
        self.name = self.__define_predictor_filename(model, fcst_data, trgt_season, trng_period)
        # Define folder
        self.folder = self.config_file.get('folders').get('predictors')
        # Define url
        self.url = self.__define_url_to_file(fcst_data, trgt_season)
        # Download raw files
        self.__download_file()

    def __define_predictor_filename(self, model, fcst_data, trgt_season, trng_period) -> str:
        months_indexes = MonthsProcessor.month_abbr_to_month_num_as_str(trgt_season.tgts)
        years_to_str = YearsProcessor.fcst_data_to_years_str(fcst_data, trgt_season, fcst_data.fyr)
        return f"{model}_{self.predictor}_{fcst_data.monf}ic_{months_indexes}_" \
               f"{trng_period.tini}-{trng_period.tend}_{years_to_str}_{fcst_data.nfcsts}.txt"

    def __define_url_to_file(self, fcst_data, trgt_season):
        spd_predictor = SpatialDomain(**self.config_file.get('spatial_domain').get('predictor'))

        mf = MonthsProcessor.num_of_months_from_month_to_month

        url = Template(self.config_file.get('url').get('predictor').get('ecmwf').get('template').get(trgt_season.type))\
            .substitute(
                variable='prcp' if self.predictor == 'precip' else 't2m',
                unit='mm/day' if self.predictor == 'precip' else 'Celsius_scale',
                init_month_str=fcst_data.init_month_str, last_year=fcst_data.lyr,
                months_after_init_month_t1=mf(fcst_data.init_month_int, trgt_season.first_trgt_month_int),
                months_after_init_month_t2=mf(fcst_data.init_month_int, trgt_season.last_trgt_month_int),
                nla=spd_predictor.nla, sla=spd_predictor.sla, wlo=spd_predictor.wlo, elo=spd_predictor.elo
            )
        # print(f"init_month: {fcst_data.init_month_int}, first_target_month: {trgt_season.first_trgt_month_int}, "
        #       f"last_target_month: {trgt_season.last_trgt_month_int}, file_name: {self.abs_path}, url: {url}")

        return url

    def __download_file(self):
        if os.path.exists(self.abs_path) and \
                not self.update_ctrl.must_be_updated('predictors', 'cpt_input_data', self.abs_path):
            return

        try:
            FilesProcessor.download_file_from_url(self.url, self.abs_path, 500000)
        except (HTTPError, AssertionError):
            os.remove(self.abs_path) if os.path.exists(self.abs_path) else None
            raise
        else:
            self.update_ctrl.report_updated_file(self.abs_path)


@dataclass
class PredictorFile:
    predictor: str
    data_source: str  # It can be "noaa" or "iridl"

    predictor_file: Union[NoaaPredictorFile, IriDLPredictorFile] = field(init=False)

    model: InitVar[str] = None
    fcst_data: InitVar[ForecastData] = None
    trgt_season: InitVar[TargetSeason] = None
    trng_period: InitVar[TrainingPeriod] = None

    @property
    def abs_path(self):
        return self.predictor_file.abs_path

    def __post_init__(self, model, fcst_data, trgt_season, trng_period):
        if self.data_source == 'noaa':
            self.predictor_file = NoaaPredictorFile(self.predictor, model, fcst_data, trgt_season, trng_period)
        elif self.data_source == 'iridl':
            self.predictor_file = IriDLPredictorFile(self.predictor, model, fcst_data, trgt_season, trng_period)


@dataclass
class ChirpsFile:
    variable: str

    type: str = field(init=False)
    name: str = field(init=False)
    url: str = field(init=False)
    folder: str = field(init=False)
    coords: dict = field(init=False)

    # Upload cpt config file (It's a singleton!!)
    config_file: ConfigFile = field(init=False, default=ConfigFile.Instance())
    # Define update control (It's a singleton!!)
    update_ctrl: UpdateControl = field(init=False, default=UpdateControl.Instance())

    @property
    def abs_path(self):
        return os.path.join(self.folder, self.name) if self.name else ""

    def __post_init__(self):
        # Define file type
        self.type = self.config_file.get('url').get('predictand').get('chirps').get('type')
        # Define file name
        self.name = self.__define_chirps_filename(self.type)
        # Define url
        self.url = self.config_file.get('url').get('predictand').get('chirps').get(self.type)
        # Define folder
        self.folder = self.config_file.get('folders').get('raw_data').get('chirps')
        # Define coords to cut chirps file
        self.coords = self.config_file.get('spatial_domain').get('predictand')

    def __define_chirps_filename(self, chirps_file_type) -> str:
        return f'{self.variable}_chirps_{chirps_file_type}.nc'

    def __cut_chirps_file(self, file_to_cut, cutted_file):
        """Cortar el archivo, chirps abarca el mundo entero, se deja solo el dominio que usamos"""

        # create progress bar to track interpolation
        run_status = f'Cutting file {file_to_cut.split("/").pop(-1)} (PID: {os.getpid()})'
        pb = SecondaryProgressBar(10, run_status)

        # open progress bar
        pb.open()

        # Open netcdf
        with xr.open_dataset(file_to_cut) as ds:
            # report status
            pb.update_count(1)
            # Create mask
            mask = ((ds.coords["latitude"] >= self.coords['sla']) & (ds.coords["latitude"] <= self.coords['nla']) &
                    (ds.coords["longitude"] >= self.coords['wlo']) & (ds.coords["longitude"] <= self.coords['elo']))
            # Report status
            pb.update_count(3)
            # Cut netcdf
            ds = ds.where(mask, drop=True)
            # Report status
            pb.update_count(7)
            # Rename variable
            ds = ds.rename(precip=self.variable)
            # Report status
            pb.update_count(8)
            # Save netcdf
            ds.to_netcdf(cutted_file)
            # report status
            pb.update_count(10)

        # Reportar que el archivo ya fue actualizado
        self.update_ctrl.report_updated_file(cutted_file)

        # close progress bar
        sleep(0.5)
        pb.close()

    def download_raw_data(self):
        if self.type == "daily":
            self.download_daily_raw_data()
        elif self.type == "monthly":
            self.download_monthly_raw_data()
        else:
            raise ConfigError(f"El tipo indicado para el/los archivo/s chirps es incorrecto! "
                              f"Solo se adminten alguno de estos 2 tipos: daily, monthly!!")

    def download_daily_raw_data(self):
        for year in range(1981, date.today().year + 1):
            year_chirps_url = Template(self.url).substitute(year=year)
            year_chirps_file = os.path.join(self.folder, year_chirps_url.split('/').pop(-1))
            year_chirps_file_world = year_chirps_file.replace('.nc', '_world.nc')

            # Download chirps for the whole world (if needed)
            if not os.path.exists(year_chirps_file_world) or \
                    (self.update_ctrl.must_be_updated('predictands', 'raw_data', year_chirps_file_world) and
                        self.config_file.get('update').get('only_last_year_chirps_daily_raw_data') is False) or \
                    (self.update_ctrl.must_be_updated('predictands', 'raw_data', year_chirps_file_world) and
                        self.update_ctrl.must_be_updated('predictands', 'only_last_year_chirps_daily_raw_data',
                                                         year_chirps_file_world) and
                        ((year == date.today().year - 1 and date.today().month <= 2) or year == date.today().year)):
                try:
                    # Download netcdf
                    min_valid_size = 60000000 if year != date.today().year else 500
                    FilesProcessor.download_file_from_url(year_chirps_url, year_chirps_file_world, min_valid_size)
                except HTTPError as e:
                    if e.code == 404 and year == date.today().year and date.today().month <= 6:
                        continue
                    else:
                        os.remove(year_chirps_file_world) if os.path.exists(year_chirps_file_world) else None
                        raise
                except AssertionError:
                    os.remove(year_chirps_file_world) if os.path.exists(year_chirps_file_world) else None
                    raise
                else:
                    # Reportar que el archivo ya fue actualizado
                    self.update_ctrl.report_updated_file(year_chirps_file_world)

            # Update intermediate file (obtained by cutting the downloaded file)
            if not os.path.exists(year_chirps_file) or \
                    (self.update_ctrl.must_be_updated('predictands', 'cutted_chirps_data', year_chirps_file) and
                        self.config_file.get('update').get('only_last_year_chirps_daily_raw_data') is False) or \
                    (self.update_ctrl.must_be_updated('predictands', 'cutted_chirps_data', year_chirps_file) and
                        self.update_ctrl.must_be_updated('predictands', 'only_last_year_chirps_daily_raw_data',
                                                         year_chirps_file_world) and
                        ((year == date.today().year - 1 and date.today().month <= 2) or year == date.today().year)):
                self.__cut_chirps_file(year_chirps_file_world, year_chirps_file)

        # Una vez descargados y cortados los archivos, se los une en uno solo
        if not os.path.exists(self.abs_path) or \
                self.update_ctrl.must_be_updated('predictands', 'raw_data', self.abs_path):

            # create progress bar to track interpolation
            run_status = f'Merging files to make {self.abs_path.split("/").pop(-1)} (PID: {os.getpid()})'
            pb = SecondaryProgressBar(10, run_status)

            # open progress bar
            pb.open()

            # Merge all chirps files
            chirps_files = Template(self.url).substitute(year="*").split('/').pop(-1)
            ds = xr.open_mfdataset(os.path.join(self.folder, chirps_files))

            # report status
            pb.update_count(3)

            # Group data by month
            ds = ds.resample(time='1M').sum()

            # report status
            pb.update_count(6)

            # Save netcdf
            ds.to_netcdf(self.abs_path)

            # report status
            pb.update_count(10)

            # Reportar que el archivo ya fue actualizado
            self.update_ctrl.report_updated_file(self.abs_path)

            # close progress bar
            sleep(0.5)
            pb.close()

    def download_monthly_raw_data(self):
        chirps_world = self.abs_path.replace('.nc', '_world.nc')

        # Download chirps for the whole world (if needed)
        if not os.path.exists(chirps_world) or \
                self.update_ctrl.must_be_updated('predictands', 'raw_data', chirps_world):
            try:
                # Download netcdf
                FilesProcessor.download_file_from_url(self.url, chirps_world, 6000000000)
            except (HTTPError, AssertionError):
                os.remove(self.abs_path) if os.path.exists(self.abs_path) else None
                raise
            else:
                # Reportar que el archivo ya fue actualizado
                self.update_ctrl.report_updated_file(chirps_world)
                # Cortar el archivo, chirps abarca el mundo entero, se deja solo el dominio que usamos
                self.__cut_chirps_file(chirps_world, self.abs_path)

        # Update intermediate file (obtained by cutting the downloaded file)
        if not os.path.exists(self.abs_path) or \
                self.update_ctrl.must_be_updated('predictands', 'cutted_chirps_data', self.abs_path):
            self.__cut_chirps_file(chirps_world, self.abs_path)


@dataclass
class Era5LandFile:
    variable: str

    name: str = field(init=False)
    folder: str = field(init=False)
    coords: dict = field(init=False)

    # Upload cpt config file (It's a singleton!!)
    config_file: ConfigFile = field(init=False, default=ConfigFile.Instance())
    # Define update control (It's a singleton!!)
    update_ctrl: UpdateControl = field(init=False, default=UpdateControl.Instance())

    @property
    def abs_path(self):
        return os.path.join(self.folder, self.name) if self.name else ""

    def __post_init__(self):
        # Define file name
        self.name = self.__define_era5land_filename()
        # Define folder
        self.folder = self.config_file.get('folders').get('raw_data').get('era5_land')
        # Define coords to cut rea5-land file
        self.coords = self.config_file.get('spatial_domain').get('predictand')

    def __define_era5land_filename(self) -> str:
        return f'{self.variable}_era5-land.nc'

    def __get_api_variable(self):
        if self.variable == 't2m':
            return '2m_temperature'
        if self.variable == 'prcp':
            return 'total_precipitation'

    def __get_api_area(self):
        return [self.coords.get('nla'), self.coords.get('wlo'), self.coords.get('sla'), self.coords.get('elo')]

    def download_raw_data(self):
        if not os.path.exists(self.abs_path) or \
                self.update_ctrl.must_be_updated('predictands', 'raw_data', self.abs_path):

            try:
                # Download file from cdsapi
                FilesProcessor.download_file_from_cdsapi(self.abs_path, self.__get_api_variable(),
                                                         self.__get_api_area(), 100000000)
            except (HTTPError, AssertionError):
                os.remove(self.abs_path) if os.path.exists(self.abs_path) else None
                raise

            # Reportar que el archivo ya fue actualizado
            self.update_ctrl.report_updated_file(self.abs_path)

            # Rename variable when needed
            if self.variable == 'prcp':
                # Open downloaded netcdf
                with xr.open_dataset(self.abs_path) as ds:
                    # Rename variable
                    ds = ds.rename(tp=self.variable)
                    ds = ds.persist()
                    # Change units (from m to mm)
                    ds[self.variable] = ds[self.variable].assign_attrs(units='mm')
                    ds[self.variable] = ds[self.variable] * 1000
                    ds = ds.persist()
                    # Get the total prcp of month from the monthly daily average
                    for i in range(len(ds.time)):
                        i_date = pd.to_datetime(ds.time[i].values)
                        n_days = monthrange(i_date.year, i_date.month)[1]
                        ds[self.variable][i, :, :] = ds[self.variable][i, :, :] * n_days
                    ds = ds.persist()
                    # Round final value
                    ds[self.variable] = ds[self.variable].round(1)
                    ds = ds.persist()
                    # Load dataset from disk to memory
                    in_mem_ds = ds.compute().load()
                # Save netcdf
                in_mem_ds.to_netcdf(self.abs_path)
                # Free memory
                del in_mem_ds
            if self.variable == 't2m':
                # Open downloaded netcdf
                with xr.open_dataset(self.abs_path) as ds:
                    # Change units (from K to °C)
                    ds[self.variable] = ds[self.variable].assign_attrs(units='°C')
                    ds[self.variable] = ds[self.variable] - 273.15
                    ds = ds.persist()
                    # Round final value
                    ds[self.variable] = ds[self.variable].round(1)
                    ds = ds.persist()
                    # Load dataset from disk to memory
                    in_mem_ds = ds.compute().load()
                # Save netcdf
                in_mem_ds.to_netcdf(self.abs_path)
                # Free memory
                del in_mem_ds


@dataclass
class CrcSasFile:
    variable: str
    trng_period: TrainingPeriod

    name: str = field(init=False)
    folder: str = field(init=False)
    coords: dict = field(init=False)

    # Upload cpt config file (It's a singleton!!)
    config_file: ConfigFile = field(init=False, default=ConfigFile.Instance())
    # Define update control (It's a singleton!!)
    update_ctrl: UpdateControl = field(init=False, default=UpdateControl.Instance())

    @property
    def abs_path(self):
        return os.path.join(self.folder, self.name) if self.name else ""

    def __post_init__(self):
        # Define file name
        self.name = self.__define_crcsas_filename()
        # Define folder
        self.folder = self.config_file.get('folders').get('raw_data').get('crcsas')
        # Define coords to cut rea5-land file
        self.coords = self.config_file.get('spatial_domain').get('predictand')

    def __define_crcsas_filename(self) -> str:
        return f'{self.variable}_crcsas.csv'

    def download_raw_data(self):
        if not os.path.exists(self.abs_path) or \
                self.update_ctrl.must_be_updated('predictands', 'raw_data', self.abs_path):

            try:
                # Download file from cdsapi
                FilesProcessor.download_data_from_crcsas(self.abs_path, self.variable,
                                                         self.trng_period.tini, self.trng_period.tend,
                                                         100000)
            except (HTTPError, AssertionError):
                os.remove(self.abs_path) if os.path.exists(self.abs_path) else None
                raise

            # Reportar que el archivo ya fue actualizado
            self.update_ctrl.report_updated_file(self.abs_path)


@dataclass
class PredictandFile:
    predictand: str
    data_source: str  # It can be "chirps" or "era5-land" or "crcsas"

    raw_data_file: Union[ChirpsFile, Era5LandFile, CrcSasFile] = field(init=False)

    name: str = field(init=False)
    folder: str = field(init=False)
    grid: CrcSasGrid = field(init=False)  # Define grid to be used (the CRC-SAS grid)

    # Upload cpt config file (It's a singleton!!)
    config_file: ConfigFile = field(init=False, default=ConfigFile.Instance())
    # Define update control (It's a singleton!!)
    update_ctrl: UpdateControl = field(init=False, default=UpdateControl.Instance())

    fcst_data: InitVar[ForecastData] = None
    trgt_season: InitVar[TargetSeason] = None
    trng_period: InitVar[TrainingPeriod] = None

    @property
    def abs_path(self):
        return os.path.join(self.folder, self.name) if self.name else ""

    def __post_init__(self, fcst_data, trgt_season, trng_period):
        # Check data_source attribute
        if self.data_source not in ['chirps', 'era5-land', 'crcsas']:
            error_msg = f'The data_source attribute must be "chirps" or ' \
                        f'"era5-land" or "crcsas", not "{self.data_source}"'
            raise AttributeError(error_msg)
        # Create predictand file (it can be a chirps file or an era5-land file)
        if self.data_source == 'chirps':
            self.raw_data_file = ChirpsFile(self.predictand)
        if self.data_source == 'era5-land':
            self.raw_data_file = Era5LandFile(self.predictand)
        if self.data_source == 'crcsas':
            self.raw_data_file = CrcSasFile(self.predictand, trng_period)
        # Define file name
        self.name = self.__define_predictand_filename(trgt_season, trng_period, fcst_data)
        # Define folder
        self.folder = self.config_file.get('folders').get('predictands')
        # Define grid
        self.grid = CrcSasGrid()
        # Download raw files
        self.__download_raw_file()
        # Create predictand file (extracting data from raw file)
        self.__create_predictand_file(trgt_season, trng_period, fcst_data)

    def __define_predictand_filename(self, trgt_season, trng_period, fcst_data) -> str:
        months_indexes = MonthsProcessor.month_abbr_to_month_num_as_str(trgt_season.tgts)
        # return f"{self.predictand}_{months_indexes}_{trng_period.tini}-{trng_period.tend}_" \
        #        f"{fcst_data.fyr}-{fcst_data.fyr + fcst_data.nfcsts - 1}.txt"
        return f"{self.predictand}_{self.data_source}_{months_indexes}.txt"

    def __download_raw_file(self):
        self.raw_data_file.download_raw_data()

    def __create_predictand_file(self, trgt_season, trng_period, fcst_data):
        if os.path.exists(self.abs_path) and not \
                self.update_ctrl.must_be_updated('predictands', 'cpt_input_data', self.abs_path):
            return

        # create progress bar to track interpolation
        run_status = f'Creating file {self.abs_path.split("/").pop(-1)} (PID: {os.getpid()})'
        pb = SecondaryProgressBar(10, run_status)

        # open progress bar
        pb.open()

        if self.data_source == 'crcsas':
            # read csv file with pandas
            df = pd.read_csv(self.raw_data_file.abs_path, sep=';')
            df['time'] = pd.to_datetime(df['time'])
            df = df.set_index(['time', 'latitude', 'longitude'])
        else:
            # define interpolated file filename
            interp_file = self.raw_data_file.abs_path.replace('.nc', '_interpolated.nc')

            if not os.path.exists(interp_file) or \
                    self.update_ctrl.must_be_updated('predictands', 'interpolated_data', interp_file):
                # interpolate recently downloaded data
                df = self.grid.interpolate_raw_data(
                    input_file=self.raw_data_file.abs_path,
                    variable=self.predictand,
                    return_df=True,
                    output_file=interp_file
                )
                # report interpolation
                self.update_ctrl.report_updated_file(interp_file)
            else:
                # read/load previously interpolated data
                df = xr.open_dataset(interp_file).to_dataframe()

        # report status
        pb.update_count(5)

        # Change index to allow filtering by months
        df['year'] = df.index.get_level_values('time').year
        df['month'] = df.index.get_level_values('time').month
        df.set_index('year', append=True, inplace=True)
        df.set_index('month', append=True, inplace=True)
        df = df.droplevel('time') \
            .reorder_levels(['latitude', 'longitude', 'month', 'year']) \
            .sort_index(ascending=[False, False, True, True])

        # report status
        pb.update_count(7.5)

        # Filter by month/s
        months = [*crange(trgt_season.first_trgt_month_int, trgt_season.last_trgt_month_int + 1, 12)]
        df = df.loc[:, :, months, :]
        # Agregar marca para agrupar, si se usa solo el año los trimetres que
        # cambien de año (ej: 12,1,2) van a ser agrupados de manera errónea
        # (ej: 12-2021, 1-2021, 2-2021 en lugar de 12-2021, 1-2022, 2-2022).
        # Como ID de cada grupo se usa el año del primero de los meses de la
        # lista de meses a agrupar (ej: para el trimestre 10,11,1 el año
        # utilizado com ID es el asociado al mes 10, que además es el menor
        # de los años cubiertos por el trimestre). Entonces, si se detecta
        # un cambio año, todos los meses entre 1 (enero) y 12 (diciembre)
        # menos la cantidad de meses a grupar, es decir, 12 - length(months)),
        # deben usar como ID el año del registro menos 1 (porque están en el
        # segundo de los dos años cubiertos por los meses a agrupar). Ej: si
        # la cantidad de meses a agrupar es 3, el primero de los grupos de 3
        # meses, con cambio de año, es 11,12,1; por o tanto, todos los meses
        # entre 1 (enero) y 10 (octubre) llevan como ID el primero de los 2
        # años cubiertos por el listado de meses a agrupar, es decir, el año
        # indicado en el registro (fila) menos 1.
        df['group_id'] = df.index.get_level_values('year')
        if MonthsProcessor.year_change_detected(months):
            df_months = df.index.get_level_values('month')
            months_in_prev_year = (df_months >= 1) & (df_months <= (12 - len(months) + 1))
            df.loc[months_in_prev_year, 'group_id'] = df.loc[months_in_prev_year, 'group_id'] - 1
        # Group data
        if self.predictand == 't2m':
            # Group data and get mean
            df = df.groupby(['latitude', 'longitude', 'group_id']).mean().round(1)
            # Rename index columns (group_id to year)
            df.index.names = ['latitude', 'longitude', 'year']
        if self.predictand == 'prcp':
            # Group data and get sum
            df = df.groupby(['latitude', 'longitude', 'group_id']).sum(min_count=1).round(1)
            # Rename index columns (group_id to year)
            df.index.names = ['latitude', 'longitude', 'year']
            # Si un punto (lon, lat) tiene poquísima lluvia, como por ejemplo en el desierto de Atacama, entonces
            # CPT falla en la predicción, para evitar esto, se agregan lluvias falsas de 0.1 mm a algunos de los
            # elementos de la serie de lluvias (los valores por año son acumulados de 1 o 3 meses)
            df1 = df.groupby(['latitude', 'longitude']).sum(min_count=1).round(1)
            if df1.prcp.min() <= 0.2:
                for lat, lon in df1.query('prcp <= 0.2').index.to_flat_index():
                    # print(f'Actualizando archivo {self.abs_path,} lat: {lat}, lon: {lon}, valores: [ '
                    #       f'{trng_period.tini} - {df.loc[lat, lon, trng_period.tini].prcp}, '
                    #       f'{trng_period.tend} - {df.loc[lat, lon, trng_period.tend].prcp}]')
                    df.loc[lat, lon, trng_period.tini].prcp = 0.1
                    df.loc[lat, lon, trng_period.tend].prcp = 0.1
            del df1
            # ds = xr.open_dataset('/home/dbonhaure/PycharmProjects/PyCPT/chirps/chirps_recortado.nc')
            # ds.precip[0, :, :].plot.imshow()
            # plot(df1.index.get_level_values('longitude'), df1.index.get_level_values('latitude'), 'bo', color='red')

        # df_only_trng_period = df.query(f'year >= {trng_period.tini} and year <= {trng_period.tend}').copy()
        # for delta in range(fcst_data.nfcsts):
        #     orig_year = fcst_data.fyr + delta
        #     df_orig_year = df.query(f'year == {orig_year}').copy()
        #     if not df_orig_year.empty:
        #         df_new_year = df_orig_year.index.to_frame().reset_index(drop=True)
        #         df_new_year['year'] = trng_period.tend + delta + 1
        #         df_new_year[self.predictand] = df_orig_year[self.predictand].to_list()
        #         df_new_year.set_index(['latitude', 'longitude', 'year'], inplace=True)
        #         df_only_trng_period = df_only_trng_period.append(df_new_year)

        # Save file in cpt format
        cpt_procesor = CPTFileProcessor(self.grid.longitudes, self.grid.latitudes)
        cpt_procesor.dataframe_to_cpt_format_file(self.data_source, self.abs_path, df)

        # report status
        pb.update_count(10)

        # Reportar que el archivo ya fue actualizado
        self.update_ctrl.report_updated_file(self.abs_path)

        # close progress bar
        pb.close()


@dataclass
class PredictorXVariables:
    spatial_domain: SpatialDomain

    # Predictor (choose between precip, tmp2m)
    predictor: str

    # Data source (choose between noaa, iridl)
    data_source: str

    # File that contains input data
    file_obj: PredictorFile = None

    # Minimum number of X modes
    xmodes_min: int = 1
    # Maximum number of X modes
    xmodes_max: int = 10


@dataclass
class PredictandYVariables:
    spatial_domain: SpatialDomain

    # Predictand (choose between prcp, t2m)
    predictand: str

    # Data source (choose between chirps, era5-land)
    data_source: str

    # File that contains input data
    file_obj: PredictandFile = None

    # Indicates the file-type.
    # Valid file types are "station files" and "unreferenced or index files"
    station: bool = False

    # Minimum number of Y modes
    ymodes_min: int = 1
    # Maximum number of Y modes
    ymodes_max: int = 10

    # Minimum number of CCA modes
    ccamodes_min: int = 1
    # Maximum number of CCAmodes
    ccamodes_max: int = 10


@dataclass
class OutputFile:
    name: str = None
    folder: str = None

    # Upload cpt config file (It's a singleton!!)
    config_file: ConfigFile = field(init=False, default=ConfigFile.Instance())

    model: InitVar[str] = None
    fcst_data: InitVar[ForecastData] = None
    trgt_season: InitVar[TargetSeason] = None
    trng_period: InitVar[TrainingPeriod] = None
    predictor_data: InitVar[PredictorXVariables] = None
    predictand_data: InitVar[PredictandYVariables] = None
    swap_years: InitVar[bool] = None

    @property
    def abs_path(self):
        return os.path.join(self.folder, self.name) if self.name else ""

    def __post_init__(self, model, fcst_data, trgt_season, trng_period, predictor_data, predictand_data, swap_years):
        # Define file name
        self.name = self.__define_filename(model, fcst_data, trgt_season, trng_period, predictor_data, predictand_data)
        # Define folder
        self.folder = self.config_file.get('folders').get('output')
        # Report output file to plotting module
        self.__add_filename_to_plot_yaml(trgt_season, predictor_data, swap_years)

    @staticmethod
    def __define_filename(model, fcst_data, trgt_season, trng_period, predictor_data, predictand_data) -> str:
        months_indexes = MonthsProcessor.month_abbr_to_month_num_as_str(trgt_season.tgts)
        years_to_str = YearsProcessor.fcst_data_to_years_str(fcst_data, trgt_season, fcst_data.fyr)
        return f"{model}_{predictor_data.predictor}-{predictand_data.predictand}_" \
               f"{predictand_data.data_source}_{fcst_data.monf}ic_{months_indexes}_" \
               f"{trng_period.tini}-{trng_period.tend}_{years_to_str}_{fcst_data.nfcsts}.txt"

    def __add_filename_to_plot_yaml(self, trgt_season, predictor_data, swap_years):
        with open(self.config_file.get('files').get('plot_yaml'), 'a') as fp_plot_yaml:
            fp_plot_yaml.write(f' - {{ file: "{self.name}", type: "{trgt_season.type}", swap_years: {swap_years} }}\n')
