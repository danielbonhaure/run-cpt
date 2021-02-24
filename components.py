
from helpers import MonthsProcessor, FilesProcessor

import os
import shutil

from typing import List, Dict
from dataclasses import dataclass, InitVar, field
from string import Template


@dataclass
class TargetSeason:
    mons: str  # ,'Feb','Mar','Apr','May','Jun','Jul','Aug']
    tgts: str  # ,'Mar-May','Apr-Jun','May-Jul','Jun-Aug','Jul-Sep']

    @property
    def type(self):
        return 'monthly' if len(self.tgts.split('-')) == 1 else 'seasonal'


@dataclass
class ForecastData:
    fyr: int  # Forecast first year
    monf: str  # Initialization month
    nfcsts: int  # Number of forecasts

    @property
    def lyr(self) -> int:  # Forecast last year
        return self.fyr + self.nfcsts - 1


@dataclass
class TrainingPeriod:
    trgt_season: InitVar[TargetSeason] = None
    # Start and end of the training period: (must be >1982 for NMME models. Because of CanSIPSv2, probably end in 2018)
    tini: int = 1982
    tend: int = 2010

    @property
    def ntrain(self) -> int:  # Length of training period
        return self.tend - self.tini + 1

    def __post_init__(self, trgt_season):
        if trgt_season and MonthsProcessor.target_in_next_year(trgt_season.mons, trgt_season.tgts):
            self.tini += 1
            self.tend += 1


@dataclass
class SpatialDomain:
    nla: int  # Northernmost latitude
    sla: int  # Southernmost latitude
    wlo: int  # Westernmost longitude
    elo: int  # Easternmost longitude


@dataclass
class HindcastFile:
    name: str = field(init=False)
    url: str = field(init=False)
    folder: str = field(init=False)

    model: InitVar[str] = None
    predictor: InitVar[str] = None
    fcst_data: InitVar[ForecastData] = None
    trgt_season: InitVar[TargetSeason] = None
    trng_period: InitVar[TrainingPeriod] = None
    cpt_config: InitVar[Dict] = None

    @property
    def abs_path(self):
        return os.path.join(self.folder, self.name) if self.name else ""

    def __post_init__(self, model, predictor, fcst_data, trgt_season, trng_period, cpt_config):
        # Define file name
        self.__define_hindcast_filename(model, predictor, fcst_data, trgt_season, trng_period, cpt_config)
        # Define url
        url_template = Template(cpt_config.get('url').get('templates').get('predictor'))
        self.url = url_template.substitute(trgt_type=trgt_season.type, file_type="hindcast", file_name=self.name)
        # Define folder
        self.folder = cpt_config.get('folders').get('raw_data').get('hindcasts')

    def __define_hindcast_filename(self, model, predictor, fcst_data, trgt_season, trng_period, cpt_config):
        model_name = cpt_config.get('models').get(model).get('hcst_name', model)
        months_indexes = MonthsProcessor.month_abbr_to_month_num_as_str(trgt_season.tgts)
        self.name = f"{model_name}_{predictor}_hcst_{fcst_data.monf}ic_{months_indexes}_" \
                    f"{trng_period.tini}-{trng_period.tend}.txt"


@dataclass
class ForecastFile:
    name: str = field(init=False)
    url: str = field(init=False)
    folder: str = field(init=False)

    model: InitVar[str] = None
    predictor: InitVar[str] = None
    fcst_data: InitVar[ForecastData] = None
    trgt_season: InitVar[TargetSeason] = None
    fcst_year: InitVar[int] = None
    cpt_config: InitVar[Dict] = None

    @property
    def abs_path(self):
        return os.path.join(self.folder, self.name) if self.name else ""

    def __post_init__(self, model, predictor, fcst_data, trgt_season, fcst_year, cpt_config):
        # Define file name
        self.__define_forecast_filename(model, predictor, fcst_data, trgt_season, fcst_year, cpt_config)
        # Define url
        url_template = Template(cpt_config.get('url').get('templates').get('predictor'))
        self.url = url_template.substitute(trgt_type=trgt_season.type, file_type="forecast", file_name=self.name)
        # Define folder
        self.folder = cpt_config.get('folders').get('raw_data').get('forecasts')

    def __define_forecast_filename(self, model, predictor, fcst_data, trgt_season, fcst_year, cpt_config):
        model_name = cpt_config.get('models').get(model).get('fcsf_name', model)
        months_indexes = MonthsProcessor.month_abbr_to_month_num_as_str(trgt_season.tgts)
        self.name = f"{model_name}_{predictor}_fcst_{fcst_data.monf}ic_{months_indexes}_" \
                    f"{fcst_year}-{fcst_year}.txt"


@dataclass
class PredictorFile:
    name: str = field(init=False)
    folder: str = field(init=False)

    hcst_file: HindcastFile = field(init=False)
    fcst_files: List[ForecastFile] = field(init=False, default_factory=list)

    model: InitVar[str] = None
    predictor: InitVar[str] = None
    fcst_data: InitVar[ForecastData] = None
    trgt_season: InitVar[TargetSeason] = None
    trng_period: InitVar[TrainingPeriod] = None
    cpt_config: InitVar[Dict] = None

    @property
    def abs_path(self):
        return os.path.join(self.folder, self.name) if self.name else ""

    def __post_init__(self, model, predictor, fcst_data, trgt_season, trng_period, cpt_config):
        # Define file name
        self.__define_predictor_filename(model, predictor, fcst_data, trgt_season, trng_period)
        # Define folder
        self.folder = cpt_config.get('folders').get('predictors')
        # Create raw files
        self.__create_raw_files(model, predictor, fcst_data, trgt_season, trng_period, cpt_config)
        # Download raw files
        self.__download_raw_files(cpt_config.get('force_raw_data_download', False))
        # Combine raw files
        fcst_years = range(fcst_data.fyr, fcst_data.lyr+1)
        false_years = range(trng_period.tend+1, trng_period.tend+fcst_data.nfcsts+1)
        force_creation = cpt_config.get('force_input_files_creation', False)
        self.__create_predictor_file(fcst_years, false_years, force_creation)

    def __define_predictor_filename(self, model, predictor, fcst_data, trgt_season, trng_period):
        months_indexes = MonthsProcessor.month_abbr_to_month_num_as_str(trgt_season.tgts)
        self.name = f"{model}_{predictor}_{fcst_data.monf}ic_{months_indexes}_" \
                    f"{trng_period.tini}-{trng_period.tend}_{fcst_data.fyr}-{fcst_data.lyr}.txt"

    def __create_raw_files(self, model, predictor, fcst_data, trgt_season, trng_period, cpt_config):
        # Create hindcast file
        self.hcst_file = HindcastFile(model, predictor, fcst_data, trgt_season, trng_period, cpt_config)
        # Create forecast files
        for year in range(fcst_data.fyr, fcst_data.lyr+1):
            self.fcst_files.append(ForecastFile(model, predictor, fcst_data, trgt_season, year, cpt_config))

    def __download_raw_files(self, force_download=False):
        if not os.path.exists(self.hcst_file.abs_path) or force_download:
            FilesProcessor.download_file(self.hcst_file.url, self.hcst_file.abs_path)
        for fcst_file in self.fcst_files:
            if not os.path.exists(fcst_file.abs_path) or force_download:
                FilesProcessor.download_file(fcst_file.url, fcst_file.abs_path)

    def __create_predictor_file(self, fcst_years, false_years, force_creation=False):
        if os.path.exists(self.abs_path):
            return

        # Copiar y renombrar el archivo hindcast
        dst = shutil.copy(self.hcst_file.abs_path, self.abs_path)

        # Agregar el contenido de los fcst_files_path
        with open(self.abs_path, 'a') as fp_comb:
            for fcst_file, fcst_year, false_year in zip(self.fcst_files, fcst_years, false_years):
                with open(fcst_file.abs_path, 'r') as fp_fcst:
                    for line in fp_fcst.readlines():
                        if all(x not in line for x in ['xmlns:cpt', 'cpt:nfields']):
                            if 'cpt:field=' in line:
                                line = line.replace(f"S={fcst_year}", f"S={false_year}")
                                line = line.replace(f"T={fcst_year}", f"T={false_year}")
                            fp_comb.write(line)


@dataclass
class PredictandFile:
    name: str = field(init=False)
    folder: str = field(init=False)

    predictand: InitVar[str] = None
    fcst_data: InitVar[ForecastData] = None
    trgt_season: InitVar[TargetSeason] = None
    cpt_config: InitVar[Dict] = None

    @property
    def abs_path(self):
        return os.path.join(self.folder, self.name) if self.name else ""

    def __post_init__(self, predictand, fcst_data, trgt_season, cpt_config):
        # Define file name
        self.__define_predictand_filename(predictand, fcst_data, trgt_season)
        # Define folder
        self.folder = cpt_config.get('folders').get('predictands')

    def __define_predictand_filename(self, predictand, fcst_data, trgt_season):
        months_indexes = MonthsProcessor.month_abbr_to_month_num_as_str(trgt_season.tgts)
        self.name = f"{predictand}_{months_indexes}_{fcst_data.fyr}.txt"


@dataclass
class PredictorXVariables:
    spatial_domain: SpatialDomain

    # Predictor (choose between GCM's PRCP, VQ, UQ, T2M)
    # VQ and UQ only works with models=['NCEP-CFSv2']
    predictor: str

    # File that contains input data
    file_obj: PredictorFile = None

    # Minimum number of X modes
    xmodes_min: int = 1
    # Maximum number of X modes
    xmodes_max: int = 10


@dataclass
class PredictandYVariables:
    spatial_domain: SpatialDomain

    # Predictand (choose between PRCP, RFREQ)
    predictand: str

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

    model: InitVar[str] = None
    fcst_data: InitVar[ForecastData] = None
    trgt_season: InitVar[TargetSeason] = None
    predictor_data: InitVar[PredictorXVariables] = None
    predictand_data: InitVar[PredictandYVariables] = None
    cpt_config: InitVar[Dict] = None

    @property
    def abs_path(self):
        return os.path.join(self.folder, self.name) if self.name else ""

    def __post_init__(self, model, fcst_data, trgt_season, predictor_data, predictand_data, cpt_config):
        # Define file name
        self.__define_output_filename(model, fcst_data, trgt_season, predictor_data, predictand_data)
        # Define folder
        self.folder = cpt_config.get('folders').get('output')

    def __define_output_filename(self, model, fcst_data, trgt_season, predictor_data, predictand_data):
        months_abbr = MonthsProcessor.month_abbr_to_month_num_as_str(trgt_season.tgts)
        self.name = f"{model}_{predictor_data.predictor}-{predictand_data.predictand}_" \
                    f"{fcst_data.monf}ic_{months_abbr}_{fcst_data.fyr}.txt"
