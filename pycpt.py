
import os
import yaml
import subprocess

from typing import List
from dataclasses import dataclass, field, InitVar

from errors import ConfigError
from helpers import MonthsProcessor, ProgressBar


@dataclass
class TargetSeason:
    mons: str  # ,'Feb','Mar','Apr','May','Jun','Jul','Aug']
    tgts: str  # ,'Mar-May','Apr-Jun','May-Jul','Jun-Aug','Jul-Sep']


@dataclass
class ForecastData:
    fyr: int  # Forecast first year
    monf: str  # Initialization month
    nfcsts: int  # Number of forecasts


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
class PredictorXVariables:
    spatial_domain: SpatialDomain

    # Predictor (choose between GCM's PRCP, VQ, UQ, T2M)
    # VQ and UQ only works with models=['NCEP-CFSv2']
    predictor: str

    # File that contains input data
    file_name: str = None

    # Minimum number of X modes
    xmodes_min: int = 1
    # Maximum number of X modes
    xmodes_max: int = 10

    def set_file_name(self, model: str, fcst_data: ForecastData, trng_period: TrainingPeriod,
                      trgt_season: TargetSeason):
        variable = 'precip' if self.predictor == 'PRCP' else 'tmp2m'
        months_indexes = MonthsProcessor.month_abbr_to_month_num_as_str(trgt_season.tgts)
        self.file_name = f"input/{model}_{variable}_{fcst_data.monf}ic_{months_indexes}_" \
                         f"{trng_period.tini}-{trng_period.tend}_" \
                         f"{fcst_data.fyr}-{fcst_data.fyr + fcst_data.nfcsts - 1}.txt"


@dataclass
class PredictandYVariables:
    spatial_domain: SpatialDomain

    # Predictand (choose between PRCP, RFREQ)
    predictand: str

    # File that contains input data
    file_name: str = None

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

    def set_file_name(self, fcst_data: ForecastData, trgt_season: TargetSeason):
        variable = 'PRECIPITACION' if self.predictand == 'PRCP' else 'TEMPERATURA'
        months_indexes = MonthsProcessor.month_abbr_to_month_num_as_str(trgt_season.tgts)
        self.file_name = f"input/{variable}_{months_indexes}_{fcst_data.fyr}.txt"


@dataclass
class ForecastVariables:
    spatial_domain: SpatialDomain

    # Variable (choose between PRCP, T2M)
    variable: str

    # File that contains input data
    file_name: str = None

    def set_file_name(self, model: str, fcst_data: ForecastData, trgt_season: TargetSeason):
        variable = 'precip' if self.variable == 'PRCP' else 'tmp2m'
        months_abbr = MonthsProcessor.month_abbr_to_month_num_as_str(trgt_season.tgts)
        add_year = MonthsProcessor.target_in_next_year(fcst_data.monf, trgt_season.tgts)
        self.file_name = f"input/{model}_{variable}_fcst_{fcst_data.monf}ic_{months_abbr}_" \
                         f"{fcst_data.fyr}-{fcst_data.fyr + 1 if add_year else fcst_data.fyr}.txt"


@dataclass
class OutputFile:

    # File that contains ouput data
    file_name: str = None

    def set_file_name(self, model: str, target_season: TargetSeason,
                      predictor_data: PredictorXVariables, predictand_data: PredictandYVariables,
                      forecast_data: ForecastData):
        months_abbr = MonthsProcessor.month_abbr_to_month_num_as_str(target_season.tgts)
        self.file_name = f"output/{model}_{predictor_data.predictor}-{predictand_data.predictand}_" \
                         f"{forecast_data.monf}ic_{months_abbr}_{forecast_data.fyr}.txt"


@dataclass
class ConfigCPT:
    """Class ..."""

    # Model (choose one model:
    # NMME, CanSIPSv2*, CMC1-CanCM3, CMC2-CanCM4, COLA-RSMAS-CCSM3, COLA-RSMAS-CCSM4*,
    # GFDL-CM2p1, GFDL-CM2p5-FLOR-A06*, GFDL-CM2p5-FLOR-B01*, NASA-GEOSS2S*, NCAR-CESM1, NCEP-CFSv2*)
    # The ones with "*" are producing operation#al forecasts, the others are frozen.
    # CanSIPSv2 forecasts are ONLY AVAILABLE after Aug 2019!!!!
    model: str

    # Mutable Data
    target_season: TargetSeason
    forecast_data: ForecastData
    predictor_data: PredictorXVariables
    predictand_data: PredictandYVariables

    # Model Output Statistic (MOS) method (choose between None, PCR, CCA)
    mos: str = 'CCA'

    # The training period can be inferred by others properties.
    training_period: TrainingPeriod = field(init=False)

    # The output file can be inferred by others properties.
    output_file: OutputFile = None

    def __post_init__(self):
        self.training_period = TrainingPeriod(self.target_season)
        self.predictor_data.set_file_name(self.model, self.forecast_data, self.training_period, self.target_season)
        self.predictand_data.set_file_name(self.forecast_data, self.target_season)
        if not self.output_file:
            self.output_file = OutputFile()
        self.output_file.set_file_name(self.model, self.target_season, self.predictor_data, self.predictand_data,
                                       self.forecast_data)

    def create_params_file(self):
        """Function to write CPT namelist file"""

        f = open(self.output_file.file_name.replace('.txt', '_params'), "w")

        if self.mos == 'CCA':
            # Opens CCA
            f.write("611\n")
        else:
            print("mos option is invalid")

        # First, ask CPT to stop if error is encountered
        f.write("571\n")
        f.write("3\n")

        # Opens X input file
        f.write("1\n")
        f.write(f"{self.predictor_data.file_name}\n")
        # Nothernmost latitude
        f.write(f"{str(self.predictor_data.spatial_domain.nla)}\n")
        # Southernmost latitude
        f.write(f"{str(self.predictor_data.spatial_domain.sla)}\n")
        # Westernmost longitude
        f.write(f"{str(self.predictor_data.spatial_domain.wlo)}\n")
        # Easternmost longitude
        f.write(f"{str(self.predictor_data.spatial_domain.elo)}\n")
        if self.mos == 'CCA' or self.mos == 'PCR':
            # Minimum number of X modes
            f.write("{}\n".format(self.predictor_data.xmodes_min))
            # Maximum number of X modes
            f.write("{}\n".format(self.predictor_data.xmodes_max))

        # Opens Y input file
        f.write("2\n")
        f.write(f"{self.predictand_data.file_name}\n")
        if not self.predictand_data.station:
            # Nothernmost latitude
            f.write(f"{str(self.predictand_data.spatial_domain.nla)}\n")
            # Southernmost latitude
            f.write(f"{str(self.predictand_data.spatial_domain.sla)}\n")
            # Westernmost longitude
            f.write(f"{str(self.predictand_data.spatial_domain.wlo)}\n")
            # Easternmost longitude
            f.write(f"{str(self.predictand_data.spatial_domain.elo)}\n")
        if self.mos == 'CCA':
            # Minimum number of Y modes
            f.write("{}\n".format(self.predictand_data.ymodes_min))
            # Maximum number of Y modes
            f.write("{}\n".format(self.predictand_data.ymodes_max))

            # Minimum number of CCA modes
            f.write("{}\n".format(self.predictand_data.ccamodes_min))
            # Maximum number of CCAmodes
            f.write("{}\n".format(self.predictand_data.ccamodes_max))

        # X training period settings
        f.write("4\n")
        # First year of X training period
        f.write(str(self.training_period.tini)+'\n')
        # Y training period settings
        f.write("5\n")
        # First year of Y training period
        f.write(str(self.training_period.tini)+'\n')

        # Forecast period settings
        f.write("6\n")
        # First year of training period
        f.write(str(self.training_period.tini)+'\n')

        # Length of training period
        f.write("7\n")
        f.write(str(self.training_period.ntrain)+'\n')

        # Number of forecasts
        f.write("9\n")
        f.write(str(self.training_period.ntrain + self.forecast_data.nfcsts)+'\n')

        # Build model and save outputs
        if self.mos == 'CCA' or self.mos == 'PCR':  # Don't use CPT to compute probabilities if MOS='None'

            # Select output format
            f.write("131\n")
            # ASCII format
            f.write("2\n")

            # Build cross-validated model
            f.write("311\n")  # In the seasonal case, training periods are usually too short to do retroactive analysis

            # Generate Forecasts Series
            f.write("451\n")

            # Output results
            f.write("111\n")
            # Save deterministic forecasts [mu for Gaussian fcst pdf]
            f.write("511\n")
            f.write(f"{self.output_file.file_name}\n")

        # Exit
        f.write("0\n")
        f.write("0\n")
        f.close()


class CPT:

    def __init__(self, config_file: str = 'config.yaml', cpt_bin_dir: str = None):
        self.config_file: str = config_file
        self.cpt_bin_dir: str = cpt_bin_dir
        self.config: dict = self.load_config()

    def load_config(self) -> dict:
        if not os.path.exists(self.config_file):
            raise ConfigError(f"Configuration file (i.e. {self.config_file}) not found!")
        with open(self.config_file, 'r') as f:
            return yaml.safe_load(f)

    def setup_cpt_environment(self) -> None:
        """Setup CPT environment"""
        if self.cpt_bin_dir and self.cpt_bin_dir != os.environ.get('CPT_BIN_DIR'):
            os.environ["CPT_BIN_DIR"] = self.cpt_bin_dir
        if not os.environ.get('CPT_BIN_DIR'):
            raise ConfigError(f"La variable de entorno CPT_BIN_DIR no ha sido definida!")

    def create_targets(self) -> List[ConfigCPT]:
        for model in self.config.get('models'):
            cpt_config: ConfigCPT = ConfigCPT(
                model=model,
                target_season=TargetSeason(mons='Jan', tgts='Feb-Apr'),
                forecast_data=ForecastData(monf='Jan', fyr=2020, nfcsts=2),
                predictor_data=PredictorXVariables(spatial_domain=SpatialDomain(-10, -60, 270, 330), predictor='PRCP'),
                predictand_data=PredictandYVariables(spatial_domain=SpatialDomain(-10, -52, -78, -36), predictand='PRCP')
            )
            yield cpt_config

    @staticmethod
    def lines_that_contain(string, fp):
        return [line for line in fp if string in line]

    def run(self) -> None:
        """Run CPT"""

        # Create progress bar
        run_status = f'Running CPT (PID: {os.getpid()})'
        progress_bar = ProgressBar(len(self.config.get('models')), run_status)
        progress_bar.report_advance(0)

        for t in self.create_targets():

            # Create params file
            t.create_params_file()

            # Define files to be used
            params_file = t.output_file.file_name.replace('.txt', '_params')
            cpt_logfile = t.output_file.file_name.replace('.txt', '.log')

            # Set up CPT environment
            self.setup_cpt_environment()

            # Run CPT
            try:
                print("fake run")
                # subprocess.check_output(f"{self.cpt_bin_dir}/CPT.x < {params_file} > {cpt_logfile}",
                #                         stderr=subprocess.STDOUT, shell=True)
            except subprocess.CalledProcessError as e:
                print(e.output.decode())
                raise
            else:
                progress_bar.report_advance(1)

            # Check for errors
            with open(cpt_logfile, "r") as fp:
                for line in self.lines_that_contain("Error:", fp):
                    print(line)

        # Close progress bar
        progress_bar.close()

        print('PROCESS COMPLETED')
