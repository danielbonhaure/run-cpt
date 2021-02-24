
from components import *

from errors import ConfigError
from helpers import ProgressBar, ConfigProcessor

import os
import yaml
import subprocess
import pathlib

from typing import List, Dict
from dataclasses import dataclass, field, InitVar


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

    # The cpt config file (config.yaml)
    cpt_config: InitVar[Dict] = None

    def __post_init__(self, cpt_config):
        self.training_period = TrainingPeriod(
            trgt_season=self.target_season
        )
        if not self.predictor_data.file_obj:
            self.predictor_data.file_obj = PredictorFile(
                model=self.model,
                predictor=self.predictor_data.predictor,
                fcst_data=self.forecast_data,
                trgt_season=self.target_season,
                trng_period=self.training_period,
                cpt_config=cpt_config
            )
        if not self.predictand_data.file_obj:
            self.predictand_data.file_obj = PredictandFile(
                predictand=self.predictand_data.predictand,
                fcst_data=self.forecast_data,
                trgt_season=self.target_season,
                cpt_config=cpt_config
            )
        if not self.output_file:
            self.output_file = OutputFile(
                model=self.model,
                fcst_data=self.forecast_data,
                trgt_season=self.target_season,
                predictor_data=self.predictor_data,
                predictand_data=self.predictand_data,
                cpt_config=cpt_config
            )

    def create_params_file(self, params_file: str):
        """Function to write CPT namelist file"""

        f = open(params_file, "w")

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
        f.write(f"{self.predictor_data.file_obj.abs_path}\n")
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
        f.write(f"{self.predictand_data.file_obj.abs_path}\n")
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
            f.write(f"{self.output_file.abs_path}\n")

        # Exit
        f.write("0\n")
        f.write("0\n")
        f.close()


class CPT:

    def __init__(self, config_file: str = 'config.yaml', cpt_bin_dir: str = None):
        self.config_file: str = config_file
        self.cpt_bin_dir: str = cpt_bin_dir
        self.config: dict = self.__load_config()
        self.__check_and_create_folders()

    def __load_config(self) -> dict:
        if not os.path.exists(self.config_file):
            raise ConfigError(f"Configuration file (i.e. {self.config_file}) not found!")
        with open(self.config_file, 'r') as f:
            return yaml.safe_load(f)

    def __check_and_create_folders(self):
        """Create directories to storage data (if needed)"""
        for folder in ConfigProcessor.nested_dict_values(self.config.get('folders')):
            if not os.access(pathlib.Path(folder).parent, os.W_OK):
                err_msg = f"{pathlib.Path(folder).parent} is not writable"
                raise ConfigError(err_msg)
            pathlib.Path(folder).mkdir(parents=True, exist_ok=True)

    def setup_cpt_environment(self) -> None:
        """Setup CPT environment"""
        if self.cpt_bin_dir and self.cpt_bin_dir != os.environ.get('CPT_BIN_DIR'):
            os.environ["CPT_BIN_DIR"] = self.cpt_bin_dir
        if not os.environ.get('CPT_BIN_DIR'):
            raise ConfigError(f"La variable de entorno CPT_BIN_DIR no ha sido definida!")

    def create_targets(self) -> List[ConfigCPT]:
        # Get spatial domains from config
        spd_predictor = self.config.get('spatial_domain').get('predictor')
        spd_predictand = self.config.get('spatial_domain').get('predictand')
        # Get target season from config
        trgt_season = self.config.get('target_season')
        # Get forecast data from config
        fcst_data = self.config.get('forecast_data')

        # Generate ConfigCPT instances
        for model in self.config.get('models').keys():
            all_predictors = self.config.get('models').get(model).get('predictors')
            all_predictands = self.config.get('models').get(model).get('predictands')
            for predictor, predictand in zip(all_predictors, all_predictands):
                cpt_config: ConfigCPT = ConfigCPT(
                    model=model,
                    target_season=TargetSeason(**trgt_season),
                    forecast_data=ForecastData(**fcst_data),
                    predictor_data=PredictorXVariables(
                        spatial_domain=SpatialDomain(**spd_predictor),
                        predictor=predictor
                    ),
                    predictand_data=PredictandYVariables(
                        spatial_domain=SpatialDomain(**spd_predictand),
                        predictand=predictand
                    ),
                    cpt_config=self.config
                )
                yield cpt_config

    @staticmethod
    def lines_that_contain(string, fp):
        return [line for line in fp if string in line]

    def run(self) -> None:
        """Run CPT"""

        # Create progress bar
        run_status = f'Running CPT (PID: {os.getpid()})'
        total_targets = ConfigProcessor.count_iterations(self.config.get('models'))
        progress_bar = ProgressBar(total_targets, run_status)

        # Open/start progress bar
        progress_bar.open()

        for t in self.create_targets():

            # Define cpt params file
            params_file = t.output_file.abs_path.replace('.txt', '_params')

            # Create params file
            t.create_params_file(params_file)

            # Check if CPT must be executed or not
            if self.config.get('run_cpt', True):

                # Set up CPT environment
                self.setup_cpt_environment()

                # Define cpt log file
                cpt_logfile = t.output_file.abs_path.replace('.txt', '.log')

                # Run CPT
                try:
                    subprocess.check_output(f"{self.cpt_bin_dir}/CPT.x < {params_file} > {cpt_logfile}",
                                            stderr=subprocess.STDOUT, shell=True)
                except subprocess.CalledProcessError as e:
                    print(e.output.decode())
                    raise

                # Check for errors
                with open(cpt_logfile, "r") as fp:
                    for line in self.lines_that_contain("Error:", fp):
                        print(line)

            # Report progress
            progress_bar.report_advance(1)

        # Close progress bar
        progress_bar.close()

        print('PROCESS COMPLETED')
