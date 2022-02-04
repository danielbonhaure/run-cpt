
from components import *

from errors import ConfigError
from helpers import ConfigProcessor, ProgressBar, SecondaryProgressBar

import os
import subprocess
import pathlib
import signal

from typing import List
from dataclasses import dataclass
from datetime import date


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
    trng_period: TrainingPeriod
    target_season: TargetSeason
    forecast_data: ForecastData
    predictor_data: PredictorXVariables
    predictand_data: PredictandYVariables

    # Model Output Statistic (MOS) method (choose between None, PCR, CCA)
    mos: str = 'CCA'

    # The output file can be inferred by others properties.
    output_file: OutputFile = None

    # Indicates whether the years should be swapped when reading output file
    swap_years: bool = None

    def __post_init__(self):
        self.swap_years = True if self.predictor_data.data_source == 'noaa' else False

        if not self.predictor_data.file_obj:
            self.predictor_data.file_obj = PredictorFile(
                predictor=self.predictor_data.predictor,
                data_source=self.predictor_data.data_source,
                model=self.model,
                fcst_data=self.forecast_data,
                trgt_season=self.target_season,
                trng_period=self.trng_period
            )
        if not self.predictand_data.file_obj:
            self.predictand_data.file_obj = PredictandFile(
                predictand=self.predictand_data.predictand,
                data_source=self.predictand_data.data_source,
                fcst_data=self.forecast_data,
                trgt_season=self.target_season,
                trng_period=self.trng_period
            )
        if not self.output_file:
            self.output_file = OutputFile(
                model=self.model,
                fcst_data=self.forecast_data,
                trgt_season=self.target_season,
                trng_period=self.trng_period,
                predictor_data=self.predictor_data,
                predictand_data=self.predictand_data,
                swap_years=self.swap_years
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
        f.write(str(self.trng_period.tini)+'\n')
        # Y training period settings
        f.write("5\n")
        # First year of Y training period
        f.write(str(self.trng_period.tini)+'\n')

        # Forecast period settings
        f.write("6\n")
        # First year of training period
        f.write(str(self.trng_period.tini)+'\n')

        # Length of training period
        f.write("7\n")
        f.write(str(self.trng_period.ntrain)+'\n')

        # Number of forecasts
        f.write("9\n")
        if self.swap_years:
            f.write(str(self.trng_period.ntrain + self.forecast_data.nfcsts)+'\n')
        else:
            f.write(str(self.trng_period.ntrain + (self.forecast_data.fyr - self.trng_period.original_tend))+'\n')

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
            # Save probabilistic forecasts
            f.write("501\n")
            file, ext = os.path.splitext(self.output_file.abs_path)
            f.write(f"{file}_prob{ext}\n")

        # Exit
        f.write("0\n")
        f.write("0\n")
        f.close()


class CPT:

    def __init__(self, cpt_bin_dir: str, config_filename: str = 'config.yaml'):
        self.cpt_bin_dir: str = cpt_bin_dir
        self.cpt_executable: str = f"{self.cpt_bin_dir}/CPT.x"
        self.detected_errors: List[str] = []
        self.__setup_configuration_singletons(config_filename)
        self.__check_and_create_folders()
        self.__create_and_setup_progress_bar()

    def __setup_configuration_singletons(self, config_filename):
        """Setup the two configuration singletons used in components.py"""
        self.config_file: ConfigFile = ConfigFile.Instance()
        if config_filename != 'config.yaml':
            self.config_file.file_name = config_filename

        self.update_ctrl: UpdateControl = UpdateControl.Instance()
        if config_filename != 'config.yaml':
            self.update_ctrl.config_file = config_filename

    def __check_and_create_folders(self):
        """Create directories to storage data (if needed)"""
        for folder in ConfigProcessor.nested_dict_values(self.config_file.get('folders')):
            if not os.access(pathlib.Path(folder).parent, os.W_OK):
                err_msg = f"{pathlib.Path(folder).parent} is not writable"
                raise ConfigError(err_msg)
            pathlib.Path(folder).mkdir(parents=True, exist_ok=True)

    def __create_and_setup_progress_bar(self):
        """Create and setup progress bar"""
        run_status = f'Running CPT (PID: {os.getpid()})'
        total_targets = ConfigProcessor.count_iterations(self.config_file.get('models'))
        self.progress_bar = ProgressBar((110 * total_targets) / 100.0, run_status)

    def setup_cpt_environment(self) -> None:
        """Setup CPT environment"""
        if self.cpt_bin_dir and self.cpt_bin_dir != os.environ.get('CPT_BIN_DIR'):
            os.environ["CPT_BIN_DIR"] = self.cpt_bin_dir
        if not os.environ.get('CPT_BIN_DIR'):
            raise ConfigError(f"La variable de entorno CPT_BIN_DIR no ha sido definida!")

    def kill_running_cpt(self) -> None:
        """Kill all cpt instances currently running"""
        for line in os.popen(f"ps ax | grep {self.cpt_executable} | grep -v grep"):
            os.kill(int(line.split()[0]), signal.SIGKILL)

    @staticmethod
    def lines_that_contain(string, fp):
        return [line for line in fp if string in line]

    def __run_cpt_for_a_specific_target(self, target: ConfigCPT, advance_to_be_reported: float):
        """Run CPT for a specific target"""

        # Define cpt params file
        params_file = target.output_file.abs_path.replace('.txt', '_params')

        # Create params file
        target.create_params_file(params_file)

        # Check if CPT must be executed or not
        if self.config_file.get('run_cpt', True):

            # Create progress bar to track interpolation
            run_status = f'Running CPT.x < {params_file} (PID: {os.getpid()})'
            pb = SecondaryProgressBar(5, run_status)

            # Open progress bar
            pb.open()

            # Set up CPT environment
            self.setup_cpt_environment()

            # report status
            pb.update_count(1)

            # Define cpt log file
            cpt_logfile = target.output_file.abs_path.replace('.txt', '.log')

            # report status
            pb.update_count(2)

            # Run CPT
            try:
                subprocess.check_output(f"{self.cpt_executable} < {params_file} > {cpt_logfile}",
                                        stderr=subprocess.STDOUT, shell=True, timeout=180)
            except subprocess.CalledProcessError as e:
                self.kill_running_cpt()
                print(f'\n{e.output.decode()}')
                raise
            except subprocess.TimeoutExpired as e:
                self.kill_running_cpt()
                self.detected_errors.append(str(e))

            # report status
            pb.update_count(4)

            # Check for errors
            with open(cpt_logfile, "r") as fp:
                add_lines = False
                for line in fp:
                    if not add_lines and any(x in line for x in ["Error:", "ERROR:"]):
                        self.detected_errors.append(f'Error in file: {cpt_logfile}')
                        add_lines = True
                    if add_lines and any(x in line for x in ["_"*70, 'Select option:']):
                        add_lines = False
                    self.detected_errors.append(f"-- {line.strip()}") if add_lines else None

            # report status
            pb.update_count(5)

            # close progress bar
            sleep(0.5)
            pb.close()

        # Report progress
        self.progress_bar.report_advance(advance_to_be_reported)

    def __create_targets_and_run_cpt_for_them(self):
        """Create targets an run CPT for each of them"""

        # Compute marginal progress bar advance (to reporte aditional advances)
        marginal_advance = ConfigProcessor.count_iterations(self.config_file.get('models')) * 10 / 100

        # Get spatial domains from config
        spd_predictor = SpatialDomain(**self.config_file.get('spatial_domain').get('predictor'))
        spd_predictand = SpatialDomain(**self.config_file.get('spatial_domain').get('predictand'))

        # Report progress
        self.progress_bar.report_advance(marginal_advance/5)

        # Get target season and forecast data from config
        trgt_data, fcst_data = self.config_file.get('target_season'), self.config_file.get('forecast_data')

        # If one of them is None, then the setting is wrong
        if trgt_data and not fcst_data or not trgt_data and fcst_data:
            raise ConfigError

        # Report progress
        self.progress_bar.report_advance(marginal_advance/5)

        # Set targets seasons
        targets: List[TargetSeason]
        if not trgt_data:
            initial_month = self.config_file.get('initial_month', date.today().month)
            tds = MonthsProcessor.gen_trgts_data(initial_month)
            targets = [TargetSeason(**td) for td in tds]
        elif [str, str] == [type(trgt_data.get(k)) for k in trgt_data.keys()]:
            targets = [TargetSeason(**trgt_data)]
        elif all([type(trgt_data.get(k)) == list for k in trgt_data.keys()]):
            zipped_data = zip(trgt_data.get('mons'), trgt_data.get('tgts'))
            targets = [TargetSeason(mons=m, tgts=t) for m, t in zipped_data]
        else:
            raise ConfigError

        # Report progress
        self.progress_bar.report_advance(marginal_advance/5)

        # Set forecasts data
        forecasts: List[ForecastData]
        if not fcst_data:
            initial_year = self.config_file.get('initial_year', date.today().year)
            initial_month = self.config_file.get('initial_month', date.today().month)
            fd = MonthsProcessor.gen_fcsts_data(initial_year, initial_month)
            forecasts = [ForecastData(**fd) for _ in range(len(targets))]
        elif [int, str, int] == [type(fcst_data.get(k)) for k in fcst_data.keys()] and \
                not all([type(trgt_data.get(k)) == list for k in trgt_data.keys()]):
            forecasts = [ForecastData(**fcst_data)]
        elif [int, str, int] == [type(fcst_data.get(k)) for k in fcst_data.keys()] and \
                all([type(trgt_data.get(k)) == list for k in trgt_data.keys()]):
            forecasts = [ForecastData(**fcst_data) for _ in range(len(targets))]
        elif all([type(fcst_data.get(k)) == list for k in fcst_data.keys()]):
            zipped_data = zip(fcst_data.get('fyr'), fcst_data.get('monf'), fcst_data.get('nfcsts'))
            forecasts = [ForecastData(fyr=y, monf=m, nfcsts=n) for y, m, n in zipped_data]
        else:
            raise ConfigError

        # Report progress
        self.progress_bar.report_advance(marginal_advance/5)

        # Generate ConfigCPT instances
        for model in self.config_file.get('models').keys():

            all_predictors = self.config_file.get('models').get(model).get('predictors').get('variables')
            all_predictors_data_sources = (self.config_file.get('models').get(model)
                                           .get('predictors').get('data_sources'))
            all_predictands = self.config_file.get('models').get(model).get('predictands').get('variables')
            all_predictands_data_sources = (self.config_file.get('models').get(model)
                                            .get('predictands').get('data_sources'))

            for predictor, predictor_data_source, predictand, predictand_data_source \
                    in zip(all_predictors, all_predictors_data_sources, all_predictands, all_predictands_data_sources):

                for a_trgt_season, a_fcst_data in zip(targets, forecasts):

                    # IRI Climate Data Library has data up to 5 months ahead of the initial month
                    mf = MonthsProcessor.num_of_months_from_month_to_month
                    if predictor_data_source == 'iridl' and (
                            mf(a_fcst_data.init_month_int, a_trgt_season.first_trgt_month_int) > 5 or
                            mf(a_fcst_data.init_month_int, a_trgt_season.last_trgt_month_int) > 5):
                        # Report progress
                        self.progress_bar.report_advance(1/len(targets))
                        # Break current iteration and continue with next iteration
                        continue

                    # NOAA ftp service has data up to 6 months ahead of the initial month
                    mf = MonthsProcessor.num_of_months_from_month_to_month
                    if predictor_data_source == 'noaa' and (
                            mf(a_fcst_data.init_month_int, a_trgt_season.first_trgt_month_int) > 6 or
                            mf(a_fcst_data.init_month_int, a_trgt_season.last_trgt_month_int) > 6):
                        # Report progress
                        self.progress_bar.report_advance(1/len(targets))
                        # Break current iteration and continue with next iteration
                        continue

                    # Define training period
                    a_trng_period = TrainingPeriod(
                        tini=(self.config_file.get('models').get(model)
                              .get('training_period', {}).get('fyr', TrainingPeriod.tini)),
                        tend=(self.config_file.get('models')
                              .get(model).get('training_period', {}).get('lyr', TrainingPeriod.tend)),
                        trgt_season=a_trgt_season
                    )

                    # Create target
                    cpt_config: ConfigCPT = ConfigCPT(
                        model=model,
                        trng_period=a_trng_period,
                        target_season=a_trgt_season,
                        forecast_data=a_fcst_data,
                        predictor_data=PredictorXVariables(
                            spatial_domain=spd_predictor,
                            predictor=predictor,
                            data_source=predictor_data_source
                        ),
                        predictand_data=PredictandYVariables(
                            spatial_domain=spd_predictand,
                            predictand=predictand,
                            data_source=predictand_data_source,
                        )
                    )

                    # Report progress
                    self.progress_bar.report_advance(0.5/len(targets))

                    # Run CPT for the created target
                    self.__run_cpt_for_a_specific_target(cpt_config, 0.5/len(targets))

        # Report progress
        self.progress_bar.report_advance(marginal_advance/5)

    def run(self) -> None:
        """Run CPT"""

        # Open/start progress bar
        self.progress_bar.open()

        # Run CPT
        self.__create_targets_and_run_cpt_for_them()

        # Close progress bar
        self.progress_bar.close()

        print('PROCESS COMPLETED')
