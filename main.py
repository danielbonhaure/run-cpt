#!/usr/bin/env python

import os

if os.path.dirname(__file__):
    os.chdir(os.path.dirname(__file__))  # Change current directory

import argparse
import logging
import time

from datetime import datetime

from pycpt import CPT
from errors import CPTRuntimeError
from script import ScriptControl


def parse_args() -> argparse.Namespace:

    parser = argparse.ArgumentParser(description='Run PyCPT')
    parser.add_argument('--year', type=int, default=datetime.now().year,
        help='Indicates the year that should be considered in the forecast calibration process.')
    parser.add_argument('--month', type=int, default=datetime.now().month,
        help='Indicates the month that should be considered in the forecast calibration process.')

    return parser.parse_args()


def run_pycpt(args: argparse.Namespace):

    # Create CPT object
    cpt = CPT(cpt_bin_dir='/opt/CPT/bin')

    # Run forecast calibration
    cpt.run(initial_year=args.year, initial_month=args.month)

    # Report CPT errors
    if cpt.detected_errors:
        print('\nInicio del Reporte de Errores:')
        for e in cpt.detected_errors:
            print(e)
        print('Fin del Reporte de Errores\n')
        raise CPTRuntimeError()


if __name__ == '__main__':

    # Catch and parse command-line arguments
    parsed_args: argparse.Namespace = parse_args()

    try:
        # Get current time
        start = time.time()
        # Create script control
        script = ScriptControl('pycpt')
        # Start script execution
        script.start_script()
        # Execute PyCPT
        run_pycpt(parsed_args)

    except CPTRuntimeError:
        error_detected = True

    except Exception:
        error_detected = True
        raise  # see: http://www.markbetz.net/2014/04/30/re-raising-exceptions-in-python/

    except SystemExit:
        error_detected = False  # when script raise SystemExit

    else:
        error_detected = False
        script.end_script_execution()

    finally:
        end = time.time()
        err_pfx = "with" if error_detected else "without"
        logging.info(f"Total time to run PyCPT ({err_pfx} errors): {end - start}")
