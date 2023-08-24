#!/usr/bin/env python

import os
import argparse

from datetime import datetime

# Change current directory
if os.path.dirname(__file__):
    os.chdir(os.path.dirname(__file__))

from pycpt import CPT
from errors import CPTRuntimeError


def main(args: argparse.Namespace):

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

    # Set pid file
    pid_file = '/tmp/pycpt.pid'

    # Get PID and save it to a file
    with open(pid_file, 'w') as f:
        f.write(f'{os.getpid()}')

    # Defines parser data
    parser = argparse.ArgumentParser(description='Run PyCPT')
    parser.add_argument('--year', type=int, default=datetime.now().year,
        help='Indicates the year that should be considered in the forecast calibration process.')
    parser.add_argument('--month', type=int, default=datetime.now().month,
        help='Indicates the month that should be considered in the forecast calibration process.')

    # Extract data from args
    parsed_args = parser.parse_args()

    # Set error as not detected
    error_detected = False

    # Run operational forecast
    try:
        print(f'Starting pyCPT at {datetime.now().strftime("%Y/%m/%d %H:%M:%S")}!')
        main(parsed_args)
    except CPTRuntimeError:
        error_detected = True
    except Exception:
        error_detected = True
        raise  # see: http://www.markbetz.net/2014/04/30/re-raising-exceptions-in-python/
    else:
        error_detected = False
        os.remove(pid_file)  # Remove pid file only if there were no errors
    finally:
        err_pfx = "with" if error_detected else "without"
        print(f'PyCPT finished {err_pfx} errors at {datetime.now().strftime("%Y/%m/%d %H:%M:%S")}!')
