import os

from datetime import datetime

# Change current directory
if os.path.dirname(__file__):
    os.chdir(os.path.dirname(__file__))

from pycpt import CPT

if __name__ == '__main__':
    print(f'Starting pyCPT at {datetime.now().strftime("%Y/%m/%d %H:%M:%S")}!')

    cpt = CPT(cpt_bin_dir='/opt/CPT/bin')

    cpt.run()

    if cpt.detected_errors:
        print('\nInicio del Reporte de Errores:')
        for e in cpt.detected_errors:
            print(e)
        print('Fin del Reporte de Errores\n')

    print(f'PyCPT finished at {datetime.now().strftime("%Y/%m/%d %H:%M:%S")}!')


