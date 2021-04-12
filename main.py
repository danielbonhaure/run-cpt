import os
import sys

from datetime import datetime

# Change current directory
os.chdir(os.path.dirname(sys.argv[0]))

from pycpt import CPT

if __name__ == '__main__':
    print(f'Starting pyCPT at {datetime.now().strftime("%Y/%m/%d %H:%M:%S")}!')
    cpt = CPT(cpt_bin_dir='/opt/CPT/15.7.11/bin/')
    cpt.run()
    print(f'PyCPT finished at {datetime.now().strftime("%Y/%m/%d %H:%M:%S")}!')


