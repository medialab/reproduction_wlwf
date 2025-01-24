'''
The aim of this script is to create a database to an accomodating format for VAR model implementation
'''

import os
import numpy as np 
import casanova
import argparse

parser = argparse.ArgumentParser()
parser.add_argument(
    "path",
    help="Path to a folder containing all CSV Time Series. Moreover, the output file will be exported in this directory",
    type=existing_dir_path,
)

parser.add_argument( #You should use it if you obtained data using small option in the precedent scripts
    "--small",
    help=("run the script on a reduce part of data"),
    action="store_true",
)



