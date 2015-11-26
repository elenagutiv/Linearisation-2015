#!/usr/bin/env python3
# Gutierrez Viedma, Elena

# SCATTER PLOT DATA GENERATION SCRIPT

# This file contains instructions to run a batch of tests located in directory P0. For each individual test, it generates P1 and P2 programs located in their
# respective  directories. Then, it runs QARMC to solve each and P0 as well. A .yml file is generated with the results.

# Output location: ../../plot-scripts/translator.yml

import sys
print (sys.hexversion)
if sys.hexversion < 0x03000000:
    sys.exit("Python 26 or newer is required to run this program.")