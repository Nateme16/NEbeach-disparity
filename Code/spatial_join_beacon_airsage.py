# -*- coding: utf-8 -*-
"""
Created on Tues 2/28/2023
@author: EBURMA01
README: 
Script to spatially join Airsage cell data and EPA water quality data 
"""

#%%---------------------------------------------------------------------------
'''IMPORT PACKAGES AND SET VARIABLES AND WORKSPACE'''
#_____________________________________________________________________________

# Before running this cell, debug (ctrl-F5). Otherwise, beachEJFunctions might not 
# import. 

import arcpy 
from arcpy import env as env
import beachEJFunctions as fun
import pandas as pd 
import os
import numpy as np
import datetime


# everything from here below needs to be re-adapted to this project
# Do we want a flexible environment so others can run this? 
# This would be hard without getting everyone set up with an ArcPy 
# environment 

#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
# Set whether or not you want to create a fresh, empty workspace for this script
# (ONLY do this the first time; otherwise, you'll have to run everything again)
createNewWorkspace = False
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


#Set your working directory (address of the project folder)
workingDirectory = r'xyz\project folder'

# Set the folder where the data is 
dataFolder = os.path.join(workingDirectory, 'Data')

## Set the folder where you want to send (non-fc) outputs 
#outFolder = os.path.join(workingDirectory , 'Outputs')

# Set the name of the arcPy workspace, whether new or old 
workspacePath = workingDirectory
workspaceName = 'beachEJ.gdb'

#%%---------------------------------------------------------------------------

if createNewWorkspace == True: 
    # Make a gdb workspace. Delete and remake if named gdb already exists.
    env.workspace = fun.makeGDB(
        gdbOutPath =  workspacePath,
        gdbName = workspaceName)

else: 
    # Set the ArcPy workspace, which is a geodatabase (gdb) where all the output 
    # files go. 
    env.workspace = os.path.join(workspacePath, workspaceName)
    
#%%---------------------------------------------------------------------------
 
# bring in beach data
# bring in airsage 
    
# spatial join with tolerance 
    
    