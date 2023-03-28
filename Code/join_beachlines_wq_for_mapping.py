# -*- coding: utf-8 -*-
"""
Created on Tue Mar 28 14:06:22 2023

@author: EBURMA01

README: 
Script to join beach lines to cleaned wq data, for mapping 
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

#Set your working directory (address of the project folder)
workingDirectory = r'C:\Users\EBURMA01\Environmental Protection Agency (EPA)\ACESD Social Science Team - General\Research Projects\Beach research STRAP4\New England beach cell data\NEbeach-disparity\data'

# Set the folder where the data is 
dataFolder = r'C:\Users\EBURMA01\Environmental Protection Agency (EPA)\ACESD Social Science Team - General\Research Projects\Beach research STRAP4\New England beach cell data\NEbeach-disparity\data'


# Set the name of the arcPy workspace, whether new or old 
workspacePath = workingDirectory
workspaceName = 'beachEJ.gdb'
# set gdb workspace
env.workspace = os.path.join(workspacePath, workspaceName)


#%%---------------------------------------------------------------------------
# bring in wq data. table has to be arcGIS table view or similar
wqPath = r"C:\Users\EBURMA01\Environmental Protection Agency (EPA)\ACESD Social Science Team - General\Research Projects\Beach research STRAP4\New England beach cell data\NEbeach-disparity\data\bacteria_window.csv"
arcpy.management.MakeTableView(wqPath, 'wqTable')


# fc must be layer
arcpy.MakeFeatureLayer_management('airSage', 'airSageLayer')
                               
arcpy.management.AddJoin('airSageLayer', 'Unique_ID',
                         'wqTable', 'poi',
                         )
# turn back into fc
arcpy.CopyFeatures_management('airSageLayer', 'airSageWindowJoin')


