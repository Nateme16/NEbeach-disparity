# -*- coding: utf-8 -*-
"""
Created on Tue Mar 28 14:06:22 2023

@author: EBURMA01

README: 
Script to join beach lines to cleaned wq data and demographic data, for mapping beach sites 
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


# Set the name of the arcPy workspace
workspacePath = workingDirectory
workspaceName = 'beachEJ.gdb'
# set gdb workspace
env.workspace = os.path.join(workspacePath, workspaceName)


#%%---------------------------------------------------------------------------
# bring in wq data. table has to be arcGIS table view or similar
wqPath = os.path.join(dataFolder, r"bacteria_window.csv")
arcpy.management.MakeTableView(wqPath, 'wqTable')
# bring in demographics data, also make into arcGIS table view
demPath = os.path.join(dataFolder, r"beach_demographics.csv")
arcpy.management.MakeTableView(demPath, 'demTable')

# airsage fc must be layer
arcpy.MakeFeatureLayer_management('airSage', 'airSageLayer')

# join everything together 
arcpy.management.AddJoin('airSageLayer', 'Unique_ID',
                         'wqTable', 'poi',
                         )
arcpy.management.AddJoin('airSageLayer', 'Unique_ID',
                         'demTable', 'Poi',
                         )


# turn back into fc
arcpy.env.overwriteOutput = True
arcpy.CopyFeatures_management('airSageLayer', 'airSageWindowJoin')

# make a point (rather than polygon) version -- better for some kinds of visualizations 

arcpy.management.FeatureToPoint('airSageWindowJoin', 'airSageWindowJoin_pt')


