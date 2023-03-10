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

#Set your working directory (address of the project folder)
workingDirectory = r'C:\Users\EBURMA01\Environmental Protection Agency (EPA)\ACESD Social Science Team - General\Research Projects\Beach research STRAP4\New England beach cell data\NEbeach-disparity\data'

# Set the folder where the data is 
dataFolder = r'C:\Users\EBURMA01\Environmental Protection Agency (EPA)\ACESD Social Science Team - General\Research Projects\Beach research STRAP4\New England beach cell data\NEbeach-disparity\data'


# Set the name of the arcPy workspace, whether new or old 
workspacePath = workingDirectory
workspaceName = 'beachEJ.gdb'
# Make a gdb workspace. Delete and remake if named gdb already exists.
env.workspace = fun.makeGDB(
        gdbOutPath =  workspacePath,
        gdbName = workspaceName)

#%%---------------------------------------------------------------------------

# bring in beach data
# turn it into points fc
# !!!is currently in RI state plane, could change projection
beachDic = {'beach':'resultphyschem.csv'}
fun.inPoints(beachDic, dataFolder)
# bring in airsage 
airDic = {'airSage' : 'NEandNY_150_d_f_Simplify_Final.shp'}
fun.putGDB(airDic, dataFolder)

    
# spatial join with tolerance 
infile = 'airSage'
jFile = 'beach'
outFile = 'beachAirsageJoin'
arcpy.analysis.SpatialJoin(infile, jFile, outFile,
#                           maybe do field mapping?
#                               field_mapping=fieldMappings,
                               join_operation = 'JOIN_ONE_TO_MANY',
                               match_option = 'WITHIN_A_DISTANCE',
                               search_radius = '200 meters'
                               # {join_operation}, {join_type}, {field_mapping}, {match_option}, {search_radius},
                               # {distance_field_name}
                               )

# convert to data frame and then export to csv

outDf = fun.table_to_data_frame('beachAirsageJoin')
currentDateTime = datetime.datetime.now().strftime("%m-%d-%Y-%H%M%S%p")
outDf.to_csv(path_or_buf= os.path.join(dataFolder, f"beachAirsageJoined_{currentDateTime}.csv"))