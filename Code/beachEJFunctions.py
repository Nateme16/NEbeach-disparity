# -*- coding: utf-8 -*-
"""
Created on Tues 2/28/2023

@author: EBURMA01

README: 
Functions for geoprocessing for beach EJ project 
This code does nothing on its own -- it must be imported into a main script

"""

#import packages 

import arcpy
from arcpy import env
import pandas as pd
import os
import numpy as np


def makeGDB(gdbOutPath, gdbName):
    # This function makes a new geodatabase (gdb), and deletes any existing gdb of same name
    # at location
    # Returns a path to the gdb that can be used as an ArcPy workspace
    # 'gdbOutPath': The path to the folder where the gdb will live
    # 'gdbName': the name you want to give the gdb
    gdbPath = (os.path.join(gdbOutPath, gdbName))  # Make complete path
    if arcpy.Exists(gdbPath):
        arcpy.Delete_management(gdbPath)  # delete gdb if it already exists
    arcpy.CreateFileGDB_management(gdbOutPath, gdbName)  # Make the gdb
    return gdbPath  # returns the gdb path

def spatJoin(): 
# description 
    print('do something')


def putGDB(fileObj, inPath, IsGDB=False, suffix = "Pts"):
    # This function puts some vector geodata files (shapefiles or feature classes) 
    # in gdb of choice and optionally renames them according to an alias
    # dictionary. If inputs are not gdb fcs, converts them to fcs.
    # 'fileObj': either a dictionary relating partial pathnames to short aliases,
    # or a list of just partial pathnames, if no renaming is needed 
    # Note: (If importing from csv, fileObj currently must be a dict)
    # 'inPath': the beginning of the path common to all pathnames
    # 'IsGDB': Boolean indicating if inputs are already gdb fcs
    # 'Suffix': string to add to end of old filename to get new filename. Only 
    # relevant if fileObj is a list. 
    
    print("Don't interrupt this process! You could accidentally delete files." +
          "\n Progress: \n")
    # This clause is for input that is NOT yet in gdb format, but is a shapefile
    if IsGDB == False:
        for alias, path in fileObj.items():
            # convert shapefile to feature class and put in gdb
            fullPath = os.path.join(inPath, path)
            arcpy.conversion.FeatureClassToGeodatabase(
                fullPath, env.workspace+'\\')
            # gets the name of the feature class just created
            # god this is horrible code
            tempName = path[path.rfind('\\')+1:path.rfind('.')]
            # rename feature class according to alias
            # but only if the original file's tempname != alias
            if tempName.casefold() != alias.casefold():
                arcpy.conversion.FeatureClassToFeatureClass(
                    in_features=tempName,
                    out_path=env.workspace,
                    out_name=alias
                )
                # delete original, long-named feature class
                arcpy.Delete_management(tempName)
            # reassure end user that the code is running
            print('''"''' + tempName + '''"''' + " added to geodatabase as " +
                  '''"''' + alias + '''"\n''')
    # This clause is if the input IS already fc(s) in a gdb
    else:
        if type(fileObj) is dict:
            for alias, path in fileObj.items():
                arcpy.conversion.FeatureClassToFeatureClass(
                    in_features=os.path.join(inPath, path),
                    out_path=env.workspace,
                    out_name=alias
                )
                # reassure end user that the code is running
                print('''"''' + alias + '''"''' + " added to geodatabase" +
                      '''\n''')
        elif type(fileObj) is list: 
            for name in fileObj:
                arcpy.conversion.FeatureClassToFeatureClass(
                    in_features=os.path.join(inPath, name),
                    out_path=env.workspace,
                    out_name=name + suffix 
                )
                # reassure end user that the code is running
                print('''"''' + name + '''"''' + " added to geodatabase" +
                      '''\n''')
        else:  
            print('Invalid parameter fileObj. This parameter must be a dict or a list.')
            

