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

def inPoints(pointsDict, inPath):
# This function turns tabular data with coordinates into point fcs 
# 'pointsDic': an object (dic or list) containing filenames to transform. If 
# pointsDic is a dic, it relates filenames to their desired fc name. 
# inPath: path to the directory where the input fiels are.

    # defines coordinate system to use. This code corresponds to the NAD RI State Plane one. 
        spatRef = arcpy.SpatialReference(4269)
    # Input files with long filenames are passed to this function in a dic relating
    # the too-long filenames to short aliases for output

        arcpy.env.overwriteOutput = True
        for alias, file in pointsDict.items():
            arcpy.management.XYTableToPoint(os.path.join(inPath, file),
                                            alias,
                                            "ActivityLocation/LongitudeMeasure", 
                                            "ActivityLocation/LatitudeMeasure",
                                            coordinate_system=spatRef)  
            print(file + " added to gdb as " + alias + "\n")
        arcpy.env.overwriteOutput = False    


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
            # this is horrible code
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
            
def table_to_data_frame(in_table, input_fields=None, where_clause=None):
    # this function converts an arcgis table (e.g. from an fc) into a pandas dataframe with an 
    # object ID index
    # 'in_table' is the fc to convert 
    # the other argments came from the internet so good luck. I guess you can 
    # subset and select which fields and attributes to include.
    # returns a converted df 
    
    OIDFieldName = arcpy.Describe(in_table).OIDFieldName
    if input_fields:
        final_fields = [OIDFieldName] + input_fields
    else:
        final_fields = [field.name for field in arcpy.ListFields(in_table)]
    data = [row for row in arcpy.da.SearchCursor(
        in_table, final_fields, where_clause=where_clause)]
    fc_dataframe = pd.DataFrame(data, columns=final_fields)
    out_df = fc_dataframe.set_index(OIDFieldName, drop=True)
    return out_df
