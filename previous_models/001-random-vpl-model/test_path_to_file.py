# -*- coding: utf-8 -*-
"""
Created on Fri Dec 09 15:22:43 2016

@author: Pedro
"""
import os
cwd = os.getcwd()
print(cwd)

wd="./models/"
model_file= '/excelModel.xlsx'


working_directory = wd

path_to_file = os.path.join(working_directory, model_file)
        
print(path_to_file)
print(path_to_file)
print(path_to_file)
print(path_to_file)

if not os.path.isfile(path_to_file):
    raise ValueError('cannot find model file:'+path_to_file)