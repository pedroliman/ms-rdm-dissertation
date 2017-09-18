# -*- coding: utf-8 -*-
"""
Created on Fri Dec 16 15:12:51 2016

@author: Pedro
"""

from ema_workbench import (Model, RealParameter, ScalarOutcome, ema_logging,
                           perform_experiments, util, analysis as analysis, load_results)

import numpy as np
import matplotlib.pyplot as plt

# import analysis.prim as prim

def classify(data):
    #get the output for deceased population
    result = data['deceased population region 1']
    
    #make an empty array of length equal to number of cases 
    classes =  np.zeros(result.shape[0])
    
    #if deceased population is higher then 1.000.000 people, classify as 1 
    classes[result[:, -1] > 1000000] = 1
    
    return classes

#load data
results = load_results(r'./results/results.tar.gz')
experiments, results = results

#extract results for 1 policy
#logicalIndex = experiments['policy'] == 'no policy'
#newExperiments = experiments[ logicalIndex ]
#newResults = {}
#for key, value in results.items():
#    newResults[key] = value[logicalIndex]
#
#results = (newExperiments, newResults)

#perform prim on modified results tuple

prim_instance = analysis.prim.setup_prim(results,'y',1000,["indice_custo","indice_faturamento","i"])




boxes = analysis.prim.perform_prim(results, classify, 
                                    threshold=1000, 
                                    threshold_type=1,
                                    pasting=True)

#print prim to std_out
analysis.prim.write_prim_to_stdout(boxes)

#visualize
analysis.prim.show_boxes_individually(boxes, results)
analysis.prim.show_boxes_together(boxes, results)
plt.show()