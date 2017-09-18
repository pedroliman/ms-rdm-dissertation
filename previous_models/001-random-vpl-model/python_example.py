'''
Created on 20 dec. 2010

This file illustrated the use the EMA classes for a contrived example
It's main purpose has been to test the parallel processing functionality

.. codeauthor:: jhkwakkel <j.h.kwakkel (at) tudelft (dot) nl>
'''
from __future__ import (absolute_import, print_function, division,
                        unicode_literals)

from ema_workbench import (Model, RealParameter, ScalarOutcome, ema_logging,
                           perform_experiments, util)

from netpresentvalue import modelvpl

'''
def some_model(x1=None, x2=None, x3=None):
    return {'y':x1*x2+x3}


def vpl_model(x1=None, x2=None, x3=None):
    return {'y':x1*x2+x3}
'''



if __name__ == '__main__':
    ema_logging.LOG_FORMAT = '[%(name)s/%(levelname)s/%(processName)s] %(message)s'
    ema_logging.log_to_stderr(ema_logging.INFO)
    
    model = Model('simpleModel', function=modelvpl) #instantiate the model

    #specify uncertainties
    model.uncertainties = [RealParameter("indice_custo", 0.9, 1.1),
                           RealParameter("indice_faturamento", 0.9,1.3),
                           RealParameter("i", 0.01,0.05)]
    #specify outcomes 
    model.outcomes = [ScalarOutcome('y')]

    results = perform_experiments(model, 10000)
    
    util.utilities.save_results(results, './results/results.tar.gz')

#    print(results)
    
    
    
    
#     ensemble = ModelEnsemble() #instantiate an ensemble
#     ensemble.model_structure = model #set the model on the ensemble
#     results = ensemble.perform_experiments(100, reporting_interval=1) #run 1000 experiments
    

