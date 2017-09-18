# -*- coding: utf-8 -*-
"""
Created on Fri Dec 09 14:59:03 2016

@author: Pedro
"""
from __future__ import (division, print_function, absolute_import, 
                        unicode_literals)

from ema_workbench import (RealParameter, ScalarOutcome, ema_logging,
                           perform_experiments)

from ema_workbench.connectors.excel import ExcelModel


if __name__ == "__main__":    
    ema_logging.log_to_stderr(level=ema_logging.DEBUG)
    
    model = ExcelModel("excelModel", wd="./models",
                      model_file= '/excelModel.xlsx')
    model.uncertainties = [RealParameter("B2", 0.04, 0.2), #we can refer to a cell in the normal way
                           RealParameter("B3", 4000,6000), # we can also use named cells
                           RealParameter("B4", 0.9,1.1),
                           RealParameter("B5", 0.9,1.1)]
    
    #specification of the outcomes
    model.outcomes = [ScalarOutcome("B7")] # we can also use named range
    
    #name of the sheet
    model.sheet = "EMA"
    
    results = perform_experiments(model, 100, parallel=True, reporting_interval=1)
    
    print("blaat")
