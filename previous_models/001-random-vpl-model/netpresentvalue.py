# -*- coding: utf-8 -*-
"""
Created on Fri Dec 09 15:43:52 2016

@author: Pedro
"""

import random

def fc(minimo_custo, maximo_custo, minimo_faturamento, maximo_faturamento, 
       indice_custo, indice_faturamento, inv, invp,n):
    fc=[]
    j=0
    while j < n:
        fc.append(random.uniform(minimo_faturamento,maximo_faturamento)*
                  indice_faturamento-random.uniform(minimo_custo,maximo_custo)*
                    indice_custo)
        if j == invp:
            fc[j]=fc[j]-inv
        j=j+1
    return fc
    
def vpl(fc, i):
    vpl = 0
    for n, x in enumerate(fc):    
        vpl=vpl + fc[n]/(1+i)**n
    return vpl  

def modelvpl(indice_custo, indice_faturamento,i):
    fcx = fc(1000, 1200, 1500, 1700, indice_custo, indice_faturamento,
             random.uniform(4000,6000), 0,20)
    output = vpl(fcx,i)
    return {'y':output}
    

''' 
n = 200
i = 0.10
inv = random.uniform(4000,6000)
indice_custo = 1
indice_faturamento = 1
minimo_custo = 1000
maximo_custo = 1200
minimo_faturamento = 1500
maximo_faturamento = 1700
invp = 0

'''


# fc = fc(minimo_custo, maximo_custo, minimo_faturamento, maximo_faturamento, indice_custo, indice_faturamento, inv, invp,n)

# print(modelvpl(minimo_custo, maximo_custo, minimo_faturamento, maximo_faturamento, indice_custo, indice_faturamento, inv, invp,n, i))