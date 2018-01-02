#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Dec 29 09:47:02 2017

@author: pedro
"""

# Exemplo da Análise PRIM Obtido em:
# https://github.com/Project-Platypus/PRIM

import prim
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Lendo o CSV de ANálise de VUlnerabilidade:
path = "df_vulnerabilidade.csv"
df_vulnerabilidade = pd.read_csv(path, index_col=0, parse_dates=True)
df_vulnerabilidade

# Gerando a Resposta:
resposta = pd.read_csv("resposta.csv", index_col=0, parse_dates=True)

# Convertendo resposta em um array unidimensional
resposta = resposta["x"]

#resposta = df_vulnerabilidade["sNPVProfit1RegretPerc"]

# Variaveis de Entrada:
#incertezas = df_vulnerabilidade.iloc[:,4:42]

incertezas = pd.read_csv("incertezas.csv", index_col=0, parse_dates=True)


p = prim.Prim(incertezas, resposta, threshold=260207766, threshold_type=">")

box = p.find_box()

box.show_tradeoff()

plt.show()

# box.show_details()


