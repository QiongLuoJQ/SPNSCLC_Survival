library(ggplot2)
library(shiny)
library(plotly)
library(mlr3proba)
# library(stargazer)
# library(compare)
library(survival)

#######################################################
#### Before publishing your lung survival prediction:
####
#### - You may need to edit the following lines if
#### data or model objects are not defined correctly
#### - You could modify ui.R or server.R for
#### making any required changes to your app
#######################################################

load('lung_modeldata.rdata')
source('functions.R')

# m.summary <- 'raw'
# covariate <- 'slider'
# clevel <- 0.95

### Please cite the package if used in publication. Use:
# Jalali A, Alvarez-Iglesias A, Roshan D, Newell J (2019) Visualising statistical models using dynamic nomograms. PLOS ONE 14(11): e0225253. https://doi.org/10.1371/journal.pone.0225253

