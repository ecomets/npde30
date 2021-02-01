############################################
# Setup
testDir<-"D:/RECHERCHES/_NPDE/Npde-2021/npde30-main/npde/tests/testthat" # folder
setwd(testDir)
source("helper-source.R")

############################################
# libraries
library(testthat)
library(mclust)
library(ggplot2)
library(gridExtra)
library(grid)

############################################
# Classes and objects

# ok
test_file("test-NpdeData-class.R") 

# ok
test_file("test-NpdeRes-class.R")

# 2 fails 
test_file("test-NpdeObject-class.R")

# ok
test_file("test-NpdeSimData-class.R")

############################################
# Binning
# ok
test_file("test-binning.R")

# Plots
# ok
test_file("test-auxScatterPlots.R")

# ok
test_file("test-auxDistPlots.R")

# ok
test_file("test-scatterPlots.R")

# ok
test_file("test-distributionPlots.R")

############################################
