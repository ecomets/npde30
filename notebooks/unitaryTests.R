############################################
# Setup
testDir<-"/home/eco/work/npde/npde30/npde/tests/testthat" # folder
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
test_file("test-NpdeData-class.R")
test_file("test-NpdeRes-class.R")
test_file("test-NpdeObject-class.R")
test_file("test-NpdeSimData-class.R")

############################################
# Binning
test_file("test-binning.R")

# Plots
test_file("test-auxScatterPlots.R")
test_file("test-auxDistPlots.R")

test_file("test-scatterPlots.R")
test_file("test-distributionPlots.R")

############################################
