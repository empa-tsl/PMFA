# This script performs a Probabilistic Material Flow Analysis (PMFA)
# based on the example excel file Input.xlsx
# The current directory should be adapted.
# 
# Author: Delphine Kawecki-Wenger
# Date of last modification: 22.10.2019

# set the working directory for the calculation
setwd("SET/WORKING/DIRECTORY")

# load the functions required to run this code
source("rtriang.perc.R")
source("rtrapez.perc.R")
source("import.input.R")
source("import.TC.R")
source("calc.rest.TC.R")
source("normalize.R")
source("solve.MC.R")

library(xlsx)

# set the number of Monte-Carlo iterations
SIM <- 100

# import the initial data
Input.data <- as.matrix(read.xlsx("../191022_Debug_Yuanfang/Feed.xlsx", sheetName = "Input"))
Coeff.data <- as.matrix(read.xlsx("../191022_Debug_Yuanfang/Feed.xlsx", sheetName = "TC"))

# find all the compartment names
Names <- unique(c(Coeff.data[,c(1,2)]))

# import the input data (THE UNIT NEEDS TO ALWAYS BE THE SAME)
Input <- import.input(Input.data, SIM)

# import the TC values
TC <- import.TC(Coeff.data, Names)

# calculate the "rest" TCs
TC <- calc.rest.TC(TC)

# normalization step: make sure that all the flows going out of a compartment sum up to 1
TC.Norm <- normalize(TC)

# the solve.MC function will solve the equation system N times after having
# reconstructed the TC matrix for every iteration
Mass <- solve.MC(TC.Distr  = TC.Norm,
                 inp.Distr = Input,
                 Names     = Names,
                 N         = SIM)
