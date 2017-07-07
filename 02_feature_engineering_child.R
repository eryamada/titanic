
# initialize --------------------------------------------------------------

source('source/00_import.R')
source('source/01_preprocess.R')


# create child variable ---------------------------------------------------

dt$child <- 0
dt$child[dt$Age < 18.00] <- 1