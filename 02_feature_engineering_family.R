
# initialize --------------------------------------------------------------

source('source/00_import.R')
source('source/01_preprocess.R')


# create family size variable ---------------------------------------------

dt$family <- dt$SibSp + dt$Parch + 1
## family sizes sometimes wrongly labeled