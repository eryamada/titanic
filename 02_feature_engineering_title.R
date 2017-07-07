
# initialize --------------------------------------------------------------

source('source/00_import.R')
source('source/01_preprocess.R')


# create title variable  --------------------------------------------------

dt$Title <- sapply(dt$Name, FUN = function(x){strsplit(x,split = '[,.]')[[1]][2]})
dt$Title <- sub(' ', '', dt$Title)


# process title variable ------------------------------------------------------

dt[, Title := as.factor(Title)]
dum_title <- dummyVars(Survived ~ Title, data= dt)
title_ohe <- predict(dum_title, dt)
dt <- cbind(dt, title_ohe)
dt$Title <- NULL