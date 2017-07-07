
# initialize --------------------------------------------------------------

source('source/00_import.R')
pacman::p_load(caret)


# process Pclass ----------------------------------------------------------

dt[, Pclass := as.factor(Pclass)]
dum_pclass <- dummyVars(Survived ~ Pclass, data= dt)
pclass_ohe <- predict(dum_pclass, dt)
dt <- cbind(dt, pclass_ohe)
dt$Pclass <- NULL


# process Sex -------------------------------------------------------------

dt[, Sex := as.factor(Sex)]
dum_sex <- dummyVars(Survived ~ Sex, data =dt)
sex_ohe <- predict(dum_sex, dt)
dt <- cbind(dt, sex_ohe)
dt$Sex <- NULL


# process Embarked --------------------------------------------------------

dt$Embarked[dt$Embarked == ''] <- 'S'
dt[, Embarked := as.factor(Embarked)]
dum_embarked <- dummyVars(Survived ~ Embarked, data=dt)
embarked_ohe <- predict(dum_embarked, dt)
dt <- cbind(dt, embarked_ohe)
dt$Embarked <- NULL


# process Age -------------------------------------------------------------

dt$Age[is.na(dt$Age)] <- 28.00


# remove ticket and cabin -------------------------------------------------

dt$Ticket <- NULL
dt$Cabin <- NULL