
# initalize ---------------------------------------------------------------

options(scipen=999)
pacman::p_load(data.table, skimr)


# load data ---------------------------------------------------------------

train <- fread('data/input/train.csv')
test <- fread('data/input/test.csv')


# combine train and test datasets -----------------------------------------

train$set <- 'train'
test$set <- 'test'
dt <- rbind(train, test, fill = TRUE)