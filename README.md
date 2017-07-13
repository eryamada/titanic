---
title: "Titanic: Machine Learning From Disaster"
subtitle: "Exploratory Data Analysis and Feature Engineering"
author: Erik Yamada
output: html_document
---

# Introduction

My first competition on Kaggle was the Titanic Competition.  In this document, I will walk through the steps that I took to analyze the dataset:

* Experimental Data Analysis and Findings
* Imputing Missing Data
* Feature Engineering


Before getting started, I suggest downloading the files from the Titanic Kaggle competition.  Once you have downloaded them, create a new folder specifically for this project.  Within this folder create a data folder where you will put the test and train data you just downloaded.  When you open R, create a new project whose working directory (wd) is set the folder you created for this project.


# Loading Data
```{r, message = FALSE}
# Loading Packages
pacman::p_load(data.table)
pacman::p_load(skimr)          # quick data overview
pacman::p_load(DescTools)      # fast statistical and graphical analyses
pacman::p_load(ggplot2)        # data visualization
pacman::p_load(caret)          # one hot encoding
```

Once the packages have been loaded, load the data from its source in the computer.

```{r, message = FALSE}
# note: you may have to change the pathway for your data
train <- fread('data/input/train.csv')
test <- fread('data/input/test.csv')
```

# Data Overview
Use the skim package to get a quick overview of the train dataset.
```{r, message = FALSE}
skim(train)
```
A few takeaways from the data:

* there are 7 numeric variables and 5 character variables
* Age, Cabin and Embarked have some missing data points

# Variables
There are certain variables in the data set that can be defined better.  For example, while it is true that Pclass is a number, it makes more sense for it to be categorical variable (it wouldn't make sense for 2.33 to be class).  Thus, we must change these variables to be factors.
```{r, message = FALSE}
train[, Parch := as.factor(Parch)]
train[, Pclass := as.factor(Pclass)]
train[, SibSp := as.factor(SibSp)]
train[, Survived := as.factor(Survived)]
train[, Cabin := as.factor(Cabin)]
train[, Embarked:= as.factor(Embarked)]
train[, Sex := as.factor(Sex)]
```

# EDA
Now, let's start with some assumptions regarding the situation of the titanic and see if the data.  In the event of a disaster, doesn't society tends to prioritize saving women and children. Would wealthier passengers be more likely to survive?  Let's analyze these assumptions.

## Variable Relationship with Survival Rate {.tabset .tabset-fade}
Click through the tabs to see how different variables are correlated with survival rate.

### Age
```{r, message = FALSE}
Desc(Survived ~ Age, data = train)
```

### Sex
```{r, message = FALSE}
Desc(Survived ~ Sex, data = train)
```

### Fare
```{r, message = FALSE}
Desc(Survived ~ Fare, data = train)
```

### Pclass
```{r, message = FALSE}
Desc(Survived ~ Pclass, data = train)
```

### Parch
```{r, message = FALSE}
Desc(Survived ~ Parch, data = train)
```

### SibSp
```{r, message = FALSE}
Desc(Survived ~ SibSp, data = train)
```

### Embarked
```{r, message = FALSE}
Desc(Survived ~ Embarked, data = train)
```

## Imputing Data {.tabset .tabset-fade}

When we skimmed the dataset, we noticed that there were three categories with missing data points: Age, Cabin, and Embarked.  What should we do about these missing values?

### Age

Age had 177 missing values, which is about 20% of the data.  Before we think of ways to fill in the data, let's take a look at the distribution of ages on the titanic.
```{r, message = FALSE}
Desc(train$Age)
```

Looking at the distribution, we see that it is slightly bimodal, and skewed right.  Thus, median is a better indicator than mean.  We can impute values using other categories to make guesses about missing Age values.  For example, we can check to see if we can infer Age based on Pclass.  But for now, let's just impute all missing Age data points with the median age value, which is 28.

```{r, message = FALSE}
train$Age[is.na(train$Age)] <- 28.00
```

### Embarked

Because Embarked only had two missing values, we can just replace the missing values with the most common Embarked factor.

```{r, message = FALSE}
Desc(train$Embarked)
train$Embarked[is.na(train$Embarked)] <- 'S'
```

### Cabin & Ticket

Because Cabin has so many missing values, it doesn't make sense to impute the data so we will remove this variable.  We will also remove the Ticket variable because there are so many unique values within this variable.
```{r, message = FALSE}
train$Cabin <- NULL
train$Ticket <- NULL
```



# Feature Engineering

To prepare for feature engineering, we combine the train and test data.

```{r, message = FALSE}
train$set <- 'train'
test$set <- 'test'
dt <- rbind(train, test, fill = TRUE)
```

## Creating Features {.tabset .tabset-fade}

Let's create new variables based on our EDA.

### Child

We noticed from our EDA that the younger you were, the better chance you have of surviving.  But if we still havent yet answered that question just by viewing the relationship between age and survival rate.  To fix this, we create a Child variable.

```{r, message = FALSE}
dt$Child <- 0
dt$Child[dt$Age < 18.00] <- 1
```

### Family

Another question that I was wondering was that if women and children have a higher chance of surivival, how does being a part of a family affect the survival rate?  Let's create a family variable based on the Parch and SibSp variables then remove these variables.

```{r, message = FALSE}
dt$Family <- dt$SibSp + dt$Parch + 1
dt$Parch <- NULL
dt$SibSp <- NULL
```

### Title

So far, we haven't done anything with the Name variable.  Yet, this variable provides a lot of potential for useful data.  The Name variable includes the first and last name of each passenger as well as the person's title.  It is possible to try to categorize passengers based on race using last name and see if there is a correlation between race and surivival rate.  However, a much easier part of the name to engineer is the title.  Thus, we can pull out the title of each passenger to create a new variable.

```{r, message = FALSE}
dt$Title <- sapply(dt$Name, FUN = function(x){strsplit(x,split = '[,.]')[[1]][2]})
dt$Title <- sub(' ', '', dt$Title)
```


It can be hard to analyze categorical variables, so we use a technique called one hot encoding to represent the same data in a way that is easy to feed to models.  We will one hot encode Sex, Pclass, Embarked, and the newly created Title variable.

## One Hot Encoding {.tabset .tabset-fade}


### Pclass

```{r, message = FALSE}
dum_pclass <- dummyVars(Survived ~ Pclass, data= dt)
pclass_ohe <- predict(dum_pclass, dt)
dt <- cbind(dt, pclass_ohe)
dt$Pclass <- NULL
```

### Sex

```{r, message = FALSE}
dum_sex <- dummyVars(Survived ~ Sex, data =dt)
sex_ohe <- predict(dum_sex, dt)
dt <- cbind(dt, sex_ohe)
dt$Sex <- NULL
```

### Embarked

```{r, message = FALSE}
dum_embarked <- dummyVars(Survived ~ Embarked, data=dt)
embarked_ohe <- predict(dum_embarked, dt)
dt <- cbind(dt, embarked_ohe)
dt$Embarked <- NULL
```

### Title

```{r, message = FALSE}
dt[, Title := as.factor(Title)]
dum_title <- dummyVars(Survived ~ Title, data= dt)
title_ohe <- predict(dum_title, dt)
dt <- cbind(dt, title_ohe)
dt$Title <- NULL
```

# Fit Models
Now that you are finished with EDA and Feature Engineering, it's time to feed the data into models!
