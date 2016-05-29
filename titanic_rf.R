library(rpart)
install.packages('randomForest')
library(randomForest)
install.packages('mice')
library(mice)
install.packages('party')
library(party)
install.packages('dplyr')
library(dplyr)

train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)

# Binding all raws together for easier operating on data
all <- bind_rows(train, test)

#Cleaning titles data
all$Title <- gsub('(.*, )|(\\..*)', '', all$Name)

all$Title[all$Title %in% c('Mlle', 'Ms')] <- 'Miss'
all$Title[all$Title %in% c('Mme', 'Lady')] <- 'Mrs'
all$Title[all$Title %in% c('Capt', 'Major', 'Sir')] <- 'Mr'

otherTitles <- c('Col', 'Don', 'Dr', 'Rev', 'the Countess', 'Jonkheer', 'Dona')

all$Title[all$Title %in% otherTitles] <- 'Other'

# Getting more info about families 
all$familySize <- all$SibSp + all$Parch + 1
all$Surname <- sapply(all$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
all$Family <- paste(all$Surname, all$familySize, sep='_')

# Filling lacks of ages
factor_vars <- c('PassengerId','Surname','Sex','Embarked','Title','Pclass','Family','familySize')

all[factor_vars] <- lapply(all[factor_vars], function(x) as.factor(x))

set.seed(100)

mice_mod <- mice(all[, !names(all) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf')
mice_output <- complete(mice_mod)
all$Age <- mice_output$Age

# Filling lacks in Embarked
which(all$Embarked == '')
all$Embarked[c(62,830)] = 'S'

# Filling lacks in Fare
all$Fare[1044] <- median(all$Fare, na.rm=TRUE)

# Spliting into test and train 
train <- all[1:891,]
test <- all[892:1309,]

# Building condition inference tree 
set.seed(625)
newTree <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + familySize ,
                   data = train, controls=cforest_unbiased(ntree=2500, mtry=3))

Prediction <- predict(newTree, test, OOB=TRUE, type = "response")
result <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(result, file = "prediction.csv", row.names = FALSE)