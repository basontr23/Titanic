library(tidyverse)
library(randomForest)
library(ggplot2)


test <- read_csv("test.csv")
train <- read_csv("train.csv")

glimpse(test)
glimpse(train)

test <- add_column(test,"Survived" = NA, .after = "PassengerId")

which(colSums(is.na(test)) > 0)
colSums(is.na(test))
?which

total <- rbind(test, train)
glimpse(total)
total <- select(total, -c(Cabin, Name,Ticket))
total$Sex <- as.factor(total$Sex)
total$Embarked <- as.factor(total$Embarked)

summary(total)


which(is.na(total$Fare))
which(is.na(total$Embarked))
which(is.na(total$Age))

unique(total$Embarked)
unique(total$Parch)
unique(total$SibSp)


table(total$Sib)

naEmbarked <- which(is.na(total$Embarked))
total$Embarked[naEmbarked] <- 'S'

nonNA <- total$Fare[which(!is.na(total$Fare))]
total[which(is.na(total$Fare)),8] <- median(nonNA)

age_formula = "Age ~ Pclass + Sex + Fare + SibSp + Parch"
age_model <- glm(formula = age_formula, data = total)
age_predict <- predict(age_model, newdata = total[which(is.na(total$Age)),] )
total[which(is.na(total$Age)), 5] <- age_predict


sum(is.na(total$Age))



test1 <- filter(total, is.na(total$Survived))
train1 <- filter(total, !is.na(total$Survived))


survive_formula <- "Survived ~ Age + Pclass + Sex + Fare + SibSp + Parch + Embarked"
model <- glm(Survived ~.,family=binomial(link='logit'),data=train1)
survive_predict <- predict(model, test1)
test1$Survived <- survive_predict
test1[,2]



titanic_submisison2 <- test1[,c(1,2)]

threshold <- boxplot.stats(titanic_submisison2$Survived)$stats[3]
index <- which(titanic_submisison2$Survived >= threshold)
titanic_submisison2[index,2] <- 1
titanic_submisison2[-index,2] <- 0



summary(titanic_submisison)
boxplot.stats(titanic_submisison2$Survived)
write_csv(titanic_submisison2,'titanic_submission2.csv')


titanic_submisison <- read_csv("titanic_submission.csv")

which(titanic_submisison$Survived != titanic_submisison2$Survived)



summary(train)
ggplot(data = train,mapping = aes(factor(Survived), fill = Sex))+
  geom_bar( )

















