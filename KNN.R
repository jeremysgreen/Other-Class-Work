# KNN 

# working directory
rm(list = ls())
setwd("/Users/jeremygreen/Desktop/")
appt <- read.csv("appointments.csv")
str(appt)
appt$MRN <- as.factor(appt$MRN)

# KNN prediction using knn function in class package
library(class)
set.seed(200)
train <- sample(nrow(appt), nrow(appt)/2)
appt.train <- appt[train, ]
appt.test <- appt[-train, ]

# function call k = 5 specifies the number of neighbors used
appt.knn <- knn(scale(appt.train[, sapply(appt, is.numeric)]), 
                scale(appt.test[, sapply(appt, is.numeric)]), 
                appt.train$Status, k = 5)

# generate the confusion matrix from knn prediction
table(appt.test$Status, appt.knn)

# choosing best k; elbow method
# to pick the best k you want the point where it turns (elbow), so its the lowest k and the highest accuracy
#if it doesn't make a big difference just pick the biggest k

vk <- seq(1, 10, 2)
accuracy <- vk
for (i in 1:length(vk)) {
  appt.knn <- knn(scale(appt.train[, sapply(appt, is.numeric)]), 
                  scale(appt.test[, sapply(appt, is.numeric)]), 
                  appt.train$Status, k = vk[i])
  accuracy[i] <- mean(appt.test$Status == appt.knn)
}
plot(vk, accuracy, xlab = "k", ylab = "test accuracy", col = "blue")


# ***************************************

# utilizing all relevant columns to make prediction
appt1 <- appt
appt1$MRN <- NULL
appt1 <- as.data.frame(data.matrix(appt1))
appt1$Status <- as.factor(appt1$Status)

set.seed(200)
train <- sample(nrow(appt1), nrow(appt1)/2)
appt1.train <- appt1[train, ]
appt1.test <- appt1[-train, ]

# Note that knn function does model fitting and prediction in one
# function call k = 21 specifies the number of neighbors used
appt1.knn <- knn(scale(appt1.train[, sapply(appt1, is.numeric)]), 
                 scale(appt1.test[, sapply(appt1, is.numeric)]), 
                 appt1.train$Status, k = 5)
# generate the confusion matrix from knn prediction
table(appt1.test$Status, appt1.knn)
mean(appt1.test$Status == appt1.knn)

# using data.matrix may not be appropriate; replace with model.matrix
appt1 <- appt
appt1$MRN <- NULL
appt1.m <- as.data.frame(model.matrix(~ .-Status, data = appt1)[,-1])
set.seed(200)
train <- sample(nrow(appt1.m), nrow(appt1.m)/2)
appt1.m.train <- appt1.m[train, ]
appt1.m.test <- appt1.m[-train, ]

# knn function does model fitting and prediction in one
# function call k = 21 specifies the number of neighbors used
appt1.m.knn <- knn(scale(appt1.m.train), scale(appt1.m.test), 
                    appt1$Status[train], k = 5)

# generate the confusion matrix from knn prediction
table(appt1$Status[-train], appt1.m.knn)
mean(appt1$Status[-train] == appt1.m.knn)




