# Logistic Regression for Medical appointments 

#set working directory ---------
rm(list = ls())
setwd("/Users/jeremygreen/Desktop/")

#download Appointments.csv
appt <- read.csv("appointments.csv", stringsAsFactors = T)
str(appt)

# appointment Lag
hist(appt$Lag, xlab = "Lag", main = "Histogram of Appointment Lag", 
     col = "blue")
#error = figure margins too large - fix by adjusting work space

# create appointment counts vs. Lag graph on slides
freq <- table(appt$Lag, appt$Status)
plot(row.names(freq), freq[, 2], type = "h", lwd = 4, col = "blue", 
     xlab = "Lag", ylab = "Number of Cancelled Appointments")

# create cancellation rate vs. Lag graph on slides
prop.freq <- prop.table(freq, 1)
plot(row.names(prop.freq), prop.freq[, 2], type = "l", lwd = 4, col = "blue", 
     xlab = "Lag", ylab = "Cancellation Rate")

# logistic regression with Lag
glm.fit <- glm(Status ~ Lag, data = appt, family = binomial)
summary(glm.fit)

# This predicted probability is almost linear in Lag
cancel.prob <- predict(glm.fit, type = "response", 
                       newdata = data.frame(Lag = seq(0, 130)))
plot(cancel.prob, type = "l")

# logistic regression with Lag and Gender
freq <- table(appt$Gender, appt$Status)
prop.freq <- prop.table(freq, 1)
barplot(prop.freq[, 2], col = "blue", xlab = "Gender", 
        ylab = "Cancellation Rate")
# logistic regression with Lag and Gender
glm.fit <- glm(Status ~ Lag + Gender, data = appt, family = binomial)
summary(glm.fit)

# logistic regression with all variables
glm.full <- glm(Status ~ . - MRN, data = appt, family = binomial)
summary(glm.full)






