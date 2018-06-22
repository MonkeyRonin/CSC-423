#Project 2
#Author: Yiyang Yang
#Date: 08/02/2016
#Part A
#Create Dataframe and Print
flour = read.table("/Users/Yiyang/Documents/CSC 423/flour.txt", header = TRUE)
print(flour)

#Means, Standard Deviation, Covariance, Correlation
cat("Mean of Weight: ", mean(flour$Weight), "\n")
cat("Mean of NBags: ", mean(flour$NBags), "\n")

cat("SD of Weight: ", sd(flour$Weight), "\n")
cat("SD of NBags: ", sd(flour$NBags), "\n")

cat("Covariance of Weight and NBags: ", cov(flour$Weight, flour$NBags), "\n")
cat("Correlation of Weight and NBags: ", cor(flour$Weight, flour$NBags), "\n")

#Simple Linear Regression
model = lm(NBags ~ Weight, data = flour)
print(summary(model))

r = residuals(model)
p = fitted(model)

#Residual Plot
plot(p, r, main = "Residual Plot for NBags", 
           xlab = "Predicted", 
           ylab = "Residuals")
abline(h = 0, lty = 2)

#Normal Plot
qqnorm(r, main = "Normal Plot for Flour Dataset")
qqline(r, lty = 2)

#Simple Linear Regression (Through the Origin) 
model1 = lm(NBags ~ Weight + 0, data = flour)
print(summary(model1))

r1 = residuals(model1)
p1 = fitted(model1)

#Residual Plot
plot(p1, r1, main = "Residual Plot for NBags (Origin)",
             xlab = "Predicted",
             ylab = "Residuals")
abline(h = 0, lty = 2)

#Normal Plot
qqnorm(r1, main = "Normal Plot for Flour Dataset (Origin)")
qqline(r1, lty = 2)

#Part B
#Create Dataframe and Print
UsedCars = read.table("/Users/Yiyang/Documents/CSC 423/UsedCar.txt", header = TRUE)
print(UsedCars)

#Scatterplot
pairs(UsedCars)

#Correlations
cor(UsedCars)

#Simple Linear Regression(Year and Price)
model2 = lm(Price ~ Year, data = UsedCars)
print(summary(model2))

r2 = residuals(model2)
p2 = fitted(model2)

#Residual Plot
plot(p2, r2, main = "Residual Plot for Price (Year)",
             xlab = "Predicted",
             ylab = "Residuals")
abline(h = 0, lty = 2)

#Normal Plot
qqnorm(r2, main = "Normal Plot for Price (Year)")
qqline(r2, lty = 2)

#Simple Linear Regression(Miles and Price)
model3 = lm(Price ~ Miles, data = UsedCars)
print(summary(model3))

r3 = residuals(model3)
p3 = fitted(model3)

#Residual Plot
plot(p3, r3, main = "Residual Plot for Price (Miles)",
             xlab = "Predicted",
             ylab = "Residuals")
abline(h = 0, lty = 2)

#Normal Plot
qqnorm(r3, main = "Normal Plot for Price (Miles)")
qqline(r3, lty = 2)

#Multiple Linear Regression
model4 = lm(Price ~ Year + Miles, data = UsedCars)
print(summary(model4))

r4 = residuals(model4)
p4 = fitted(model4)

#Residual Plot (Predicted)
plot(p4, r4, main = "Residual Plot for Price",
             xlab = "Predicted",
             ylab = "Residuals")
abline(h = 0, lty = 2)

#Residual Plot (Year)
plot(UsedCars$Year, r4, main = "Residual Plot for Price",
                        xlab = "Year",
                        ylab = "Residuals")
abline(h = 0, lty = 2)

#Residual Plot (Miles)
plot(UsedCars$Miles, r4, main = "Residual Plot for Price",
                         xlab = "Miles",
                         ylab = "Residuals")
abline(h = 0, lty = 2)

#Normal Plot
qqnorm(r4, main = "Normal Plot for Price")
qqline(r4, lty = 2)