#Project 3
#Author: Yiyang Yang
#Date: 08/03/2016
#Part A
#Create Dataframe
Banking = read.table("/Users/Yiyang/Documents/CSC 423/banking.txt", header = TRUE)
print(Banking)

#Scatterplot
plot(Banking$Age, Banking$Balance,
     main = "Bank Balance",
     xlab = "Median age of the population (Age)",
     ylab = "Average bank balance (Balance)")
plot(Banking$Education, Banking$Balance,
     main = "Bank Balance",
     xlab = "Median year of education (Education)",
     ylab = "Average bank balance (Balance)")
plot(Banking$Income, Banking$Balance,
     main = "Bank Balance",
     xlab = "Median income (Income)",
     ylab = "Average bank balance (Balance)")
plot(Banking$HomeVal, Banking$Balance,
     main = "Bank Balance",
     xlab = "Median home value (HomeVal)",
     ylab = "Average bank balance (Balance)")
plot(Banking$Wealth, Banking$Balance,
     main = "Bank Balance",
     xlab = "Median household wealth (Wealth)",
     ylab = "Average bank balance (Balance)")

#Correlation
cat("Correlation of Age and Balance: ", cor(Banking$Age, Banking$Balance))
cat("Correlation of Education and Balance: ", cor(Banking$Education, Banking$Balance))
cat("Correlation of Income and Balance: ", cor(Banking$Income, Banking$Balance))
cat("Correlation of Home Value and Balance: ", cor(Banking$HomeVal, Banking$Balance))
cat("Correlation of Wealth and Balance: ", cor(Banking$Wealth, Banking$Balance))

#Regression
model = lm(Balance ~ Age + Education + Income + HomeVal + Wealth, data = Banking)
print(summary(model))

#Influence Points
summary(influence.measures(model))

#Regression(removed)
model1 = lm(Balance ~ Age + Education + Income + Wealth, data = Banking)
print(summary(model1))

#Standardized Coeffiecents
b1 = 3.242e+02
sx1 = sd(Banking$Age)
sy1 = sd(Banking$Balance)
SC1 = b1 * sx1/sy1
print(SC1)

b2 = 7.498e+02
sx2 = sd(Banking$Education)
sy2 = sd(Banking$Balance)
SC2 = b2 * sx2/sy2
print(SC2)

b3 = 1.615e-01
sx3 = sd(Banking$Income)
sy3 = sd(Banking$Balance)
SC3 = b3 * sx3/sy3
print(SC3)

b4 = 7.265e-02
sx4 = sd(Banking$Wealth)
sy4 = sd(Banking$Balance)
SC4 = b4 * sx4/sy4
print(SC4)

qf(0.95, 4, 97)

#Residual Plot(Predict)
ra = residuals(model1)
pa = predict(model1)

plot(pa, ra, main = "Residual Plot for Balance",
             xlab = "Predicted",
             ylab = "Residuals")
abline(h = 0, lty = 2)

#Residual Plot(Age)
plot(Banking$Age, ra, main = "Residual Plot for Balance",
                      xlab = "Age",
                      ylab = "Residuals")
abline(h = 0, lty = 2)

#Residual Plot(Education)
plot(Banking$Education, ra, main = "Residual Plot for Balance",
                            xlab = "Education",
                            ylab = "Residuals")
abline(h = 0, lty = 2)

#Residual Plot(Income)
plot(Banking$Income, ra, main = "Residual Plot for Balance",
                         xlab = "Income",
                         ylab = "Residuals")
abline(h = 0, lty = 2)

#Residual Plot(Wealth)
plot(Banking$Wealth, ra, main = "Residual Plot for Balance",
                         xlab = "Wealth",
                         ylab = "Residuals")
abline(h = 0, lty = 2)

#Normal Plot
qqnorm(ra, main = "Normal Plot for hours")
qqline(ra, lty = 2)

#Influence Point
summary(influence.measures(model1))


#Part B
#Create Dataframe
Lathe = read.table("/Users/Yiyang/Documents/CSC 423/lathe.txt", header = TRUE)
print(Lathe)

#Regression Model (Dummy Variable) 
type_dum = as.numeric(Lathe$type == "B")
with_dummy_B = data.frame(rpm = Lathe$rpm,
                          hour = Lathe$hour,
                          type_dum = type_dum)
print(with_dummy_B)
modelB = lm(hour ~ rpm + type_dum, data = with_dummy_B)
print(summary(modelB))

#Scatterplot
r = residuals(modelB)
p = predict(modelB)
attach(with_dummy_B)
plot(hour, rpm,
     main = "Scatterplot Using Plotting Symbol type_dum",
     xlab = "Tool life in hours",
     ylab = "Lathe speen in RPM",
     pch = as.character(Lathe$type))

#Residual Plot
plot(p, r, main = "Residual Plot for hours",
           xlab = "Predicted",
           ylab = "Residuals")
abline(h = 0, lty = 2)

#Normal Plot
qqnorm(r, main = "Normal Plot for hours")
qqline(r, lty = 2)

#Part C
#Create Datafrome
SS = read.table("/Users/Yiyang/Documents/CSC 423/salary-survey.txt", header = TRUE)
print(SS)

#Dummy Variables
educ_dum_1 = as.numeric(SS$educ == 2)
educ_dum_2 = as.numeric(SS$educ == 3)

mgt_dum = as.numeric(SS$mgt == 0)

with_dummy_C = data.frame(salary = SS$salary,
                          exper = SS$exper,
                          mgt_dum = mgt_dum,
                          educ_dum_1 = educ_dum_1,
                          educ_dum_2 = educ_dum_2)
print(with_dummy_C)

#Regression Model
modelC = lm(salary ~ exper + educ_dum_1 + educ_dum_2 + mgt_dum, data = with_dummy_C)
print(summary(modelC))

#Pairs Scatterplot
pairs(SS)

#Residual Plot (Predicted)
r1 = residuals(modelC)
p1 = predict(modelC)

plot(p1, r1, main = "Residual Plot for Salary",
             xlab = "Predicted",
             ylab = "Residuals")
abline(h = 0, lty = 2)

#Residual Plot(Exper)
plot(with_dummy_C$exper, r1, main = "Residual Plot for Salary",
                             xlab = "Exper",
                             ylab = "Residuals")
abline(h = 0, lty = 2)

#Residual Plot(Educ)
plot(with_dummy_C$educ_dum_1, r1, main = "Residual Plot for Salary",
                                  xlab = "Edu",
                                  ylab = "Residuals")
abline(h = 0, lty = 2)

plot(with_dummy_C$educ_dum_2, r1, main = "Residual Plot for Salary",
                                  xlab = "Edu",
                                  ylab = "Residuals")
abline(h = 0, lty = 2)

#Redidual Plot(Mgt)
plot(with_dummy_C$mgt_dum, r1, main = "Residual Plot for Salary",
                               xlab = "Mgt",
                               ylab = "Residuals")
abline(h = 0, lty = 2)

#Normal Plot
qqnorm(r1, main = "Normal Plot for Salary")
qqline(r1, lty = 2)

#Prediction Interval
newData = data.frame(exper = 3, educ_dum_1 = 0, educ_dum_2 = 0, mgt_dum = 1)
predict(modelC, newData, interval = "predict")