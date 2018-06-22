#Project 4
#Author: Yiyang Yang
#Date: 08/15/2016
#Part A
#Create Dataframe
Prestige = read.table("/Users/Yiyang/Documents/CSC 423/prestige.txt", header = TRUE)
print(Prestige)

#Create Transformed Variables
log_income = log(Prestige$income)
sqrt_income = sqrt(Prestige$income)

PrestigeT = data.frame(title = Prestige$title,
                       education = Prestige$education,
                       Education = Prestige$income,
                       Women = Prestige$women,
                       rating = Prestige$prestige,
                       log_income = log_income,
                       sqrt_income = sqrt_income)

print(PrestigeT)

#Model 1
model1 = lm(rating ~ education + Education, data = PrestigeT)
print(summary(model1))

#Model 2
model2 = lm(rating ~ education + log_income, data = PrestigeT)
print(summary(model2))

#Model 3
model3 = lm(rating ~ education + sqrt_income, data = PrestigeT)
print(summary(model3))

#Final Model(Model 2)
r = residuals(model2)
p = predict(model2)
#Residual Plot(education)
plot(PrestigeT$education, r, main = "Residual Plot for Prestige",
                             xlab = "Education",
                             ylab = "Residuals")
abline(h = 0, lty = 2)
#Residual Plot(Income)
plot(PrestigeT$log_income, r, main = "Residual Plot for Prestige",
                              xlab = "Log of Income",
                              ylab = "Residuals")
abline(h = 0, lty = 2)
#Residual Plot(Predicted)
plot(p, r, main = "Residual Plot for Prestige",
           xlab = "Predicted",
           ylab = "Residuals")
abline(h = 0, lty = 2)
#Normal Plot
qqnorm(r, main = "Normal Plot for Prestige")
qqline(r, lty = 2)

#F interval
qf(0.95, 2, 99)

#Cross Validation
PRESS(model2)
aov(model2)

#Part B
#Create Dataframe
Response = read.table("/Users/Yiyang/Documents/CSC 423/response.txt", header = TRUE)
print(Response)

#New Scale
t = (Response$temp - 250)/10
print(t)
p = (Response$pressure - 145)/5
print(p)

tt = t * t
pp = p * p
tp = t * p
#Model 1
modelt = lm(yield ~ t, data = Response)
print(summary(modelt))

#Model 2
modeltt = lm(yield ~ tt, data = Response)
print(summary(modeltt))

#Model 3
modelp = lm(yield ~ p, data = Response)
print(summary(modelp))

#Model 4
modelpp = lm(yield ~ pp, data = Response)
print(summary(modelpp))

#Model 5
modeltp = lm(yield ~ tp, data = Response)
print(summary(modeltp))

#Model 6
modelfull = lm(yield ~ t + tt + p + pp + tp, data = Response)
print(summary(modelfull))