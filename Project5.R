#Project 5
#Author: Yiyang Yang
#Date: 08/15/2016

#Create Dataframe
ha = read.table("/Users/Yiyang/Documents/CSC 423/heart-attack.txt", header = T)
print(ha)

#Regression model
modelh = glm(ha2 ~ ang + sco, data = ha, family = binomial(link = "logit"))
print(summary(modelh))

#Predicted Probability
print(fitted(modelh, type = "response"))

#Probability of a second heart attack
new = data.frame(ang = 1, sco = 35)
predict(modelh, new, type = "response")