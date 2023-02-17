install.packages("ggcorrplot")
install.packages("ggplot2")
install.packages("survey")
library(ggcorrplot)
library(survey)
b= read.csv("data_linearstat_project.csv",header = TRUE, sep = ",")
for(i in 1:569){
  if(b[i,1]== "M"){
    b[i,1]=1
  }else 
    b[i,1]=0
}
str(b)
table(b$diagnosis)
b[,1]=as.numeric(b$diagnosis)
y=b[,1]
hist(y,breaks = 2,col = "blue")

b=b[,2:31]

r <- cor(b)                      # Correlation matrix
ggcorrplot(r)

cor_matrix_rm <- cor(b)                  # Modify correlation matrix
cor_matrix_rm[upper.tri(cor_matrix_rm)] <- 0
diag(cor_matrix_rm) <- 0
cor_matrix_rm
b <- b[ , !apply(cor_matrix_rm,    # Remove highly correlated variables
                 2,
                 function(x) any(x > 0.85))]
library(MASS)

b=cbind(y,b)
str(b)
fit=glm(b$y~.,family = binomial(link = "logit"), data = b)

summary(fit)
split <- sample.split(b, SplitRatio = 0.8)
split
train <- subset(b, split == "TRUE")
test <- subset(b, split == "FALSE")

str(train)
y_train=train$y
train=as.data.frame(scale(train[,-1]))

fit1= glm(y_train ~., family = binomial(link = "logit"), data = train)
fit1
summary(fit1)
sum(round(predict(fit1,train, type = "response"))==y_train)/length(train[,2])

y_test=test$y
test=as.data.frame(scale(test[,-1]))

sum(round(predict(fit1,test, type = "response"))==y_test)/length(test[,2])



str(b)
null=glm(b$y~1,family= binomial,data=b)
full=glm(b$y~.,family=binomial,data=b)
step(null,scope = list(lower=null,upper=full),direction = "forward")
fit2=glm( b$y ~ area_worst + concave.points_worst + texture_worst + 
            area_se + smoothness_worst + compactness_se + symmetry_worst, 
          family = binomial, data = b)
summary(fit2)
sum(round(predict(fit2,train, type = "response"))==y_train)/length(train[,2])
y_test=test$y
test=as.data.frame(scale(test[,-1]))
sum(round(predict(fit2,test, type = "response"))==y_test)/length(test[,2])


prediction_test= predict(fit2,test, type = "response")
prediction_test= ifelse(prediction_test > 0.5, 1, 0)
error = mean(prediction_test!= y_test)
print(paste('Model Accuracy',1-error))

p = predict(fit2, test, type="response")
pr = prediction(p,y_test)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)#

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc#prediction_test= predict(fit1,test, type = "response")
#prediction_test= ifelse(prediction_test > 0.5, 1, 0)
#error = mean(prediction_test!= y_test)
#print(paste('Model Accuracy',1-error))

#p = predict(fit1, test, type="response")
#pr = prediction(p,y_test)
#prf = performance(pr, measure = "tpr", x.measure = "fpr")
#plot(prf)

str(b)
prediction_test= predict(fit2,b, type = "response")
prediction_test= ifelse(prediction_test > 0.5, 1, 0)
error = mean(prediction_test!= b$y)
print(paste('Model Accuracy',1-error))

p = predict(fit2, b , type="response")
pr = prediction(p,b$y)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# brian

exp(coef(fit2))
exp(cbind(OR=coef(fit2),confint(fit2)))
anova(fit2,test="Chisq")
cooks.distance<-cooks.distance(fit2)
cooks.distance
which(cooks.distance>1)
table(y_test, prediction_test >= 0.5)
104/nrow(test)
vif(fit2)
vif(fit1)
#walds test 
regTermTest(fit2,"area_worst")
regTermTest(fit2,"concave.points_worst")
regTermTest(fit2,"texture_worst")
regTermTest(fit2,"area_se")
regTermTest(fit2,"smoothness_worst")
regTermTest(fit2,"compactness_se")
regTermTest(fit2,"symmetry_worst")

rm(list=ls())


varImp(fit2)
install.packages("earth")
library(pscl)
install.packages("measures")
library(pscl)
pR2(fit2)
pR2(fit1)
VarImp(fit2)
anova(fit2,test="Chisq")
