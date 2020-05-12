remove(list = ls())

setwd("/Users/iseonmi/Desktop/1-1/ME 특론/")
DATA <- read.csv(file="heart.csv",head=TRUE)
a<-c('age','sex','cp','trestbps','chol','fbs','restecg','thalach','exang','oldpeak','slope','ca','thal','target')
colnames(DATA) <- a



#make null
DATA <- DATA[!(DATA$ca==4),]
DATA <- DATA[!(DATA$thal ==0),]

#row number
r <- dim(DATA)[1]
#correlation graph
group <- NA
group[DATA$target == 0]<- 1
group[DATA$target ==1]<-2

pairs(DATA,
      col = c("blue", "red")[group],   # Change color by group
      pch = c(8, 18)[group],
      main = "Correlation")



#binomial = sex, fbs --> same factor 로 작용하도록 납두기(inregression model)
#nomial = 'cp','restecg','thal'
#for logistic regression --> numeric change

new <- c(1:r)
for (i in 1:r){
  if (DATA$cp[i]==0){
    new[i] = 0
  }else{
    new[i] = 1
  }
}
new_D <- cbind(DATA,new)
colnames(new_D) <- c('age','sex','cp','trestbps','chol','fbs','restecg','thalach','exang','oldpeak','slope','ca','thal','target','cp_ox')


ECG_normality <- c(1:r)
for (i in 1:r){
  if (DATA$restecg[i]==1){
    ECG_normality[i] = 0
  }else{
    ECG_normality[i] = 1
  }
}

new_D <- cbind(new_D,ECG_normality)


defect_ox <- c(1:r)
for (i in 1:r){
  if (DATA$thal[i]==2){
  defect_ox[i] = 0
  }else{
    defect_ox[i] = 1
  }
}
new_D <- cbind(new_D,defect_ox)

slope_new <- c(1:r)
for (i in 1:r){
  if (DATA$slope[i]==2){
    slope_new[i] = 1
  }else{
    slope_new[i] = 0
  }
}
new_D <- cbind(new_D,slope_new)

ca_new <- c(1:r)
for (i in 1:r){
  if (DATA$ca[i]==0){
    ca_new[i] = 0
  }else{
    ca_new[i] = 1
  }
}
new_D <- cbind(new_D,ca_new)


#visualizing nomial categories
colour = c('#365b7a','#c12e29')
attach(mtcars)
par(mfrow = c(3,5))
counts <- table(DATA$target, DATA$cp)
barplot(counts,
        xlab="cp 4 types", col=colour,
        beside=TRUE)
counts <- table(new_D$target, new_D$restecg)
barplot(counts,
        xlab="restECG_strange/normal/strange", col=colour,
        beside=TRUE)
counts <- table(new_D$target, new_D$thal)
barplot(counts, 
        xlab="fixed/normal/reversable defect", col=colour,
        beside=TRUE)
counts <- table(new_D$target, new_D$ca)
barplot(counts, #main="restECG and disease rate",
        xlab="ca num", col=colour,
        beside=TRUE)
counts <- table(new_D$target, new_D$slope)
barplot(counts, #main="restECG and disease rate",
        xlab="0 = downsloping, 1 = flat, 2 = upsloping", col=colour,
        beside=TRUE)

counts <- table(new_D$target, new_D$cp_ox)
barplot(counts, #main="restECG and disease rate",
        xlab="chestpain_xo", col=colour,
        beside=TRUE)
counts <- table(new_D$target, new_D$ECG_normality)
barplot(counts, #main="restECG and disease rate",
        xlab="restECG_normal/abnormal", col=colour,
        beside=TRUE)
counts <- table(new_D$target, new_D$defect_ox)
barplot(counts, #main="restECG and disease rate",
        xlab="blood stream defect ox", col=colour,
        beside=TRUE)
counts <- table(new_D$target, new_D$ca_new)
barplot(counts, #main="restECG and disease rate",
        xlab="existance of ca", col=colour,
        beside=TRUE)
counts <- table(new_D$target, new_D$slope_new)
barplot(counts, #main="restECG and disease rate",
        xlab="down or flat / upsloping", col=colour,
        beside=TRUE)

####
#logistic regression
fitting_num <- new_D
glm.fit_num <- glm(target ~ age + sex + thalach + exang + oldpeak + slope +ca +restecg + cp + slope + thal, family=binomial(), data = fitting_num)
summary(glm.fit_num)
car::vif(glm.fit_num)
pR2(glm.fit_num)
qchisq(0.95, df=288)
hoslem.test(glm.fit_num$y, fitted(glm.fit_num))


#accuracy
fitting_num$target<-factor(fitting_num$target)
library(tidyverse)
library(caret)
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
model <- train(target ~ age + sex + thalach + exang + oldpeak + slope +ca +restecg + cp + slope + thal,
               data = fitting_num,
               trControl = train.control,
               method = "glm",
               family=binomial())
confusionMatrix(model)




###############
#categorizing continuous data to verify dependence of variables for regression model and naive bayes
#n_D = dataset with categorized continuous data


n_D <- DATA

#qunatile

for (j in c('age', 'trestbps','chol', "thalach")){
  a<-quantile(DATA[,j])
  for (i in 1:r){
    if (DATA[i,j]<a[2]){
      n_D[i,j] = 1
    }else if ((a[2]-0.001<DATA[i,j])&(DATA[i,j]<a[3])){
      n_D[i,j] = 2
    }else if ((a[3]-0.001<DATA[i,j])&(DATA[i,j]<a[4])){
      n_D[i,j] = 3
    }else{
      n_D[i,j] = 4
    }
  }
}

j <- 'oldpeak'
a<-quantile(DATA$oldpeak)
for (i in 1:r){
  if (DATA[i,j]==a[2]){
    n_D[i,j] = 1
  }else if ((a[2]<DATA[i,j])&(DATA[i,j]<a[3])){
    n_D[i,j] = 2
  }else if ((a[3]-0.001<DATA[i,j])&(DATA[i,j]<a[4])){
    n_D[i,j] = 3
  }else{
    n_D[i,j] = 4
  }
}
#visualizing 
colour = c('#365b7a','#c12e29')
attach(mtcars)
par(mfrow = c(3,5))
for (i in colnames(n_D)){
  count<-table(n_D$target,n_D[,i])
  barplot(count,col = colour ,beside = TRUE,
          main = i )
}

n_D$target <-as.factor(n_D$target)


#naive bayes model accuracy with all variables(before dependence test)
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)

model_1 <- train(target ~ ., 
                 data = n_D, method = "nb",
                 trControl = train.control)


confusionMatrix(model_1)



#######
## Selecting variables after dependence test
#variables
category<- c(2,3,6,7,9,11,12,13)
variables <- n_D[,c('sex','trestbps','chol','fbs','slope','ca')]
#variables <- n_D_2[,c(2,3, 4, 6, 7,11,12)]
l<-length(colnames(variables))

#dependence test result 'sex', 'cp', 'trestbps', 'chol', 'fbs', 'restecg', 'slope', 'ca'
dep<-array(0,c(l,l))
as.data.frame(dep)
colnames(dep)<-colnames(variables[,1:l])
rownames(dep)<-colnames(variables[,1:l])

#chi-square test-if 2 variables are dependent, print out 1
library(prettyR)
for (i in 1:(l-1)){
  for (j in 1:(l-i)){
    inter_<-cbind(variables[,l+1-i],variables[,j])
    as.data.frame(inter_)
    colnames(inter_)<-c(colnames(variables)[l+1-i],colnames(variables)[j])
    (crosstab<-xtabs(~., data = inter_))
    p<-chisq.test(crosstab,correct = TRUE)
    if (p['p.value'] < 0.05){
      dep[l+1-i,j] <- 1
    }
  }  
}

dep_naive_bayes<-dep
dep_naive_bayes

#for regression variables
#variables
category<- c(2,3,6,7,9,11,12,13)
variables <- n_D[,c('sex','trestbps','chol','fbs')]
variables <- cbind(variables,new_D[,c('slope_new','ca')])
#variables <- n_D_2[,c(2,3, 4, 6, 7,11,12)]
l<-length(colnames(variables))

#dependence test result 'sex', 'cp', 'trestbps', 'chol', 'fbs', 'restecg', 'slope', 'ca'
dep<-array(0,c(l,l))
as.data.frame(dep)
colnames(dep)<-colnames(variables[,1:l])
rownames(dep)<-colnames(variables[,1:l])

#chi-square test-if 2 variables are dependent, print out 1
library(prettyR)
for (i in 1:(l-1)){
  for (j in 1:(l-i)){
    inter_<-cbind(variables[,l+1-i],variables[,j])
    as.data.frame(inter_)
    colnames(inter_)<-c(colnames(variables)[l+1-i],colnames(variables)[j])
    (crosstab<-xtabs(~., data = inter_))
    p<-chisq.test(crosstab,correct = TRUE)
    if (p['p.value'] < 0.05){
      dep[l+1-i,j] <- 1
    }
  }  
}

dep_regression<-dep
dep_regression



#logistic regression
#nomial <- c('cp','restecg','thal')
#b_nomial <- c('sex','fbs')

fitting <- new_D[,c('sex','trestbps','chol','fbs','slope_new','ca','target')]
glm.fit <- glm(target ~ . , family=binomial(), data = fitting)
summary(glm.fit)
car::vif(glm.fit)
#install.packages('pscl')
library(pscl)
pR2(glm.fit)
library(ResourceSelection)
library(oddsratio)
qchisq(0.95, df=284)
hoslem.test(glm.fit$y, fitted(glm.fit))

#accuracy
fitting$target<- factor(fitting$target)
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)

model_1 <- train(target ~ ., 
                 data = fitting, method = "nb",
                 trControl = train.control)


confusionMatrix(model_1)





#naive bayes model

fitting <- n_D[,c('sex','trestbps','chol','fbs','ca','target')]
fitting <- cbind(fitting,new_D[,'slope_new'])
fitting$target<-factor(fitting$target)
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
model <- train(target ~.,
               data = n_D,
               trControl = train.control,
               method = "glm",
               family=binomial())
confusionMatrix(model)

