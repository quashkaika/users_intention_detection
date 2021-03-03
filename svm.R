library(readr)
library(dplyr)
library(tidyverse)
library(e1071)
library(class)
library(caret)

str(condition1)
#cleaning NA
clean_condition1 <- condition1 %>% rowwise %>% do({
  result = as.data.frame(.)
  if (is.infinite(result$fix_rate)) {
    result$fix_rate  = NA
  }
  if (is.infinite(result$sac_rate)) {
    result$sac_rate  = NA
  }
  if (is.infinite(result$fix_proportion)) {
    result$fix_proportion  = NA
  }
  if (is.infinite(result$sac_proportion)) {
    result$sac_proportion  = NA
  }
  result
})
nrow(clean_condition1)
clean_condition1_upd <- na.omit(clean_condition1)
nrow(clean_condition1_upd)

clean_condition1_upd1 <- clean_condition1_upd %>% rowwise %>% do({
  result = as.data.frame(.)
  if (result$reaction_type == 1) {
    result$reaction_type  = 1
  }
  if (result$reaction_type == 0) {
    result$reaction_type  = 1
  }
  if (result$reaction_type == 999) {
    result$reaction_type  = 0
  }
  result
})

nrow(clean_condition1_upd1)
#clean_condition1_upd1$reaction_type <- as.factor(clean_condition1_upd1$reaction_type)

clean_condition1_upd1 <- data.frame(clean_condition1_upd1)
str(clean_condition1_upd1)
#filter by reaction type
clean_condition1_upd1_r1 <- clean_condition1_upd1 %>%
  filter(reaction_type == 1)
nrow(clean_condition1_upd1_r1)
#view(clean_condition1_upd1_r1)

clean_condition1_upd1_r0 <-  clean_condition1_upd1 %>%
  filter(reaction_type == 0)
nrow(clean_condition1_upd1_r0)
#str(clean_condition1_upd1_r0)

#splitting the data
r0_samplesize =floor(0.8*nrow(clean_condition1_upd1_r0))


r0_picked = sample(seq_len(nrow(clean_condition1_upd1_r0)),size = r0_samplesize)
r1_picked = sample(seq_len(nrow(clean_condition1_upd1_r1)),size = r0_samplesize)

r1_train = clean_condition1_upd1_r1[r1_picked,]
nrow(r1_train)
r0_train = clean_condition1_upd1_r0[r0_picked,]
nrow(r0_train)
train <- rbind(r0_train, r1_train)
str(train)

r0_test = clean_condition1_upd1_r0[-r0_picked,]
nrow(r0_test)
r1_test1 = clean_condition1_upd1_r1[-r1_picked,]
r1_test <- r1_test1[sample(nrow(r1_test1), 78), ]
nrow(r1_test)
test <- rbind(r0_test, r1_test)
str(test)




train0 <-  train %>%
  filter(reaction_type==0)
train1 <-  train %>%
  filter(reaction_type==1)
nrow(train0)
nrow(train1)
nrow(train)


nrow(test)
test0 <-  test %>%
  filter(reaction_type==0)
nrow(test0)

test1 <-  test %>%
  filter(reaction_type==1)
nrow(test1)

set.seed(12)
r0_samplesize2 =floor(0.5*nrow(test0))

r0_picked2 = sample(seq_len(nrow(test0)),size = r0_samplesize2)
r1_picked2 = sample(seq_len(nrow(test1)),size = r0_samplesize2)

r1_test_val = test1[r1_picked2,]
nrow(r1_test_val)
r0_test_val = test0[r0_picked2,]
nrow(r0_test_val)
test_val <- rbind(r0_test_val, r1_test_val)
nrow(test_val)

test_val0 <-  test_val %>%
  filter(reaction_type==0)
nrow(test_val0)

test_val1 <-  test_val %>%
  filter(reaction_type==1)
nrow(test_val1)

r1_test_test = test1[-r1_picked2,]
r0_test_test = test0[-r0_picked2,]
test_test <- rbind(r0_test_test, r1_test_test)
nrow(test_test)

test_test0 <-  test_test %>%
  filter(reaction_type==0)
nrow(test_test0)

test_test1 <-  test_test %>%
  filter(reaction_type==1)
nrow(test_test1)

nrow(train)
nrow(test_test)
nrow(test_val)
#normalization
nor <- function(x){
  (x-min(x))/(max(x)-min(x))
}
train_norm <- as.data.frame(lapply(train[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)],nor))
train_norm$reaction_type <- as.factor(train_norm$reaction_type)
str(train_norm)

test_test_norm <- as.data.frame(lapply(test_test[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)],nor))
test_test_norm$reaction_type <- as.factor(test_test_norm$reaction_type)
str(test_test_norm)

test_val_norm <- as.data.frame(lapply(test_val[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)],nor))
test_val_norm$reaction_type <- as.factor(test_val_norm$reaction_type)
str(test_val_norm)

#train the model
model <- svm(reaction_type ~ fix_total+fix_mean+pupil_radius+sac_peak_velocity+pupil_radius_dif,data=train_norm, kernel="radial", cost =1)
summary(model)
#plot1 <-plot(model,data=train_norm,in_range_fix ~ in_range_sac)

#tune the model
#tune.out=tune(svm ,reaction_type ~ fix_mean+fix_total+pupil_radius+sac_peak_velocity,data=train_norm,kernel ="radial",
              #ranges =list(cost=c(0.001,0.01,0.1,1,5,10,100)),gamma=c(.5,1,2))
#svm_tune <- tune(svm, train.x=x, train.y=y, 
                # kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

#print(svm_tune)
#summary(tune.out)
#bestmod =tune.out$best.model
#bestmod

#validation
pred <- predict(model, test_val_norm)
m <- table(pred = pred, true = test_val_norm$reaction_type)
m
confusionMatrix(test_val_norm$reaction_type,pred)
plot2 <-plot(model,data=test_val_norm, pupil_radius ~ fix_mean)

#test
pred <- predict(model, test_test_norm)
l <- table(pred = pred, true = test_test_norm$reaction_type)
l
confusionMatrix(test_test_norm$reaction_type,pred)
plot3 <-plot(model,data=test_test_norm, pupil_radius ~ fix_mean)

