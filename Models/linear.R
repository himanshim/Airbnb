#Loading libraries
library(caret)
library(rsample)
library(gbm)
library(randomForest)
library(DAAG)
library(glmnet)
library(mltools)
library(gmodels)
library(rpart)
library(ggRandomForests)

mydata<-read.csv("listings-real.csv")
str(mydata)

#Converting to numeric
mydata$price<-as.numeric(mydata$price)
mydata$extra_people<-as.numeric(mydata$extra_people)
mydata$cleaning_fee<-as.numeric(mydata$cleaning_fee)
mydata$security_deposit<-as.numeric(mydata$security_deposit)

#full model with all the variables
full_m <- lm(price~host_is_superhost+room_type+bathrooms+bedrooms
             +accommodates+number_of_reviews+security_deposit+cleaning_fee
             +extra_people+review_scores_rating+review_scores_communication
             +reviews_per_month, data=mydata)
summary(full_m)

#To find most useful model
step(full_m,data=mydata, direction="backward")

#Creating a new dataset with those features
newdata<-mydata[c(61,29,53,54,55,56,83,64,65,87,91,106)]
str(newdata)

#omitting rows with NA values
newdata<-na.omit(newdata)
View(newdata)
str(newdata)

#Building training and testind data
testidx<-which(1:nrow(newdata)%%20==0)
testidx
traindata<-newdata[-testidx,]
testdata<-newdata[testidx,]
testdata

#Building linear model with each predictor
mclean <- lm(price~cleaning_fee, data=traindata)
summary(mclean)
predictionclean<-predict(mclean,newdata = testdata)
predictionclean
data.frame( MSE =mse(predictionclean, testdata$price))
cor(predictionclean,testdata$price)
#Plot residuals 
rsclean<-residuals(mclean)
qqnorm(rsclean)
qqline(rsclean)
#confidence and prediction bands
pc <- predict(mclean, int="c",newdata =testdata )
pp <- predict(mclean, int="p",newdata =testdata )
nrow(testdata)
nrow(pc)
plot(newdata$price,newdata$cleaning_fee)
matlines(testdata$price,pc)
matlines(testdata$price,pp)


mnum <- lm(price~number_of_reviews, data=traindata)
summary(mnum)
predictionnum<-predict(mnum,newdata = testdata)
predictionnum
data.frame( MSE =mse(predictionnum, testdata$price))
cor(predictionnum,testdata$price)
#Plot residuals 
rsnum<-residuals(mnum)
qqnorm(rsnum)
qqline(rsnum)

#Build the linear model with the best features
model<-lm(price~host_is_superhost+room_type+accommodates+bathrooms+bedrooms+number_of_reviews+security_deposit+cleaning_fee+review_scores_rating
          +review_scores_communication+reviews_per_month,data=traindata)
summary(model)
sqrt(mean(model$residuals^2))

# Make predictions and compute the R2, RMSE and MAE
prediction<-predict(model,newdata = testdata)
prediction
data.frame( R2 = R2(prediction, testdata$price),
            RMSE = RMSE(prediction, testdata$price),
            MSE =mse(prediction, testdata$price),
            MAE = MAE(prediction, testdata$price))

cor(prediction,testdata$price)

#Plot residuals 
rs<-residuals(model)
qqnorm(rs)
qqline(rs)

# Define train control for k fold cross validation
set.seed(123) 
train_control <- trainControl(method="cv", number=10, verbose=TRUE)
# Fit Linear Model
lm1<-train(price~host_is_superhost+room_type+accommodates+bathrooms+bedrooms+number_of_reviews+security_deposit+cleaning_fee+review_scores_rating
           +review_scores_communication+reviews_per_month, data=newdata, method="lm",trControl=train_control)
# Summarise Results
print(lm1)



#Regularization using Lasso and Ridge
str(traindata)

#Lasso
cv.fitl<-cv.glmnet(as.matrix(traindata[,c(-1,-2,-3)]),as.vector(traindata[,1]),alpha=1)
cv.fitl$lambda.min
plot(cv.fitl)
coef(cv.fitl)
predictionl<-predict(cv.fitl,newx = as.matrix(testdata[,c(-1,-2,-3)]))
data.frame( R2 = R2(predictionl, testdata$price),
            RMSE = RMSE(predictionl, testdata$price),
            MSE =mse(predictionl, testdata$price),
            MAE = MAE(predictionl, testdata$price))#RMSE-260.2237
cor(predictionl,as.vector(testdata[,1]),use = "complete.obs")

#Ridge
cv.fitr<-cv.glmnet(as.matrix(traindata[,c(-1,-2,-3)]),as.vector(traindata[,1]),alpha=0)
cv.fitr$lambda.min
plot(cv.fitr)
coef(cv.fitr)

predictionr<-predict(cv.fitr,newx = as.matrix(testdata[,c(-1,-2,-3)]))
data.frame( R2 = R2(predictionr, testdata$price),
            RMSE = RMSE(predictionr, testdata$price),
            MSE =mse(predictionr, testdata$price),
            MAE = MAE(predictionr, testdata$price))#RMSE-260.1439

cor(predictionr,as.vector(testdata[,1]),use = "complete.obs")



#----------DECISION TREE-------------

set.seed(12345)
data_rand <- newdata[order(runif(38097)),]



#generating training and testing
new_train	<-	data_rand[1:30930,]	
new_test		<-	data_rand[30931:38097,]	

#Checking distribution 
summary(newdata$price)
summary(new_train$price)
summary(new_test$price)


#building decision tree
	
fit<-rpart(new_train$price~.,method="anova",data=new_train)
printcp(fit)
plotcp(fit)
summary(fit)


#plot tree
plot(fit,uniform=TRUE,main="Regression tree for listing price")
text(fit,use.n=TRUE, all=TRUE,cex=.8)
fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]

#pruning tree
pfit<-prune(fit,cp="0.01")
plot(fit,uniform=TRUE,main="Pruned Regression tree for listing price")
text(fit,use.n=TRUE, all=TRUE,cex=.8)


#prediction
price_pred <- predict(fit,new_test)
price_pred

data.frame( R2 = R2(price_pred, new_test$price),
            RMSE = RMSE(price_pred, new_test$price),
            MSE =mse(price_pred, new_test$price),
            MAE = MAE(price_pred, new_test$price))
cor(price_pred,new_test$price)
plot(price_pred,new_test$price)
abline(c(0,1),col=2)

#BOOSTING

bm.fit <- gbm(
  formula = price~host_is_superhost+room_type+bathrooms+bedrooms+accommodates+number_of_reviews+security_deposit+cleaning_fee+review_scores_rating
  +review_scores_communication+reviews_per_month,
  data = newdata,
  n.trees = 5000,
  n.minobsinnode = 10,
  shrinkage = 0.1,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  


print(bm.fit)

bestTreeForPrediction <- gbm.perf(bm.fit, method = "cv")
bestTreeForPrediction

boost_pred = predict(object = bm.fit,newdata=new_test,n.trees = bestTreeForPrediction)
boost_pred
data.frame( R2 = R2(boost_pred, new_test$price),
            RMSE = RMSE(boost_pred, new_test$price),
            MSE =mse(boost_pred,new_test$price),
            MAE = MAE(boost_pred, new_test$price))                    

cor(boost_pred,new_test$price) 
plot(boost_pred,new_test$price)
abline(c(0,1), col=2)


#Bagging
bagging_train<-sample(1:nrow(newdata),nrow(newdata)/2)
bag.price<-randomForest(price~.,data = newdata,subset = bagging_train,mtry=12,importance = TRUE)
bag.price
bag_pred<-predict(bag.price,newdata = newdata[-bagging_train,])
bag_pred
cor(bag_pred,newdata[-bagging_train,]$price) 
sqrt(mean((bag_pred-newdata[-bagging_train,1])^2))
plot(bag_pred,newdata[-bagging_train,1])
abline(c(0,1),col=2)

#Most important features
plot(gg_vimp(bag.price))
importance(bag.price)
varImpPlot(bag.price)


#Random Forests using the best mtry
rand.price<-randomForest(price~.,data = newdata,subset = bagging_train,mtry=4,importance = TRUE)
rand.price
rand_pred<-predict(rand.price,newdata = newdata[-bagging_train,])
rand_pred
cor(rand_pred,newdata[-bagging_train,]$price) 
sqrt(mean((rand_pred-newdata[-bagging_train,1])^2))
plot(rand_pred,newdata[-bagging_train,1])
abline(c(0,1),col=2)
