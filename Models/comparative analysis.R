host = read.csv("host_aggregators.csv")

#checking NA values
colSums(is.na(host))

#removing columns with NA values and which we dont need

host$list_count_frame <- NULL
host$diff_value <- NULL
host$diff_change <- NULL
host$X <- NULL
host$review_scores_rating <- NULL
#host_dt$host_since <- NULL

library(tidyr)
host <- drop_na(host) #count row dropped from 37689 to 37681

colSums(is.na(host)) #check na values in each column

#Changing values for dates
host$host_since <- as.numeric(2019 - host$host_since)
host$first_seen <- as.numeric(2019 - host$first_seen)
host$last_seen <- as.numeric(2019 - host$last_seen)

#Dealing with missing values
levels(host$host_identity_verified)[1] = "missing"
levels(host$host_is_superhost_2019)[1] = "missing"

host$host_identity_verified <- factor(host$host_identity_verified)
host$host_is_superhost_2019 <- factor(host$host_is_superhost_2019)

host$first_seen<-NULL
host$last_seen<-NULL
host$max_reviews_per_month<-NULL
host$min_reviews_per_month<-NULL


View(host)



library(caret)
set.seed(3233)

control <- trainControl(method="cv", number=10)
#preProcValues <- preProcess(host, method = c("center","scale"))
set.seed(3)
model_SVM <- train(exists_in_2019~.,data=mysample,trControl=control,method="svmRadial",preProcess=c("center","scale"))
nnetGrid <-  expand.grid(size = c(2,2),
                         decay = c(0,0.1))
set.seed(2)
model_nn <- train(exists_in_2019~.,data=mysample,trControl=control,method="nnet", preProcess=c("center","scale"),tuneGrid = nnetGrid,verbose=FALSE)
set.seed(1)
model_trees <- train(exists_in_2019~.,data=mysample,trControl=control,method="C5.0",verbose=FALSE)
confusionMatrix(model_trees)
set.seed(4)
mtry <- 2
tunegrid <- expand.grid(.mtry=mtry)
model_rf <- train(exists_in_2019~.,data=mysample,trControl=control,method="rf", tuneGrid=tunegrid)

# collect resamples
results <- resamples(list(SVM=model_SVM, NNet=model_nn, DTree=model_trees, RForest=model_rf))
# summarize the distributions
summary(results)
densityplot(results , metric = "Accuracy" ,auto.key = list(columns = 3))

bwplot(results , metric = c("Kappa","Accuracy","Sensitivity"))
dotplot(results)


#------------------------------------------------

#####Repeated
control_repeat <- trainControl(method="repeatedcv", number=10,repeats=3)

set.seed(3)
model_SVM_r <- train(exists_in_2019~.,data=mysample,trControl=control_repeat,method="svmRadial",preProcess=c("center","scale"))
set.seed(2)
nnetGrid_r <-  expand.grid(size = c(2,2),
                              decay = c(0,0.1))
model_nn_r <- train(exists_in_2019~.,data=mysample,trControl=control_repeat,method="nnet", preProcess=c("center","scale"),tuneGrid = nnetGrid_r,verbose=FALSE)
set.seed(1)
model_trees_r <- train(exists_in_2019~.,data=mysample,trControl=control_repeat,method="C5.0",verbose=FALSE)
# confusionMatrix(model_trees)
set.seed(4)
mtry <- 2
tunegrid <- expand.grid(.mtry=mtry)
model_rf_r <- train(exists_in_2019~.,data=mysample,trControl=control_repeat,method="rf", tuneGrid=tunegrid)

# collect resamples
results_r <- resamples(list(SVM=model_SVM_r, NNet=model_nn_r, DTree=model_trees_r, RForest=model_rf_r))
# summarize the distributions
summary(results_r)
densityplot(results_r , metric = "Accuracy" ,auto.key = list(columns = 3))

bwplot(results_r , metric = c("Kappa","Accuracy","Sensitivity"))
dotplot(results_r)


#----------------------------------------------
#####LOOCV
control_leave <- trainControl(method="LOOCV",preProcOptions = list(thresh = 0.8))
#preProcValues <- preProcess(host, method = c("center","scale"))
set.seed(3)
model_SVM_1<- train(exists_in_2019~.,data=mysample1,trControl=control_leave,method="svmRadial",preProcess=c("center","scale"))
nnetGrid <-  expand.grid(size = c(2,2),
                              decay = c(0,0.1))
set.seed(2)
model_nn_1 <- train(exists_in_2019~.,data=mysample1,trControl=control_leave,method="nnet", preProcess=c("center","scale"),tuneGrid = nnetGrid,verbose=FALSE)
set.seed(1)
model_trees_1 <- train(exists_in_2019~.,data=mysample1,trControl=control_leave,method="C5.0",preProcess=c("center","scale"),verbose=FALSE)
set.seed(4)
mtry <- 2
tunegrid <- expand.grid(.mtry=mtry)
model_rf_1 <- train(exists_in_2019~.,data=mysample1,trControl=control_leave,method="rf", preProcess=c("center","scale"),tuneGrid=tunegrid)


# collect resamples
results_l <- resamples(list(SVM=model_SVM_1, NNet=model_nn_1, DTree=model_trees_1, RForest=model_rf_1))
# summarize the distributions
summary(results_l)

