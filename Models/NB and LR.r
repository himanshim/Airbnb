library(data.table)
library(Amelia)
library(pscl)
library(InformationValue)
library(ggplot2)
library(pROC)
library(ROCR)
library(e1071)
library(caret)
library(BBmisc)
library(klaR)

df<-fread("host_aggregators.csv")
# df_copy<-data.frame(df)
# df<-data.frame(df_copy)

#reading 2018 data
l2018<-fread("listings 2018 march.csv")
r2018<-fread("reviews 2018 march.csv")
c2018<-fread("calendar 2018 march.csv")

#reading 2019 data
l2019<-fread("listings 2019 march.csv")
r2019<-fread("reviews 2019 march.csv")
c2019<-fread("calendar 2019 march.csv")

#formatting some fields
df$mean_price<-round(df$mean_price)
df$avg_number_reviews<-round(df$avg_number_reviews)
df$review_scores_rating<-round(df$review_scores_rating)

#converting chr to factor
df$host_identity_verified<-ifelse(df$host_identity_verified=="t","t",
                                  ifelse(df$host_identity_verified=="f","f","t"))

df$host_identity_verified<-ifelse(is.na(df$host_identity_verified),"t",paste(df$host_identity_verified))
df$host_identity_verified<-factor(df$host_identity_verified)

df$host_is_superhost_2019<-ifelse(df$host_is_superhost_2019=="t","t",
                                  ifelse(df$host_is_superhost_2019=="f","f","t"))
df$host_is_superhost_2019<-ifelse(is.na(df$host_is_superhost_2019),"t",paste(df$host_is_superhost_2019))
df$host_is_superhost_2019<-factor(df$host_is_superhost_2019)

df$exists_in_2019<-ifelse(df$exists_in_2019=="y",1,0)
df$exists_in_2019<-factor((df$exists_in_2019))
contrasts(df$exists_in_2019)

#setting sample size for splitting into test and train data
size=floor(0.75 * nrow(nb_df))
set.seed(123)
train_ind<-sample(seq_len(nrow(nb_df)),size=size)
train<-nb_df[train_ind,]
test<-nb_df[-train_ind,]

model<-glm(exists_in_2019 ~ min_price + max_price +count_multilisting + host_since + 
             review_scores_rating + avg_number_reviews, data=train, family="binomial")
exp(coef(model))

prob<-predict(model,newdata=test,type="response")
pred<-prediction(prob,test$exists_in_2019)
perf<-performance(pred,measure="tpr",x.measure = "fpr")
plot(perf)
auc<-performance(pred,measure = "auc")
auc<-auc@y.values[[1]]
auc


################## Naive Bayes ####################
nb_df <- df
nb_df$V1<-NULL
nb_df$host_id<-NULL
nb_df$exists_in_2019<-ifelse(nb_df$exists_in_2019=="y",1,0)
nb_df$exists_in_2019<-as.factor((nb_df$exists_in_2019))
nb_df <- nb_df[!is.infinite(nb_df$min_reviews_per_month),]
nb_df <- nb_df[!is.infinite(nb_df$first_seen),]
nb_df <- nb_df[!is.infinite(nb_df$last_seen),]
nb_df <- nb_df[!is.infinite(nb_df$host_since),]
nb_df$host_since<-as.factor(nb_df$host_since)
nb_df$first_seen<-as.factor(nb_df$first_seen)
nb_df$last_seen<-as.factor(nb_df$last_seen)
nb_df$list_count_frame<-as.numeric(ifelse(is.na(nb_df$list_count_frame),0,paste(nb_df$list_count_frame)))
nb_df$diff_value<-as.numeric(ifelse(is.na(nb_df$diff_value),0,paste(nb_df$diff_value)))
nb_df$diff_change<-ifelse(is.na(nb_df$diff_change),0,paste(nb_df$diff_change))
nb_df <- nb_df[!is.na(nb_df$count_multilisting),]
nb_df <- nb_df[!is.na(nb_df$review_scores_rating),]
nb_df$diff_change<-as.factor(nb_df$diff_change)

#converting chr to factor
nb_df$host_identity_verified<-ifelse(nb_df$host_identity_verified=="t","t",
                                  ifelse(nb_df$host_identity_verified=="f","f","t"))
nb_df$host_identity_verified<-ifelse(is.na(nb_df$host_identity_verified),"t",paste(nb_df$host_identity_verified))
nb_df$host_identity_verified<-as.factor(nb_df$host_identity_verified)
str(nb_df)

nb_df$host_is_superhost_2019<-ifelse(nb_df$host_is_superhost_2019=="t","t",
                                  ifelse(nb_df$host_is_superhost_2019=="f","f","t"))
nb_df$host_is_superhost_2019<-ifelse(is.na(nb_df$host_is_superhost_2019),"t",paste(nb_df$host_is_superhost_2019))
nb_df$host_is_superhost_2019<-as.factor(nb_df$host_is_superhost_2019)

#setting sample size for splitting into test and train data
size=floor(0.75 * nrow(nb_df))
set.seed(123)
train_ind<-sample(seq_len(nrow(nb_df)),size=size)
train<-nb_df[train_ind,]
test<-nb_df[-train_ind,]
str(nb_df)
model2 <- naiveBayes(exists_in_2019 ~ ., data=train,usekernel = TRUE)
model2
actuals <- test$exists_in_2019
#test$exists_in_2019<-NULL

#predicting for NB
prediction <- predict(model2, newdata=test)

#creating confusion matrix
cm<-table(actuals,prediction)
cm

confusionMatrix(prediction,actuals)
model_laplace<-naiveBayes(exists_in_2019 ~ .,data=train,laplace = 1)
laplace_pred<-predict(model_laplace,newdata=test, type="class")
table(actuals,laplace_pred)

(accuracy <- sum(diag(cm)) / sum(cm))