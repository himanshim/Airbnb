library(data.table)
library(fastDummies)
library(stats)
library(bit64)
library(neuralnet)
library(dplyr)


df<-fread("C:/Users/kamat/Downloads/MIM/Semester 2/INST 737/project/nyc/nyc 2018 2019/log reg/host_aggregators.csv")

# #reading 2018 data
# l2018<-fread("C:/Users/kamat/Downloads/MIM/Semester 2/INST 737/project/nyc/nyc 2018 2019/log reg/listings 2018 march.csv")
# r2018<-fread("C:/Users/kamat/Downloads/MIM/Semester 2/INST 737/project/nyc/nyc 2018 2019/log reg/reviews 2018 march.csv")
# c2018<-fread("C:/Users/kamat/Downloads/MIM/Semester 2/INST 737/project/nyc/nyc 2018 2019/log reg/calendar 2018 march.csv")
 
# #reading 2019 data
# l2019<-fread("C:/Users/kamat/Downloads/MIM/Semester 2/INST 737/project/nyc/nyc 2018 2019/log reg/listings 2019 march.csv")
# r2019<-fread("C:/Users/kamat/Downloads/MIM/Semester 2/INST 737/project/nyc/nyc 2018 2019/log reg/reviews 2019 march.csv")
# c2019<-fread("C:/Users/kamat/Downloads/MIM/Semester 2/INST 737/project/nyc/nyc 2018 2019/log reg/calendar 2019 march.csv")
 
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

df$V1<-NULL
df$host_id<-NULL
df <- df[!is.infinite(df$min_reviews_per_month),]
df <- df[!is.infinite(df$first_seen),]
df <- df[!is.infinite(df$last_seen),]
df <- df[!is.infinite(df$host_since),]
df$host_since<-2019-df$host_since
df$first_seen<-2019-df$first_seen
df$last_seen<-2019-df$last_seen

# df$host_since<-as.factor(df$host_since)
# df$first_seen<-as.factor(df$first_seen)
# df$last_seen<-as.factor(df$last_seen)

df$list_count_frame<-as.numeric(ifelse(is.na(df$list_count_frame),0,paste(df$list_count_frame)))
df$diff_value<-as.numeric(ifelse(is.na(df$diff_value),0,paste(df$diff_value)))
df$diff_change<-ifelse(is.na(df$diff_change),0,paste(df$diff_change))
df <- df[!is.na(df$count_multilisting),]
df <- df[!is.na(df$review_scores_rating),]
df$diff_change<-as.factor(df$diff_change)
names(df)
str(df)

#normalisation
normalization <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

df$mean_price<-normalization(df$mean_price)
df$min_price<-normalization(df$min_price)
df$max_price<-normalization(df$max_price)
df$count_multilisting<-normalization(df$count_multilisting)
df$review_scores_rating<-normalization(df$review_scores_rating)
df$max_reviews_per_month<-normalization(df$max_reviews_per_month)
df$min_reviews_per_month<-normalization(df$min_reviews_per_month)
df$avg_number_reviews<-normalization(df$avg_number_reviews)
df$list_count_frame<-normalization(df$list_count_frame)
df$diff_value<-normalization(df$diff_value)
df$first_seen<-normalization(df$first_seen)
df$last_seen<-normalization(df$last_seen)
df$host_since<-normalization(df$host_since)

#dummy variables
df<-dummy_cols(df,select_columns = c("host_identity_verified","host_is_superhost_2019",
                                     "diff_change","exists_in_2019"),remove_first_dummy = FALSE)
df_copy<-df

#remove unnec. dummy
df$exists_in_2019_0<-NULL
df$host_is_superhost_2019_t<-NULL
df$host_identity_verified_t<-NULL
#remove dummy og variables
# df$host_since<-NULL
# df$first_seen<-NULL
# df$last_seen<-NULL
df$host_identity_verified<-NULL
df$host_is_superhost_2019<-NULL
df$diff_change<-NULL
df$exists_in_2019<-NULL

#training, test
n<-sample(nrow(df),0.70*nrow(df))
train<-df[n,]
test<-df[-n,]

# #model1 correlation: 0.99
# model1<-neuralnet(exists_in_2019_1 ~ .,data=train)
# plot(model1)
# pred1<-compute(model1,test[,1:19])
# results1<-pred1$net.result
# results1
# cor(results1,test$exists_in_2019_1)
# plot(results1,test$exists_in_2019_1)
# results1<-ifelse(results1<0.5,0,1)
# table(test$exists_in_2019_1,results1)

#model2
model2<-neuralnet(exists_in_2019_1 ~ min_price + max_price + host_since + review_scores_rating, data=train)
plot(model2)
pred2<-compute(model2,test[,1:19])
results2<-pred2$net.result
results2<-ifelse(results2<0.5,0,1)
table(test$exists_in_2019_1,results2)
mean(results2==test$exists_in_2019_1)

#model3
sigmoid=function (x) {
  1/(1+exp(-x))
}
model3<-neuralnet(exists_in_2019_1 ~ min_price + max_price + host_since + review_scores_rating, data=train, 
                  act.fct = sigmoid, linear.output = FALSE, err.fct = "ce")
plot(model3)
pred3<-compute(model3,test[,1:19])
results3<-pred3$net.result
results3<-ifelse(results3<0.5,0,1)
table(test$exists_in_2019_1,results3)
mean(results3==test$exists_in_2019_1)

#model4
start.time <- Sys.time()
model4<-neuralnet(exists_in_2019_1 ~ min_price + max_price + host_since + review_scores_rating, data=train, 
                  act.fct = sigmoid, linear.output = FALSE,hidden=4,threshold = 0.02)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
plot(model4)
pred4<-compute(model4,test[,1:19])
results4<-pred4$net.result
results4<-ifelse(results4<0.5,0,1)
table(test$exists_in_2019_1,results4)
mean(results4==test$exists_in_2019_1)

#model5
start.time <- Sys.time()
model5<-neuralnet(exists_in_2019_1 ~ min_price + max_price + host_since + review_scores_rating, data=train, 
                  act.fct = sigmoid, linear.output = FALSE,hidden=6,threshold = 0.02)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
plot(model5)
pred5<-compute(model5,test[,1:19])
results5<-pred4$net.result
results5<-ifelse(results5<0.5,0,1)
table(test$exists_in_2019_1,results5)
mean(results5==test$exists_in_2019_1)

#model6
start.time <- Sys.time()
model6<-neuralnet(exists_in_2019_1 ~ min_price + max_price + host_since + review_scores_rating, data=train, 
                  act.fct = sigmoid, linear.output = FALSE,hidden=c(3,2),threshold = 0.01)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
plot(model6)
pred6<-neuralnet::compute(model6,test[,1:19])
results6<-pred6$net.result
results6<-ifelse(results6<0.5,0,1)
table(test$exists_in_2019_1,results6)
mean(results6==test$exists_in_2019_1)

#model7
start.time <- Sys.time()
model7<-neuralnet(exists_in_2019_1 ~ min_price + max_price + host_since + review_scores_rating, data=train, 
                  act.fct = sigmoid, linear.output = FALSE,hidden=c(2,2),threshold = 0.01)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
plot(model7,main="abcd")
pred7<-neuralnet::compute(model7,test[,1:19])
results7<-pred7$net.result
results7<-ifelse(results7<0.5,0,1)
table(test$exists_in_2019_1,results7)
mean(results7==test$exists_in_2019_1)
pred7$results.matrix
RMSE.NN = (sum((test$exists_in_2019_1 - results7)^2) / nrow(test)) ^ 0.5
RMSE.NN
#model8
start.time <- Sys.time()
model8<-neuralnet(exists_in_2019_1 ~ min_price + max_price + host_since + review_scores_rating, data=train, 
                  act.fct = sigmoid, linear.output = FALSE,hidden=c(2,2,2),threshold = 0.02)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
plot(model8,main="abcd")
pred8<-neuralnet::compute(model8,test[,1:19])
results8<-pred8$net.result
results8<-ifelse(results8<0.5,0,1)
table(test$exists_in_2019_1,results8)
mean(results8==test$exists_in_2019_1)

#model9
start.time <- Sys.time()
model9<-neuralnet(exists_in_2019_1 ~ min_price + max_price + host_since + review_scores_rating, data=train, 
                  act.fct = sigmoid, linear.output = FALSE,hidden=c(2,2,7),threshold = 0.01)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
plot(model9,main="abcd")
pred9<-neuralnet::compute(model9,test[,1:19])
results9<-pred9$net.result
results9<-ifelse(results9<0.5,0,1)
table(test$exists_in_2019_1,results9)
mean(results9==test$exists_in_2019_1)
