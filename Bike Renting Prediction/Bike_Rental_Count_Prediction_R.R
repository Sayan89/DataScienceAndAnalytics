#remove all the objects stored
rm(list=ls())

#set current working directory
setwd("C:/Users/SAYAN/Desktop/Data Science/Projects/Bike Rental Count/R Folder")

#Current working directory
getwd()

#instaling packages
install.packages(c("corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
                   "MASS", "rpart", "gbm", "ROSE", "sampling", "DataCombine", "inTrees","Hmisc","corrplot","usdm","ggplot2"))

#reading CSV data sheet
data = read.csv("Bike_Rental_Count_data.csv", header = T, sep = ",")

#checking data summary
summary(data)

#checking data dimensions
dim(data)

#checking column names of data
colnames(data)

#structure of data
str(data)

#creating reference object with same data
data_1 = data
#str(data_1)

#getting model details

#names(getModelInfo())

#help

#help("<datasetname>")

#changing dteday type to date
data_1$dteday = as.factor(data_1$dteday)
#data_1$dteday = as.numeric(as.POSIXct(data_1$dteday))

#changing season, yr, mnth, holiday, weekday, workingday, weathersit types to factor
data_1$season = as.factor(data_1$season)
data_1$yr = as.factor(data_1$yr)
data_1$mnth = as.factor(data_1$mnth)
data_1$holiday = as.factor(data_1$holiday)
data_1$weekday = as.factor(data_1$weekday)
data_1$workingday = as.factor(data_1$workingday)
data_1$weathersit = as.factor(data_1$weathersit)
  

#Missing value analysis 
sum(is.na(data_1))

# Missing value analysis is hence not required since all the variables contain the same number of observations, 
# i.e., 731 without any NA values.

#Converting INT datatype variables to numeric NUM
data_1$casual = as.numeric(data_1$casual)
data_1$registered = as.numeric(data_1$registered)
data_1$cnt = as.numeric(data_1$cnt)

#check structure of data
str(data_1)

#Outlier analysis in un-normalized independent numeric variables
library(ggplot2)
numerics = sapply(data_1, is.numeric)
numeric_cols = data_1[,numerics]
cnames = c("temp","atemp","hum","windspeed","casual","registered")

for (i in 1:length(cnames))
{
  assign(paste0("oa",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), 
                                data = subset(data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste("Box plot",i)))
}

# Plotting box plots to show outlier values
gridExtra::grid.arrange(oa1,oa2,ncol=2)
gridExtra::grid.arrange(oa3,oa4,ncol=2)
gridExtra::grid.arrange(oa5,oa6,ncol=2)

#Replace all outliers with 0 and impute
for(i in cnames){
  #print(i)
  val = data_1[,i][data_1[,i] %in% boxplot.stats(data_1[,i])$out]
  #print(length(val))
  data_1[,i][data_1[,i] %in% val] = NA
}

#replacing NA values in variable "casual" with median
data_1$casual[is.na(data_1$casual)] = median(data_1$casual, na.rm = TRUE)

for (i in 1:length(data_1$instant))
{
  data_1$cnt[i] = data_1$casual[i] + data_1$registered[i]
}

#replacing NA values of "hum" and "windspeed" with median
data_1$hum[is.na(data_1$hum)] = median(data_1$hum, na.rm = TRUE)
data_1$windspeed[is.na(data_1$windspeed)] = median(data_1$windspeed, na.rm = TRUE)

#correlation test on numeric variables
value = round(cor(as.matrix(data_1[,numerics])),2)

library(corrgram)
corrgram(data_1[,numerics], order=FALSE, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Correlation plot")

#extracting only days from "dteday variable
data_1$dteday = weekdays(as.Date(data_1$dteday))

#from dataset we can see and understand that variables "dteday" and "weekday" are redundant in nature. So
#we can drop either of these variables from further analysis.

#anova test on continuous target/dependent variable and categorical independent variables
anova_result = aov(cnt~season+yr+mnth+holiday+weekday+workingday+weathersit, data=data_1)
summary(anova_result)

#dropping the variables "instant", "atemp" and "dteday" from the dataset after correlation and anova tests

data_preprocessed = subset(data_1, select = -c(instant,atemp,dteday,casual,registered))

#dropping variables "casual" and "registered"

#data_preprocessed = subset(data_1, select = -c(casual,registered))

#reordering dataset
data_preprocessed = data_preprocessed[,c(1,2,3,5,4,6,7,8,9,10,11)]

#checking variable distribution
#hist(data_preprocessed$casual)
#hist(data_preprocessed$registered)
hist(data_preprocessed$temp)
hist(data_preprocessed$hum)
hist(data_preprocessed$windspeed)

#normalizing variables "casual" and "registered"
#for(i in 1:length(data_preprocessed$casual)){
  #data_preprocessed$casual[i] = (data_preprocessed$casual[i] - min(data_preprocessed$casual))/
  #                              (max(data_preprocessed$casual)-min(data_preprocessed$casual))
#}
#for(i in 1:length(data_preprocessed$registered)){
#  data_preprocessed$registered[i] = (data_preprocessed$registered[i] - min(data_preprocessed$registered))/
 #   (max(data_preprocessed$registered)-min(data_preprocessed$registered))
#}

#modifying "cnt" variable so that it is sum of "casual" and "registered"
#for(i in 1:length(data_preprocessed$cnt)){
 # data_preprocessed$cnt[i] = data_preprocessed$casual[i] + data_preprocessed$registered[i]
#}

#-------------------------------------------------------------------------------
#head of data

head(data_preprocessed,10)

data_preprocessed$season = as.numeric(data_preprocessed$season)
data_preprocessed$yr = as.numeric(data_preprocessed$yr)
data_preprocessed$mnth = as.numeric(data_preprocessed$mnth)
data_preprocessed$holiday = as.numeric(data_preprocessed$holiday)
data_preprocessed$weekday = as.numeric(data_preprocessed$weekday)
data_preprocessed$workingday = as.numeric(data_preprocessed$workingday)
data_preprocessed$weathersit = as.numeric(data_preprocessed$weathersit)

#removing skewness in the variable "casual"

#data_preprocessed$casual = sqrt(data_preprocessed$casual)

#for(i in 1:length(data_preprocessed$cnt)){
  #data_preprocessed$cnt[i] = data_preprocessed$casual[i] + data_preprocessed$registered[i]
#}

# data_preprocessed$season = as.factor(data_preprocessed$season)
# data_preprocessed$yr = as.factor(data_preprocessed$yr)
# data_preprocessed$mnth = as.factor(data_preprocessed$mnth)
# data_preprocessed$holiday = as.factor(data_preprocessed$holiday)
# data_preprocessed$weekday = as.factor(data_preprocessed$weekday)
# data_preprocessed$workingday = as.factor(data_preprocessed$workingday)
# data_preprocessed$weathersit = as.factor(data_preprocessed$weathersit)

#perform k-fold cross-validation to handle overfitting
library(caret)
library(caTools)
index_cv = createDataPartition(data_preprocessed$cnt, p=0.80, list = FALSE)
train_cv = data_preprocessed[index_cv,]
test_cv = data_preprocessed[-index_cv,]
control_parameters = trainControl(method = "cv", number = 10, 
                                  savePredictions = TRUE, classProbs = TRUE)

#cross-validated for KNN
model_cv_knn = train(cnt~., data = train_cv, method = "knn", 
                     trControl = control_parameters)        # optimal k = 5, MAE=12.84%, RMSE=18.56%, Rsquared=76.96%

#cross-validated model for Linear Regression
model_cv_lm = train(cnt~., data = train_cv, method = "lm", 
                    trControl = control_parameters)       # MAE=0%, RMSE=0%, Rsquared=100%

#cross-validated model for Random Forest
model_cv_rf = train(cnt~., data = train_cv, method = "rf", 
                    trControl = control_parameters)  # optimal mtry = 12, MAE=1.61%, RMSE=2.43%, Rsquared=99.63%

#predict values for KNN, for example
predictions_cv = predict(model_cv_knn,test_cv)

#create confusion matrix for continuous target variable by using table function
conf_matrix = table(predictions = predictions_cv,actual = test_cv$cnt)

#----------------------------------------------------------------------------

#linear regression modeling
library(usdm)
vif(data_preprocessed[,-11])
vifcor(data_preprocessed[,-11], th=0.9)

#checking linear relation for "casual", "registered" and "cnt" to check impact on
#"casual" and "registered" variables, which in turn determine the total count "cnt".
#lm_model_cas = lm(casual~.,data = train_dataset)
#summary(lm_model_cas)

#lm_model_reg = lm(registered~.,data = train_dataset)
#summary(lm_model_reg)

lm_model = lm(cnt~.,data = train_cv)
summary(lm_model)
predictions_lm = predict(lm_model, test_cv[1:10])

#calculate model accuracy for linear regression
library(DMwR)
regr.eval(test_cv[,11], predictions_lm, stats = c("mae","rmse","mape","mse"))

#Residual Standard Error
sigma(lm_model)/mean(test_cv$cnt)  #19%

#Confidence Interval
confint(lm_model)

#Accuracy = 81%

#-----------------------------------------------------------------

#K Nearest Neighbor regression modeling
library(FNN)
cnt_train = train_cv$cnt
cnt_test = test_cv$cnt
predictions_knn = knn.reg(train_cv,test_cv,cnt_train,k=5)

#calculate model accuracy for KNN
#Mean Absolute Error
mean(abs(cnt_test-predictions_knn$pred))  #10.65%

#MAPE
mean(abs((cnt_test-predictions_knn$pred)/(cnt_test)))*100  #22.08% 

#Mean Squared Error
mean((cnt_test-predictions_knn$pred)^2)  #2.27%

#Accuracy = 100 - 10.65 = 89.35%

#elbow function
# install.packages("sjPlot")
# install.packages("tidyselect")
# library(sjPlot)
# install.packages('TMB', type = 'source')
# sjc.elbow(test_dataset, steps = 23)

#---------------------------------------------------------------------

#Random Forest Regression Modeling
library(randomForest)
model_rf = randomForest(cnt~., data=train_cv, mtry=12, importance=TRUE, type="anova")

#No. of variables tried at each split = 12
#No. of trees = 500 (by default)

#prediction on train data
pred_rf_train = predict(model_rf, train_cv)
table_pred_rf_train = data.frame(pred_rf_train,train_cv$cnt)

#prediction on test data
pred_rf_test = predict(model_rf, test_cv)
table_pred_rf_test = data.frame(pred_rf_test,test_cv$cnt)

#calculate model accuracy for Random Forest

#MAE
mean(abs(pred_rf_test - test_cv$cnt))  

#RMSE
sqrt(mean((pred_rf_test - test_cv$cnt)^2))  


#---------------------------------------------------------------------

#Linear Regression Equation
sn = data_preprocessed$season
yr = data_preprocessed$yr
wkd = data_preprocessed$weekday
hd = data_preprocessed$holiday
wd = data_preprocessed$workingday
ws = data_preprocessed$weathersit
tm = data_preprocessed$temp
hm = data_preprocessed$hum
wds = data_preprocessed$windspeed

cnt_sample = 490.54*sn + 1848.13*yr + 64.63*wkd - 522.57*hd +567.26*wd - 612.45*ws + 4732.21*tm - 1000.76*hm - 2605.04*wds

#sample input output verification
#Taking 1st observation from dataset
sn_sample = 1
yr_sample = 0
wkd_sample = 6
hd_sample = 0
wd_sample = 0
ws_sample = 2
tm_sample = 0.344167 (normalized) = 8.17585 (actual)
hm_sample = 0.805833 (normalized) = 80.5833 (actual)
wds_sample = 0.160446 (normalized) = 10.74988 (actual)
cnt_sample = 490.54*1 + 64.63*6 - 612.45*2 + 4732.21*0.344167 - 1000.76*0.805833 - 2605.04*0.160446
print(cnt_sample)

#-------------------------------------------------------------------------

