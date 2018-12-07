#remove all the objects stored
rm(list=ls())

#set current working directory
setwd("C:/Users/SAYAN/Desktop/Data Science/Projects/R Folder")

#Current working directory
getwd()

#instaling packages
install.packages(c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','Hmisc','corrplot','usdm'))

#reading Excel sheet
library(xlsx)
data = read.xlsx("Absenteeism_at_work_Project.xls", sheetIndex = 1, header = T)
#data

#Getting the column names of the dataset
colnames(data)

#Getting the structure of the dataset
str(data)

#Getting the number of variables and obervation in the datasets
#dim(data)

#data type
#class(data)

#Summary of a varaible 
#summary(data)

df_summary = as.data.frame(summary(data))

df_summary

as.character(data$Reason.for.absence)

data

# Replacing values in "Reason for absence"

# With ICD
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="1", "Certain infectious and parasitic diseases")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="2", "Neoplasms")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="3", "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="4", "Endocrine, nutritional and metabolic diseases")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="5", "Mental and behavioural disorders")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="6", "Diseases of the nervous system")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="7", "Diseases of the eye and adnexa")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="8", "Diseases of the ear and mastoid process")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="9", "Diseases of the circulatory system")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="10", "Diseases of the respiratory system")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="11", "Diseases of the digestive system")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="12", "Diseases of the skin and subcutaneous tissue")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="13", "Diseases of the musculoskeletal system and connective tissue")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="14", "Diseases of the genitourinary system")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="15", "Pregnancy, childbirth and the puerperium")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="16", "Certain conditions originating in the perinatal period")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="17", "Congenital malformations, deformations and chromosomal abnormalities")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="18", "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="19", "Injury, poisoning and certain other consequences of external causes")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="20", "External causes of morbidity and mortality")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="21", "Factors influencing health status and contact with health services")
# Without ICD
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="22", "patient follow-up")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="23", "medical consultation")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="24", "blood donation")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="25", "laboratory examination")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="26", "unjustified absence")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="27", "physiotherapy")
data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=="28", "dental consultation")

# Replacing values in "Day of the week"

data$Day.of.the.week = replace(data$Day.of.the.week,data$Day.of.the.week==3, "Tuesday")
data$Day.of.the.week = replace(data$Day.of.the.week,data$Day.of.the.week==2, "Monday")
data$Day.of.the.week = replace(data$Day.of.the.week,data$Day.of.the.week==4, "Wednesday")
data$Day.of.the.week = replace(data$Day.of.the.week,data$Day.of.the.week==5, "Thursday")
data$Day.of.the.week = replace(data$Day.of.the.week,data$Day.of.the.week==6, "Friday")

# Replacing "Seasons"

data$Seasons = replace(data$Seasons,data$Seasons==1, "summer")
data$Seasons = replace(data$Seasons,data$Seasons==2, "autumn")
data$Seasons = replace(data$Seasons,data$Seasons==3, "winter")
data$Seasons = replace(data$Seasons,data$Seasons==4, "spring")

# Replacing "Disciplinary failure"

data$Disciplinary.failure = replace(data$Disciplinary.failure,data$Disciplinary.failure==1,"yes")
data$Disciplinary.failure = replace(data$Disciplinary.failure,data$Disciplinary.failure==0,"no")

# Replacing "Education"

data$Education = replace(data$Education,data$Education==1,"high school")
data$Education = replace(data$Education,data$Education==2,"graduate")
data$Education = replace(data$Education,data$Education==3,"postgraduate")
data$Education = replace(data$Education,data$Education==4,"master and doctor")

# Replacing "Social smoker"

data$Social.smoker = replace(data$Social.smoker,data$Social.smoker==1,"yes")
data$Social.smoker = replace(data$Social.smoker,data$Social.smoker==0,"no")

# Replacing "Social drinker"

data$Social.drinker = replace(data$Social.drinker,data$Social.drinker==1,"yes")
data$Social.drinker = replace(data$Social.drinker,data$Social.drinker==0,"no")

#str(data)
View(data)

#missing_val = as.data.frame(data[NA])

missing_val = as.data.frame(apply(data,2,function(x){sum(is.na(x))}))
View(missing_val)
missing_val$Columns = row.names(missing_val)
row.names(missing_val) = NULL
names(missing_val)[1] = 'Missing percentage'
missing_val = missing_val[,c(2,1)]
missing_val$`Missing percentage` = (missing_val$`Missing percentage`/nrow(data))*100

# Replacing missing values with 0

data[is.na(data)] = 0

# Replacing 0s with mode in categorical variables

getmode = function(x) {
  uniqv = unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

data$Reason.for.absence = replace(data$Reason.for.absence,data$Reason.for.absence=='0',getmode(data$Reason.for.absence))
data$Month.of.absence = replace(data$Month.of.absence,data$Month.of.absence=='0',getmode(data$Month.of.absence))
data$Day.of.the.week = replace(data$Day.of.the.week,data$Day.of.the.week=='0',getmode(data$Day.of.the.week))
data$Seasons = replace(data$Seasons,data$Seasons=='0',getmode(data$Seasons))
data$Disciplinary.failure = replace(data$Disciplinary.failure,data$Disciplinary.failure=='0',getmode(data$Disciplinary.failure))
data$Education = replace(data$Education,data$Education=='0',getmode(data$Education))
data$Social.drinker = replace(data$Social.drinker,data$Social.drinker=='0',getmode(data$Social.drinker))
data$Social.smoker = replace(data$Social.smoker,data$Social.smoker=='0',getmode(data$Social.smoker))

#write.(data,'No_Missing_or_Zeroes')

#write.xlsx(data,'NoMissingorZeroes.xls',sheetName = 'Sheet1',row.names = T,col.names = T)

# Replacing 0s with median in continuous variables

data$Work.load.Average.day. = replace(data$Work.load.Average.day.,data$Work.load.Average.day.==0,median(data$Work.load.Average.day.))
data$Hit.target = replace(data$Hit.target,data$Hit.target==0,median(data$Hit.target))
data$Absenteeism.time.in.hours = replace(data$Absenteeism.time.in.hours,data$Absenteeism.time.in.hours==0,median(data$Absenteeism.time.in.hours))

# Sorting data with ID

data = data[order(data$ID),]
# View(data)

#Converting ID and Month of Absence as factor

data$ID = as.factor(data$ID)


unique(data$ID)
length(unique(data$ID))
for (i in range(1,length(unique(data$ID)))) {
    data$Transportation.expense = replace(data$Transportation.expense,data$Transportation.expense==0,aggregate(data$Transportation.expense, by=list(ID=data$ID), FUN=median)[2][i,])
    data$Distance.from.Residence.to.Work = replace(data$Distance.from.Residence.to.Work,data$Distance.from.Residence.to.Work==0,aggregate(data$Distance.from.Residence.to.Work, by=list(ID=data$ID), FUN=median)[2][i,])
    data$Service.time = replace(data$Service.time,data$Service.time==0,aggregate(data$Service.time, by=list(ID=data$ID), FUN=median)[2][i,])
    data$Age = replace(data$Age,data$Age==0,aggregate(data$Age, by=list(ID=data$ID), FUN=median)[2][i,])
    data$Son = replace(data$Son,data$Son==0,aggregate(data$Son, by=list(ID=data$ID), FUN=median)[2][i,])
    data$Pet = replace(data$Pet,data$Pet==0,aggregate(data$Pet, by=list(ID=data$ID), FUN=median)[2][i,])
    data$Weight = replace(data$Weight,data$Weight==0,aggregate(data$Weight, by=list(ID=data$ID), FUN=median)[2][i,])
    data$Height = replace(data$Height,data$Height==0,aggregate(data$Height, by=list(ID=data$ID), FUN=median)[2][i,])
    data$Body.mass.index = replace(data$Body.mass.index,data$Body.mass.index==0,aggregate(data$Body.mass.index, by=list(ID=data$ID), FUN=median)[2][i,])
    
}

#write.xlsx(data,'NoMissingorZeroes_2.xls',sheetName = 'Sheet1',row.names = T,col.names = T)

# Outlier analysis

library(ggplot2)
numerics = sapply(data, is.numeric)
numeric_cols = data[,numerics]
cnames = colnames(numeric_cols)

for (i in 1:length(cnames))
   {
     assign(paste0("oa",i), ggplot(aes_string(y = (cnames[i]), x = "Absenteeism.time.in.hours"), 
     data = subset(data))+ 
     stat_boxplot(geom = "errorbar", width = 0.5) +
     geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
     outlier.size=1, notch=FALSE) +
     theme(legend.position="bottom")+
     labs(y=cnames[i],x="Absenteeism.time.in.hours")+
     ggtitle(paste("Box plot",i)))
}

# Plotting box plots to show outlier values

gridExtra::grid.arrange(oa1,oa2,ncol=2)
gridExtra::grid.arrange(oa3,oa4,ncol=2)
gridExtra::grid.arrange(oa5,oa6,ncol=2)
gridExtra::grid.arrange(oa7,oa8,ncol=2)
gridExtra::grid.arrange(oa9,oa10,ncol=2)
gridExtra::grid.arrange(oa11,ncol=1)

#Remove outliers using boxplot method

df = data
data = df

#for(i in cnames){
  #print(i)
  #val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  #print(length(val))
  #data = data[which(!data[,i] %in% val),]
#}

#Replace all outliers with 0 and impute

for(i in cnames){
  #print(i)
  val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  print(length(val))
  data[,i][data[,i] %in% val] = 0
}

#write.xlsx(data,'Zeroes.xls',sheetName = 'Sheet1',row.names = T,col.names = T)

# Replacing 0s with median in continuous variables

data$Work.load.Average.day. = replace(data$Work.load.Average.day.,data$Work.load.Average.day.==0,median(data$Work.load.Average.day.))
data$Hit.target = replace(data$Hit.target,data$Hit.target==0,median(data$Hit.target))
data$Absenteeism.time.in.hours = replace(data$Absenteeism.time.in.hours,data$Absenteeism.time.in.hours==0,median(data$Absenteeism.time.in.hours))

# View(data)

unique(data$ID)
length(unique(data$ID))
for (i in range(1,length(unique(data$ID)))) {
  data$Transportation.expense = replace(data$Transportation.expense,data$Transportation.expense==0,aggregate(data$Transportation.expense, by=list(ID=data$ID), FUN=median)[2][i,])
  data$Distance.from.Residence.to.Work = replace(data$Distance.from.Residence.to.Work,data$Distance.from.Residence.to.Work==0,aggregate(data$Distance.from.Residence.to.Work, by=list(ID=data$ID), FUN=median)[2][i,])
  data$Service.time = replace(data$Service.time,data$Service.time==0,aggregate(data$Service.time, by=list(ID=data$ID), FUN=median)[2][i,])
  data$Age = replace(data$Age,data$Age==0,aggregate(data$Age, by=list(ID=data$ID), FUN=median)[2][i,])
  data$Son = replace(data$Son,data$Son==0,aggregate(data$Son, by=list(ID=data$ID), FUN=median)[2][i,])
  data$Pet = replace(data$Pet,data$Pet==0,aggregate(data$Pet, by=list(ID=data$ID), FUN=median)[2][i,])
  data$Weight = replace(data$Weight,data$Weight==0,aggregate(data$Weight, by=list(ID=data$ID), FUN=median)[2][i,])
  data$Height = replace(data$Height,data$Height==0,aggregate(data$Height, by=list(ID=data$ID), FUN=median)[2][i,])
  data$Body.mass.index = replace(data$Body.mass.index,data$Body.mass.index==0,aggregate(data$Body.mass.index, by=list(ID=data$ID), FUN=median)[2][i,])
  
}

#write.xlsx(data,'Final1.xls',sheetName = 'Sheet1',row.names = T,col.names = T)

# Feature selection

# Correlation calculation and plot for numeric variables

value = round(cor(as.matrix(data[,numerics])),2)

value

library(corrgram)
corrgram(data[,numerics], order=FALSE, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Correlation plot")

# Anova test for categorical independent and continuous dependent variable

data$Month.of.absence = as.factor(data$Month.of.absence)
data$Reason.for.absence = as.factor(data$Reason.for.absence)
data$Day.of.the.week = as.factor(data$Day.of.the.week)
data$Seasons = as.factor(data$Seasons)
data$Disciplinary.failure = as.factor(data$Disciplinary.failure)
data$Education = as.factor(data$Education)
data$Social.drinker = as.factor(data$Social.drinker)
data$Social.smoker = as.factor(data$Social.smoker)

facts = sapply(data, is.factor)
facts_cols = data[,facts]
cnames_facts = colnames(facts_cols)

anova_result <- aov(Absenteeism.time.in.hours~Reason.for.absence*Month.of.absence*Day.of.the.week
                    *Seasons*Disciplinary.failure*Education*Social.drinker*Social.smoker , data = data)
summary(anova_result)

# Data after pre-processing

data_preprocessed = subset(data, select = -c(Month.of.absence, Day.of.the.week, Seasons, Disciplinary.failure,
                                             Education, Social.drinker, Social.smoker,Distance.from.Residence.to.Work,
                                             Son,Pet,Weight,Body.mass.index))

# Writing pre-processed data in the disc

#write.xlsx(data_preprocessed,'Preprocessed_data.xls',sheetName = 'Sheet1',row.names = T,col.names = T)

# Checking distribution for continuous variables in pre-processed data

qqnorm(data_preprocessed$Transportation.expense)
hist(data_preprocessed$Transportation.expense)

qqnorm(data_preprocessed$Service.time)
hist(data_preprocessed$Service.time)

qqnorm(data_preprocessed$Age)
hist(data_preprocessed$Age)

qqnorm(data_preprocessed$Work.load.Average.day.)
hist(data_preprocessed$Work.load.Average.day.)

qqnorm(data_preprocessed$Hit.target)
hist(data_preprocessed$Hit.target)

qqnorm(data_preprocessed$Son)
hist(data_preprocessed$Son)

qqnorm(data_preprocessed$Height)
hist(data_preprocessed$Height)

qqnorm(data_preprocessed$Absenteeism.time.in.hours)
hist(data_preprocessed$Absenteeism.time.in.hours)

df = data_preprocessed
list_norm = c("Transportation.expense","Service.time","Age","Hit.target","Absenteeism.time.in.hours",
              "Work.load.Average.day.","Height")
for (i in list_norm){
  df[,i] = (df[,i] - min(df[,i]))/((max(df[,i])) - (min(df[,i])))
}

# Check distribution post normalization

hist(df$Transportation.expense)
hist(df$Service.time)
hist(df$Age)
hist(df$Work.load.Average.day.)
hist(df$Hit.target)
hist(df$Height)
hist(df$Absenteeism.time.in.hours)


# Decision Tree regression model design

library(rpart)
#df = data_preprocessed
#df = data

#df = df[,-1]

# Dividing data into train and test

train_index_norm = sample(1:nrow(df), 0.95*nrow(df))
train_norm = df[train_index_norm,]
test_norm = df[-train_index_norm,]

fit = rpart(Absenteeism.time.in.hours ~. , data = train_norm, method = "anova")
predictions_DT = predict(fit,test_norm[,-9])

# Calculating model accuracy

library(DMwR)
regr.eval(test_norm[,9], predictions_DT, stats = c('mae','rmse','mse','mape'))

mape = function(y,yhat){
  mean(abs((y-yhat)/y))*100
}

mape(test[,9],predictions_DT)


# Linear Regression model design

library(usdm)
df2 = df
df2 = df2[-1]
df2 = df2[-1]
vif(df2[,-7])
vifcor(df2[,-7], th=0.9)

train_index_LM = sample(1:nrow(df2), 0.95*nrow(df2))
train_LM = df2[train_index_LM,]
test_LM = df2[-train_index_LM,]

lm_model = lm(Absenteeism.time.in.hours~., data = train_LM)

summary(lm_model)

predictions_LR=predict(lm_model,test_LM[,1:6])

regr.eval(test_LM[,7], predictions_LR, stats = c('mae','rmse','mse','mape'))


# Logic for calculating loss per month. We will calculate loss from the original dataset "data".

df1 = data
list1 = c("Work.load.Average.day.","Hit.target","Absenteeism.time.in.hours")
for (i in list1){
  df1[,i] = (df1[,i] - min(df1[,i]))/((max(df1[,i])) - (min(df1[,i])))
}
a = aggregate(df1$Work.load.Average.day., by=list(Month.of.absence=df1$Month.of.absence), FUN=sum)
b = aggregate(df1$Hit.target, by=list(Month.of.absence=df1$Month.of.absence), FUN=sum)
c = aggregate(df1$Absenteeism.time.in.hours, by=list(Month.of.absence=df1$Month.of.absence), FUN=sum)
d = aggregate(df1$ID, by=list(Month.of.absence=df1$Month.of.absence), FUN=unique)

# Assumption is that every month has 30 days

# We are comparing the workload (in hours) for every month (expected output) and comparing it with the hit target.
# Also, we are subtracting the absenteeism (in hours) to get the actual output and calculate loss.

for (i in c(1,2,3,4,5,6,7,8,9,10,11,12)) {
  # Calculating loss
  #print(i)
  loss = (b[2][i,]-(b[2][i,]/(a[2][i,]*31*length(d[2][i,][[1]])))*((a[2][i,]*30*length(d[2][i,][[1]]))-c[2][i,]))/b[2][i,]*100
  print(loss)
}

x = c(3.279278,3.293305,3.301384,3.294327,3.321255,3.316309,3.356788,3.377586,3.301268,3.310392,3.289171,3.324995)
y = c("1","2","3","4","5","6","7","8","9","10","11","12")
barplot(x,names.arg=y,xlab="Months",ylab="Percentage loss",col="red",
        main="Loss chart",border="black")



