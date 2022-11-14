library("plotmo")
library("stats")
library("dplyr")
library("randomForest")
#----------------------------------------------------------------------
#random forest with cougar locations
#----------------------------------------------------------------------



#prepare and split data into training and testing

full_data <- read.csv("D:/Connor/thesis/thesis/master_march11.csv", header=TRUE, sep = ",", na.strings="NA", dec=".")#%>% sample_n(500000) %>% na.omit()

colSums(is.na(full_data))

mydata2 <- full_data

# consider replacing na's with the mean of that column:
for(i in 1:ncol(mydata2)){
  mydata2[is.na(mydata2[,i]), i] <- mean(mydata2[,i], na.rm = TRUE)
}

# take a sample and omit na's
mydata <- mydata2 %>%  na.omit()%>% sample_n(500000)
nrow(mydata)

mydata$used <- as.factor(mydata$used)

#create random sample function
# index <- sample(2,nrow(mydata),replace = TRUE,prob=c(0.5,0.5))
# 
# #create training and testing data
# Training <- mydata[index==1,]
# Testing <- mydata[index==2,]
# 
# Training$used <- as.factor(Training$used)
# 
# Testing$used <- as.factor(Testing$used)

#===============================================================================

# consider this code to run on full data then combine:
# rf1 <- randomForest(used ~ ., mydata[1:500000,c(4,5:17)], ntree=500, norm.votes=FALSE, do.trace=10,importance=TRUE)
# saveRDS(rf1, "rf_full_data_chunks/rf1.rds")
# rf2 <- randomForest(used ~ ., mydata[500001:1000000,c(4,5:17)], ntree=500, norm.votes=FALSE, do.trace=10,importance=TRUE)
# saveRDS(rf2, "rf_full_data_chunks/rf2.rds")
# rf3 <- randomForest(used ~ ., mydata[1000001:1500000,c(4,5:17)], ntree=500, norm.votes=FALSE, do.trace=10,importance=TRUE)
# saveRDS(rf3, "rf_full_data_chunks/rf3.rds")
# rf4 <- randomForest(used ~ ., mydata[1500001:2000000,c(4,5:17)], ntree=500, norm.votes=FALSE, do.trace=10,importance=TRUE)
# saveRDS(rf4, "rf_full_data_chunks/rf4.rds")
# rf5 <- randomForest(used ~ ., mydata[2000001:2500000,c(4,5:17)], ntree=500, norm.votes=FALSE, do.trace=10,importance=TRUE)
# saveRDS(rf5, "rf_full_data_chunks/rf5.rds")
# #this one doesnt work cause its all 0s
# rf6 <- randomForest(used ~ ., mydata[2500001:2741858,c(4,5:17)], ntree=500, norm.votes=FALSE, do.trace=10,importance=TRUE)
# saveRDS(rf6, "rf_full_data_chunks/rf6.rds")
# View(mydata[2500001:2741858,])
# 
# rf1 <- readRDS("rf_full_data_chunks/rf1.rds")
# rf2 <- readRDS("rf_full_data_chunks/rf2.rds")
# rf3 <- readRDS("rf_full_data_chunks/rf3.rds")
# rf4 <- readRDS("rf_full_data_chunks/rf4.rds")
# rf5 <- readRDS("rf_full_data_chunks/rf5.rds")
# 
# 
# rf.combined <- combine(rf1,rf2,rf3,rf4,rf5)
# saveRDS(rf.combined, "rf_full_data_chunks/rf.combined.rds")
# 



#---------------------------------------------------------------------
# #create random forest model and test prediction accuracy
RFM <-  randomForest(used~., data = mydata[, c(4,6:16)], ntree=500)
RFM
# plot(RFM)
# saveRDS(RFM, "rf_model_march14.rds")
# 
# 
# #devtools::install_github("sumbose/iRF")
# 
# t <- tuneRF(Training[,c(7:12,14:18,20)], Training[,4],
#             stepFactor = 0.5,
#             plot = TRUE,
#             ntreeTry = 150, # try 500 and see what happens
#             trace = TRUE,
#             improve = 0.05)
# 
# t
# # i guess mtry = 6 is best ? 
#-------------------------------------------------------------------

Testing <- full_data
# confusion matrix
#use model to predict probability of default
predicted <- predict(RFM, Testing, type="prob")
probs <- predicted[,2]
#calculating optimal cutpoint
library(pROC)

rocobj <- roc(Testing$used, probs)

#saveRDS(rocobj, "March14_RF_rocobj.rds")
choose.files()
rocobj <- readRDS( "D:\\Connor\\thesis\\thesis\\March14_RF_rocobj.rds")

c <- coords(rocobj, x="best", input="threshold", best.method="youden") # Same than last line
c  # c is 0.543

#define cut off value
Testing$predicted <- ifelse(predicted[,2] > c$threshold, 1, 0)

# just messing with the threshhold here... use previous line for actual analysis
#Testing$predicted <- ifelse(predicted[,2] > 0.4, 1, 0) # so a .4 threshold gives 0.9080 sensitivity... use that for rsf calculation

Testing$predicted <- as.factor(Testing$predicted)
Testing$used <- as.factor(Testing$used)

#create confusion matrix
object <- caret::confusionMatrix(Testing$used, Testing$predicted)
object



#saveRDS(object, "plots_for_committee_meeting/RF_cm.rds")
table(factor(predicted, levels=min(Testing):max(Testing)), 
      factor(Testing, levels=min(Testing):max(Testing)))
#---------------------------------------------------------------------
rocobj2 <- readRDS("D:\\Connor\\thesis\\thesis\\march13_glmer_roc.rds")

plot(rocobj, col="red", lwd=3, main="ROC curve", asp=NA)

auc(rocobj)

#---------------------------------------------------------------------
partials <- readRDS("D:/Connor/thesis/thesis/rds_files/RF_partial_plots.rds")

plot(partials)

#---------------------------------------------------------------------


#RFM <- readRDS("RF_model_full.rds")


#---------------------------------------------------------------------
#variable importance plots
importance(RFM)
rank(importance(RFM))
variable_importance <- varImpPlot(RFM)

saveRDS(variable_importance, "RF_variable_importance_march14.rds")



library('pdp')
#univariate plots
library(randomForest)
partialPlot(RFM, pred.data = mydata, x.var = "ndvi") 
partialPlot(RFM, pred.data = mydata, x.var = "my_elev")
partialPlot(RFM, pred.data = mydata, x.var = "popDens")
partialPlot(RFM, pred.data = mydata, x.var = "zlope")
partialPlot(RFM, pred.data = mydata, x.var = "puma_northernness")



#-----------------------------------------------------------------------

## interactive plots
# degree2=TRUE plots intractions 
partial_plots <- plotmo(RFM, pmethod = "partdep", degree1 = FALSE, degree2 = TRUE, ylim = c(1,2)) 


## simple plots
library(plotmo)


# degree = 1 plots the 10 most important variables.. all2=TRUE should make
# it plot all the variables but you might have to take off degree1=TRUE
partial_plots <- plotmo(RFM, pmethod = "partdep", which=1, degree1 = TRUE, degree2 = FALSE, ylim = c(1,2),  all1=TRUE) 

saveRDS(partial_plots, "RF_partial_plots_March11.rds")


partials <- readRDS("D:/Connor/thesis/thesis/rds_files/RF_partial_plots.rds")

partials






mydata$used <- as.factor(mydata$used)

mod2 <- randomForest(used~.,mydata[,c(4,7,8)])
plotmo(mod2, ylim = c(1,2))























