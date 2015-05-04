#Final Project Code

#load packages that might be used
library(gdata)
library(dplyr)
library(ggplot2)

#Set Working Directory for Dataset 
setwd("Documents/Legal Analytics/Final Project/")

#had to convert data from xlsx to csv
pat_lit_data <- tbl_df(read.csv("Pat_Lit_Data_CSV.csv"))

#make a dummy variable for whether it or not it is a reissue (reissue=1, not = 0)
  #first had to change the type of data from character to numeric
pat_lit_data$ReissueOf<-as.numeric(as.character(pat_lit_data$ReissueOf))
  #this function lets me turn the NAs into 0s
tf<-is.na(pat_lit_data$ReissueOf)
pat_lit_data$ReissueOf[tf]<-0
  #the following uses DplyR to create a new column for a reissue dummy variable
pat_lit_data<-pat_lit_data  %>%
  mutate(Reissuedummy=as.numeric(ReissueOf>1))

#The following changes the number of references from a factor to a numeric
  #Most of the imported data came in, in the form of a factor - which is not useable for some operations
pat_lit_data$NumRefs<-as.numeric(as.character(pat_lit_data$NumRefs))


#Pairing and reaming data for easier use
workdata<-pat_lit_data %>%
  select(Renewal, Reissuedummy, Continuation, Division, Claims, NumRefs, DJ, PatDef, DCNumPatents, DCDecision) %>% #took out district because the info wasn't useful
  na.omit()
    #the Class variable will not work so it was not included
    #this is because there were too many classifications and no coherent way
    #to break them into smaller groupings as discussed with Prof. Katz.
#Random Forest Analysis
library(randomForest)

dim(workdata)
#Separating data into training and test subsets
set.seed(1234) 

ind<-sample(2,nrow(workdata),replace=TRUE,prob=c(0.7,0.3))
workdata.train <- workdata[ind==1,]
workdata.test <- workdata[ind==2,]

# Random Forest
forest_train=randomForest(DCDecision ~ ., data = workdata.train, ntree=5000, importance= TRUE)
print(forest_train)

#Importance
importance(forest_train)
varImpPlot(forest_train)

#Test my Random Forest
testforest = predict(forest_train, newdata=workdata.test)
table(testforest, workdata.test$DCDecision) #confusion matrix for test set
