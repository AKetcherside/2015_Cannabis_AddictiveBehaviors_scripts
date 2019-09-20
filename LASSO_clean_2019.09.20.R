#THIS SCRIPT USES ELASTIC NET REGRESSION TO PREDICT BEHAVIOR SCORES FROM RESTING STATE FUNCITONAL CONNECTIVITY SCORES
#SUBJECT IDs AND VARIABLE NAMES HAVE BEEN REDACTED IN ACCORDANCE WITH HIPAA 
#AND PRE-PUBLICATION STANDARDS

# WRITTEN BY ARIEL KETCHERSIDE, 2019
# RAN IN R version 3.5.1 (2018-07-02),  nickname "Feather Spray" 




rm(list=ls())

library(glmnet)


setwd("FILEPATH/PROJECT/Data_2019.06.11")


#CONNECTIVITY VARIABLES 
BetweenCort<-read.csv("BetweenNetworksCortCortAverages_abbrv.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
rownames(BetweenCort)<-BetweenCort$SubjectID


BetweenSub<-read.csv("BetweenNetworksSubcortCortAverages_abbrv.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
rownames(BetweenSub)<-BetweenSub$SubjectID


AllConnx<- merge(BetweenCort, BetweenSub, by.x="SubjectID", by.y="SubjectID", all.x=T, all.y=T)
rownames(AllConnx)<-AllConnx$SubjectID




#After transfering the subject ID to the rownames, 
#remove column for Subject ID so it doesn't get included in the model

AllConnx$SubjectID<-NULL
BetweenCort$SubjectID<-NULL
BetweenSub$SubjectID<-NULL




#Behavior data, clean

Data<-read.csv("FILEPATH/BehavData_2019.08.29", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

Data<-Data[!is.na(Data$Subject),]
rownames(Data)<-Data$Subject




#Behavior/X variables: Drop variables that I don't want in my model
drop <- c("t1dem01","t1dem03","t1suh01a", "t1suh09","t1tlfb06","t1tlfb15", 
          "ATC","dmPFC","IFG","PCC","TPJ","ATC_dmPFC","ATC_IFG","ATC_PCC","ATC_TPJ","dmPFC_IFG",
          "dmPFC_PCC","dmPFC_TPJ","IFG_PCC","IFG_TPJ","PCC_TPJ")
df = Data[,!(names(Data) %in% drop)]



#Columns with too many missing observations
coldrops2<-c("t1suh12a","t1suh12b","t1suh13","t1suh14","t1suh09a","t1suh09b","t1suh10","t1suh11","t1suhmss01","t1suhmss02","t1tlfb15a","t1tlfb16","t1tlfb17")
df2 = df[,!(names(df) %in% coldrops2)]

#Rows with too many NAs
# REDACTED
rowdrops<-c("ID.X1","ID.X2","ID.X3")



df3 = df2[!(rownames(df2) %in% rowdrops),]


#Sadly this won't work with NAs. You have to omit those too. 
DFclean<-na.omit(df3)
rownames(DFclean)<-DFclean$Subject
DFclean$Subject<-NULL




#Cannabis related behavior variables. Scale them to normalize within sample. 
#If variable SD = 0 , it can't be included; omitted here in this df build. 





#COMPOSITE SCORES

#Boolean items scaled as F == 0 and T == 1

#Likert items scaled to normal distribution



# MODEL 1 variables: 

MODEL1<-(
    DFclean$t1cpq01+ 
    DFclean$t1cpq02+ 
    DFclean$t1cpq03+ 
    DFclean$t1cpq08+ 
    DFclean$t1cpq10+ 
    DFclean$t1cpq11+ 
    DFclean$t1cpq12+ 
    DFclean$t1cpq13+ 
    DFclean$t1cpq15+
    DFclean$t1cpq20+ 
    DFclean$t1cpq39+ 
    DFclean$t1cpq41+ 
    DFclean$t1cpq43+ 
    DFclean$t1cpq44+ 
    DFclean$t1cpq46+ 
    DFclean$t1cud01+ 
    DFclean$t1cud02+ 
    DFclean$t1cud03+ 
    DFclean$t1cud04+ 	
    DFclean$t1cud05+ 	
    DFclean$t1cud06+ 
    DFclean$t1cud07+ 	
    DFclean$t1cud08+ 	
    DFclean$t1cud09+ 	
    DFclean$t1cud10+ 	
    DFclean$t1cud11+ 

    #this is where the variables start to be nonbinary; include scaling 
    scale(DFclean$t1cudit01)+ 
    scale(DFclean$t1cudit02)+  
    scale(DFclean$t1cudit03)+ 
    scale(DFclean$t1cudit04)+ 
    scale(DFclean$t1cudit05)+ 
    scale(DFclean$t1cudit06)+ 
    scale(DFclean$t1cudit07)+ 
    scale(DFclean$t1cudit08)+ 	
    scale(DFclean$t1cudit10)+ 	 
    scale(DFclean$t1tlfb12)+ 	
    scale(DFclean$t1tlfb14)) 	


rownames(MODEL1)<-rownames(DFclean)





#REVERSE CODE SOME ITEMS FOR MODEL2

#MODEL2VAR03 IS ALL KINDS OF JACKED UP AND NEEDS TO BE RECODED 

DFclean$NewMODEL2VAR3<-replace(DFclean$t1MODEL2VAR03, DFclean$t1MODEL2VAR03==4, 0) #friends wouldn't care 
DFclean$NewMODEL2VAR3<-replace(DFclean$NewMODEL2VAR3, DFclean$NewMODEL2VAR3==3, -2) #friends would disapprove, wouldn't be friends, but no one answered this   
DFclean$NewMODEL2VAR3<-replace(DFclean$NewMODEL2VAR3, DFclean$NewMODEL2VAR3==2, -1) #friends would disapprove, would be friends 
#1= friends would approve 


#NEGATIVE PROJECT MODEL2 VARS; NEED TO REVERSE-SCORE
  #Oringinal: 0= no, 1 = yes
  DFclean$CPQ4new <- 1-(DFclean$t1cpq04)  



#   MODEL2 COMPOSITE SCORE
  
#Positive PROJECT VAR: higher score = more favorible perception from MODEL2s 

MODEL2<-(
    DFclean$t1cpq03+
    DFclean$NewMODEL2VAR3+ # Already scaled
    DFclean$CPQ4new+ #already scaled 
    scale(DFclean$t1psu04)+ #	binary 
    scale(DFclean$t1psu05)+ 
    scale(DFclean$t1psu06)+ 
    scale(DFclean$t1psu10)+ 
    scale(DFclean$t1psu12)) 

rownames(MODEL2)<-rownames(DFclean)

MODEL2DF<-as.data.frame(MODEL2)
colnames(MODEL2DF)<-c("Value")




#Visual check to make sure everything came out ok 

dev.new()
hist(MODEL1, breaks=100, color="purple")

dev.new()
hist(MODEL2, breaks=100)





#ONLY INCLUDE SUBJECTS WITH BOTH BEHAVIOR AND IMAGING DATA

AllConnx117 =  AllConnx[(rownames(AllConnx) %in% rownames(DFclean)),]
write.csv(AllConnx, file="Connectivitydata_VisualInspection_2019.09.05.csv")

MODEL1.117= MODEL1[(rownames(MODEL1) %in% rownames(BetweenSub117)),]
MODEL2.117= MODEL2[(rownames(MODEL2) %in% rownames(BetweenCort117)),]

x.AllConnx<-as.matrix(AllConnx117)

y.MODEL1behav<-as.matrix(MODEL1.117)
y.MODEL2behav<-as.matrix(MODEL2.117)





######################
# MODEL 1 Sx
######################

# DETERMINE THE BEST ALPHA TO DETERMINE THE BEST LAMBDA
# alpha = 1 gives best lambda; i.e. pure lasso is better than ridge/elastic net regression

cvMODEL1 <- cv.glmnet(x.AllConnx, y.MODEL1behav, family="gaussian", type.measure="mse", #this is what we use in the cross validation measure 
                  alpha=1, #RANGE BETWEEN 0 AND 1 TO SPECIFY RIDGE VS LASSO
                  nfolds=100)

plot(cvMODEL1)

# Cross-validated model
MODEL1fit=glmnet(x.AllConnx, y.MODEL1behav, family="gaussian", #type.measure="mse", #this is what we use in the cross validation measure 
             alpha=1, 
             lambda=cvMODEL1$lambda.1se)


head(sort(MODEL1fit$beta[,1]))





###########################
# MODEL2 Sx
###########################

# DETERMINE THE BEST ALPHA TO DETERMINE THE BEST LAMBDA
# alpha = 1 gives best lambda; i.e. pure lasso is better than ridge/elastic net regression


cvMODEL2 <- cv.glmnet(x.AllConnx, y.MODEL2behav, family="gaussian", type.measure="mse", 
                    alpha=1, nlambda=100, nfolds=100)

plot(cvMODEL2)

#Cross-validated model
MODEL2fit=glmnet(x.AllConnx, y.MODEL2behav, family="gaussian", #type.measure="mse", #this is what we use in the cross validation measure 
               alpha=1,  #Totally LASSO. Something more like 
               lambda=cvMODEL2$lambda.1se)


head(sort(MODEL2fit$beta[,1]))




