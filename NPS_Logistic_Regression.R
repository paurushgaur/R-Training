
#################Set Working Directory#############################
getwd()

setwd("D:/R Training/Training/NPS data")

dir()

#############import dataset#################################

library("readxl")
library("dplyr")

?read_excel

read_excel(path, sheet = NULL, range = NULL, col_names = TRUE,
           col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
           guess_max = min(1000, n_max))

NPS_Data <- read_excel(path="D:/R Training/Training/NPS data/NPS Data.xlsx", sheet = 'Sheet1',col_names = T)


#######################Create a binary column based on likert scale################

NPS_Data$Recommend <- ifelse(NPS_Data$`Recommendation-Likert Scale`>8,1,0)

summary(NPS_Data)
table(NPS_Data$`Manages Account`)

NPS_Data$mang_account <- as.factor(NPS_Data$`Manages Account`)

NPS_Data$`Manages Account` <- NULL

#####################Convert binary variables into factor########################

NPS_Data$mang_account<- factor(NPS_Data$mang_account, levels = c("No", "Yes"),labels = c("0", "1"))

NPS_Data$Recommend <- factor(NPS_Data$Recommend,levels = c("0", "1"),labels = c("0", "1"))


factor(NPS_Data$`Manages Account`, levels = c(0,1),labels = c("No", "Yes"))

summary(NPS_Data)

######################Missing Values Treatment###########################

histogram(NPS_Data$Complaints)

pie(table(NPS_Data$Complaints))

table(NPS_Data$Complaints, NPS_Data$Tenure)

complete.cases(NPS_Data)

NPS_Data[!complete.cases(NPS_Data),]

NPS_Data$Complaints <- ifelse(is.na(NPS_Data$Complaints) & NPS_Data$Tenure== 5 , 0, NPS_Data$Complaints)

NPS_Data$Complaints <- ifelse(is.na(NPS_Data$Complaints) & NPS_Data$Tenure== 8 , 0, NPS_Data$Complaints)

##################### Remove Unnecessary columns#######################

NPS_Data1 <- NPS_Data[,c(2:6,8:9)]


####################Develop Logistic Regression Model#####################

model = glm(Recommend ~., data=NPS_Data1, family='binomial')

model1 = glm(Recommend ~Age+Complaints+Tenure, data=NPS_Data1, family='binomial')

summary(model)

summary(model1 )

step(glm(Recommend ~., data=NPS_Data1, family='binomial'))

model2 <- glm(formula = Recommend ~ Age + Complaints + Tenure, family = "binomial", 
    data = NPS_Data1)

summary(model2)

########################## Add Predictive Values in column ####################

fitted(model2)

NPS_Data2<-cbind(NPS_Data1[,1:6], Pred_recommend=ifelse(fitted(model2)<0.5,0,1),prob=fitted(model2))
summary(NPS_Data2)

confusionMatrix(NPS_Data2$Recommend, factor(NPS_Data2$Pred_recommend
                                            ,levels = c("0", "1"),labels = c("0", "1")))





