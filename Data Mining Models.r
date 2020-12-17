####Import Data and Data Cleaning#####


library(dplyr)
library(ggplot2)

#read the data
telco.df <- read.csv("Telco-Customer-Churn.csv", stringsAsFactors = TRUE)


#understand the data structure
#View(telco.df)
summary(telco.df)
glimpse(telco.df)
str(telco.df)

#check if data contain null -> FALSE
is.null(telco.df)

#data cleaning and preprocessing
#convert SeniorCitizen column from int to facter
telco.df$SeniorCitizen <- as.factor(ifelse(telco.df$SeniorCitizen==1, 'Yes', 'No'))
telco.df$SeniorCitizen



####Clustering for mixed data-type dataset#####


telco.df <- read.csv("Telco-Customer-Churn.csv",stringsAsFactors = TRUE)
View(telco.df)
summary(telco.df)
glimpse(telco.df)
str(telco.df)

#check if data contain null -> FALSE
is.null(telco.df)

#data cleaning and preprocessing
#convert SeniorCitizen column from int to factor
telco.df$SeniorCitizen <- as.factor(ifelse(telco.df$SeniorCitizen==1, 'Yes', 'No'))
telco.df$SeniorCitizen

telco.df$Churn <- as.numeric(telco.df$Churn == "Yes")

head(telco.df, n=3)

###Applying kmediod algorithm



####TRYING GOWER"S DISTANCE###########
install.packages("Rtsne")
library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)

gower_dist <- daisy(telco.df, metric = "gower")
nrow(telco.df)
gower_mat <- as.matrix(gower_dist)
telco.df[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ] ######Print MOST similar clients
telco.df[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ] ###Print MOST dissimilar clients




sil_width <- c(NA)
for(i in 2:8){
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}


plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)

k <- 2
pam_fit <- pam(gower_dist, diss = TRUE, k)
str(pam_fit)
pam_results <- telco.df %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))


pam_results$the_summary



tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))





ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

#######End of Telco PAM CODE##########
ncol(telco.df)

index <- telco.df$SeniorCitizen == "Yes"
seniorciti <- telco.df[index,]
nrow(seniorciti) ###1142/7043

churn <- telco.df$Churn == "Yes"
churnRate <- telco.df[churn,]
nrow(churnRate) ####1869

seniorchurn <- churnRate$SeniorCitizen == "Yes"
seniorchurnrate <- churnRate[seniorchurn,]
nrow(seniorchurnrate)   ####476






#######End of Telco PAM CODE##########


#######Start of Telco GK Matrix CODE##########


library(GoodmanKruskal)
copyTelco.df <- telco.df
grpdTenure <- GroupNumeric(copyTelco.df$tenure, n = 5)
copyTelco.df$grpTenure <- NULL
copyTelco.df$grpTenure <- grpdTenure
View(copyTelco.df)
varset1<- c(9,10,13,16,21,22)
sample.df<- subset(copyTelco.df, select = varset1)
GKmatrix1<- GKtauDataframe(sample.df)
plot(GKmatrix1, corrColors = "blue")

#######End of Telco GK Matrix CODE##########


#######Start of Telco chi-square CODE##########


chisq.test(telco.df$Churn,telco.df$Contract,correct = FALSE)
chisq.test(copyTelco.df$Churn,copyTelco.df$grpTenure,correct = FALSE)
chisq.test(telco.df$Churn,telco.df$OnlineSecurity,correct = FALSE)
chisq.test(telco.df$Churn,telco.df$TechSupport,correct = FALSE)
chisq.test(telco.df$Churn,telco.df$InternetService,correct = FALSE)

#######End of Telco chi-square CODE##########


#######Start of Telco Decision Tree CODE##########

set.seed(1111)
train.index <- sample(nrow(telco.df), 0.6 * nrow(telco.df))



train.df <- telco.df[train.index, -1]
valid.df <- telco.df[-train.index, -1]

library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)

head(train.df)
head(valid.df)

telco.ct <- rpart(Churn~., data = train.df,
                  method = "class", control = rpart.control(maxdepth = 3, minbucket = 30))



prp(telco.ct, type = 1, extra = 1)

telco.pred <- predict( telco.ct, valid.df, type = "class"   )
library(caret)
confusionMatrix(telco.pred, factor(valid.df$Churn))



#logistic regression
logit.df <- glm(Churn ~ .,data = train.df, family = "binomial")


summary(logit.df)


##########ONly month to month customers##########
mtm.df <- telco.df[telco.df[,"Contract"] == 'Month-to-month',]

mtm.train.index <- sample(nrow(mtm.df), 0.6 * nrow(mtm.df))
mtm.train.df <- mtm.df[mtm.train.index, c(-1,-16)]
mtm.valid.df <- mtm.df[-mtm.train.index, c(-1,-16)]
#mtm.train.df <- mtm.df[mtm.train.index, c(-1,-6,-16,-20)]
#mtm.valid.df <- mtm.df[-mtm.train.index, c(-1,-6,-16,-20)]
mtm.train.df

mtm.ct <- rpart(Churn~., data = mtm.train.df,
                  method = "class")
mtm.ct
prp(mtm.ct, type = 1, extra = 1)

mtm.pred <- predict( mtm.ct, mtm.valid.df, type = "class"   )
library(caret)
confusionMatrix(mtm.pred, factor(mtm.valid.df$Churn))


#######End of Telco Decision Tree CODE##########


#######Start of Telco Logistic Regression CODE##########

telco.df$gender <- relevel(telco.df$gender, ref = "Male")
telco.df$SeniorCitizen <- relevel(telco.df$SeniorCitizen, ref = "Yes")
telco.df$Partner <- relevel(telco.df$Partner, ref = "Yes")
telco.df$Dependents <- relevel(telco.df$Dependents, ref = "Yes")
telco.df$PhoneService <- relevel(telco.df$PhoneService, ref = "Yes")
telco.df$InternetService <- relevel(telco.df$InternetService, ref = "No")
telco.df$PaymentMethod <- relevel(telco.df$PaymentMethod, ref = "Mailed check")
telco.df$Contract <- relevel(telco.df$Contract, ref = "Two year")
telco.df$Churn <- as.numeric(telco.df$Churn == "Yes")

#all variables
set.seed(1111)
selected.var <- c(2, 3, 4, 5, 6, 7, 8,9,10, 11, 12,13 , 14, 15,16,17, 18, 19,20, 21)
selected.df <- telco.df[, selected.var]
train.index <- sample(1:nrow(telco.df), nrow(telco.df)*0.6)
train.df <- selected.df[train.index, ]
valid.df <- selected.df[-train.index, ]
logit.reg <- glm(Churn ~ ., data = train.df, family = "binomial")
summary(logit.reg)

#no 7 variables
set.seed(1111)
selected.var <- c(2, 3, 4, 5, 6, 7,9,16,17, 18, 19,20, 21)
selected.df <- telco.df[, selected.var]
train.index <- sample(1:nrow(telco.df), nrow(telco.df)*0.6)
train.df <- selected.df[train.index, ]
valid.df <- selected.df[-train.index, ]
logit.reg <- glm(Churn ~ ., data = train.df, family = "binomial")
summary(logit.reg)

#no 7 & no totalcharges
set.seed(1111)
selected.var <- c(2, 3, 4, 5, 6, 7,9,16,17, 18, 19,21)
selected.df <- telco.df[, selected.var]
train.index <- sample(1:nrow(telco.df), nrow(telco.df)*0.6)
train.df <- selected.df[train.index, ]
valid.df <- selected.df[-train.index, ]
#View(train.df)
logit.reg <- glm(Churn ~ ., data = train.df, family = "binomial")
summary(logit.reg)

logit.reg.pred <- predict(logit.reg, valid.df,  type = "response")
logit.reg.pred
pred <- ifelse(logit.reg.pred > 0.5, 1 , 0)
pred
library(caret)
confusionMatrix(factor(pred), factor(valid.df$Churn), positive = "1")
library(pROC)
r <- roc(valid.df$Churn, logit.reg.pred)
plot.roc(r)
coords(r, x = "best", transpose = FALSE)


#optimal
pred <- ifelse(logit.reg.pred > 0.2685455, 1 , 0)
pred

library(caret)
confusionMatrix(factor(pred), factor(valid.df$Churn), positive = "1")

library(pROC)

r <- roc(valid.df$Churn, logit.reg.pred)
plot.roc(r)

#######End of Telco Logistic Regression CODE##########

