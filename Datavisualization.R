library(ggplot2)
library(dplyr)

#read the data
telco.df <- read.csv("Churn.csv", stringsAsFactors = TRUE)

#understand the data & variables
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

#demographic distribution

gender_p <- ggplot(telco.df, aes(x=gender,fill=Churn))+ geom_bar( show.legend = FALSE) +scale_fill_manual(values = c("Yes"="#990604", "No"="#71706e"))+ theme_minimal()
gender_p

SeniorCitizen_p <- ggplot(telco.df, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(show.legend = TRUE) +scale_fill_manual(values = c("Yes"="#990604", "No"="#71706e"))+ theme_minimal()
SeniorCitizen_p

dependents_p <- ggplot(telco.df, aes(x=Dependents,fill=Churn))+ geom_bar(show.legend = FALSE) +scale_fill_manual(values = c("Yes"="#990604", "No"="#71706e"))+ theme_minimal()
dependents_p

partner_p <- ggplot(telco.df, aes(x=Partner,fill=Churn))+ geom_bar(show.legend = TRUE) +scale_fill_manual(values = c("Yes"="#990604", "No"="#71706e"))+ theme_minimal()
partner_p 

grid.arrange (gender_p,SeniorCitizen_p,dependents_p,partner_p, nrow=2)

#internet service breakdown
onlinesecurity_p <- ggplot(subset(telco.df, OnlineSecurity %in% c("No","Yes")), aes(x=OnlineSecurity,fill=Churn))+ geom_bar( show.legend = FALSE) + theme_minimal()+ scale_fill_manual(values = c("Yes"="#990604", "No"="#71706e"))
onlinesecurity_p

onlinebackup_p <- ggplot(subset(telco.df, OnlineBackup %in% c("No","Yes")), aes(x=OnlineBackup,fill=Churn))+ geom_bar( show.legend = FALSE) + theme_minimal()+ scale_fill_manual(values = c("Yes"="#990604", "No"="#71706e"))
onlinebackup_p

deviceprotection_p <- ggplot(subset(telco.df, DeviceProtection %in% c("No","Yes")), aes(x=DeviceProtection,fill=Churn))+ geom_bar( show.legend = TRUE) + theme_minimal()+ scale_fill_manual(values = c("Yes"="#990604", "No"="#71706e"))
deviceprotection_p

techsupport_p <- ggplot(subset(telco.df, TechSupport %in% c("No","Yes")), aes(x=TechSupport,fill=Churn))+ geom_bar( show.legend = FALSE) + theme_minimal()+ scale_fill_manual(values = c("Yes"="#990604", "No"="#71706e"))
techsupport_p

streaming_tv_p <- ggplot(subset(telco.df, StreamingTV %in% c("No","Yes")), aes(x=StreamingTV,fill=Churn))+ geom_bar( show.legend = FALSE) + theme_minimal()+ scale_fill_manual(values = c("Yes"="#990604", "No"="#71706e"))
streaming_tv_p

streaming_movies_p <- ggplot(subset(telco.df, StreamingMovies %in% c("No","Yes")), aes(x=StreamingMovies,fill=Churn))+ geom_bar( show.legend = TRUE) + theme_minimal()+ scale_fill_manual(values = c("Yes"="#990604", "No"="#71706e"))
streaming_movies_p

grid.arrange(onlinesecurity_p,onlinebackup_p,deviceprotection_p,techsupport_p,streaming_tv_p,streaming_movies_p, nrow=2)

#tenure vs average total charge breakdown by churn
month <- ggplot(subset(telco.df, Contract %in% c("Month-to-month")),aes(x = tenure, color=Churn)) + geom_freqpoly(size=2) + theme_minimal() + labs(title="Month to month", x = "Tenure(month)")+scale_color_manual(values = c("Yes"="#990604", "No"="#71706e"))
one_year <- ggplot(subset(telco.df, Contract %in% c("One year")),aes(x = tenure, color=Churn)) + geom_freqpoly(size=2) + theme_minimal() + labs(title="One year", x = "Tenure(month)")+scale_color_manual(values = c("Yes"="#990604", "No"="#71706e"))
two_year <- ggplot(subset(telco.df, Contract %in% c("Two year")),aes(x = tenure, color=Churn)) + geom_freqpoly(size=2) + theme_minimal() + labs(title="Two year", x = "Tenure(month)")+scale_color_manual(values = c("Yes"="#990604", "No"="#71706e"))
grid.arrange(month,
             one_year,
             two_year)

ggplot(data = telco.df) + 
  geom_line(aes(x=tenure, y=MonthlyCharges, color=Churn), stat="summary",fun="mean")+labs(title="Tenure vs average monthly charge",x = "Tenure(month)",y="average monthly charge")+scale_color_manual(values = c("Yes"="#990604", "No"="#71706e"))+theme_minimal()

#Scatter plot for monthly charges vs total charges
ggplot(data=telco.df) + geom_point(aes(x=MonthlyCharges, y=TotalCharges, colour = "#990604"))+ scale_color_manual(values=c('#990604'))+ theme_minimal()

#boxplot for tenure and total charge
ggplot(data=telco.df) + geom_boxplot(aes(x=Churn, y=TotalCharges,fill=Churn))+scale_fill_manual(values = c("Yes"="#990604", "No"="#71706e"))+theme_minimal()+ labs(title="Total Charges vs Churn")
ggplot(data=telco.df) + geom_boxplot(aes(x=Churn, y=tenure, fill=Churn))+scale_fill_manual(values = c("Yes"="#990604", "No"="#71706e"))+theme_minimal()+ labs(title="Tenure vs Churn")

#histogram
#monthly charge breakdown by both
both_p <- ggplot(subset(telco.df, PhoneService %in% c("Yes") & InternetService %in% c("DSL","	Fiber optic")),aes(x = MonthlyCharges, fill = Churn, binwidth=20)) + geom_histogram(size=2) + theme_minimal() + labs(title = "The customer who has both Internet service and phone serivce", y = "Count", x = "Monthly charge") +scale_fill_manual(values = c("Yes"="#990604", "No"="#71706e"))

#monthy charge breakdown by only phone
only_phone <- ggplot(subset(telco.df, PhoneService %in% c("Yes") & TechSupport %in% c("No internet service")),aes(x = MonthlyCharges, fill = Churn, binwidth=20)) + geom_histogram(size=2) + theme_minimal() + labs(title = "The customer who only has phone service", y = "Count", x = "Monthly charge") +scale_fill_manual(values = c("Yes"="#990604", "No"="#71706e"))

#monthly charge breakdown by only internet
only_internet <- ggplot(subset(telco.df, PhoneService %in% c("No") & InternetService %in% c("DSL","	Fiber optic")),aes(x = MonthlyCharges, fill = Churn, binwidth=20)) + geom_histogram(size=1) + theme_minimal()+  labs(title = "The customer who only has internet service", y = "Count", x = "Monthly charge") +scale_fill_manual(values = c("Yes"="#990604", "No"="#71706e"))
only_internet 
#monthly charge breakdown
grid.arrange(both_p,
             only_phone,
             only_internet)
