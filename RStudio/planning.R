# Dummies: sex, address, famsize, Pstatus, schoolsup, famsup, paid, activities, nursery, higher, internet, romantic
# Categorical coded: Medu, Fedu, traveltime, studytime, failures, famrel, freetime, goout, Dalc, Walc
# Categorical uncoded: Mjob, Fjob, reason, guardian
# Numerical: age, absences, G1, G2, G3

# note: 2 schools (GP and MS), 2 classes (math and Portuguese)

# regressants: Dalc and Walc
# regressors: everything else

# regressants: G3 or change from G1 to G3
# regressors: everything else (excluding G2 for change version)

# import raw data
data_math <- read.csv("Kaggle Datasets/Student Alcohol Consumption/student-mat.csv", header=TRUE)
data_port <- read.csv("Kaggle Datasets/Student Alcohol Consumption/student-por.csv", header=TRUE)

# clean data
data_math_clean <- data_math
# recode dummies
data_math_clean$sex <- ifelse(data_math_clean$sex=="M", 1, 0)
data_math_clean$address <- ifelse(data_math_clean$address=="R", 1, 0)
data_math_clean$famsize <- ifelse(data_math_clean$famsize=="GT3", 1, 0)
data_math_clean$Pstatus <- ifelse(data_math_clean$Pstatus=="A", 1, 0)
data_math_clean$schoolsup <- ifelse(data_math_clean$schoolsup=="yes", 1, 0)
data_math_clean$famsup <- ifelse(data_math_clean$famsup=="yes", 1, 0)
data_math_clean$paid <- ifelse(data_math_clean$paid=="yes", 1, 0)
data_math_clean$activities <- ifelse(data_math_clean$activities=="yes", 1, 0)
data_math_clean$nursery <- ifelse(data_math_clean$nursery=="yes", 1, 0)
data_math_clean$higher <- ifelse(data_math_clean$higher=="yes", 1, 0)
data_math_clean$internet <- ifelse(data_math_clean$internet=="yes", 1, 0)
data_math_clean$romantic <- ifelse(data_math_clean$romantic=="yes", 1, 0)
# recode uncoded categoricals
data_math_clean$Mjob <- as.factor(as.character(data_math_clean$Mjob))
data_math_clean$Mjob <- factor(data_math_clean$Mjob, levels(data_math_clean$Mjob)[c(1, 2, 4, 5, 3)])
data_math_clean$Fjob <- as.factor(as.character(data_math_clean$Fjob))
data_math_clean$Fjob <- factor(data_math_clean$Fjob, levels(data_math_clean$Fjob)[c(1, 2, 4, 5, 3)])
data_math_clean$reason <- as.factor(as.character(data_math_clean$reason))
data_math_clean$reason <- factor(data_math_clean$reason, levels(data_math_clean$reason)[c(2, 4, 1, 3)])
data_math_clean$guardian <- as.factor(as.character(data_math_clean$guardian))
data_math_clean$guardian <- factor(data_math_clean$guardian, levels(data_math_clean$guardian)[c(2, 1, 3)])

# data_math_clean$Mjob <- as.factor(data_math_clean$Mjob, c("at_home", "health", "services", "teacher", "other"))
# data_math_clean[,'Mjob'] <- as.factor(data_math_clean[,'Mjob'])

# install.packages("plyr")
# library(plyr)
# data_math_clean$Mjob <- revalue(data_math_clean$Mjob,
#                              c("at_home"=0, "health"=1, "services"=2, "teacher"=3, "other"=4))
# data_math_clean$Fjob <- revalue(data_math_clean$Fjob,
#                                 c("at_home"=0, "health"=1, "services"=2, "teacher"=3, "other"=4))
# data_math_clean$reason <- revalue(data_math_clean$reason,
#                                 c("home"=0, "reputation"=1, "course"=2, "other"=3))
# data_math_clean$guardian <- revalue(data_math_clean$guardian,
#                                 c("mother"=0, "father"=1, "other"=2))

# regressions
reg_math_test <- lm(Dalc ~ sex + address + famsize + Pstatus + schoolsup + famsup + paid + 
                 activities + nursery + higher + internet + romantic + Mjob + Fjob + 
                 reason + guardian, data=data_math_clean)
summary(reg_math_test)
reg_math_test <- lm(Dalc ~ sex + address + famsize + paid + activities + nursery + reason, data=data_math_clean)
summary(reg_math_test)






