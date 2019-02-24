setwd("D:/GreatLakes/Group Assignment 5_ANN and CART")
orig <- read.csv("HR_Employee_Attrition_Data.csv")


hr <- read.csv("HR_Employee_Attrition_Data.csv")
str(hr)
summary(hr)

hr <- hr[,c(-9,-10,-22,-27)] #removing unwanted columns from the dataset.
View(hr)

attach(hr)

travel.matrix <- model.matrix(~ BusinessTravel - 1, data = hr)
hr <- data.frame(hr, travel.matrix) #3

dept.matrix <- model.matrix(~ Department - 1, data = hr)
hr <- data.frame(hr,dept.matrix) #5

edu.matrix <- model.matrix(~ EducationField - 1, data = hr)
hr <- data.frame(hr,edu.matrix) #8

role.matrix <- model.matrix(~ JobRole - 1, data = hr)
hr <- data.frame(hr,role.matrix) #14

marital.matrix <- model.matrix(~ MaritalStatus - 1, data = hr)
hr <- data.frame(hr, marital.matrix) #16

hr <- hr[,c(-3,-5,-8,-14,-16)] #removing the above changed hot coded columns

hr$Gender <- as.numeric(factor(hr$Gender,
                                    levels = c("Female","Male")))

hr$OverTime <- as.factor(hr$OverTime,
                                 levels = c("Yes","No"),
                                 labels = c(1,0))

hr$Attrition <- as.numeric(factor(hr$Attrition,
                               levels = c("Yes","No"),
                               labels = c(1,0)))

hr$OverTime[hr$OverTime == 2] <- 0
hr$Attrition[hr$Attrition == 2] <- 0

# 
# OutVals = boxplot(hr$MonthlyIncome, plot=FALSE)$out
# 
# install.packages("outliers")
# library(outliers)
# outlier(hr)
# boxplot(hr$MonthlyIncome)

library(caTools)
split <- sample.split(hr$Attrition,SplitRatio = 0.7)
training_set <- subset(hr,split==TRUE)
test_set <- subset(hr,split ==FALSE)

training_set[-2] <- scale(training_set[-2])
test_set[-2] <- scale(test_set[-2])

library(h2o)
h2o.init(nthreads = -1)
classifier <- h2o.deeplearning(y = 'Attrition',
                               training_frame = as.h2o(training_set),
                               activation = 'Rectifier',
                               hidden = c(25,25),
                               epochs = 100,
                               train_samples_per_iteration = -2)

prob_pred <- h2o.predict(classifier,newdata = as.h2o(test_set[-2]))
prob_pred
y_pred <- (prob_pred > 0.5)
y_pred <- as.vector(y_pred)

y_predfact <- as.factor(y_pred)
curr <- as.factor(test_set$Attrition)

library(caret)
confusionMatrix(curr,y_predfact)

h2o.shutdown()

library(neuralnet)

nn1 <- neuralnet(formula = Attrition ~ 
                   Age+DailyRate+Education+Gender+JobInvolvement+JobSatisfaction+MonthlyRate+OverTime+PerformanceRating+StockOptionLevel+TrainingTimesLastYear+YearsAtCompany+YearsSinceLastPromotion+BusinessTravelNon.Travel+BusinessTravelTravel_Rarely+DepartmentResearch...Development+EducationFieldHuman.Resources+EducationFieldMarketing+EducationFieldOther+JobRoleHealthcare.Representative+JobRoleLaboratory.Technician+JobRoleManufacturing.Director+JobRoleResearch.Scientist+JobRoleSales.Representative+MaritalStatusMarried+DistanceFromHome+EnvironmentSatisfaction+HourlyRate+JobLevel+MonthlyIncome+NumCompaniesWorked+PercentSalaryHike+RelationshipSatisfaction+TotalWorkingYears+WorkLifeBalance+YearsInCurrentRole+YearsWithCurrManager+BusinessTravelTravel_Frequently+DepartmentHuman.Resources+DepartmentSales+EducationFieldLife.Sciences+EducationFieldMedical+EducationFieldTechnical.Degree+JobRoleHuman.Resources+JobRoleManager+JobRoleResearch.Director+JobRoleSales.Executive+MaritalStatusDivorced+MaritalStatusSingle ,
                 data = training_set, 
                 hidden = 2,
                 err.fct = "sse",
                 linear.output = FALSE,
                 lifesign = "full",
                 lifesign.step = 10,
                 threshold = 0.1,
                 stepmax = 2000
                 ##startweights = startweightsObj
)

plot (nn1)
training_set$Prob = nn1$net.result[[1]] 
training_set$Prob <- ifelse(training_set$Prob >= 0.5, 1, 0) 
table(training_set$Attrition,training_set$Prob)


