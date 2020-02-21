#####################################################
#
# INC 491 - Data Science and Intelligent Techniques
# Mini Project  - Credit Approval Data Set
#
# Life Expectancy (WHO)
# Statistical Analysis on factors influencing Life 
# Expectancy
#
# Developed by: Twitty Manymoon
#
#####################################################

# ----- 1.) Import -----
library(gbm)
library(readr)
library(e1071)
library(Metrics)
library(ggplot2)
library(reshape2)
library(corrplot)
library(tidyverse)
library(randomForest)
dataset <- read_csv("Life Expectancy Data.csv")

# ----- 2.) Tidy up -----
# pattern of data, and find modelling strategies
str(dataset)
summary(dataset)
View(dataset)

# NA removeing
#dataset_2 <- na.omit(dataset)
dataset_2 <- dataset
for(i in 4:ncol(dataset_2)){
  dataset_2[is.na(dataset_2[,i]), i] <- colMeans(dataset_2[,i], na.rm = TRUE)
}

# ----- 3.) Transform ----- 
names(dataset_2)[names(dataset_2) == "Life expectancy"] <- "Life_Expectancy"
names(dataset_2)[names(dataset_2) == "Adult Mortality"] <- "Adult_Mortality"
names(dataset_2)[names(dataset_2) == "infant deaths"] <- "Infant_Deaths"
names(dataset_2)[names(dataset_2) == "percentage expenditure"] <- "Percentage_Expenditure"
names(dataset_2)[names(dataset_2) == "Hepatitis B"] <- "Hepatitis_B"
names(dataset_2)[names(dataset_2) == "under-five deaths"] <- "Under_Five_Deaths"
names(dataset_2)[names(dataset_2) == "Total expenditure"] <- "Total_Expenditure"
names(dataset_2)[names(dataset_2) == "HIV/AIDS"] <- "HIV_AIDS"
names(dataset_2)[names(dataset_2) == "thinness  1-19 years"] <- "Thinness_1_19"
names(dataset_2)[names(dataset_2) == "thinness 5-9 years"] <- "Thinness_5_9"
names(dataset_2)[names(dataset_2) == "Income composition of resources"] <- "Income_Composition"

dataset_2$Status <- as.factor(dataset_2$Status)
dataset_2$Status_type <- as.numeric(dataset_2$Status)
df_life_developing <- subset(dataset_2, dataset_2$Status == "Developing")
df_life_developed <- subset(dataset_2, dataset_2$Status == "Developed")

# ----- 4.) Visualize -----
# Status
df_status <- data.frame(
  Status = c("Developing", "Developed"),
  Sum = c(sum(dataset_2$Status == "Developing")*100/nrow(dataset_2), 
          sum(dataset_2$Status == "Developed")*100/nrow(dataset_2))
)
ggplot(df_status, aes(x="", y=Sum, fill=Status)) + geom_bar(stat="identity", width=1) + coord_polar("y", start=0)

# : Life Expectancy
ggplot() + 
  geom_density(data = df_life_developing, aes(x = df_life_developing$Life_Expectancy), fill = "red", color = "red", alpha = 0.5) +
  geom_density(data = df_life_developed, aes(x = df_life_developed$Life_Expectancy), fill = "blue", color = "blue", alpha = 0.5) +
  xlab('Life Expectancy') +
  ylab('Density')

# : Adult Mortality
ggplot() + 
  geom_density(data = df_life_developing, aes(x = df_life_developing$Adult_Mortality), fill = "red", color = "red", alpha = 0.5) +
  geom_density(data = df_life_developed, aes(x = df_life_developed$Adult_Mortality), fill = "blue", color = "blue", alpha = 0.5) +
  xlab('Adult Mortality') +
  ylab('Density')

# : Infant Deaths
ggplot() + 
  geom_density(data = df_life_developing, aes(x = df_life_developing$Infant_Deaths), fill = "red", color = "red", alpha = 0.5) +
  geom_density(data = df_life_developed, aes(x = df_life_developed$Infant_Deaths), fill = "blue", color = "blue", alpha = 0.5) +
  xlab('Infant Deaths') +
  ylab('Density')

# : Alcohol
ggplot() + 
  geom_density(data = df_life_developing, aes(x = df_life_developing$Alcohol), fill = "red", color = "red", alpha = 0.5) +
  geom_density(data = df_life_developed, aes(x = df_life_developed$Alcohol), fill = "blue", color = "blue", alpha = 0.5) +
  xlab('Alcohol') +
  ylab('Density')

# : Percentage Expenditure
ggplot() + 
  geom_density(data = df_life_developing, aes(x = df_life_developing$Percentage_Expenditure), fill = "red", color = "red", alpha = 0.5) +
  geom_density(data = df_life_developed, aes(x = df_life_developed$Percentage_Expenditure), fill = "blue", color = "blue", alpha = 0.5) +
  xlab('Percentage Expenditure') +
  ylab('Density')

# : Hepatitis B 
ggplot() + 
  geom_density(data = df_life_developing, aes(x = df_life_developing$Hepatitis_B), fill = "red", color = "red", alpha = 0.5) +
  geom_density(data = df_life_developed, aes(x = df_life_developed$Hepatitis_B), fill = "blue", color = "blue", alpha = 0.5) +
  xlab('Hepatitis B') +
  ylab('Density')

# : Measles
ggplot() + 
  geom_density(data = df_life_developing, aes(x = df_life_developing$Measles), fill = "red", color = "red", alpha = 0.5) +
  geom_density(data = df_life_developed, aes(x = df_life_developed$Measles), fill = "blue", color = "blue", alpha = 0.5) +
  xlab('Measles') +
  ylab('Density')

# : BMI
ggplot() + 
  geom_density(data = df_life_developing, aes(x = df_life_developing$BMI), fill = "red", color = "red", alpha = 0.5) +
  geom_density(data = df_life_developed, aes(x = df_life_developed$BMI), fill = "blue", color = "blue", alpha = 0.5) +
  xlab('BMI') +
  ylab('Density')

# : Under-5 Deaths
ggplot() + 
  geom_density(data = df_life_developing, aes(x = df_life_developing$Under_Five_Deaths), fill = "red", color = "red", alpha = 0.5) +
  geom_density(data = df_life_developed, aes(x = df_life_developed$Under_Five_Deaths), fill = "blue", color = "blue", alpha = 0.5) +
  xlab('Under-5 Deaths') +
  ylab('Density')

# : Polio
ggplot() + 
  geom_density(data = df_life_developing, aes(x = df_life_developing$Polio), fill = "red", color = "red", alpha = 0.5) +
  geom_density(data = df_life_developed, aes(x = df_life_developed$Polio), fill = "blue", color = "blue", alpha = 0.5) +
  xlab('Polio') +
  ylab('Density')

# : Total Expenditure
ggplot() + 
  geom_density(data = df_life_developing, aes(x = df_life_developing$Total_Expenditure), fill = "red", color = "red", alpha = 0.5) +
  geom_density(data = df_life_developed, aes(x = df_life_developed$Total_Expenditure), fill = "blue", color = "blue", alpha = 0.5) +
  xlab('Total Expenditure') +
  ylab('Density')

# : Diphtheria 
ggplot() + 
  geom_density(data = df_life_developing, aes(x = df_life_developing$Diphtheria), fill = "red", color = "red", alpha = 0.5) +
  geom_density(data = df_life_developed, aes(x = df_life_developed$Diphtheria), fill = "blue", color = "blue", alpha = 0.5) +
  xlab('Diphtheria') +
  ylab('Density')

# : HIV/AIDS
ggplot() + 
  geom_density(data = df_life_developing, aes(x = df_life_developing$HIV_AIDS), fill = "red", color = "red", alpha = 0.5) +
  geom_density(data = df_life_developed, aes(x = df_life_developed$HIV_AIDS), fill = "blue", color = "blue", alpha = 0.5) +
  xlab('HIV/AIDS') +
  ylab('Density')

# : GDP
ggplot() + 
  geom_density(data = df_life_developing, aes(x = df_life_developing$GDP), fill = "red", color = "red", alpha = 0.5) +
  geom_density(data = df_life_developed, aes(x = df_life_developed$GDP), fill = "blue", color = "blue", alpha = 0.5) +
  xlab('GDP') +
  ylab('Density')

# : Population
ggplot() + 
  geom_density(data = df_life_developing, aes(x = df_life_developing$Population), fill = "red", color = "red", alpha = 0.5) +
  geom_density(data = df_life_developed, aes(x = df_life_developed$Population), fill = "blue", color = "blue", alpha = 0.5) +
  xlab('Population') +
  ylab('Density')

# thinness  10-19 years
ggplot() + 
  geom_density(data = df_life_developing, aes(x = df_life_developing$Thinness_1_19), fill = "red", color = "red", alpha = 0.5) +
  geom_density(data = df_life_developed, aes(x = df_life_developed$Thinness_1_19), fill = "blue", color = "blue", alpha = 0.5) +
  xlab('Thinness 1-19 yrs.') +
  ylab('Density')

# thinness 5-9 years
ggplot() + 
  geom_density(data = df_life_developing, aes(x = df_life_developing$Thinness_5_9), fill = "red", color = "red", alpha = 0.5) +
  geom_density(data = df_life_developed, aes(x = df_life_developed$Thinness_5_9), fill = "blue", color = "blue", alpha = 0.5) +
  xlab('Thinness 5-9 yrs.') +
  ylab('Density')

# Income composition of resources
ggplot() + 
  geom_density(data = df_life_developing, aes(x = df_life_developing$Income_Composition), fill = "red", color = "red", alpha = 0.5) +
  geom_density(data = df_life_developed, aes(x = df_life_developed$Income_Composition), fill = "blue", color = "blue", alpha = 0.5) +
  xlab('Income Composition') +
  ylab('Density')

# Schooling
ggplot() + 
  geom_density(data = df_life_developing, aes(x = df_life_developing$Schooling), fill = "red", color = "red", alpha = 0.5) +
  geom_density(data = df_life_developed, aes(x = df_life_developed$Schooling), fill = "blue", color = "blue", alpha = 0.5) +
  xlab('Schooling') +
  ylab('Density')

# Life Expectancy ~ Adult Mortality
Relationship_LE_AdultMortality <- ggplot(dataset_2, aes(x = dataset_2$Life_Expectancy, y = dataset_2$Adult_Mortality))
Relationship_LE_AdultMortality + geom_point(color = "#00AFBB", size = 2) + geom_smooth(method = lm) + xlab('Life Expectancy') + ylab('Adult Mortaility') 

# Life Expectancy ~ Infant Deaths
Relationship_LE_InfantDeaths <- ggplot(dataset_2, aes(x = dataset_2$Life_Expectancy, y = dataset_2$Infant_Deaths))
Relationship_LE_InfantDeaths + geom_point(color = "#FC4E07", size = 2) + geom_smooth(method = lm) + xlab('Life Expectancy') + ylab('Infant Deaths') 

# Life Expectancy ~ Alcohol
Relationship_LE_Alcohol <- ggplot(dataset_2, aes(x = dataset_2$Life_Expectancy, y = dataset_2$Alcohol))
Relationship_LE_Alcohol + geom_point(color = "#E7B800", size = 2) + geom_smooth(method = lm) + xlab('Life Expectancy') + ylab('Alcohol Consumption per Capita (Litre)')

# Life Expectancy ~ Population
Relationship_LE_Population <- ggplot(dataset_2, aes(x = dataset_2$Life_Expectancy, y = dataset_2$Population))
Relationship_LE_Population + geom_point(color = "#00FF0A", size = 2) + geom_smooth(method = lm) + xlab('Life Expectancy') + ylab('Population') 

# Life Expectancy ~ Schooling
Relationship_LE_Population <- ggplot(dataset_2, aes(x = dataset_2$Life_Expectancy, y = dataset_2$Schooling))
Relationship_LE_Population + geom_point(color = "#FF4D88", size = 2) + geom_smooth(method = lm) + xlab('Life Expectancy') + ylab('Schooling')

# Correlation 
correlation <- cor(dataset_2 %>% select(4:23))
melted_correlation <- melt(correlation)
melted_correlation <- subset(melted_correlation, melted_correlation$Var2 == "Life_Expectancy")
melted_correlation_sorted <- melted_correlation[order(melted_correlation$value), ]

cor = cor(dataset_2[4:23])
corrplot(cor, method = "color") # heat map
ggplot(data = melted_correlation_sorted, aes(x=Var2, y=Var1, fill=value)) + geom_tile() # life expectancy only

# ----- 5.) Model -----
# Create Training and Test data
set.seed(100)
train_dataset <- subset(dataset_2, select=c(4:23))
train_rows <- sample(1:nrow(train_dataset), 0.7*nrow(train_dataset))
train_data <- train_dataset[train_rows, ]
test_data <- train_dataset[-train_rows, ]

# Multiple Linear Regression
model_lm <- lm(`Life_Expectancy` ~ ., data = train_data)
predicted_lm <- predict(model_lm, test_data)

model_lm_fit_1 <- lm(`Life_Expectancy` ~ `Adult_Mortality` + 
                       `Infant_Deaths` + `BMI` + `Under_Five_Deaths` + 
                       `Polio` + `Diphtheria` + `HIV_AIDS`+ `Income_Composition` + 
                       `Schooling` + `Status_type`, data = train_data)
predicted_lm_fit_1 <- predict(model_lm_fit_1, test_data)

# Random Forest
model_rf <- randomForest(`Life_Expectancy` ~ ., data = train_data)
predicted_rf <- predict(model_rf, test_data)

# Gradient Boosting
model_gbm <- gbm(`Life_Expectancy` ~ . ,data = train_data, distribution = "gaussian", 
                 n.trees = 10000, shrinkage = 0.01, interaction.depth = 1)
n.trees = seq(from=100 ,to=10000, by=100)
predicted_gbm <- predict(model_gbm, test_data, n.trees = n.trees)

# SVM
model_svm <- svm(`Life_Expectancy` ~ . , 
                 data = train_data, type = 'eps-regression', kernel = 'linear')
predicted_svm <- predict(model_svm, newdata = test_data) 

# ----- 6.) Accuracy -----

# MSE
MSE_lm = mse(predicted_lm, test_data$`Life_Expectancy`) 
MSE_lm_fit_1 = mse(predicted_lm_fit_1, test_data$`Life_Expectancy`)
MSE_rf = mse(predicted_rf, test_data$`Life_Expectancy`)
MSE_gbm = mse(predicted_gbm, test_data$`Life_Expectancy`)
MSE_svm = mse(predicted_svm, test_data$`Life_Expectancy`)

# RMSE
error_lm <- predicted_lm - test_data$`Life_Expectancy`
RMSE_lm = sqrt(mean(error_lm^2))

error_lm_fit_1 <- predicted_lm_fit_1 - test_data$`Life_Expectancy`
RMSE_lm_fit_1 = sqrt(mean(error_lm_fit_1^2))

error_rf <- predicted_rf - test_data$`Life_Expectancy`
RMSE_rf = sqrt(mean(error_rf^2))

error_gbm <- predicted_gbm - test_data$`Life_Expectancy`
RMSE_gbm = sqrt(mean(error_gbm^2))

error_svm <- predicted_svm - test_data$`Life_Expectancy`
RMSE_svm <- sqrt(mean(error_svm^2))

# Min/Max Accuracy
minmax_lm <- mean(min(test_data$`Life_Expectancy`, predicted_lm)/max(test_data$`Life_Expectancy`, predicted_lm))
minmax_lm_fit_1 <- mean(min(test_data$`Life_Expectancy`, predicted_lm_fit_1)/max(test_data$`Life_Expectancy`, predicted_lm_fit_1))
minmax_rf <- mean(min(test_data$`Life_Expectancy`, predicted_rf)/max(test_data$`Life_Expectancy`, predicted_rf))
minmax_gbm <- mean(min(test_data$`Life_Expectancy`, predicted_gbm)/max(test_data$`Life_Expectancy`, predicted_gbm))
minmax_svm <- mean(min(test_data$`Life_Expectancy`, predicted_svm)/max(test_data$`Life_Expectancy`, predicted_svm))

# ----- 7.) Summary -----
df_summary <- data.frame(
  Model = c("gbm", "lm", "lm_fit_1", "rf", "svm"),
  MSE = c(MSE_gbm, MSE_lm, MSE_lm_fit_1, MSE_rf, MSE_svm),
  RMSE = c(RMSE_gbm, RMSE_lm, RMSE_lm_fit_1, RMSE_rf, RMSE_svm),
  Min_Max_Acc = c(minmax_gbm, minmax_lm, minmax_lm_fit_1, minmax_rf, minmax_svm)
)

plot(model_rf)

summary(model_lm)
summary(model_lm_fit_1)
summary(model_rf)
summary(model_gbm)
summary(model_svm)

