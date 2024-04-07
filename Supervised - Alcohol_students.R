library(tidyverse)
library(ggplot2)
library(gridExtra)
library(caret)
library(RColorBrewer)
library(ggpubr)
library(corrplot)
library(ggfortify)
library(lares)
library(car)
library(MASS)
library(olsrr)
library(lmtest)
library(estimatr)
library(sandwich)
library(caTools)
library(tree)
library(partykit)
library(rpart.plot)
library(performanceEstimation)
library(AICcmodavg)
library(gbm)
library(xgboost)
library(randomForest)
library(glmnet)
library(e1071)



################ IMPORTING THE DATA ############################################


data.raw_alc.mat <- read.csv("C:/Users/sangi/Desktop/SL exam/StudentAlcohol/student-mat.csv")
data.raw_alc.por <- read.csv("C:/Users/sangi/Desktop/SL exam/StudentAlcohol/student-por.csv")

data.raw_alc.mat$Subject <- "Maths"
data.raw_alc.por$Subject <- "Portuguese"

merged_data <- rbind(data.raw_alc.mat, data.raw_alc.por)

new_cols <- c("School", "Sex", "Age", "House_location", "Fam_size", "Parents_status",
              "Mother_edu", "Father_edu", "Mother_job", "Father_job", "Reason", 
              "Guardian", "Travel_time", "Study_time", "Class_failures", "School_supp",
              "Fam_supp", "Private_lessons", "Extra_act", "Nursery_school", "Continue_studies",
              "Internet", "Relationship", "Family_rel", "Free_time", "Going_out", 
              "WD_alc", "WE_alc", "Health_status", "Absences", "Grades_t1", "Grades_t2",
              "Final_grade", "Subject")
colnames(merged_data) <- new_cols

merged_data$School[merged_data$School == "GP"] <- "Gabriel Pereira"
merged_data$School[merged_data$School == "MS"] <- "Mousinho da Silveira"
merged_data$House_location[merged_data$House_location == "U"] <- "Urban"
merged_data$House_location[merged_data$House_location == "R"] <- "Rural"
merged_data$Parents_status[merged_data$Parents_status == "T"] <- "Together"
merged_data$Parents_status[merged_data$Parents_status == "A"] <- "Alone"

merged_data$Total_drinking <- merged_data$WE_alc + merged_data$WD_alc

merged_data <- mutate(merged_data, Drinking_level = case_when(
                        Total_drinking <= 4 ~ "Low",
                        Total_drinking <= 6 & Total_drinking >= 5 ~ "Moderate",
                        Total_drinking >= 7 ~ "High"))

alcohol_data <- merged_data %>%
  dplyr::select("Sex", "Age", "WD_alc", "WE_alc", "Drinking_level", "School", "Subject", 
         "Final_grade", "Class_failures", "Study_time", "Travel_time", "Absences", 
         "Health_status", "Mother_job", "Father_job", "Mother_edu", "Father_edu", 
         "Parents_status", "Fam_size", "Private_lessons", "Extra_act", "Continue_studies", 
         "Internet", "Relationship", "Family_rel", "Free_time", "Going_out")

alcohol_data$Sex <- factor(alcohol_data$Sex)
alcohol_data$Drinking_level <- factor(alcohol_data$Drinking_level, levels = c("Low", "Moderate", "High"), ordered = TRUE)
alcohol_data$School <- factor(alcohol_data$School)
alcohol_data$Subject <- factor(alcohol_data$Subject)
alcohol_data$Mother_job <- factor(alcohol_data$Mother_job)
alcohol_data$Father_job <- factor(alcohol_data$Father_job)
alcohol_data$Parents_status <- factor(alcohol_data$Parents_status)
alcohol_data$Fam_size <- factor(alcohol_data$Fam_size)
alcohol_data$Private_lessons <- factor(alcohol_data$Private_lessons)
alcohol_data$Extra_act <- factor(alcohol_data$Extra_act)
alcohol_data$Continue_studies <- factor(alcohol_data$Continue_studies)
alcohol_data$Internet <- factor(alcohol_data$Internet)
alcohol_data$Relationship <- factor(alcohol_data$Relationship)

view(alcohol_data)

data_regr <- alcohol_data %>%
  dplyr::select("Sex":"WE_alc", "School":"Going_out")

view(data_regr)

binary_columns <- c("Private_lessons", "Internet", "Relationship", "Extra_act", "Continue_studies")
alcohol_data_with.dummy <- alcohol_data %>%
  mutate_at(vars(binary_columns), ~ifelse(. == "yes", 1, 0)) %>%
  mutate_at(vars("Sex"), ~ifelse(. == "M", 1, 0)) %>%
  mutate_at(vars("School"), ~ifelse(. == "Gabriel Pereira", 1, 0)) %>%
  mutate_at(vars("Subject"), ~ifelse(. == "Maths", 1, 0)) %>%
  mutate_at(vars("Parents_status"), ~ifelse(. == "Together", 1, 0)) %>%
  mutate_at(vars("Fam_size"), ~ifelse(. == "GT3", 1, 0)) %>%
  dplyr::select("Sex":"WE_alc", "School":"Health_status", "Mother_edu":"Going_out")

view(alcohol_data_with.dummy)


new_alcohol_data <- merged_data %>%
  dplyr::select("Sex", "Age", "WD_alc", "WE_alc", "Drinking_level", "School", "Subject",
                "Grades_t1", "Grades_t2",
                "Final_grade", "Class_failures", "Study_time", "Travel_time", "Absences", 
                "Health_status", "Mother_job", "Father_job", "Mother_edu", "Father_edu", 
                "Parents_status", "Fam_size", "Private_lessons", "Extra_act", "Continue_studies", 
                "Internet", "Relationship", "Family_rel", "Free_time", "Going_out")


new_alcohol_data$Sex <- factor(alcohol_data$Sex)
new_alcohol_data$Drinking_level <- factor(alcohol_data$Drinking_level, levels = c("Low", "Moderate", "High"), ordered = TRUE)
new_alcohol_data$School <- factor(alcohol_data$School)
new_alcohol_data$Subject <- factor(alcohol_data$Subject)
new_alcohol_data$Mother_job <- factor(alcohol_data$Mother_job)
new_alcohol_data$Father_job <- factor(alcohol_data$Father_job)
new_alcohol_data$Parents_status <- factor(alcohol_data$Parents_status)
new_alcohol_data$Fam_size <- factor(alcohol_data$Fam_size)
new_alcohol_data$Private_lessons <- factor(alcohol_data$Private_lessons)
new_alcohol_data$Extra_act <- factor(alcohol_data$Extra_act)
new_alcohol_data$Continue_studies <- factor(alcohol_data$Continue_studies)
new_alcohol_data$Internet <- factor(alcohol_data$Internet)
new_alcohol_data$Relationship <- factor(alcohol_data$Relationship)

view(new_alcohol_data)

new_data_regr <- new_alcohol_data %>%
  dplyr::select("Sex":"WE_alc", "School":"Going_out")

new_alcohol_data_with.dummy <- new_alcohol_data %>%
  mutate_at(vars(binary_columns), ~ifelse(. == "yes", 1, 0)) %>%
  mutate_at(vars("Sex"), ~ifelse(. == "M", 1, 0)) %>%
  mutate_at(vars("School"), ~ifelse(. == "Gabriel Pereira", 1, 0)) %>%
  mutate_at(vars("Subject"), ~ifelse(. == "Maths", 1, 0)) %>%
  mutate_at(vars("Parents_status"), ~ifelse(. == "Together", 1, 0)) %>%
  mutate_at(vars("Fam_size"), ~ifelse(. == "GT3", 1, 0)) %>%
  dplyr::select("Sex":"WE_alc", "School":"Health_status", "Mother_edu":"Going_out")

view(new_alcohol_data_with.dummy)



################### EXPLORING THE DATA #########################################


sex_alcohol_data <- alcohol_data %>%
  group_by(Sex, Drinking_level) %>%
  summarise(count = n()) %>%
  group_by(Sex) %>%
  mutate(percentage = count/sum(count) * 100) %>%
  mutate(Drinking_level = factor(Drinking_level, levels = c("Low", "Moderate", "High")))

ggplot(sex_alcohol_data, aes(x = Drinking_level, y = percentage, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Drinking level by gender\n", x = "\nDrinking level", y = "Percentage\n") +
  scale_fill_manual(values = c("pink", "blue"), name = "Sex") + 
  theme(axis.text = element_text(size=15), axis.title = element_text(size = 14))

ggplot(alcohol_data) + geom_histogram(aes(x = Age), binwidth = 1, fill = "darkgreen", col = "black") + 
  labs(x = "\nAge", y = "", title = "Distribution of students' age\n") +
  theme(axis.text = element_text(size=15), axis.title = element_text(size = 14))

numbers_boxplot <- alcohol_data %>%
  dplyr::select("WD_alc", "WE_alc", "Study_time":"Travel_time", "Health_status",
         "Mother_edu", "Father_edu", "Family_rel":"Going_out")

boxplot(numbers_boxplot)

correlation_matrix <- cor(alcohol_data_with.dummy)
corrplot(correlation_matrix, method = "color", type = "lower")
corr_cross(alcohol_data_with.dummy, top = 5, grid = TRUE, max_pvalue = 0.05)

corr_values <- round(cor(alcohol_data_with.dummy), 3)



############################ REGRESSION ########################################


set.seed(516)
regr_sampleSplit <- createDataPartition(data_regr$Final_grade, p = 0.75, list = FALSE)
regr_trainSet <- data_regr[regr_sampleSplit, ]
regr_testSet <- data_regr[-regr_sampleSplit, ]


linear_model <- lm(Final_grade ~ ., data = regr_trainSet)
linear_pred <- predict(linear_model, regr_testSet)
linear_mse <- sum(linear_pred - regr_testSet$Final_grade)^2 / length(linear_pred)
summary(linear_model)

vif(linear_model)
shapiro.test(linear_model$residuals)
ggqqplot(linear_model$residuals)
anova(linear_model)

res_fitted <- autoplot(linear_model, which = 1, ncol = 1)
res_lev <- autoplot(linear_model, which = 5, ncol = 1)

linear_eval <- cbind(regr_testSet$Final_grade, linear_pred)
colnames(linear_eval) <- c("Actual", "Predicted")
linear_eval <- as.data.frame(linear_eval)
head(linear_eval)
linear_rmse <- sqrt(linear_mse)

ggplot(linear_eval, aes(x = linear_pred, y = regr_testSet$Final_grade)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") + labs(title = "Linear Model - Predicted vs actual values", 
                                                            x = "Predicted value", y = "Actual value")


set.seed(516)
new_regr_sampleSplit <- createDataPartition(new_data_regr$Final_grade, p = 0.75, list = FALSE)
new_regr_trainSet <- new_data_regr[new_regr_sampleSplit, ]
new_regr_testSet <- new_data_regr[-new_regr_sampleSplit, ]


new_linear_model <- lm(Final_grade ~ ., data = new_regr_trainSet)
new_linear_pred <- predict(new_linear_model, new_regr_testSet)
new_linear_mse <- sum(new_linear_pred - new_regr_testSet$Final_grade)^2 / length(new_linear_pred)
summary(new_linear_model)

vif(new_linear_model)
shapiro.test(new_linear_model$residuals)
ggqqplot(new_linear_model$residuals)
anova(new_linear_model)
dwtest(new_linear_model)

new_res_fitted <- autoplot(new_linear_model, which = 1, ncol = 1)
new_res_lev <- autoplot(new_linear_model, which = 5, ncol = 1)

new_linear_eval <- cbind(new_regr_testSet$Final_grade, new_linear_pred)
colnames(new_linear_eval) <- c("Actual", "Predicted")
new_linear_eval <- as.data.frame(new_linear_eval)
head(new_linear_eval)
new_linear_rmse <- sqrt(new_linear_mse)

ggplot(new_linear_eval, aes(x = new_linear_pred, y = new_regr_testSet$Final_grade)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") + labs(title = "Linear Model - Predicted vs actual values", 
                                                            x = "Predicted value", y = "Actual value")


cv_train <- model.matrix(~ . - 1, data = regr_trainSet)
cv_test <- model.matrix(~ . - 1, data = regr_testSet)
cv_y <- regr_trainSet$Final_grade
ridge_model <- glmnet(cv_train, cv_y, alpha = 0)
lasso_model <- glmnet(cv_train, cv_y, alpha = 1)
cv_ridge <- cv.glmnet(cv_train, cv_y, alpha = 0)
cv_lasso <- cv.glmnet(cv_train, cv_y, alpha = 1)

plot(cv_ridge, main = "Ridge\n\n")
plot(cv_lasso, main = "Lasso\n\n")

ridge_lambda <- cv_ridge$lambda.min
lasso_lambda <- cv_lasso$lambda.min
ridge_model_optimal <- glmnet(cv_train, cv_y, alpha = 0, lambda = ridge_lambda)
lasso_model_optimal <- glmnet(cv_train, cv_y, alpha = 1, lambda = lasso_lambda)
ridge_pred <- predict(ridge_model_optimal, s = ridge_lambda, newx = cv_test)
lasso_pred <- predict(lasso_model_optimal, s = lasso_lambda, newx = cv_test)
ridge_mse <- mean((ridge_pred - regr_testSet$Final_grade)^2)
lasso_mse <- mean((lasso_pred - regr_testSet$Final_grade)^2)
ridge_rmse <- sqrt(ridge_mse)
lasso_rmse <- sqrt(lasso_mse)
ridge_r_squared <- 1 - (sum((regr_testSet$Final_grade - ridge_pred)^2) / sum((regr_testSet$Final_grade - mean(regr_testSet$Final_grade))^2))
lasso_r_squared <- 1 - (sum((regr_testSet$Final_grade - lasso_pred)^2) / sum((regr_testSet$Final_grade - mean(regr_testSet$Final_grade))^2))

cv_results <- data.frame(regr_testSet$Final_grade, ridge_pred, lasso_pred)

ggplot(cv_results, aes(x = regr_testSet.Final_grade, y = s1)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "blue") + 
  labs(title = "Ridge predicted vs actual values", x = "Predicted value", y = "Actual value")
ggplot(cv_results, aes(x = regr_testSet.Final_grade, y = s1.1)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "brown") + 
  labs(title = "Lasso predicted vs actual values", x = "Predicted value", y = "Actual value")


data_regr.school <- data_regr %>%
  dplyr::select("Final_grade", "School", "Subject", "Class_failures", "Study_time", 
         "Travel_time", "Absences", "Private_lessons", "Extra_act", "Continue_studies")
data_regr.social <- data_regr %>%
  dplyr::select("Final_grade", "Sex", "Age", "WD_alc", "WE_alc", "Health_status",
                "Mother_edu", "Father_edu", "Parents_status", "Fam_size", "Internet",
                "Relationship", "Family_rel", "Free_time", "Going_out")

school_sampleSplit <- createDataPartition(data_regr.school$Final_grade, p = 0.75, list = FALSE)
school_trainSet <- data_regr.school[school_sampleSplit, ]
school_testSet <-  data_regr.school[-school_sampleSplit, ]
social_sampleSplit <- createDataPartition(data_regr.social$Final_grade, p = 0.75, list = FALSE)
social_trainSet <- data_regr.social[social_sampleSplit, ]
social_testSet <- data_regr.social[-social_sampleSplit, ]

linear_model.school <- lm(Final_grade ~ ., data = school_trainSet) %>%
  stepAIC(direction = "both", trace = FALSE)
linear_model.social <- lm(Final_grade ~ ., data = social_trainSet) %>%
  stepAIC(direction = "both", trace = FALSE)
linear_school.pred <- predict(linear_model.school, school_testSet)
linear_social.pred <- predict(linear_model.social, social_testSet)

school_mallows <- ols_mallows_cp(linear_model.school, linear_model)
social_mallows <- ols_mallows_cp(linear_model.social, linear_model)

lm_names <- c("Social", "School")
mymodels <- list(linear_model.social, linear_model.school)
aictab(mymodels, lm_names)


regr_sampleSplit_dummy <- createDataPartition(alcohol_data_with.dummy$Final_grade, p = 0.75, list = FALSE)
regr_trainSet_dummy <- alcohol_data_with.dummy[regr_sampleSplit, ]
regr_testSet_dummy <- alcohol_data_with.dummy[-regr_sampleSplit, ]

grid_tune <- expand.grid(nrounds = 100, max_depth = 4, eta = 0.01, gamma = 1,
  colsample_bytree = 0.4, min_child_weight = 5, subsample = 0.5)
control <- trainControl(method = "cv", number = 5)
cv_model <- train(x = regr_trainSet_dummy[, -ncol(regr_trainSet_dummy)], y = regr_trainSet_dummy[, "Final_grade"], 
                  method = "xgbTree", trControl = control, tuneGrid = grid_tune)
xgb_pred <- predict(cv_model, regr_testSet_dummy)
xgb_mse <- sum(xgb_pred - regr_testSet_dummy$Final_grade)^2 / length(xgb_pred)
xgb_r_squared <- cor(xgb_pred, regr_testSet_dummy$Final_grade)^2
print(cv_model)

gbm_model <- gbm(formula = formula("Final_grade ~ ."), data = regr_trainSet, 
                 distribution = "gaussian", n.trees = 2000, interaction.depth = 3,
                 shrinkage = 0.01, cv.folds = 10, n.cores = NULL)
print(gbm_model)
gbm.perf(gbm_model, method = "cv")
summary(gbm_model, method = permutation.test.gbm)

new_regr_sampleSplit_dummy <- createDataPartition(new_alcohol_data_with.dummy$Final_grade, p = 0.75, list = FALSE)
new_regr_trainSet_dummy <- new_alcohol_data_with.dummy[new_regr_sampleSplit, ]
new_regr_testSet_dummy <- new_alcohol_data_with.dummy[-new_regr_sampleSplit, ]

new_grid_tune <- expand.grid(nrounds = 100, max_depth = 4, eta = 0.01, gamma = 1,
                             colsample_bytree = 0.4, min_child_weight = 5, subsample = 0.5)
new_control <- trainControl(method = "cv", number = 5)
new_cv_model <- train(x = new_regr_trainSet_dummy[, -ncol(new_regr_trainSet_dummy)], y = new_regr_trainSet_dummy[, "Final_grade"], 
                      method = "xgbTree", trControl = new_control, tuneGrid = new_grid_tune)
new_xgb_pred <- predict(new_cv_model, new_regr_testSet_dummy)
new_xgb_mse <- sum(new_xgb_pred - new_regr_testSet_dummy$Final_grade)^2 / length(new_xgb_pred)
new_xgb_r_squared <- cor(new_xgb_pred, new_regr_testSet_dummy$Final_grade)^2
print(new_cv_model)

new_gbm_model <- gbm(formula = formula("Final_grade ~ ."), data = new_regr_trainSet, 
                     distribution = "gaussian", n.trees = 2000, interaction.depth = 3,
                     shrinkage = 0.01, cv.folds = 10, n.cores = NULL)
print(new_gbm_model)
gbm.perf(new_gbm_model, method = "cv")
summary(new_gbm_model, method = permutation.test.gbm)


rf_regr_model <- randomForest(formula = Final_grade ~ ., data = regr_trainSet, 
                              importance = TRUE, ntree = 2812)
plot(rf_regr_model)
varImpPlot(rf_regr_model)
rf_regr_pred <- predict(rf_regr_model, regr_testSet)
rf_eval <- cbind(regr_testSet$Final_grade, rf_regr_pred)
colnames(rf_eval) <- c("Actual", "Predicted")
rf_eval <- as.data.frame(rf_eval)
head(rf_eval)

ggplot(rf_eval, aes(x = rf_regr_pred, y = regr_testSet$Final_grade)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "darkgreen") + labs(title = "Random Forest - Predicted vs actual values", 
                                                                  x = "Predicted value", y = "Actual value")


new_rf_regr_model <- randomForest(formula = Final_grade ~ ., data = new_regr_trainSet, 
                                  importance = TRUE, ntree = 2812)

new_rf_regr_pred <- predict(new_rf_regr_model, new_regr_testSet)
new_rf_eval <- cbind(new_regr_testSet$Final_grade, new_rf_regr_pred)
colnames(new_rf_eval) <- c("Actual", "Predicted")
new_rf_eval <- as.data.frame(new_rf_eval)
head(new_rf_eval)

ggplot(new_rf_eval, aes(x = new_rf_regr_pred, y = new_regr_testSet$Final_grade)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "darkgreen") + labs(title = "Random Forest - Predicted vs actual values", 
                                                                  x = "Predicted value", y = "Actual value")


#################### CLASSIFICATION ############################################


data_class <- alcohol_data %>%
  dplyr::select("Sex":"Age", "Drinking_level":"Going_out")

view(data_class)

set.seed(782)
class_trainIndex <- createDataPartition(data_class$Drinking_level, p = 0.8,
                                  list = FALSE, times = 1)
class_train <- data_class[class_trainIndex,]
class_test  <- data_class[-class_trainIndex,]

clsf.models <- c("Random Forest", "Recursive Partitioning", 
                 "Linear Discriminant Analysis", "K-Nearest Neighbors")

model.rf <- train(Drinking_level ~ ., data = class_train, method = "rf")
model.rpart <- train(Drinking_level ~ ., data = class_train, method = "rpart")
model.lda <- train(Drinking_level ~ ., data = class_train, method = "lda")
model.knn <- train(Drinking_level ~ ., data = class_train, method = "knn")

prediction.rf <- predict(model.rf, class_test)
prediction.rpart <- predict(model.rpart, class_test)
prediction.lda <- predict(model.lda, class_test)
prediction.knn <- predict(model.knn, class_test)

rf.matrix <- confusionMatrix(prediction.rf, as.factor(class_test$Drinking_level))
rpart.matrix <- confusionMatrix(prediction.rpart, as.factor(class_test$Drinking_level))
lda.matrix <- confusionMatrix(prediction.lda, as.factor(class_test$Drinking_level))
knn.matrix <- confusionMatrix(prediction.knn, as.factor(class_test$Drinking_level))

rf.accuracy <- rf.matrix$overall["Accuracy"]
rpart.accuracy <- rpart.matrix$overall["Accuracy"]
lda.accuracy <- lda.matrix$overall["Accuracy"]
knn.accuracy <- knn.matrix$overall["Accuracy"]

accuracy_table <- data.frame(
  Model = clsf.models,
  Accuracy = c(rf.accuracy, rpart.accuracy, lda.accuracy, knn.accuracy)) %>%
  arrange(desc(Accuracy))
accuracy_table

color_scheme <- brewer.pal(n = 3, name = "YlOrRd")
rf.plot <- plot(rf.matrix$table, main = "Random Forest", col = color_scheme)
rpart.plot <- plot(rpart.matrix$table, main = "Recursive Partitioning", col = color_scheme)
lda.plot <- plot(lda.matrix$table, main = "Linear Discriminant Analysis", col = color_scheme)
knn.plot <- plot(knn.matrix$table, main = "K-Nearest Neighbors", col = color_scheme)



############# CLASSIFICATION WITH OVERSAMPLING #################################


set.seed(442)
oversampled_data_class <- smote(Drinking_level~ ., data_class, k = 5, perc.over = 10, perc.under = 1.5)
table(oversampled_data_class$Drinking_level)

oversample_trainIndex <- createDataPartition(oversampled_data_class$Drinking_level, p = 0.8,
                                  list = FALSE, times = 1)

oversample_data_train <- oversampled_data_class[oversample_trainIndex,]
oversample_data_test  <- oversampled_data_class[-oversample_trainIndex,]

os.model.rf <- train(Drinking_level ~ ., data = oversample_data_train, method = "rf")
os.model.lda <- train(Drinking_level ~ ., data = oversample_data_train, method = "lda")
os.model.rpart <- train(Drinking_level ~ ., data = oversample_data_train, method = "rpart")
os.model.knn <- train(Drinking_level ~ ., data = oversample_data_train, method = "knn")

os.prediction.rf <- predict(os.model.rf, oversample_data_test)
os.prediction.lda <- predict(os.model.lda, oversample_data_test)
os.prediction.rpart <- predict(os.model.rpart, oversample_data_test)
os.prediction.knn <- predict(os.model.knn, oversample_data_test)

os.rf.matrix <- confusionMatrix(os.prediction.rf, as.factor(oversample_data_test$Drinking_level))
os.lda.matrix <- confusionMatrix(os.prediction.lda, as.factor(oversample_data_test$Drinking_level))
os.rpart.matrix <- confusionMatrix(os.prediction.rpart, as.factor(oversample_data_test$Drinking_level))
os.knn.matrix <- confusionMatrix(os.prediction.knn, as.factor(oversample_data_test$Drinking_level))

os.rf.accuracy <- os.rf.matrix$overall["Accuracy"]
os.lda.accuracy <- os.lda.matrix$overall["Accuracy"]
os.rpart.accuracy <- os.rpart.matrix$overall["Accuracy"]
os.knn.accuracy <- os.knn.matrix$overall["Accuracy"]

os.accuracy_table <- data.frame(
  Model = clsf.models,
  Accuracy = c(os.rf.accuracy, os.lda.accuracy, os.rpart.accuracy, os.knn.accuracy)) %>%
  arrange(desc(Accuracy))
os.accuracy_table


merged_accuracy <- merge(os.accuracy_table, accuracy_table, by = "Model", suffixes = c("_os", "_original"))
merged_accuracy$Accuracy_Difference <- merged_accuracy$Accuracy_os - merged_accuracy$Accuracy_original
accuracy_difference <- data.frame(
  Model = merged_accuracy$Model,
  Accuracy_Difference_Percent = merged_accuracy$Accuracy_Difference*100) %>%
  arrange(desc(Accuracy_Difference_Percent))
accuracy_difference

color_scheme <- brewer.pal(n = 3, name = "YlOrRd")

os.rf.plot <- plot(os.rf.matrix$table, main = "Random Forest", col = color_scheme)
os.lda.plot <- plot(os.lda.matrix$table, main = "Linear Discriminant Analysis", col = color_scheme)
os.rpart.plot <- plot(os.rpart.matrix$table, main = "Recursive Partitioning", col = color_scheme)
os.knn.plot <- plot(os.knn.matrix$table, main = "K-Nearest Neighbors", col = color_scheme)

rf.vimp <- varImp(os.model.rf)
plot(rf.vimp, main = "Variable importance in Random Forest classification")
rf.vimp



##################### DECISION TREES ###########################################


normal_tree <- tree(Drinking_level ~ ., data = data_class)
summary(normal_tree)
plot(normal_tree)
text(normal_tree, pretty = 0)
title("Decision tree with the original data")

os.normal_tree <- tree(Drinking_level ~ ., data = oversampled_data_class)
summary(os.normal_tree)
plot(os.normal_tree)
text(os.normal_tree, pretty = 0)
title("Decision tree with the oversampled data")

best_nodes <- cv.tree(normal_tree, FUN = prune.misclass)
best_nodes.results <- data.frame(size = best_nodes$size, dev = best_nodes$dev)
ggplot(best_nodes.results, aes(x = size, y = dev)) + geom_line() + geom_point() +
  labs(x = "\nNodes", y = "Cross-validation error\n", title = "Cross-validation Error vs. Tree Size\n")

prune.alcohol <- prune.misclass(normal_tree, best = 3)
plot(prune.alcohol, type = "u")
text(prune.alcohol, pretty = 0)      
title("Decision tree with optimal number of nodes (3)\n")

normal_rpart.tree <- rpart(Drinking_level ~ ., data = data_class, method = 'class')
over_rpart.tree <- rpart(Drinking_level ~ ., data = oversampled_data_class, method = 'class')
rpart.plot(normal_rpart.tree, extra = "auto")
rpart.plot(over_rpart.tree, extra = "auto")
