#PSYP14 Subcourse 1 Home Assignments 

#########################################
#Assignment 1

#Firstly, load the data file 

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
view(data_sample_1)

original_data <- data_sample_1 
original_data

#Install needed packages and upload into library

install.packages(lmtest)
install.packages(lm.beta)
install.packages(sandwich)
install.packages(boot)
install.packages(lmboot)

library(psych)
library(car)
library(lmtest)
library(sandwich)
library(boot)
library(lmboot)
library(lm.beta)
library(gridExtra)
library(tidyverse)

#Investigate data and do basic descriptive statistics do understand data better

view(original_data) 
str(original_data) #Have a look at the structure (class of objects, the number of rows and columns, type of each variable)
describe(original_data)
summary/original_data) 

#Build model 1

model_1 <- lm(pain ~ age + sex, data = data_sample_1)

#Do explorative analysis through visualization (scatterplot) and identify extreme cases 

original_data %>% 
  mutate(rownum = row.names(original_data)) %>% 
  ggplot() + aes(x = pain, y = STAI_trait, label = rownum) + 
  geom_point() + geom_text()

#For better visualisation

original_data %>% 
  mutate(rownum = row.names(original_data)) %>% 
  ggplot() + aes(x = pain, y = STAI_trait, label = rownum) + 
  geom_label()

#Participant 34 rated STAI_trait 4.2. but the State Trait Anxiety Inventors scale is from 20 to 80
#Participant 88 rated his pain 55 but pain scale is from 0 to 10
#Participant 65 is not too extreme and will not be corrected or excluded

#Want to correct and not exclude the two outliers (participant 34 and 88) to not lose any important data

data_sample_1$pain <- as.numeric(replace(data_sample_1$pain, data_sample_1$pain == 55, 5))
View(data_sample_1) #function only works if I use data_sample_1 and not original_data

data_sample_1$STAI_trait <- as.numeric(replace(data_sample_1$STAI_trait, data_sample_1$STAI_trait == 4.2, 42.0))
view(data_sample_1)



#Assumption of linear regression
#Normality

#QQ-Plot

model_1 %>% 
  plot(which = 2)

#Histogram

residuals_data_sample_2 = enframe(residuals(model_1))

residuals_data_sample_2 %>% 
  ggplot() + aes(x = value) + geom_histogram()

#Skew and Kurtosis

describe(residuals(model_1)) #Values are between -1 and 1

#Result: normality is not violated

#Cook's distance

model_1 %>% 
  plot(which = 4) #under 1.0

#Linearity

model_1 %>% 
  residualPlots() 

#Homogeneity of variance

model_1 %>% 
  plot(which = 3) 

#Multicollinearity

model_1 %>% 
  vif() #good, below 3

#result: the assumptions of linear regression are hold true



#Building model 2

model_2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_1)

#Compare model 1 and 2

summary(model_1)$adj.r.squared

summary(model_2)$adj.r.squared 

#Does not show if difference is significant. Therefore, perform an AIC

AIC(model_1) 

AIC(model_2)

#Result: model 2 has a lower AIC score and, thus, is the better model (less error and better model fit)

#Check for coding errors, influential outliers and investigate assumptions of linear regression

summary(model_2)

#Normality
#QQ-Plot

model_2 %>% 
  plot(which = 2)

#Histogram

residuals_data_sample_3 = enframe(residuals(model_2))

residuals_data_sample_3 %>% 
  ggplot() + aes(x = value) + geom_histogram()

#Skew and kurtosis

describe(residuals(model_2)) 

#Cook's distance

model_2 %>% 
  plot(which = 4) 

#Linearity

model_2 %>% 
  residualPlots() #non are significant; linear relationship

#Homogeneity of variance

model_2 %>% 
  plot(which = 3) 

#Multicollinearity

model_2 %>% 
  vif()

#Result: cortisol_serum and cortisol_saliva are above 3 so these values are problematic
#Solution: create new model 3 and do AIC to decide which of the two variables to keep

model_3 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1)

#Do comparison to look at variance

AIC(model_2, model_3)

#Models are significantly different (< 2 points apart)
#Result: Model 2 has lower AIC score but Model 3 is based on previous research. Therefore, exclude cortisol_saliva. 

#Because I am using Model 3, I have to run all assumptions of linear regression again 
#Normality
#QQ-Plot

model_3 %>% 
  plot(which = 2)

#Histogram

residuals_data_sample_4 = enframe(residuals(model_3))

residuals_data_sample_4 %>% 
  ggplot() + aes(x = value) + geom_histogram()

#Skew and kurtosis

describe(residuals(model_3)) #good, the values are between -1 and 1

#Cook's distance

model_3 %>% 
  plot(which = 4) #under 1.0

#Linearity

model_3 %>% 
  residualPlots() 

#Homogeneity of variance

model_3 %>% 
  plot(which = 3) 

#Multicollinearity

model_3 %>% 
  vif() #now below 3

#Result: the assumptions of linear regression are hold true



#Report results of both models

summary(model_1) 

summary(model_3) 

#standardised beta of model 1 and 3 

lm.beta(model_1)
lm.beta(model_3) 

#non standardised beta of model 1 and 3

summary(model_1)
summary(model_3) 

#Coefficients of model 1 and 3

confint(model_1)
confint(model_3)



#Regression equation (with undstandardised intercept)

Y = 1.41 - 0.04*age + 0.16*sexmale - 0.01*STRAI_trait + 0.11*pain_cat - 0.28*mindfulness + 0.57*cortisol_serum

#Compare model 1 and 3

AIC(model_1) 
AIC(model_3)
AIC(model_1, model_3)

anova(model_1) 
anova(model_3) 
anova(model_1, model_3)



#########################################
#Assignment 2

#Need to correct outliers of participant 34 and 88 again

data_sample_1$pain <- as.numeric(replace(data_sample_1$pain, data_sample_1$pain == 55, 5))
View(data_sample_1)

data_sample_1$STAI_trait <- as.numeric(replace(data_sample_1$STAI_trait, data_sample_1$STAI_trait == 4.2, 42.0))
view(data_sample_1)

#Create new model 5 (based on the researcher's comments), which is the initial model

initial_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_sample_1)

#Descriptive statistics

view(data_sample_1)
str(data_sample_1)
describe(data_sample_1)
summary(data_sample_1)

#Result: No significant errors or outliers



#Explorative analysis

data_sample_1 %>% 	
  ggplot() +	
  aes(x = STAI_trait) +	
  geom_histogram() #warning message about bins=30

data_sample_1 %>% 	
  ggplot() +	
  aes(x = STAI_trait) +	
  geom_histogram(bins=30)

data_sample_1 %>% 	
  ggplot() +	
  aes(x = pain_cat) +	
  geom_histogram(bins=30)

data_sample_1 %>% 	
  ggplot() +	
  aes(x = mindfulness) +	
  geom_histogram(bins=30)

data_sample_1 %>% 	
  ggplot() +	
  aes(x = cortisol_serum) +	
  geom_histogram(bins=30)

data_sample_1 %>% 	
  ggplot() +	
  aes(x = weight) +	
  geom_histogram(bins=30)

data_sample_1 %>% 	
  ggplot() +	
  aes(x = IQ) +	
  geom_histogram(bins=30)

data_sample_1 %>% 	
  ggplot() +	
  aes(x = household_income) +	
  geom_histogram(bins=30)

#Cook's distance

model_5 %>% 
  plot(which = 4) 

#Assumption of linear regression
#Normality
model_5 %>% 
  plot(which = 2)

residuals_data_sample_1 = enframe(residuals(model_5))

residuals_data_sample_1 %>% 
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model_5)) 

#Linearity

model_5 %>% 
  residualPlots() 

#Homogeneity of variance

model_5 %>% 
  plot(which = 3)

#Multicollinearity

model_5 %>% 
  vif()

#Result: no violations



#Backward regression

back_reg_initial = step(initial_model, direction = "backward") 

#Result of backward regression: stick to the 4 variables age, mindfulness, cortisol_serum and pain_cat

#Build final model 6

backward_model <- lm(pain ~ age + mindfulness + cortisol_serum + pain_cat, data = data_sample_1)

model_3 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1)

theory_based_model <- model_3 
theory_based_model

#Compare both models 3 and 6

AIC(model_3) 
AIC(backward_model) 
AIC(model_3, backward_model)

AIC(initial_model)

#Result: no significant difference


#Load data file with new sample

home_sample_2 = read.csv("https://tinyurl.com/87v6emky")
home_sample_2

#Explore data set

view(home_sample_2)
describe(home_sample_2)
summary(home_sample_2)

home_sample_2 %>% 	
  ggplot() +	
  aes(x = pain) +	
  geom_histogram(bins = 30)

home_sample_2 %>% 	
  ggplot() +	
  aes(x = age) +	
  geom_histogram(bins = 30)

home_sample_2 %>% 	
  ggplot() +	
  aes(x = STAI_trait) +	
  geom_histogram(bins = 30)

home_sample_2 %>% 	
  ggplot() +	
  aes(x = pain_cat) +	
  geom_histogram(bins = 30)

home_sample_2 %>% 	
  ggplot() +	
  aes(x = cortisol_serum) +	
  geom_histogram(bins = 30)

home_sample_2 %>% 	
  ggplot() +	
  aes(x = cortisol_saliva) +	
  geom_histogram(bins = 30)

home_sample_2 %>% 	
  ggplot() +	
  aes(x = mindfulness) +	
  geom_histogram(bins = 30)

home_sample_2 %>% 	
  ggplot() +	
  aes(x = weight) +	
  geom_histogram(bins = 30)

home_sample_2 %>% 	
  ggplot() +	
  aes(x = IQ) +	
  geom_histogram(bins = 30)

home_sample_2 %>% 	
  ggplot() +	
  aes(x = household_income) +	
  geom_histogram(bins = 30)

#Result: no further errors in data

#Make predictions on pain - use regression equation from data file 1 to predict pain on data file 2
#Calculate predicted values

theory_based_model_predicted = predict(theory_based_model, home_sample_2)
theory_based_model_predicted

backward_model_predicted = predict(backward_model, home_sample_2)
backward_model_predicted

#Compare predicted values with actual pain ratings
#Calculate sum of squared residuals between predicted and actual pain

RSS_theory = sum((home_sample_2$pain - theory_based_model_predicted) ^2)
RSS_theory 

RSS_backward = sum((home_sample_2$pain - backward_model_predicted) ^2)
RSS_backward 

RSS_backward - RSS_theory 

#Or calculate the sum of absolute differences for each model

RAD_theory = sum(abs(home_sample_2$pain - theory_based_model_predicted))
RAD_theory 

RAD_backwards = sum(abs(home_sample_2$pain - backward_model_predicted))
RAD_backwards 

#So, which model better predicts pain in data file 2?
#Result: both analyses show that the backward model has more error 


#For report
#Standardised beta for backward regression model 

lm.beta(backward_model) 

#Non-standardised beta 

summary(backward_model) 
summary(theory_based_model)

#Coefficients

confint(backward_model)



#########################################
#Assignment 3

#Load data files

data_file_3 = read.csv("https://tinyurl.com/b385chpu")
data_file_4 = read.csv("https://tinyurl.com/4f8thztv")

#Load needed packages

library(psych) 
install.packages("r2glmm")
library(r2glmm) 
install.packages("lme4")
library(lme4) 
install.packages("lmerTest")
library(lmerTest) 
install.packages("cAIC4")
library(cAIC4)
install.packages("MuMIn")
library(MuMIn)
library(tidyverse)

#Do basic descriptives

View(data_file_3)
describe(data_file_3)

data_file_3 %>% 	
  ggplot() +	
  aes(x = pain) +	
  geom_histogram(bins=30)

data_file_3 %>% 	
  ggplot() +	
  aes(x = age) +	
  geom_histogram(bins=30)

data_file_3 %>% 	
  ggplot() +	
  aes(x = STAI_trait) +	
  geom_histogram(bins=30)

data_file_3 %>% 	
  ggplot() +	
  aes(x = pain_cat) +	
  geom_histogram(bins=30)

data_file_3 %>% 	
  ggplot() +	
  aes(x = cortisol_serum) +	
  geom_histogram(bins=30)

data_file_3 %>% 	
  ggplot() +	
  aes(x = cortisol_saliva) +	
  geom_histogram(bins=30)

data_file_3 %>% 	
  ggplot() +	
  aes(x = mindfulness) +	
  geom_histogram(bins=30)

data_file_3 %>% 	
  ggplot() +	
  aes(x = weight) +	
  geom_histogram(bins=30)

data_file_3 %>% 	
  ggplot() +	
  aes(x = IQ) +	
  geom_histogram(bins=30)

data_file_3 %>% 	
  ggplot() +	
  aes(x = household_income) +	
  geom_histogram(bins=30)

#Explore data for errors: min. household_income of participant 2 is negative (-7884) - exclude participant
#Participant 25 stated woman instead of female

data_file_3_final <- data_file_3 %>% 
  slice(-c(2)) %>% 
  mutate(sex = replace(sex, sex=="woman", "female"))

view(data_file_3_final)

#Assign hospital as grouping factor

data_file_3_final_factor <- data_file_3_final %>%
  mutate(hospital = factor(hospital))

#Build linear mixed model on data file 3, including hospital ID and predictors of the model of assignment 1

random_int_mod3 = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1 | hospital), data = data_file_3_final_factor)
random_int_mod3

summary(random_int_mod3)

#Note the model coefficients and the confidence intervals of the coefficients for all fixed effect predictors

summary(random_int_mod3) 
confint(random_int_mod3)

#Compare them to the ones obtained in assignment part 1 
#Note: model 3 was assigned to theory-based model in assignment 2

model_3 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1)

theory_based_model <- model_3 
theory_based_model

#Compute variance explained by fixed effect predictors using marginal R2

r2beta(random_int_mod3, method = "nsj", data = data_file_3_final_factor)

#Compute the variance explained by the fixed and random effect terms combined using conditional R2 

r.squaredGLMM(random_int_mod3)

#Calculate standardised beta

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}

stdCoef.merMod(random_int_mod3)

#Use regression equation obtained on data file 3 to predict pain in data file 4
#First, include file 4 and explore

data_file_4 = read.csv("https://tinyurl.com/4f8thztv")

view(data_file_4)
describe(data_file_4)

data_file_4 %>% 	
  ggplot() +	
  aes(x = pain) +	
  geom_histogram(bins=30)

data_file_4 %>% 	
  ggplot() +	
  aes(x = age) +	
  geom_histogram(bins=30)

data_file_4 %>% 	
  ggplot() +	
  aes(x = STAI_trait) +	
  geom_histogram(bins=30)

data_file_4 %>% 	
  ggplot() +	
  aes(x = pain_cat) +	
  geom_histogram(bins=30)

data_file_4 %>% 	
  ggplot() +	
  aes(x = cortisol_serum) +	
  geom_histogram(bins=30)

data_file_4 %>% 	
  ggplot() +	
  aes(x = cortisol_saliva) +	
  geom_histogram(bins=30)

data_file_4 %>% 	
  ggplot() +	
  aes(x = mindfulness) +	
  geom_histogram(bins=30)

data_file_4 %>% 	
  ggplot() +	
  aes(x = weight) +	
  geom_histogram(bins=30)

data_file_4 %>% 	
  ggplot() +	
  aes(x = IQ) +	
  geom_histogram(bins=30)

data_file_4 %>% 	
  ggplot() +	
  aes(x = household_income) +	
  geom_histogram(bins=30)

#Result: no further errors found

#Assign hospital as grouping factor

data_file_4_factor <- data_file_4 %>% 
  mutate(hospital = factor(hospital))

#Use regression equation obtained on data file 3 to predict pain in data file 4

prediction_pain <- predict(random_int_mod3, data_file_4_factor, allow.new.levels = TRUE)
prediction_pain

#Compute the variance explained by the model on data file 4 using 1-(RSS/TSS)

RSS_4 = sum((data_file_4_factor$pain - prediction_pain)^2)
RSS_4

mod_mean_4 <- lm(pain ~ 1, data = data_file_4_factor)
mod_mean_4

TSS_4 = sum((data_file_4_factor$pain - predict(mod_mean_4))^2)
TSS_4 

R2_4 <- (1-RSS_4/TSS_4)
R2_4

#Compare this R2 to the marginal and conditional R2 values computed for the model on data file 3 

#Build a new linear mixed effects model on data set 3 predicting pain 
#Allow for both random intercept and random slope 
#Most influential predictor is cortisol serum (looked at highest coefficient of marginal R2)

random_slope_mod = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = data_file_3_final_factor)
random_slope_mod

intercept_mod_new = lmer(pain ~ cortisol_serum + (1|hospital), data = data_file_3_final_factor)
intercept_mod_new

#Visualize the fitted regression lines for each hospital separately 

data_file_3_final_factor = data_file_3_final_factor %>% 
  mutate(pred_int_2 = predict(intercept_mod_new))

data_file_3_final_factor %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
                                                          aes(y = pred_int_2, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)

data_file_3_final_factor_cor = data_file_3_final_factor %>% 
  mutate(hospital = factor(hospital, levels = c(
    "hospital_1",
    "hospital_2",
    "hospital_3",
    "hospital_4",
    "hospital_5",
    "hospital_6",
    "hospital_7",
    "hospital_8",
    "hospital_9",
    "hospital_10")))

data_file_3_final_factor_cor %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
                                                          aes(y = pred_int_2, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)
























