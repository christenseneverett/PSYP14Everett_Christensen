library(psych)
library(dplyr)
library(lmtest)
library(lmboot)
library(sandwich)
library(boot)
library(lm.beta)
library(tidyverse)

#custom function from exercise 12
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}

#All data files
data_sample_1 <- read.csv("https://tinyurl.com/yxm5rd89") #in an initial summary of data_sample_1, participant 88 rated his pain as "55", which is an impossible variable. It is potentially reasonable to assume that the participant meant to record "5". This feels safe because this is the median value. participant 88 also recorded a pain_cat score essentially at the median.participant 34 also redorded an impossible value which was similarly changed. 
data_sample_2 <- read.csv("https://tinyurl.com/87v6emky")
data_sample_3 <- read.csv("https://tinyurl.com/b385chpu") #one incorrect entry for sex: "woman" instead of "female". negative income was kept.
data_sample_4 <- read.csv("https://tinyurl.com/4f8thztv")


### ASSIGNMENT PART 1 ###

#recoding impossible entry for participant 88 (55 to 5)
data_sample_1$pain <- as.numeric(replace(data_sample_1$pain, 88, 5))

#in addition, participant 34's is recoded from 4.2 to 42.0 
data_sample_1$STAI_trait <- as.numeric(replace(data_sample_1$STAI_trait, data_sample_1$STAI_trait == 4.2, 42))

#model1: containing age and sex as predictors of pain 
model_1 <- lm(pain ~ sex + age, data = data_sample_1)
summary(model_1)
#model2: age, sex, STAI, pain catastrophizing, mindfulness, and cortisol measures predicting pain
model_2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_1)
summary(model_2)

#comparing the two models reveals that model 2 is the preferable model in both tests
AIC(model_1, model_2)
anova(model_1,model_2)

###The following code checks the data and model_2 for errors

#cook's distance
model_2 %>% 
  plot(which = 4)

#Q-Q plot
model_2 %>% 
  plot(which = 2)

#histogram
residuals_model_2 = enframe(residuals(model_2))
residuals_model_2 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()

#lookin at the skew and kurtosis
describe(residuals(model_2))

#Looking at linearity
model_2 %>% 
  residualPlots()
residualPlots

#looking at homoscedasiticiticisy
model_2 %>% 
  plot(which = 3)

#looking at multicoliniarity. !!! the two hormonal measures show high multicolinearity !!!
model_2 %>% 
  vif()

# the theory-based model drops cortisol_saliva to reduce multicolinearity
Theory_based_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1)

#checking Theory_based_model in a similar fashion as model_2

#cook's distance
Theory_based_model %>% 
  plot(which = 4)
#Q-Q plot
Theory_based_model %>% 
  plot(which = 2)
#histogram
residuals_Theory_based_model = enframe(residuals(model_3))
residuals_Theory_based_model %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()
#lookin at the skew and kurtosis
describe(residuals(Theory_based_model))
#Looking at linearity
Theory_based_model %>% 
  residualPlots()
#looking at homoscedasiticiticisy
Theory_based_model %>% 
  plot(which = 3)
#looking at multicolinearity
Theory_based_model %>% 
  vif()

# Reporting coefficients (R2, F, df, and p value)

#####m MODEL 1 ######
summary(model_1)
lm.beta(model_1)
coef_table(model_1)

######### MODEL 3 ##########
summary(Theory_based_model)
lm.beta(Theory_based_model)
coef_table(Theory_based_model)

####FINAL COMPARISON###
AIC(model_1, Theory_based_model)
anova(model_1, Theory_based_model)


####################################### ASSIGNMENT PART 2 ############################################

#Initial model (before bacward regression)
initial_model <- lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_sample_1)
summary(initial_model)

#once again running model diagnostics

#cook's distance
initial_model %>% 
  plot(which = 4)
#Q-Q plot
initial_model %>% 
  plot(which = 2)
#histogram
risiduals_initial_model = enframe(residuals(initial_model))
risiduals_initial_model %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()
#lookin at the skew and kurtosis
describe(residuals(initial_model))
#Looking at linearity
initial_model %>% 
  residualPlots()
#looking at homoscedasiticiticisy
initial_model %>% 
  plot(which = 3)
#looking at multicolinearity
initial_model %>% 
  vif()

###Running a backwards regression
back_initial <- step(initial_model, direction = "backward")
#New model containing the predictors identified by the backwards regression
backward_model <- lm(pain ~ age + mindfulness + cortisol_serum + pain_cat, data = data_sample_1)

#comparing backward model to the theory-based model shows that the backward model is a better fit for data_sample_1
AIC(Theory_based_model)
AIC(backward_model)
anova(backward_model) #ANOVA shows insignificant difference, so we default to AIC

#Using both theory-based model and backward model to predict pain on new data

### Theory-based ###
pred_test <- predict(Theory_based_model, data_sample_2)
### Backwards ###
pred_test_backward <- predict(backward_model, data_sample_2)

#Checking the error of both models' predictons

### Theory-based ###
RSS_pred_test <- sum((data_sample_2[, "pain"] - pred_test)^2)
### backwards ###
RSS_back_pred <- sum((data_sample_2[, "pain"] - stet_derp)^2)

#The backward model contains more error when applied to new data, despite initially seeming like the preferred model
RSS_pred_test
RSS_back_pred


#reporting coefficients of the backwards model
summary(backward_model)
coef_table(backward_model)

#Comparison of the initial model, backward model, and theory-based model
AIC(Theory_based_model)
AIC(initial_model)
AIC(backward_model)

################################# ASSIGNMENT PART 3 ##############################################

#correciton of the one incorrect entry for sex: "woman" instead of "female". 
data_sample_3$sex <- replace(data_sample_3$sex, 25, "female")

#linear mixed model: allowing for random intercept on theory-based model  (trained on data_sample_3)
LMM_1 <- lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1 | hospital), data = data_sample_3)

#Using random intercept model to predict pain on new data (data_sample_4)
pred_LMM <- predict(LMM_1, data_sample_4, allow.new.levels = TRUE)

#Finding RSS and TSS of this mosdel to find R^2. 
### RSS ###
RSS_pred_LMM <- sum((data_sample_4[, "pain"] - pred_LMM)^2)
### TSS ###
mod_mean <- lm(pain ~ 1, data = data_sample_4)
summary(LMM_1)
TSS_pred_LMM <- sum((data_sample_4$pain - predict(mod_mean))^2)
### Final R^2 ###
R2_pred_LMM <- (1 - (RSS_pred_LMM /TSS_pred_LMM))
R2_pred_LMM

#building a new model with only the most influential variable allowing for both random intercept and slope

#Shows that cortisol_serum is the most influential variable
r2beta(LMM_1)

#Random slope model using only cortisol_serum
influential_model <- lmer(pain ~ cortisol_serum + (cortisol_serum | hospital), data = data_sample_3)

#visualize
data_sample_3 = data_sample_3%>% 
  mutate(influential_slope = predict(influential_model))


final_plot <- data_sample_3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 2) +
  geom_smooth(method = "lm", aes(y = influential_slope, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)
final_plot


#reporting coefficients, confidence intervals, cAIC, etc. 
summary(LMM_1)
sum(residuals(LMM_1)^2)
confint(LMM_1)
stdCoef.merMod(LMM_1)
r2beta(LMM_1)
r.squaredGLMM(LMM_1)
cAIC(LMM_1)





