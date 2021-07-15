#########################
#########################
#### Clear Workspace ####
#########################
#########################

rm(list = ls()) 
# clear global environment to remove all loaded data sets, functions and so on.

###################
###################
#### Libraries ####
###################
###################

library(easypackages) # enables the libraries function
suppressPackageStartupMessages(
  libraries("DataExplorer",
            "tidyverse",
            "tableone", # specifies abnormal variables
            "odds.n.ends", # displays odds ratios etc
            "lmtest", # for lrtest()
            "aod", # enables wald.test
            "mgcv", # enables gam() models
            "smbinning", # for information value of variables
            "ROCR" # for performance function
            ))

###############################
###############################
#### Set Working Directory ####
###############################
###############################

setwd("C:/R Portfolio/Logistic Regression")

libraries.cleaned <- read.csv("pew_libraries_2016_cleaned.csv")
str(libraries.cleaned)
glimpse(libraries.cleaned)
summary(libraries.cleaned)

###########################
###########################
# Exploratory Data Analysis 
###########################
###########################

libraries.cleaned %>%
  ggplot(aes(age)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_classic() +
  labs(y = "Probability Density", x = "Age in Years")
# Age is abnormally distributed, there is a slight positive skew

# Get a table of descriptive statistics

table.desc <- CreateTableOne(data = libraries.cleaned)
print(table.desc,
      nonnormal = 'age',
      showAllLevels = TRUE)

# get a table of descriptive statistics with bivariate tests

# Age is an abnormal continous variable and the outcome variable of interest is the library use variable, which indicates whether someone used library facilities or not. This library use is a categorical variable with two classes. To examine a relationship between one categorical variables (with two categories) and a continous one, the Mann-Whitney U test is suitable. The CreateTableOne () function below automatically uses the correct test based on variable types in the data.
# The p-value indicates the statistical significance of association between each individual variable and the outcome variable which is library use.
# SES and raceth variables are insignficant when paired with library use.
# It is important to also identify possible categories that may drive significant results for the bivariate test.Sex is particularly significant. 59% of those who dont use the library are men, while for library users, 55% are women.

table.desc <- CreateTableOne(data = libraries.cleaned,
                             strata = 'uses.lib',
                             vars = c("age", "sex", "parent", "disabled",
                                      "ses", "raceth", "educ", "rurality"))

print(table.desc,
      nonnormal = 'age',
      showAllLevels = TRUE)

##############################
##############################
# Compute Information Values #
##############################
##############################

# Computing the IV for each variable provides indication how useful they are to explaining the age variable

# segregate continuous and factor variables
factor_vars <- c ("sex", "parent", "disabled", "ses", "raceth", "educ", "rurality")
continuous_vars <- c("age", "standardized")

iv_df <- data.frame(VARS = c(factor_vars, continuous_vars), 
                    IV = numeric(9)) # 9 variables

# compute IV for categoricals
for(factor_var in factor_vars){
  smbc <- smbinning.factor(libraries.cleaned, y="uses.lib", x=factor_var)  # WOE table
  if(class(smbc) != "character"){ # check if some error occured
    iv_df[iv_df$VARS == factor_var, "IV"] <- smbc$iv
  }
}

# compute IV for continuous vars
for(continuous_var in continuous_vars){
  smb <- smbinning(libraries.cleaned, y="uses.lib", x=continuous_var)  # WOE table
  if(class(smb) != "character"){  # any error while calculating scores.
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}

iv_df <- iv_df[order(-iv_df$IV), ]  # sort
iv_df

####################
####################
#### Hypotheses ####
####################
####################

# H0: The model is no improvement than the baseline for library use prediction
# H1: The model is better than the baseline for library use prediction

################################################
################################################
# Determine Correct Coding of Outcome Variable #
################################################
################################################

# To indicate the categorical levels, the variable must be in factor format

libraries.cleaned$uses.lib <- as.factor(libraries.cleaned$uses.lib)
levels(x = libraries.cleaned$uses.lib)

# Because the yes category is second, the model will predict the yes rather than the no. In this situation, the ordering makes sense, but in situations where it is not, mutate and relevel functions can be used

# make no the reference group
# libraries.cleaned <- libraries.cleaned %>%
  # mutate(uses.lib = relevel(x = uses.lib, ref = "no"))
  
#############################
# Logistic Regression Model #
#############################
  
# estimate the library use model and print results
lib.model.small <- glm(formula = uses.lib ~ age,
                       data = libraries.cleaned,
                       family = binomial("logit"))
summary(object = lib.model.small)

# The model deviance with predictor variable included is less (closer to zero) than the null model, which means the full model is doing a statistically significant better job at predicting observed values than the null model.

# The p value of 0.00105 means there is a 0.00105% probability that this sample came from a population where there was no relationship between age and library use

#########################
# Computing Odds Ratios #
#########################

odds.n.ends(lib.model.small)

# the odds ratio for the age varible is 0.991201, which means that the odds of libray use decrease by 1% for every 1-year increase in age. Where odds ratios are less than 1, interpretation becomes easier when one subtracts the odds ratio from 1 and multiply by 100 to convert it into a percentage. The confidence interval doesn't cross one, so we can be sure the OR value is statistically significant. An odds ratio of 1 is that the odds are 1 times higher or 1 times a high for a change in the predictor, which is essentially the same odds. When the CI includes 1, the odds ratio could be 1 and this indicates it is not statistically different from 1. A relative risk of 1 is also similarly problematic. The null hypothesis cannot be rejected.

#####################################################
# Prediction Accuracy Percentage or Count R squared #
#####################################################

# simple logistic with sex predicting library use

libraries.cleaned$sex <- as.factor(libraries.cleaned$sex)
levels(x = libraries.cleaned$sex)

lib.by.sex <- glm(formula = uses.lib ~ sex,
                  data = libraries.cleaned,
                  family = binomial("logit"))
# The model predicts male use of the library, so the null model is female use of the library.
summary(lib.by.sex)
odds.n.ends(lib.by.sex)
lib.by.sex.odds.n.ends <- odds.n.ends(lib.by.sex)

#contingency table
lib.by.sex.cont.tab <- lib.by.sex.odds.n.ends$`Contingency tables (model fit): frequency predicted`

#(correct - most common )/ (total - most common): Adjusted count R2
(lib.by.sex.adjCountR2 <- (lib.by.sex.cont.tab[1,1] + lib.by.sex.cont.tab[2,2]- #add up the correct
                             max(c(lib.by.sex.cont.tab[3,1], lib.by.sex.cont.tab[3,2]))) / #subtract MAXIMUM of observed
   (lib.by.sex.cont.tab[3,3] - 
      max(c(lib.by.sex.cont.tab[3,1], lib.by.sex.cont.tab[3,2])))) #divide by tot-max

#################################################
# Logistic Regression Model with more variables #
#################################################

# This model incorporates categorical and continous variables 

# Convert appropriate variables into factors

libraries.cleaned$parent <- as.factor(libraries.cleaned$parent)
libraries.cleaned$disabled <- as.factor(libraries.cleaned$disabled)
libraries.cleaned$ses <- as.factor(libraries.cleaned$ses)
libraries.cleaned$raceth <- as.factor(libraries.cleaned$raceth)
libraries.cleaned$educ <- as.factor(libraries.cleaned$educ)
libraries.cleaned$rurality <- as.factor(libraries.cleaned$rurality)

# estimate the library use model and print results
lib.model <- glm(uses.lib ~ age + sex + educ + parent + disabled + rurality +                               raceth + ses,
                 data = libraries.cleaned,
                 na.action = na.exclude,
                 family = binomial("logit"))
odds.n.ends(lib.model)
summary(lib.model)

# a model containing all these variables was statistically significantly better thn the baseline probability at predicting library use. 
# The chi squared statistic of 94.74, while the p - value is <.001. Therefore, the probability of obtaining a chi square statistic of this value or larger if the null hypothesis is true is <.001. Therefore, the null hypothesis is rejected.

# With predictor variables of at least two levels, the odds ratio indicates the change going from the reference level to the other levels. In this example, sexmale is listed, which means that men are the reference level. The odds ratio is .51 (1 - .49 = .51) * 100. This means that men have 51% lower odds of using the library than women. 
# For categorical variables with more than two levels, each individual level is listed as seperate predictors in the logistic regression output. The level that is not listed is the reference group. Therefore, in this example, the education reference group is high school. Therefore, when we take education of at least four year degree or more, the odds ratio is 1.9040694. This means that people with degrees of four or more years are 1.9 times more likely to use a library than high school students.

# Non Significant odds ratios greater than 1 #

# The odds ratios of urban and suburban have CI's that include 1.
# Therefore, the odds of library use were not statistically significantly different for urban residents compared to rural residents.

# Non Significant odds ratios less than 1 #

# The low ses category has an odds ratio of .93 and a CI that includes 1. Therefore, the odds of library use ae not statistically significantly different for those with low SES compared to those in the reference group of high SES.

#########################################
#########################################
# Check for overall effect of variables #
#########################################
#########################################

# use of wald test enables overall effect of rurality to be known

wald.test(b = coef(lib.model), Sigma = vcov(lib.model), Terms = 8:9)
# a p value of .25, indicates that the overall effect of rurality is not significant

# use of wald test enables overall effect of education to be known

wald.test(b = coef(lib.model), Sigma = vcov(lib.model), Terms = 4:5)
# a p value of 7.1e-05, indicates that the overall effect of education is very significant

# To determine if the coefficient for suburban rurality is different to urban rurality
# to instruct the wald test to ignore all other coefficients in the model, it is necessary to create a vector with zero's to indicate these coefficients

l <- cbind(0, 0, 0, 0, 0, 0, 0,  1, -1, 0, 0, 0, 0)
wald.test(b = coef(lib.model), Sigma = vcov(lib.model), L = l)

# with a p value of 0.85, the difference in coefficients is not significant

#########################################
#########################################
# Check Logistic Regression Assumptions #
#########################################
#########################################

# There are three assumptions for logistic regression: independence of observations, linearity and no perfect multicollinearity. 

############################
# Independence Assumptions #
############################

# Here, such independence can be determined with chisq.tests.
# Where the count of both variables is less than five, use the exact fisher.tests

# there are three other methods to perform the Chi-square test of independence in R:

# with the summary() function
# with the assocstats() function from the {vcd} package
# with the ctable() function from the {summarytools} package

test <- chisq.test(table(libraries.cleaned$parent, libraries.cleaned$age))
test
f.test <- fisher.test(libraries.cleaned$parent, libraries.cleaned$age, simulate.p.value = TRUE)
f.test

########################
# Linearity Assumption #
########################

# With logistic regression, it is first necessary to plot the log odds of the predicted probabilities for the outcome against each of the continous predictors in the model

# make a variable of the log-odds of the predicted values
logit.use <- log(x = lib.model$fitted.values/(1-lib.model$fitted.values))

# make a small data frame with the log-odds variable and the age predictor
linearity.data <- data.frame(logit.use, age = lib.model$model$age)

# create a plot 
linearity.data %>%
  ggplot(aes(x = age, y = logit.use))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Age in years", y = "Log-odds of library use predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")

# The loess curve is very close to linear fit line apart from those aged less than 25. Perhaps young people can be removed from the sample. Therefore, apart from young people, linear assumption is met.

# To indicate probabilities of each observation 

# in the environment pane in R studio, click the small arrow next to the model name (lib.model) and then look for fitted values in the drop down text/ Alternatively, use the below code

lib.model$fitted.values

##############################
# If no linearity is evident #
##############################

# can use a generalised additive model instead, using a gam() function

lib.model_gam <- gam(uses.lib ~ age + sex + educ + parent + disabled + 
                       rurality + raceth + ses,
                 data = libraries.cleaned,
                 na.action = na.exclude,
                 family = binomial("logit"))

summary(lib.model_gam)

###########################################
# No Perfect Multicollinearity Assumption #
###########################################

# compute GVIF

# GVIF stands for generalised variance inflation factor

car::vif(lib.model)

# The threshold for multicollinearity is a value of 2.5 in the right hand column and since all values are less than this, we can assume there is no multicollinearity.

#####################
# Model Diagnostics #
#####################

# Here, the purpose is to identify if any observations are outliers, which can significantly impact on the model output.

###############################################
# Use standardised residuals to find outliers #
###############################################

# Standarised residuals follow the z-score and the threshold for outlier values is either 1.96 of -1.96
# Such residuals are calculated using the rstandard()

# get standardized residuals and add to data frame
libraries.cleaned <- libraries.cleaned %>%
  mutate(standardized = rstandard(model = lib.model))

# check the residuals for large values > 2
libraries.cleaned %>%
  drop_na(standardized) %>%
  summarize(max.resid = max(abs(x = standardized)))

# The maximum residual is 1.816214, so below 1.96, so there are no outlier values.

#############################################
# Using df-Betas to find influential values #
#############################################

# Observations with high df-betas (more than 2) may influence the model causing large differences in the intercept or coefficients. 

# get influence statistics
influence.lib.mod <- influence.measures(model = lib.model)

# summarize data frame with dfbetas, cooks, leverage
summary(object = influence.lib.mod$infmat)

# none of the maximum values are 2 or higher, so there are no outliers.

# save the data frame
influence.lib <- data.frame(influence.lib.mod$infmat)

####################################################
# Using Cook's Distance to find influential values #
####################################################

# The threshold figure is 4/number of rows in the data, which here is 1427

dim(libraries.cleaned)
influence.lib %>%
  filter(cook.d > 4/1601)

# The value numbers listed on the extreme left indicate the row numbers of problematic observations. 

#############################################
# Using Leverage to find influential values #
#############################################

# Leverage values range between 0 and 1.
# To determine which leverage values indicate influential observations, a formula is used: 2*13/1601. 13 is the number of parameters, which is the total number of predictors in the model, including the intercept. The full list of predictors is listed in the summary() function. The last number is the number of rows in the data set, which in this case is 1601

# observations with high Leverage
influence.lib %>%
  filter(hat > 2*13/1601)
# There are lots of outliers 

# observations with high leverage and Cook's D
influence.lib %>%
  filter(hat > 2*13/1601 & cook.d > 4/1601)

# make row names as a variable
influence.lib <- influence.lib %>%
  rownames_to_column()

# merge data frame with diagnostic stats
libraries.cleaned.diag <- libraries.cleaned %>%
  rownames_to_column() %>%
  merge(x = influence.lib, by = 'rowname') %>%
  mutate(pred.prob = predict(object = lib.model, type = "response"))

# find mean predicted probability
libraries.cleaned.diag %>%
  summarize(mean.predicted = mean(x = pred.prob, na.rm = TRUE))

# review influential observations
libraries.cleaned.diag %>%
  filter(hat > 2*13/1601 & cook.d > 4/1601) %>%
  select(rowname, age, sex, educ, parent, disabled,
         rurality, raceth, ses, hat, cook.d, pred.prob)

# Use model to predict probabilities for observations outside the data set #

# make a new data frame containing the observations of interest
examp.data <- data.frame(age = c(35, 65, 68),
                         sex = c("male", "female", "male"),
                         educ = c("Four-year degree or more",
                                  "Four-year degree or more",
                                  "Four-year degree or more"),
                         disabled = c("no", "no", "no"),
                         parent = c("not parent", "parent", "parent"),
                         rurality = c("rural", "rural", "rural"),
                         raceth = c("Non-Hispanic White",
                                    "Non-Hispanic White",
                                    "Non-Hispanic White"),
                         ses = c("low", "medium", "medium"))

# use the new data frame to predict
predictions <- predict(object = lib.model, newdata = examp.data,
                       type = "response")
predictions

##################################
# Indicate Predicted Probabilities
##################################

predictions$rankP <- predict(lib.model, newdata = libraries.cleaned, type = "response")
predictions

newdata3 <- cbind(libraries.cleaned, predict(lib.model, newdata = libraries.cleaned, type = "link",
                                    se = TRUE))

newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

head(newdata3)

# Plot the predicted probabilities #

ggplot(newdata3, aes(x = age, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = educ), alpha = 0.2) + 
  geom_line(aes(colour = educ),size = 1)

###########################
###########################
# Prediction Optimization #
###########################
###########################

###################################
# How to find the threshold value #
###################################

libraries.cleaned <- na.omit(libraries.cleaned)
res <- predict(lib.model, 
             libraries.cleaned,
             type = "response")

table(Actualvalue = libraries.cleaned$uses.lib, Predictedvalue = res > 0.5)

ROCRPred = prediction(res, libraries.cleaned$uses.lib)

ROCRPerf <- performance(ROCRPred, "tpr", "fpr")

plot(ROCRPerf, colorize = TRUE, print.cutoffs.at = seq(0.1, by = 0.1))

table(Actual = libraries.cleaned$uses.lib, Predicted = res > 0.3)
# using this threshold, the number of false positives drops dramatically, so it is advisable to use this threshold of 0.3

#############################################
# Adding and Interpreting interaction terms #
#############################################

# the relationship between parent status and library use 
libraries.cleaned %>%
  drop_na(parent) %>%
  ggplot(aes(x = parent, fill = factor(uses.lib))) +
  geom_bar(position = "dodge") +
  theme_classic() +
  labs(x = "Parent status", y = "Number of participants") +
  scale_fill_manual(values = c("#7463AC", "gray"),
                    name = "Library use")

# library use by sex
libraries.cleaned %>%
  drop_na(parent) %>%
  ggplot(aes(x = sex, fill = factor(uses.lib))) +
  geom_bar(position = "dodge") +
  theme_classic() +
  labs(x = "Sex", y = "Number of participants") +
  scale_fill_manual(values = c("#7463AC", "gray"),
                    name = "Library use")

# the relationship among sex, parent status, and library use
libraries.cleaned %>%
  drop_na(parent) %>%
  ggplot(aes(x = parent, fill = factor(uses.lib))) +
  geom_bar(position = "dodge") +
  theme_classic() +
  labs(x = "Parent status", y = "Number of participants") +
  scale_fill_manual(values = c("#7463AC", "gray"),
                    name = "Library use") +
  facet_grid("sex")

# estimate the library use model and print results
lib.model.int <- glm(formula = uses.lib ~ age + sex + educ + parent +
                       disabled + rurality + ses + raceth + sex*parent,
                     data = libraries.cleaned,
                     family = binomial("logit"))

odds.n.ends(lib.model.int)
summary(lib.model.int)

#########################
#########################
# Likelihood Ratio Test #
#########################
#########################

# A LR test compares two models where one model contains a subset of the variables in the other model.
# Such tests determine if additional variables in a model make the model better enough to warrant the complexity of adding more variables to the model. 
# The LR test computes the difference between the log likelihoods of the two models and multiplies it by two. The result has a chi squared distribution

##############
# Hypotheses #
##############

# H0: The larger model including the interaction term is no better at explaining library use compared to the model without the interaction term
# HA: The larger model is better than the smaller model in terms of explaining library use.

lmtest::lrtest(lib.model, lib.model.int)

# The p-value is 0.2117, so the null hypothesis is retained, which means the larger model with the interaction provides no better explanation. 
# Where the larger model is not statistically significant, it is preferable to use the more simpler model where possible.

###########################################################
###########################################################
# Visualisations for odds ratios and confidence intervals #
###########################################################
###########################################################

# get odds ratio table from lib.model
odds.lib.mod <- data.frame(odds.n.ends(lib.model)[6]) 

# make row names a variable
odds.lib.mod$var <- row.names(odds.lib.mod)

# change variable names for easier use
names(odds.lib.mod) <- c("OR", "lower", "upper", "variable")

# clean variable names for graph
odds.lib.mod.cleaned <- odds.lib.mod %>%
  mutate(variable = recode(.x = variable,
                           "sexmale" = "Male",
                           "ruralityurban" = "Urban residence",
                           "ruralitysuburban" = "Suburban residence",
                           "parentparent" = "Parent",
                           "educHS to 2-year degree" = "HS to 2-year degree",
                           "educFour-year degree or more" = "Four-year degree or more",
                           "disabledyes" = "Disabled",
                           "age" = "Age",
                           "seslow" = "Low socioeconomic status",
                           "sesmedium" = "Medium socioeconomic status",
                           "racethNon-Hispanic White" = "Non-Hispanic white",
                           "racethNon-Hispanic Black" = "Non-Hispanic black",
                           "(Intercept)" = "Intercept"))

# reorder the variable names by odds ratio size
# The scale_y_log10() ensures the full range or variance of each CI is indicated, by transforming to a log 10 scale.
odds.lib.mod.cleaned %>%
  ggplot(aes(x = reorder(variable, OR), y = OR, ymin = lower, ymax =
               upper)) +
  geom_pointrange(color = "#7463AC") +
  geom_hline(yintercept = 1, lty = 2, color = "deeppink", size = 1) +
  scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10), minor_breaks = NULL)+
  coord_flip() +
  labs(x = "Variable from library use model", y = "Odds ratio (95% CI)") +
  theme_minimal()
