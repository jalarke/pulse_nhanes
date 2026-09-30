## Title: Demographic summary variables for NHANES participants across DGA recommended intake for pulses
## Author: Jules Larke
## Date: 05222025
## Purpose: Create table 1 for participant demographic data and table 2 for odds ratios for comparing DGA recommendations for pulse intake by MetS, IR and CRP
## Added sensitivity analysis for quantile (terile) models to assess dose-response relationship between pulse intake and metabolic outcomes

library(dplyr)
library(ggplot2)
library(survey)
options(survey.lonely.psu='adjust')

# Load data
cov <- data.table::fread('../../data/01/all/wweia_cov_wts.csv')
pulse_dga = read.csv('../../data/01/all/all_nhanes_pulse_dga.csv')

demo = left_join(cov, pulse_dga, by = 'SEQN')

xtabs(~ met_dga, data= demo)

demo %>% group_by(met_dga) %>% summarize(mean_cup = mean(cup_eq), median_cup = median(cup_eq))
# create survey design object
svy_all <- svydesign(data=demo, id=~SDMVPSU, strata=~SDMVSTRA, weights=~diet_wts_combined, nest=TRUE)

# quality of life fxn to get the output formatted for table 1
format_svyciprop <- function(formula, subset_data, method = "me", digits = 1) {
  result <- svyciprop(formula, subset_data, method = method)
  
  est  <- as.numeric(result)
  ci   <- attr(result, "ci")
  low  <- ci[1]
  high <- ci[2]
  
  fmt <- function(x) formatC(round(x * 100, digits), format = "f", digits = digits)
  
  cat(sprintf("%s (%s, %s)\n", fmt(est), fmt(low), fmt(high)))
}

# calculate the proportions and 95% CIs
format_svyciprop(~I(Sex=='Female'), subset(svy_all, met_dga == 'no'), method="me")
format_svyciprop(~I(Sex=='Male'), subset(svy_all, met_dga == 'no'), method="me")
format_svyciprop(~I(Sex=='Female'), subset(svy_all, met_dga == 'yes'), method="me")
format_svyciprop(~I(Sex=='Male'), subset(svy_all, met_dga == 'yes'), method="me")

format_svyciprop(~I(ethnicity=='Mexican_American'), subset(svy_all, met_dga == 'no'), method="me")
format_svyciprop(~I(ethnicity=='Non-Hispanic_Black'), subset(svy_all, met_dga == 'no'), method="me")
format_svyciprop(~I(ethnicity=='Non-Hispanic_White'), subset(svy_all, met_dga == 'no'), method="me")
format_svyciprop(~I(ethnicity=='Other_Hispanic'), subset(svy_all, met_dga == 'no'), method="me")
format_svyciprop(~I(ethnicity=='Other_Multi-Racial'), subset(svy_all, met_dga == 'no'), method="me")

format_svyciprop(~I(ethnicity=='Mexican_American'), subset(svy_all, met_dga == 'yes'), method="me")
format_svyciprop(~I(ethnicity=='Non-Hispanic_Black'), subset(svy_all, met_dga == 'yes'), method="me")
format_svyciprop(~I(ethnicity=='Non-Hispanic_White'), subset(svy_all, met_dga == 'yes'), method="me")
format_svyciprop(~I(ethnicity=='Other_Hispanic'), subset(svy_all, met_dga == 'yes'), method="me")
format_svyciprop(~I(ethnicity=='Other_Multi-Racial'), subset(svy_all, met_dga == 'yes'), method="me")

format_svyciprop(~I(ever_smoker=='no'), subset(svy_all, met_dga == 'no'), method="me")
format_svyciprop(~I(ever_smoker=='yes'), subset(svy_all, met_dga == 'no'), method="me")
format_svyciprop(~I(ever_smoker=='no'), subset(svy_all, met_dga == 'yes'), method="me")
format_svyciprop(~I(ever_smoker=='yes'), subset(svy_all, met_dga == 'yes'), method="me")

format_svyciprop(~I(education=='college graduate'), subset(svy_all, met_dga == 'no'), method="me")
format_svyciprop(~I(education=='some college'), subset(svy_all, met_dga == 'no'), method="me")
format_svyciprop(~I(education=='high school graduate or equivalent'), subset(svy_all, met_dga == 'no'), method="me")
format_svyciprop(~I(education=='less than high school graduate'), subset(svy_all, met_dga == 'no'), method="me")

format_svyciprop(~I(education=='college graduate'), subset(svy_all, met_dga == 'yes'), method="me")
format_svyciprop(~I(education=='some college'), subset(svy_all, met_dga == 'yes'), method="me")
format_svyciprop(~I(education=='high school graduate or equivalent'), subset(svy_all, met_dga == 'yes'), method="me")
format_svyciprop(~I(education=='less than high school graduate'), subset(svy_all, met_dga == 'yes'), method="me")

# calculate the means and SEs
svyciprop(~Age, subset(svy_all, met_dga == 'no'), method="me")
svyciprop(~Age, subset(svy_all, met_dga == 'yes'), method="me")

svyciprop(~BMI, subset(svy_all, met_dga == 'no'), method="me")
svyciprop(~BMI, subset(svy_all, met_dga == 'yes'), method="me")

svyciprop(~family_pir, subset(svy_all, met_dga == 'no'), method="me")
svyciprop(~family_pir, subset(svy_all, met_dga == 'yes'), method="me")

# Perform t-tests and chi-squared tests using survey design elements for table 1
svyttest(BMI~met_dga, svy_all)
svyttest(Age~met_dga, svy_all)
svyttest(family_pir~met_dga, svy_all)

svychisq(~Sex+met_dga, svy_all, statistic="adjWald")
svychisq(~ethnicity+met_dga, svy_all, statistic="adjWald")
svychisq(~ever_smoker+met_dga, svy_all, statistic="adjWald")
svychisq(~education+met_dga, svy_all, statistic="adjWald")


# load clinical endpoints to calculate prevalence / biomarker in high/low met_dgas
getwd()

#setwd("/Users/jules.larke/work/project/pulse_update_copy/src/04_demographics/")
crp_target = read.csv('../../data/01/crp/crp_target.tsv', sep = '\t')

crp_target_demo = left_join(crp_target, demo, by = 'SEQN')
crp_target_demo = crp_target_demo %>% select(-diet_wts_combined) # CRP dataset is 7 cycles (demo has the full 9 cycles so the combined cycle diet weights differ, drop and add back the correctly calculated weights for 7 cycles)

crp_intake = read.csv('../../data/01/crp/crp_dfg2_intake.tsv', sep = '\t')
crp_wts = crp_intake %>% select('SEQN', 'diet_wts_combined')

crp_design = left_join(crp_target_demo, crp_wts, by = 'SEQN')

# now for the IR outcome
ir_target = read.csv('../../data/01/ir/ir_target.tsv', sep = '\t')

ir_design = left_join(ir_target, demo, by = 'SEQN') # merge with the covariates and pulse intake data

# for MetS outcome
mets_target = read.csv('../../data/01/mets/mets_target.tsv', sep = '\t')

mets_design = left_join(mets_target, demo, by = 'SEQN') # merge with the covariates and pulse intake data

# MetS
svy_mets <- svydesign(data=mets_design, id=~SDMVPSU, strata=~SDMVSTRA, weights=~diet_wts_combined, nest=TRUE)

xtabs(~met_dga + mets, data = mets_design)

format_svyciprop(~I(mets==0), subset(svy_mets, met_dga == 'no'), method="me")
format_svyciprop(~I(mets==1), subset(svy_mets, met_dga == 'no'), method="me")
format_svyciprop(~I(mets==0), subset(svy_mets, met_dga == 'yes'), method="me")
format_svyciprop(~I(mets==1), subset(svy_mets, met_dga == 'yes'), method="me")


# Test the association of met_dga with no covariates
mets_model_no_cov <- svyglm(mets ~ met_dga, 
                            design = svy_mets, 
                            family = quasibinomial())

summary(mets_model_no_cov)
exp(cbind(OR = coef(mets_model_no_cov), confint(mets_model_no_cov)))


# add covariates to the model 
# test using logistic regression 
# Fit logistic regression model
mets_model <- svyglm(mets ~ met_dga + Sex + BMI + Age + ever_smoker + ethnicity + education + family_pir, 
                     design = svy_mets, 
                     family = quasibinomial())


# Test the association of met_dga adjusting for covariates
regTermTest(mets_model, "met_dga", method = "Wald")

# View adjusted odds ratios
summary(mets_model)
exp(cbind(OR = coef(mets_model), confint(mets_model)))


##### test for continuous / dose-response relationship between pulse intake and metabolic outcomes

# First, check the distribution of cup_eq
summary(svy_mets$variables$cup_eq)
quantile(svy_mets$variables$cup_eq, probs = seq(0, 1, 0.1), na.rm = TRUE)

# How many are exactly zero (non-consumers)?
mean(svy_mets$variables$cup_eq == 0, na.rm = TRUE)

# 64% of people are non-pulse consumers, zero-inflated model is not-appropriate
# rather, split into non-consumer, and tertiles of consumers to assess dose response
# the zero inflation issue occurs with IR and CRP as well


# Example: non-consumer, tertile 1, tertile 2, tertile 3 among consumers
mets_consumer_tertiles <- svyquantile(~cup_eq, subset(svy_mets, cup_eq > 0), 
                                      quantiles = c(0, 1/3, 2/3, 1), na.rm = TRUE)


## MetS ordinal dose-response model

svy_mets <- update(svy_mets, cup_eq_cat = cut(cup_eq, 
                                              breaks = c(-Inf, 0, mets_consumer_tertiles$cup_eq[2,1], mets_consumer_tertiles$cup_eq[3,1], Inf),
                                              labels = c("None", "Low", "Medium", "High"),
                                              include.lowest = TRUE))

mets_model_cat <- svyglm(mets ~ cup_eq_cat + Sex + Age + BMI + ever_smoker + ethnicity + education + family_pir, 
                         design = svy_mets, 
                         family = quasibinomial())
summary(mets_model_cat)
exp(cbind(OR = coef(mets_model_cat), confint(mets_model_cat)))

svytable(~cup_eq_cat, svy_mets)

# CRP
svy_crp <- svydesign(data=crp_design, id=~SDMVPSU, strata=~SDMVSTRA, weights=~diet_wts_combined, nest=TRUE)

xtabs(~met_dga + crp_class, data = crp_design)

svyciprop(~I(crp_class==0), subset(svy_crp, met_dga == 'no'), method="me")
svyciprop(~I(crp_class==1), subset(svy_crp, met_dga == 'no'), method="me")
svyciprop(~I(crp_class==0), subset(svy_crp, met_dga == 'yes'), method="me")
svyciprop(~I(crp_class==1), subset(svy_crp, met_dga == 'yes'), method="me")

# test using logistic regression 

## compare DGA model with no covariate adjustments
crp_model_no_cov <- svyglm(crp_class ~ met_dga, 
                           design = svy_crp, 
                           family = quasibinomial())

summary(crp_model_no_cov)
exp(cbind(OR = coef(crp_model_no_cov), confint(crp_model_no_cov)))

# compare DGA model with covariate adjustments
crp_model <- svyglm(crp_class ~ met_dga + Sex + Age + BMI + ever_smoker + ethnicity + education, 
                   design = svy_crp, 
                   family = quasibinomial())

# View adjusted odds ratios
summary(crp_model)

exp(cbind(OR = coef(crp_model), confint(crp_model)))

## CRP ordinal dose-response model
crp_consumer_tertiles <- svyquantile(~cup_eq, subset(svy_crp, cup_eq > 0), 
                                     quantiles = c(0, 1/3, 2/3, 1), na.rm = TRUE)


svy_crp <- update(svy_crp, cup_eq_cat = cut(cup_eq, 
                                            breaks = c(-Inf, 0, crp_consumer_tertiles$cup_eq[2,1], crp_consumer_tertiles$cup_eq[3,1], Inf),
                                            labels = c("None", "Low", "Medium", "High"),
                                            include.lowest = TRUE))

crp_model_cat <- svyglm(crp_class ~ cup_eq_cat + Sex + Age + BMI + ever_smoker + ethnicity + education + family_pir, 
                        design = svy_crp, 
                        family = quasibinomial())

summary(crp_model_cat)
exp(cbind(OR = coef(crp_model_cat), confint(crp_model_cat)))


# IR
svy_ir <- svydesign(data=ir_design, id=~SDMVPSU, strata=~SDMVSTRA, weights=~diet_wts_combined, nest=TRUE)

svyciprop(~I(ir==0), subset(svy_ir, met_dga == 'no'), method="me")
svyciprop(~I(ir==1), subset(svy_ir, met_dga == 'no'), method="me")
svyciprop(~I(ir==0), subset(svy_ir, met_dga == 'yes'), method="me")
svyciprop(~I(ir==1), subset(svy_ir, met_dga == 'yes'), method="me")

## DGA model with no covariate adjustments
ir_model_no_cov <- svyglm(ir ~ met_dga, 
                          design = svy_ir, 
                          family = quasibinomial())

summary(ir_model_no_cov)
exp(cbind(OR = coef(ir_model_no_cov), confint(ir_model_no_cov)))

## DGA model with covariate adjustment
ir_model <- svyglm(ir ~ met_dga + Sex + Age + BMI + ever_smoker + ethnicity + education, 
                     design = svy_ir, 
                     family = quasibinomial())

# View adjusted odds ratios
summary(ir_model)
exp(cbind(OR = coef(ir_model), confint(ir_model)))

## IR ordinal dose-response model
ir_consumer_tertiles <- svyquantile(~cup_eq, subset(svy_ir, cup_eq > 0), 
                                 quantiles = c(0, 1/3, 2/3, 1), na.rm = TRUE)


svy_ir <- update(svy_ir, cup_eq_cat = cut(cup_eq, 
                                              breaks = c(-Inf, 0, ir_consumer_tertiles$cup_eq[2,1], ir_consumer_tertiles$cup_eq[3,1], Inf),
                                              labels = c("None", "Low", "Medium", "High"),
                                              include.lowest = TRUE))

ir_model_cat <- svyglm(ir ~ cup_eq_cat + Sex + Age + BMI + ever_smoker + ethnicity + education + family_pir, 
                         design = svy_ir, 
                         family = quasibinomial())
summary(ir_model_cat)
exp(cbind(OR = coef(ir_model_cat), confint(ir_model_cat)))

svytable(~cup_eq_cat, svy_ir)