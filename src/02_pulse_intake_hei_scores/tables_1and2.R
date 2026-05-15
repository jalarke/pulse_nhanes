## Title: Demographic summary variables for NHANES participants across DGA recommended intake for pulses
## Author: Jules Larke
## Date: 05222025
## Purpose: Create table and figure for participant demographic data. Perform statistics for table 1

## table will show proportions and 95% CIs as in this article: https://ajcn.nutrition.org/article/S0002-9165(23)66222-7/fulltext

library(dplyr)
library(ggplot2)
library(survey)
options(survey.lonely.psu='adjust')

# Load data
cov <- data.table::fread('../../data/01/all/wweia_cov_wts.csv')
pulse_dga = read.csv('../../data/01/all/all_nhanes_pulse_dga.csv')

demo = left_join(cov, pulse_dga, by = 'SEQN')

xtabs(~ met_dga, data= demo)

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

svychisq(~mets+met_dga, svy_mets, statistic="Chisq")

# add covariates to the model 
# test using logistic regression 
# Fit logistic regression model
mets_model <- svyglm(mets ~ met_dga + Sex + Age + ever_smoker + ethnicity + education + family_pir, 
                design = svy_mets, 
                family = quasibinomial())


# Test the association of met_dga with mets, adjusting for covariates
regTermTest(mets_model, "met_dga", method = "Wald")

# View adjusted odds ratios
summary(mets_model)
exp(coef(mets_model))  # Exponentiate for odds ratios
exp(confint(mets_model))  # Confidence intervals

# CRP
svy_crp <- svydesign(data=crp_design, id=~SDMVPSU, strata=~SDMVSTRA, weights=~diet_wts_combined, nest=TRUE)

xtabs(~met_dga + crp_class, data = crp_design)

svyciprop(~I(crp_class==0), subset(svy_crp, met_dga == 'no'), method="me")
svyciprop(~I(crp_class==1), subset(svy_crp, met_dga == 'no'), method="me")
svyciprop(~I(crp_class==0), subset(svy_crp, met_dga == 'yes'), method="me")
svyciprop(~I(crp_class==1), subset(svy_crp, met_dga == 'yes'), method="me")

svychisq(~crp_class+met_dga, svy_crp, statistic="Chisq")

# test using logistic regression 
# Fit logistic regression model
crp_model <- svyglm(crp_class ~ met_dga + Sex + Age + BMI + ever_smoker + ethnicity + education, 
                   design = svy_crp, 
                   family = quasibinomial())

# Test the association of met_dga with mets, adjusting for covariates
regTermTest(crp_model, "met_dga", method = "Wald")

# View adjusted odds ratios
summary(crp_model)
exp(coef(crp_model))  # Exponentiate for odds ratios
exp(confint(crp_model))  # Confidence intervals

# IR
svy_ir <- svydesign(data=ir_design, id=~SDMVPSU, strata=~SDMVSTRA, weights=~diet_wts_combined, nest=TRUE)

svyciprop(~I(ir==0), subset(svy_ir, met_dga == 'no'), method="me")
svyciprop(~I(ir==1), subset(svy_ir, met_dga == 'no'), method="me")
svyciprop(~I(ir==0), subset(svy_ir, met_dga == 'yes'), method="me")
svyciprop(~I(ir==1), subset(svy_ir, met_dga == 'yes'), method="me")

svychisq(~ir+met_dga, svy_ir, statistic="Chisq")

# test using logistic regression 
# Fit logistic regression model
ir_model <- svyglm(ir ~ met_dga + Sex + Age + BMI + ever_smoker + ethnicity + education, 
                     design = svy_ir, 
                     family = quasibinomial())

# Test the association of met_dga with mets, adjusting for covariates
regTermTest(ir_model, "met_dga", method = "Wald")

# View adjusted odds ratios
summary(ir_model)
exp(coef(ir_model))  # Exponentiate for odds ratios
exp(confint(ir_model))  # Confidence intervals