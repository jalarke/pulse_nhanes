## Title: 03_plot_pulse_intake_fiber_HEI
## Author: Jules Larke
## Date: 080124
## Purpose: calculate pulse consumption across participants and quantify glycan intake in pulse and non-pulse foods
library(tidyverse)
library(dplyr)
library(ggplot2)
library(cowplot)
library(stringr)
library(survey)

getwd()
# load datasets
pulse_intake = read.csv('../../data/01/all/pulse_consumer_population_mean_intake.csv')
pulse_fiber = read.csv('../../data/01/all/pulse_fiber_dfg_source_means.csv')

# create directory to save plots
dir.create('../../plot/figure_1/', recursive = TRUE)

# 1. plot pulse intake

# Pivot wide to long format
pulse_intake_long <- pulse_intake %>%
  pivot_longer(
    cols      = c(consumer_mean, population_mean),
    names_to  = "group",
    values_to = "mean"
  ) %>%
  mutate(
    sem = if_else(group == "consumer_mean", consumer_sem, population_sem),
    group = recode(group,
                   "consumer_mean"   = "Consumer",
                   "population_mean" = "Population"
    ),
    ingred_desc = factor(ingred_desc)
  )

# Extract one row per food (n is the same for both groups after pivot)

# Compute desired order (ascending mean so coord_flip shows descending top-to-bottom)
ordered_levels <- pulse_intake_long %>%
  filter(group == "Population") %>%
  arrange(mean) %>%
  pull(ingred_desc) %>%
  as.character()

# Apply order as factor levels
pulse_intake_long <- pulse_intake_long %>%
  mutate(ingred_desc = factor(ingred_desc, levels = ordered_levels))

# Rebuild n_labels (same as before)
n_labels <- pulse_intake_long %>%
  distinct(ingred_desc, n_consumers) %>%
  mutate(label = paste0(ingred_desc, "\n(n = ", n_consumers, ")")) %>%
  select(ingred_desc, label) %>%
  deframe()

pulse_plot <- ggplot(pulse_intake_long, aes(x = ingred_desc, y = mean, fill = group)) +
  geom_col(
    position = position_dodge(width = 0.7),
    width    = 0.65
  ) +
  geom_errorbar(
    aes(ymin = mean - sem, ymax = mean + sem),
    position = position_dodge(width = 0.7),
    width    = 0.25,
    linewidth = 0.5
  ) +
  scale_fill_manual(
    values = c("Population" = "darkorchid", "Consumer" = "darkcyan"),
    name   = NULL
  ) +
  scale_x_discrete(
    limits = ordered_levels,
    labels = n_labels
  ) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = NULL,
    y = "Pulse intake (cup eq. per day)"
  ) +
  theme_classic(base_size = 10) +
  theme(
    legend.position      = "top",
    legend.justification = "left",
    axis.text.y          = element_text(size = 9),
    plot.title           = element_text(face = "bold"),
    panel.grid.major.x   = element_line(color = "grey90", linewidth = 0.4)
  )
pulse_plot


# just show the consumer level intake with population percent
consumer_intake = pulse_intake_long %>% filter(group == 'Consumer')
consumer_intake$pop_percent = consumer_intake$n_consumers / consumer_intake$n_population

# Highest population mean at the top after coord_flip()
consumer_intake <- consumer_intake %>%
  mutate(ingred_desc = fct_reorder(ingred_desc, pop_percent))
# Extract one row per food (n is the same for both groups after pivot)

n_labels <- consumer_intake %>%
  distinct(ingred_desc, pop_percent) %>%
  select(ingred_desc, pop_percent) %>%  # ensure exactly two columns for deframe
  mutate(label = paste0(ingred_desc, "\n(n% = ", round(pop_percent*100, 1), ")")) %>%
  select(ingred_desc, label) %>%       # deframe needs: name col, value col
  deframe()

n_labels[13] = "Fava beans\n(n% < 0.1)"
n_labels[15] = "Great Northern beans\n(n% < 0.1)"

# plot consumer average with n% for popluation intake
consumer_plot <- ggplot(consumer_intake, aes(x = ingred_desc, y = mean, fill = group)) +
  geom_col(
    position = position_dodge(width = 0.7),
    width    = 0.65
  ) +
  geom_errorbar(
    aes(ymin = mean - sem, ymax = mean + sem),
    position = position_dodge(width = 0.7),
    width    = 0.25,
    linewidth = 0.5
  ) +
  scale_fill_manual(
    values = c("Consumer" = "darkcyan"),
    name   = NULL
  ) +
  scale_x_discrete(
    limits = levels(consumer_intake$ingred_desc),
    labels = n_labels
  ) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = NULL,
    y = "Pulse intake (cup eq. per day)"
  ) +
  theme_classic(base_size = 10) +
  theme(
    legend.position      = "none",
    legend.justification = "left",
    axis.text.y          = element_text(size = 9),  # slightly smaller to accommodate n
    plot.title           = element_text(face = "bold"),
    panel.grid.major.x   = element_line(color = "grey90", linewidth = 0.4)
  )
consumer_plot

# survey-weighted t-test comparing fiber intake by DGA 2020 pulse consumer/non-consumer status
# group by data to compare intake of fiber using survey weighted t-test
fiber_dga = read.csv('/Users/jules.larke/work/project/pulse_update_copy/data/01/all/fiber_intake_dga.csv')

svy_data = data.table::fread('/Users/jules.larke/work/project/pulse_update_copy/data/01/all/wweia_cov_wts.csv')
svy_data = svy_data %>% select(SEQN, SDMVPSU, SDMVSTRA, diet_wts_combined)

fiber_svy = merge(fiber_dga, svy_data, by = 'SEQN')

# convert consumer to 0/1
fiber_svy$met_dga = ifelse(fiber_svy$met_dga == 'yes', 1, 0)

# create svydesign object
svy_fiber <- svydesign(data=fiber_svy, id=~SDMVPSU, strata=~SDMVSTRA, weights=~diet_wts_combined, nest=TRUE)

# Perform t-test
fiber_test = svyttest(Fiber..total.dietary~met_dga, svy_fiber) 
fiber_test$p.value # annotate plot below with p-value


# plot fiber intake for those meeting DGA2020 recommendations for pulse intake, separate fiber sources as pulse/non-pulse
# ── 1. Prepare stacked bars (pulse + non_pulse only) ──────────────────────────

fiber_plot <- pulse_fiber %>%
  filter(fiber_source != "total") %>%
  mutate(
    met_dga = recode(met_dga,
                     "yes" = "DGA adherent",
                     "no"  = "DGA non-adherent"
    ),
    met_dga      = factor(met_dga, levels = c("DGA non-adherent", "DGA adherent")),
    fiber_source = recode(fiber_source,
                          "pulse"     = "Pulse",
                          "non_pulse" = "Non-Pulse"
    ),
    fiber_source = factor(fiber_source, levels = c("Non-Pulse", "Pulse"))
  )

# ── 2. Error bars from total rows only ────────────────────────────────────────

fiber_error <- pulse_fiber %>%
  filter(fiber_source == "total") %>%
  mutate(
    met_dga = recode(met_dga,
                     "yes" = "DGA adherent",
                     "no"  = "DGA non-adherent"
    ),
    met_dga = factor(met_dga, levels = c("DGA non-adherent", "DGA adherent"))
  )

# ── 3. P-value annotation — update after you calculate ───────────────────────

p_value <- "p = 3.9e-53"   # replace with your value
y_bar   <- max(fiber_error$population_mean) * 1.10
y_text  <- max(fiber_error$population_mean) * 1.15

# ── 4. Plot ───────────────────────────────────────────────────────────────────

fiber_dga_plot <- ggplot(fiber_plot, aes(x = met_dga, y = population_mean, fill = fiber_source)) +
  geom_col(
    position = position_stack(reverse = TRUE),
    width    = 0.6
  ) +
  geom_errorbar(
    data = fiber_error,
    aes(
      x    = met_dga,
      ymin = population_mean - population_sem,
      ymax = population_mean + population_sem
    ),
    inherit.aes = FALSE,
    width     = 0.15,
    linewidth = 0.5
  ) +
  annotate("segment",
           x = 1, xend = 2,
           y = y_bar, yend = y_bar,
           color = "black", linewidth = 0.6
  ) +
  annotate("text",
           x = 1.5, y = y_text,
           label = p_value,
           color = "red", size = 3
  ) +
  scale_fill_manual(
    values = c("Non-Pulse" = "#4472C4", "Pulse" = "#ED7D31"),
    name   = NULL
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, max(fiber_error$population_mean) * 1.2)
  ) +
  labs(
    x = NULL,
    y = "Dietary fiber (g per day)"
  ) +
  theme_classic(base_size = 10) +
  theme(
    legend.position      = "top",
    legend.justification = "left",
    axis.text.x          = element_text(size = 10),
    panel.grid.major.y   = element_line(color = "grey90", linewidth = 0.4)
  )
fiber_dga_plot







## Title: 01_calculate_hei
## Author: Jules Larke
## Date: 072224
## Purpose: Calculate HEI scores for all NHANES participants

library(dplyr)
library(ggplot2)
library(dietaryindex)
library(survey)
options(survey.lonely.psu='adjust')

# NHANES mets dataset
nhanes <- data.table::fread('/Users/jules.larke/work/project/pulse_update_copy/data/01/all/all_nhanes_hei_v2.txt', sep = '\t', select = c("SEQN", "ingred_code", "ingred_desc", "Ingred_consumed_g", "Energy", "Fatty acids, total monounsaturated",	"Fatty acids, total polyunsaturated",	"Fatty acids, total saturated", "Sodium", "Age", "SDMVPSU",	"SDMVSTRA",	"diet_wts_combined"))
nhanes_design <- nhanes %>% distinct(SEQN, .keep_all = TRUE) %>% select("SEQN", "SDMVPSU",	"SDMVSTRA",	"diet_wts_combined") # survey design elements
codes <- nhanes %>% distinct(ingred_code, .keep_all = TRUE) # get unique codes for matching with Food Patterns Ingredient Database (FPID) for calculating HEI-2020 score

# NHANES pulse intake data
pulse_dga <- read.csv('/Users/jules.larke/work/project/pulse_update_copy/data/01/all/all_nhanes_pulse_dga.csv')

# FPID databases for each FNDDS cycle
fpid_2006 <- read.csv('/Users/jules.larke/work/project/pulse_update_copy/data/02/fpid_0506.csv')
fpid_2008 <- read.csv('/Users/jules.larke/work/project/pulse_update_copy/data/02/fpid_0708.csv')
fpid_2010 <- read.csv('/Users/jules.larke/work/project/pulse_update_copy/data/02/fpid_0910.csv')
fpid_2012 <- read.csv('/Users/jules.larke/work/project/pulse_update_copy/data/02/fpid_1112.csv')
fpid_2014 <- read.csv('/Users/jules.larke/work/project/pulse_update_copy/data/02/fpid_1314.csv')
fpid_2016 <- read.csv('/Users/jules.larke/work/project/pulse_update_copy/data/02/fpid_1516.csv')
fpid_2018 <- read.csv('/Users/jules.larke/work/project/pulse_update_copy/data/02/fpid_1718.csv')

# select the HEI components used to calculate the HEI-2020 scores (total calories are from diet recalls)
fpid_2006 <- fpid_2006 %>% select(CODE, DESCRIPTION, `F_TOTAL..cup.eq..`, `F_CITMLB..cup.eq..`, `F_OTHER..cup.eq..`, `V_TOTAL..cup.eq..`, `V_LEGUMES..cup.eq..`, `V_DRKGR..cup.eq..`, `PF_MPS_TOTAL..oz..eq..`, `PF_SOY..oz..eq..`, `PF_EGGS..oz..eq..`, `PF_NUTSDS..oz..eq..`, `PF_LEGUMES..oz..eq..`, `PF_SEAFD_HI..oz..eq..`, `PF_SEAFD_LOW..oz..eq..`, `G_WHOLE..oz..eq..`, `D_TOTAL..cup.eq..`, `OILS..grams.`, `SOLID_FATS..grams.`, `G_REFINED..oz..eq..`, `ADD_SUGARS..tsp..eq..`)
fpid_2008 <- fpid_2008 %>% select(CODE, DESCRIPTION, `F_TOTAL..cup.eq..`, `F_CITMLB..cup.eq..`, `F_OTHER..cup.eq..`, `V_TOTAL..cup.eq..`, `V_LEGUMES..cup.eq..`, `V_DRKGR..cup.eq..`, `PF_MPS_TOTAL..oz..eq..`, `PF_SOY..oz..eq..`, `PF_EGGS..oz..eq..`, `PF_NUTSDS..oz..eq..`, `PF_LEGUMES..oz..eq..`, `PF_SEAFD_HI..oz..eq..`, `PF_SEAFD_LOW..oz..eq..`, `G_WHOLE..oz..eq..`, `D_TOTAL..cup.eq..`, `OILS..grams.`, `SOLID_FATS..grams.`, `G_REFINED..oz..eq..`, `ADD_SUGARS..tsp..eq..`)
fpid_2010 <- fpid_2010 %>% select(CODE, DESCRIPTION, `F_TOTAL..cup.eq..`, `F_CITMLB..cup.eq..`, `F_OTHER..cup.eq..`, `V_TOTAL..cup.eq..`, `V_LEGUMES..cup.eq..`, `V_DRKGR..cup.eq..`, `PF_MPS_TOTAL..oz..eq..`, `PF_SOY..oz..eq..`, `PF_EGGS..oz..eq..`, `PF_NUTSDS..oz..eq..`, `PF_LEGUMES..oz..eq..`, `PF_SEAFD_HI..oz..eq..`, `PF_SEAFD_LOW..oz..eq..`, `G_WHOLE..oz..eq..`, `D_TOTAL..cup.eq..`, `OILS..grams.`, `SOLID_FATS..grams.`, `G_REFINED..oz..eq..`, `ADD_SUGARS..tsp..eq..`)
fpid_2012 <- fpid_2012 %>% select(CODE, DESCRIPTION, `F_TOTAL..cup.eq..`, `F_CITMLB..cup.eq..`, `F_OTHER..cup.eq..`, `V_TOTAL..cup.eq..`, `V_LEGUMES..cup.eq..`, `V_DRKGR..cup.eq..`, `PF_MPS_TOTAL..oz..eq..`, `PF_SOY..oz..eq..`, `PF_EGGS..oz..eq..`, `PF_NUTSDS..oz..eq..`, `PF_LEGUMES..oz..eq..`, `PF_SEAFD_HI..oz..eq..`, `PF_SEAFD_LOW..oz..eq..`, `G_WHOLE..oz..eq..`, `D_TOTAL..cup.eq..`, `OILS..grams.`, `SOLID_FATS..grams.`, `G_REFINED..oz..eq..`, `ADD_SUGARS..tsp..eq..`)
fpid_2014 <- fpid_2014 %>% select(CODE, DESCRIPTION, `F_TOTAL..cup.eq..`, `F_CITMLB..cup.eq..`, `F_OTHER..cup.eq..`, `V_TOTAL..cup.eq..`, `V_LEGUMES..cup.eq..`, `V_DRKGR..cup.eq..`, `PF_MPS_TOTAL..oz..eq..`, `PF_SOY..oz..eq..`, `PF_EGGS..oz..eq..`, `PF_NUTSDS..oz..eq..`, `PF_LEGUMES..oz..eq..`, `PF_SEAFD_HI..oz..eq..`, `PF_SEAFD_LOW..oz..eq..`, `G_WHOLE..oz..eq..`, `D_TOTAL..cup.eq..`, `OILS..grams.`, `SOLID_FATS..grams.`, `G_REFINED..oz..eq..`, `ADD_SUGARS..tsp..eq..`)
fpid_2016 <- fpid_2016 %>% select(CODE, DESCRIPTION, `F_TOTAL..cup.eq..`, `F_CITMLB..cup.eq..`, `F_OTHER..cup.eq..`, `V_TOTAL..cup.eq..`, `V_LEGUMES..cup.eq..`, `V_DRKGR..cup.eq..`, `PF_MPS_TOTAL..oz..eq..`, `PF_SOY..oz..eq..`, `PF_EGGS..oz..eq..`, `PF_NUTSDS..oz..eq..`, `PF_LEGUMES..oz..eq..`, `PF_SEAFD_HI..oz..eq..`, `PF_SEAFD_LOW..oz..eq..`, `G_WHOLE..oz..eq..`, `D_TOTAL..cup.eq..`, `OILS..grams.`, `SOLID_FATS..grams.`, `G_REFINED..oz..eq..`, `ADD_SUGARS..tsp..eq..`)
fpid_2018 <- fpid_2018 %>% select(CODE, DESCRIPTION, `F_TOTAL..cup.eq..`, `F_CITMLB..cup.eq..`, `F_OTHER..cup.eq..`, `V_TOTAL..cup.eq..`, `V_LEGUMES..cup.eq..`, `V_DRKGR..cup.eq..`, `PF_MPS_TOTAL..oz..eq..`, `PF_SOY..oz..eq..`, `PF_EGGS..oz..eq..`, `PF_NUTSDS..oz..eq..`, `PF_LEGUMES..oz..eq..`, `PF_SEAFD_HI..oz..eq..`, `PF_SEAFD_LOW..oz..eq..`, `G_WHOLE..oz..eq..`, `D_TOTAL..cup.eq..`, `OILS..grams.`, `SOLID_FATS..grams.`, `G_REFINED..oz..eq..`, `ADD_SUGARS..tsp..eq..`)

fpid_all <- rbind(fpid_2006, fpid_2008, fpid_2010, fpid_2012, fpid_2014, fpid_2016, fpid_2018)
fpid_all <- fpid_all %>% distinct(CODE, .keep_all = TRUE)

# look for any unmatched codes from our NHANES data and the FPID database
matched_codes <- fpid_all %>% filter(CODE %in% codes$ingred_code)
unmatched <- codes %>% filter(!ingred_code %in% fpid_all$CODE) 

# 7 unmatched codes mostly RECUDED SODIUM products that were added after FPID was updated. Remove unmatched codes.
nhanes_matched <- nhanes %>% filter(!ingred_code %in% unmatched$ingred_code)

# Rename CODE to ingred_code for merging
colnames(fpid_all)[1] = 'ingred_code'

# Merage NHANES diet recalls with FPID
nhanes_fpid <- merge(nhanes_matched, fpid_all, by = 'ingred_code')

# Calculate HEI components by multipling the amount of an ingredients consumed by the FPID components. To prevent R from crashing, Sys.sleep for 10 seconds in between calculations.
nhanes_fpid$total_fruit = nhanes_fpid$Ingred_consumed_g * nhanes_fpid$F_TOTAL..cup.eq.. / 100
Sys.sleep(10)
nhanes_fpid$whole_fruit = (nhanes_fpid$Ingred_consumed_g * nhanes_fpid$F_CITMLB..cup.eq.. / 100 + nhanes_fpid$Ingred_consumed_g * nhanes_fpid$F_OTHER..cup.eq.. / 100)
Sys.sleep(10)
nhanes_fpid$total_vegetable = (nhanes_fpid$Ingred_consumed_g * nhanes_fpid$V_TOTAL..cup.eq.. / 100) 
Sys.sleep(10)
nhanes_fpid$green_and_bean = (nhanes_fpid$Ingred_consumed_g * nhanes_fpid$V_DRKGR..cup.eq.. / 100 + nhanes_fpid$Ingred_consumed_g * nhanes_fpid$V_LEGUMES..cup.eq.. / 100) 
Sys.sleep(10)
nhanes_fpid$meat_and_soy_protein = (nhanes_fpid$Ingred_consumed_g * nhanes_fpid$PF_MPS_TOTAL..oz..eq.. / 100 + nhanes_fpid$Ingred_consumed_g * nhanes_fpid$PF_SOY..oz..eq.. / 100) 
Sys.sleep(10)
nhanes_fpid$meat_nuts_seeds_and_soy_protein = (nhanes_fpid$meat_and_soy_protein + nhanes_fpid$Ingred_consumed_g * nhanes_fpid$PF_NUTSDS..oz..eq.. / 100)
Sys.sleep(10)
nhanes_fpid$meat_nuts_seeds_eggs_and_soy_protein = (nhanes_fpid$meat_nuts_seeds_and_soy_protein + nhanes_fpid$Ingred_consumed_g * nhanes_fpid$PF_EGGS..oz..eq.. / 100)
Sys.sleep(10)
nhanes_fpid$total_protein = (nhanes_fpid$meat_nuts_seeds_eggs_and_soy_protein + nhanes_fpid$Ingred_consumed_g * nhanes_fpid$PF_LEGUMES..oz..eq.. / 100)
Sys.sleep(10)
nhanes_fpid$seafood_protein = (nhanes_fpid$Ingred_consumed_g * nhanes_fpid$PF_SEAFD_HI..oz..eq.. / 100 + nhanes_fpid$Ingred_consumed_g * nhanes_fpid$PF_SEAFD_LOW..oz..eq.. / 100)
Sys.sleep(10)
nhanes_fpid$seafood_soy_protein = (nhanes_fpid$seafood_protein + nhanes_fpid$Ingred_consumed_g * nhanes_fpid$PF_SOY..oz..eq.. / 100)
Sys.sleep(10)
nhanes_fpid$seafood_soy_nuts_seeds_protein = (nhanes_fpid$seafood_soy_protein + nhanes_fpid$Ingred_consumed_g * nhanes_fpid$PF_NUTSDS..oz..eq.. / 100)
Sys.sleep(10)
nhanes_fpid$seafood_plant_protein = (nhanes_fpid$seafood_soy_nuts_seeds_protein + nhanes_fpid$Ingred_consumed_g * nhanes_fpid$PF_LEGUMES..oz..eq.. / 100)
Sys.sleep(10)
nhanes_fpid$whole_grain = nhanes_fpid$Ingred_consumed_g * nhanes_fpid$G_WHOLE..oz..eq.. / 100
Sys.sleep(10)
nhanes_fpid$total_dairy = nhanes_fpid$Ingred_consumed_g * nhanes_fpid$D_TOTAL..cup.eq.. / 100
Sys.sleep(10)
nhanes_fpid$refined_grain = nhanes_fpid$Ingred_consumed_g * nhanes_fpid$G_REFINED..oz..eq.. / 100
Sys.sleep(10)
nhanes_fpid$added_sugar = nhanes_fpid$Ingred_consumed_g * nhanes_fpid$ADD_SUGARS..tsp..eq.. / 100

# Summarize variables (Kcals, fatty acids and sodium are from NHANES data, not FPID components)
nhanes_hei <- nhanes_fpid %>% group_by(SEQN) %>% summarize(
  kcal = sum(Energy),
  MUFA = sum(`Fatty acids, total monounsaturated`),
  PUFA = sum(`Fatty acids, total polyunsaturated`),
  SFA = sum(`Fatty acids, total saturated`),
  sodium_mg = sum(Sodium),
  total_fruit = sum(total_fruit),
  whole_fruit = sum(whole_fruit),
  total_vegetable = sum(total_vegetable),
  green_and_bean = sum(green_and_bean),
  total_protein = sum(total_protein),
  seafood_plant_protein = sum(seafood_plant_protein),
  whole_grain = sum(whole_grain),
  total_dairy = sum(total_dairy),
  refined_grain = sum(refined_grain),
  added_sugar = sum(added_sugar)
)

# Calulate ratio (MUFA + PUFA / SFA) for HEI component
nhanes_hei$fatty_acid_ratio <- (nhanes_hei$MUFA + nhanes_hei$PUFA / nhanes_hei$SFA)

# Get Age variable for HEI scoring
n_age <- nhanes_fpid %>% distinct(SEQN, .keep_all = TRUE) %>% select(SEQN, Age) 
nhanes_hei <- merge(nhanes_hei, n_age, by = 'SEQN')

# convert sodium to grams from milligrams
nhanes_hei$sodium_g <- nhanes_hei$sodium_mg / 1000

# Calculate HEI-2020 score using dietaryindex package
hei_score <- HEI2020(
  SERV_DATA = nhanes_hei,
  RESPONDENTID = nhanes_hei$SEQN,
  AGE = nhanes_hei$Age,
  TOTALKCAL_HEI2020 = nhanes_hei$kcal,
  TOTALFRT_SERV_HEI2020 = nhanes_hei$total_fruit,
  FRT_SERV_HEI2020 = nhanes_hei$whole_fruit,
  VEG_SERV_HEI2020 = nhanes_hei$total_vegetable,
  GREENNBEAN_SERV_HEI2020 = nhanes_hei$green_and_bean,
  TOTALPRO_SERV_HEI2020 =  nhanes_hei$total_protein,
  SEAPLANTPRO_SERV_HEI2020 = nhanes_hei$seafood_plant_protein,
  WHOLEGRAIN_SERV_HEI2020 = nhanes_hei$whole_grain,
  DAIRY_SERV_HEI2020 = nhanes_hei$total_dairy,
  FATTYACID_SERV_HEI2020 = nhanes_hei$fatty_acid_ratio,
  REFINEDGRAIN_SERV_HEI2020 = nhanes_hei$refined_grain,
  SODIUM_SERV_HEI2020 = nhanes_hei$sodium_g,
  ADDEDSUGAR_SERV_HEI2020 = nhanes_hei$added_sugar,
  SATFAT_SERV_HEI2020 = nhanes_hei$SFA
)
colnames(hei_score)
# Extract HEI scores
hei_score <- hei_score %>% select(RESPONDENTID, HEI2020_SATFAT, HEI2020_SODIUM, HEI2020_SEAPLANTPRO, HEI2020_TOTALPRO, HEI2020_FATTYACID, HEI2020_ALL)
colnames(hei_score)[1] = 'SEQN'

# Combine HEI score with pulse intake data
pulse_hei <- merge(pulse_dga, hei_score, by = 'SEQN')


# N for each pulse intake group (based on DGA 2020 if meeting the DGA for beans, peas, lentils subgroup per calorie level [defined in previous script])
xtabs(~ met_dga, data = pulse_dga)

# Assumptions for GLM
pulse_hei %>% filter(met_dga == 'no') %>% summarise(mean = mean(HEI2020_ALL), sd = sd(HEI2020_ALL))
pulse_hei %>% filter(met_dga == 'yes') %>% summarise(mean = mean(HEI2020_ALL), sd = sd(HEI2020_ALL))
# equal variance across groups

# Create survey design
pulse_design <- merge(pulse_hei, nhanes_design, by = 'SEQN')
pulse_svy <- svydesign(data=pulse_design, id=~SDMVPSU, strata=~SDMVSTRA, weights=~diet_wts_combined, nest=TRUE)

## survey t-tests
# total HEI score
hei_test = svyttest(HEI2020_ALL ~ met_dga, design= pulse_svy)
hei_test$p.value

# Unsat fat to Sat fat ratio (The serving size of Dark Green Vegetables + Legumes (Beans and Peas) in cup equivalents, unit= cup eq.)
svyttest(HEI2020_FATTYACID ~ met_dga, design= pulse_svy)
boxplot(HEI2020_FATTYACID ~ met_dga, data = pulse_design)


# SATFAT
svyttest(HEI2020_SATFAT ~ met_dga, design= pulse_svy)
boxplot(HEI2020_SATFAT ~ met_dga, data = pulse_design) #(higher scores reflect lower intakes, because lower intakes are more desirable!)

# TOTALPRO
svyttest(HEI2020_TOTALPRO ~ met_dga, design= pulse_svy)
boxplot(HEI2020_TOTALPRO ~ met_dga, data = pulse_design)

# SODIUM
svyttest(HEI2020_SODIUM ~ met_dga, design= pulse_svy)
boxplot(HEI2020_SODIUM ~ met_dga, data = pulse_design)

# ANOVA is not part of the survey package, for more than two groups use GLM (svyglm) as it performs essentially as an ANOVA
#survey::regTermTest(model, ~ met_dga) # calculates overall difference between groups

# x-axis labels
labs <- c("DGA non-adherent", "DGA adherent")

hei_plot <- ggplot(data = pulse_design,
                   aes(
                     x = met_dga,
                     y = HEI2020_ALL,
                     fill = factor(met_dga),
                   )) +  geom_boxplot(notch = FALSE, outliers = FALSE) +
  #geom_point(position = position_jitter(), size=3, alpha=0.02) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  scale_x_discrete(labels= labs) +
  scale_fill_manual(values = c(
    "orange",
    "darkorange3"
  )) +
  annotate(
    "segment",
    x = 1,
    xend = 2,
    y = 88,
    yend = 88,
    linewidth = 0.5
  ) +
  annotate(
    "text",
    x = 1.5,
    y = 92,
    label = "p = 2.5e-55",
    size = 3,
    color = 'red',
  ) +
  labs(x = '', y = 'Total HEI-2020') +
  theme_bw() +
  theme(
    title = element_text(size = 10),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(
      size = 10,
      color = "black",
      vjust = 0.2,
    ),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.ticks.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.position = 'none'
  )
hei_plot


library(patchwork)

# Arrange the figures: fig1 on top, fig2 and fig3 on bottom
layout <- pulse_plot | (fiber_dga_plot / hei_plot)

# Display the combined figure
layout


ggsave('../../plot/figure_1/figure1.png', height = 6, width = 7.5, units = 'in', dpi = 300)
