#!/usr/bin/env python
# coding: utf-8

# import packages
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path

# create output dir
output_dir = Path('../../plot/figure_s2')
output_dir.mkdir(exist_ok=True, parents=True)

# create list of the pulse enriched glycans
pulse_glycans = ['Free_Stachyose',
                 'Free_Verbascose',
                 'Free_Raffinose',
                 'Mono_Arabinose',
                 'Mono_Mannose',
                 'Mono_Fucose',
                 'Mono_Galactose']

#### CRP
crp_imp = pd.read_csv('../../output/ml/crp/shap_analysis/shap_feature_importance.csv')
crp_covariates = [
    'Sex_Male', 'education_high school graduate or equivalent', 'education_less than high school graduate', 'education_some college', 'ethnicity_Non-Hispanic_Black', 'ethnicity_Non-Hispanic_White', 'ethnicity_Other_Hispanic', 'ethnicity_Other_Multi-Racial', 'ever_smoker_yes', 'family_pir', 'Age', 'BMI'
]
crp_imp['type'] = np.where(crp_imp['feature'].isin(crp_covariates), 'Covariate', 'Glycan')
crp_imp['type'] = np.where(crp_imp['feature']=='Fiber, total dietary', 'Dietary Fiber', crp_imp['type'])
crp_imp['type'] = np.where(crp_imp['feature'].isin(pulse_glycans), 
                             'Pulse enriched glycan', 
                             crp_imp['type'])


#### IR
ir_imp = pd.read_csv('../../output/ml/ir/shap_analysis/shap_feature_importance.csv')
ir_covariates = [
    'Sex_Male', 'education_high school graduate or equivalent', 'education_less than high school graduate', 'education_some college', 'ethnicity_Non-Hispanic_Black', 'ethnicity_Non-Hispanic_White', 'ethnicity_Other_Hispanic', 'ethnicity_Other_Multi-Racial', 'ever_smoker_yes', 'family_pir', 'Age', 'BMI'
]
ir_imp['type'] = np.where(ir_imp['feature'].isin(ir_covariates), 'Covariate', 'Glycan')
ir_imp['type'] = np.where(ir_imp['feature']=='Fiber, total dietary', 'Dietary Fiber', ir_imp['type'])
ir_imp['type'] = np.where(ir_imp['feature'].isin(pulse_glycans), 
                             'Pulse enriched glycan', 
                             ir_imp['type'])

#### MetS
mets_imp = pd.read_csv('../../output/ml/mets/shap_analysis/shap_feature_importance.csv')
mets_covariates = [
    'Sex_Male', 'education_high school graduate or equivalent', 'education_less than high school graduate', 'education_some college', 'ethnicity_Non-Hispanic_Black', 'ethnicity_Non-Hispanic_White', 'ethnicity_Other_Hispanic', 'ethnicity_Other_Multi-Racial', 'ever_smoker_yes', 'family_pir', 'Age', 'BMI'
]
mets_imp['type'] = np.where(mets_imp['feature'].isin(mets_covariates), 'Covariate', 'Glycan')
mets_imp['type'] = np.where(mets_imp['feature']=='Fiber, total dietary', 'Dietary Fiber', mets_imp['type'])
mets_imp['type'] = np.where(mets_imp['feature'].isin(pulse_glycans), 
                             'Pulse enriched glycan', 
                             mets_imp['type'])

## plot the importances for each dataset

# Add a dataset identifier to each dataframe
mets_imp['dataset'] = 'Metabolic syndrome'
crp_imp['dataset'] = 'C-reactive protein'
ir_imp['dataset'] = 'Insulin resistance'

# Combine all datasets
combined = pd.concat([mets_imp, crp_imp, ir_imp], ignore_index=True)

# Group by dataset and type, sum the importance
grouped = combined.groupby(['dataset', 'type'])['importance'].sum().unstack(fill_value=0)

# Create the stacked bar plot
fig, ax = plt.subplots(figsize=(10, 6))
grouped.plot(kind='bar', stacked=True, ax=ax, width=0.7, edgecolor='black')

# Define custom colors for each type
colors = {
    'Covariate': '#1f77b4',
    'Pulse enriched glycan': '#ff7f0e',
    'Other Glycan': '#2ca02c',
    'Fiber': '#d62728'
}

ax.set_xlabel('Dataset', fontsize=12)
ax.set_ylabel('Sum of |SHAP| values', fontsize=12)
ax.set_title('', fontsize=14)
ax.legend(title='Feature Type', bbox_to_anchor=(1.05, 1), loc='upper left')
plt.xticks(rotation=0)
plt.tight_layout()
plt.savefig('../../plot/figure_s2/absolute_feature_importance_type.png', dpi=300, bbox_inches='tight')
plt.show()


# Convert to relative abundance as percentages for plotting
ir_imp['rel_imp'] = (ir_imp['importance'] / ir_imp['importance'].sum()) * 100
crp_imp['rel_imp'] = (crp_imp['importance'] / crp_imp['importance'].sum()) * 100
mets_imp['rel_imp'] = (mets_imp['importance'] / mets_imp['importance'].sum()) * 100

# Add a dataset identifier to each dataframe
mets_imp['dataset'] = 'Metabolic syndrome'
crp_imp['dataset'] = 'C-reactive protein'
ir_imp['dataset'] = 'Insulin resistance'

# Combine all datasets
combined = pd.concat([mets_imp, crp_imp, ir_imp], ignore_index=True)

# Group by dataset and type, sum the importance
grouped = combined.groupby(['dataset', 'type'])['rel_imp'].sum().unstack(fill_value=0)

# Create the stacked bar plot
fig, ax = plt.subplots(figsize=(10, 6))
grouped.plot(kind='bar', stacked=True, ax=ax, width=0.7, edgecolor='black')

# Define custom colors for each type
colors = {
    'Covariate': '#1f77b4',
    'Pulse enriched glycan': '#ff7f0e',
    'Other Glycan': '#2ca02c',
    'Fiber': '#d62728'
}

ax.set_xlabel('Dataset', fontsize=12)
ax.set_ylabel('SHAP importance (%)', fontsize=12)
ax.set_title('', fontsize=14)
ax.legend(title='Feature Type', bbox_to_anchor=(1.05, 1), loc='upper left')
plt.xticks(rotation=0)
plt.tight_layout()
plt.savefig('../../plot/figure_s2/relative_feature_importance_type.png', dpi=300, bbox_inches='tight')
plt.show()