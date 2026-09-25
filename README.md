# pulse_nhanes

This repository contains the anyltic code and data needed to reproduce the results and visualizations for the manuscript *Pulse‑derived dietary carbohydrates associated with metabolic health in NHANES 2003–2023*

## Use and Requirements

### Prerequisites

- Anaconda or Miniconda
- Git
- R

### Installation
1. Clone the repository
```
git clone https://github.com/jalarke/pulse_nhanes.git
cd pulse_nhanes
```
2. Create the conda environment and activate the environment
```
conda env create -f environment.yml
conda activate pulse_nhanes
```
### Execute the code
1. Navigate to the first scipt and launch jupyter lab
```
cd src/00_dfg_mapping/
jupyter lab
```
2. Run the notebooks (and R scripts) in each folder in order from 00 to 05
- 00_dfg_mapping - download NHANES data, diet data cleaning, and mapping to nutrients and glycan database
- 01_build_datasets - add person specific covariates and outcomes, glycan intake calculations, pulse group definitions, and further preprocessing steps
- 02_pulse_intake_hei_scores - figure1.R creates plots for figure 1. tables_1and2.R generates data for tables 1 and 2.
- 03_glycan_profiles_intakes - create the plots for figure 2, and table 3.
- 04_ml - machine learning (random forest) code for each of the three outcomes. plot_curves_combined.py is run after to combine ROC-AUC curves into single plot
- 05_shap - SHAP analysis and visualizations. First run scripts to calculate SHAP values for each outcomes and then plot_importance_by_type.py and shap_dependence.py
