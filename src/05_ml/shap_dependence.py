#!/usr/bin/env python
# coding: utf-8

# load packages
import joblib
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import shap
import os
from statsmodels.nonparametric.smoothers_lowess import lowess
from pathlib import Path

# Set up output directory for plots
output_dir = Path('../../output/figure_5')
output_dir.mkdir(exist_ok=True, parents=True)

# get SHAP values for each dataset

### CRP
# SHAP Feature Importance Analysis for CRP Prediction with Random Forest model_crp

# Load the model_crp and data
print("Loading model_crp and data...")
model_crp = joblib.load('../../output/ml/crp/best_rf_model.pkl')
X_test_crp = joblib.load('../../output/ml/crp/X_test.pkl')

# Extract preprocessor and classifier from the pipeline
# Check if the model_crp is a pipeline
if hasattr(model_crp, 'steps'):
    print("model_crp is a pipeline. Extracting components...")
    preprocessor = model_crp.named_steps['preprocessor'] if 'preprocessor' in model_crp.named_steps else model_crp.steps[0][1]
    classifier = model_crp.named_steps['classifier'] if 'classifier' in model_crp.named_steps else model_crp.steps[-1][1]
    print(f"Extracted preprocessor and classifier: {type(classifier).__name__}")
else:
    print("model_crp is not a pipeline. Using as is...")
    preprocessor = None
    classifier = model_crp

# ============= EXTRACT FEATURE NAMES =============
print("\nExtracting feature names after preprocessing...")
feature_names_crp = X_test_crp.columns

print(f"Final shape of test data for SHAP analysis: {X_test_crp.shape}")
print(f"Number of features for SHAP analysis: {len(feature_names_crp)}")

# Create the TreeExplainer for Random Forest model_crps
print("Creating SHAP TreeExplainer...")
# Use the classifier component rather than the full pipeline
explainer = shap.TreeExplainer(classifier)

# Calculate SHAP values
print("Calculating SHAP values...")
# Use transformed data for SHAP calculations
shap_values_crp = explainer.shap_values(X_test_crp)

# Get the expected value
expected_value = explainer.expected_value
print(f"Expected value type: {type(expected_value)}")
if isinstance(expected_value, list):
    print(f"Expected value is a list with {len(expected_value)} elements")
    expected_value_for_analysis = expected_value[1]  # Use positive class
    print(f"Using positive class expected value: {expected_value_for_analysis}")
elif isinstance(expected_value, np.ndarray) and len(expected_value.shape) >= 1:
    expected_value_for_analysis = expected_value[1]  # Use positive class
    print(f"Using positive class expected value: {expected_value_for_analysis}")
else:
    expected_value_for_analysis = expected_value
    print(f"Using expected value: {expected_value_for_analysis}")

# Calculate mean absolute SHAP values for the positive class
print("\nCalculating feature importance...")

if isinstance(shap_values_crp, list) and len(shap_values_crp) == 2:
    # Use positive class SHAP values
    mean_abs_shap_crp = np.abs(shap_values_crp[1]).mean(axis=0)
elif shap_values_crp.ndim == 3:
    # If 3D array, use positive class
    mean_abs_shap_crp = np.abs(shap_values_crp[:, :, 1]).mean(axis=0)
else:
    mean_abs_shap_crp = np.abs(shap_values_crp).mean(axis=0)

# Create DataFrame with feature importance
feature_importance_crp = pd.DataFrame({
    'feature': feature_names_crp,
    'importance': mean_abs_shap_crp
})
feature_importance_crp = feature_importance_crp.sort_values('importance', ascending=False)

### IR
# SHAP Feature Importance Analysis for IR Prediction with Random Forest model_ir

# Load the model_ir and data
print("Loading model_ir and data...")
model_ir = joblib.load('../../output/ml/ir/best_rf_model.pkl')
X_test_ir = joblib.load('../../output/ml/ir/X_test.pkl')

# Extract preprocessor and classifier from the pipeline
# Check if the model_ir is a pipeline
if hasattr(model_ir, 'steps'):
    print("model_ir is a pipeline. Extracting components...")
    preprocessor = model_ir.named_steps['preprocessor'] if 'preprocessor' in model_ir.named_steps else model_ir.steps[0][1]
    classifier = model_ir.named_steps['classifier'] if 'classifier' in model_ir.named_steps else model_ir.steps[-1][1]
    print(f"Extracted preprocessor and classifier: {type(classifier).__name__}")
else:
    print("model_ir is not a pipeline. Using as is...")
    preprocessor = None
    classifier = model_ir

# ============= EXTRACT FEATURE NAMES =============
print("\nExtracting feature names after preprocessing...")
feature_names_ir = X_test_ir.columns

print(f"Final shape of test data for SHAP analysis: {X_test_ir.shape}")
print(f"Number of features for SHAP analysis: {len(feature_names_ir)}")

# Create the TreeExplainer for Random Forest model_irs
print("Creating SHAP TreeExplainer...")
# Use the classifier component rather than the full pipeline
explainer = shap.TreeExplainer(classifier)

# Calculate SHAP values
print("Calculating SHAP values...")
# Use transformed data for SHAP calculations
shap_values_ir = explainer.shap_values(X_test_ir)

# Get the expected value
expected_value = explainer.expected_value
print(f"Expected value type: {type(expected_value)}")
if isinstance(expected_value, list):
    print(f"Expected value is a list with {len(expected_value)} elements")
    expected_value_for_analysis = expected_value[1]  # Use positive class
    print(f"Using positive class expected value: {expected_value_for_analysis}")
elif isinstance(expected_value, np.ndarray) and len(expected_value.shape) >= 1:
    expected_value_for_analysis = expected_value[1]  # Use positive class
    print(f"Using positive class expected value: {expected_value_for_analysis}")
else:
    expected_value_for_analysis = expected_value
    print(f"Using expected value: {expected_value_for_analysis}")

# Calculate mean absolute SHAP values for the positive class
print("\nCalculating feature importance...")

if isinstance(shap_values_ir, list) and len(shap_values_ir) == 2:
    # Use positive class SHAP values
    mean_abs_shap_ir = np.abs(shap_values_ir[1]).mean(axis=0)
elif shap_values_ir.ndim == 3:
    # If 3D array, use positive class
    mean_abs_shap_ir = np.abs(shap_values_ir[:, :, 1]).mean(axis=0)
else:
    mean_abs_shap_ir = np.abs(shap_values_ir).mean(axis=0)

# Create DataFrame with feature importance
feature_importance_ir = pd.DataFrame({
    'feature': feature_names_ir,
    'importance': mean_abs_shap_ir
})
feature_importance_ir = feature_importance_ir.sort_values('importance', ascending=False)


#### MetS

# SHAP Feature Importance Analysis for MetS Prediction with Random Forest model_mets

# Load the model_mets and data
print("Loading model_mets and data...")
model_mets = joblib.load('../../output/ml/mets/best_rf_model.pkl')
X_test_mets = joblib.load('../../output/ml/mets/X_test.pkl')

# Extract preprocessor and classifier from the pipeline
# Check if the model_mets is a pipeline
if hasattr(model_mets, 'steps'):
    print("model_mets is a pipeline. Extracting components...")
    preprocessor = model_mets.named_steps['preprocessor'] if 'preprocessor' in model_mets.named_steps else model_mets.steps[0][1]
    classifier = model_mets.named_steps['classifier'] if 'classifier' in model_mets.named_steps else model_mets.steps[-1][1]
    print(f"Extracted preprocessor and classifier: {type(classifier).__name__}")
else:
    print("model_mets is not a pipeline. Using as is...")
    preprocessor = None
    classifier = model_mets

# ============= EXTRACT FEATURE NAMES =============
print("\nExtracting feature names after preprocessing...")
feature_names_mets = X_test_mets.columns

print(f"Final shape of test data for SHAP analysis: {X_test_mets.shape}")
print(f"Number of features for SHAP analysis: {len(feature_names_mets)}")

# Create the TreeExplainer for Random Forest model_metss
print("Creating SHAP TreeExplainer...")
# Use the classifier component rather than the full pipeline
explainer = shap.TreeExplainer(classifier)

# Calculate SHAP values
print("Calculating SHAP values...")
# Use transformed data for SHAP calculations
shap_values_mets = explainer.shap_values(X_test_mets)

# Get the expected value
expected_value = explainer.expected_value
print(f"Expected value type: {type(expected_value)}")
if isinstance(expected_value, list):
    print(f"Expected value is a list with {len(expected_value)} elements")
    expected_value_for_analysis = expected_value[1]  # Use positive class
    print(f"Using positive class expected value: {expected_value_for_analysis}")
elif isinstance(expected_value, np.ndarray) and len(expected_value.shape) >= 1:
    expected_value_for_analysis = expected_value[1]  # Use positive class
    print(f"Using positive class expected value: {expected_value_for_analysis}")
else:
    expected_value_for_analysis = expected_value
    print(f"Using expected value: {expected_value_for_analysis}")

# Calculate mean absolute SHAP values for the positive class
print("\nCalculating feature importance...")

if isinstance(shap_values_mets, list) and len(shap_values_mets) == 2:
    # Use positive class SHAP values
    mean_abs_shap_mets = np.abs(shap_values_mets[1]).mean(axis=0)
elif shap_values_mets.ndim == 3:
    # If 3D array, use positive class
    mean_abs_shap_mets = np.abs(shap_values_mets[:, :, 1]).mean(axis=0)
else:
    mean_abs_shap_mets = np.abs(shap_values_mets).mean(axis=0)

# Create DataFrame with feature importance
feature_importance_mets = pd.DataFrame({
    'feature': feature_names_mets,
    'importance': mean_abs_shap_mets
})
feature_importance_mets = feature_importance_mets.sort_values('importance', ascending=False)

# Create lists for the features that ranked higher than dietary fiber, these will be bolded in the figures
ir_feature_plot = ['Raffinose', 'Verbascose', 'Mannose']
mets_feature_plot = ['Raffinose', 'Stachyose', 'Mannose']
crp_feature_plot = ['Raffinose']

# clean the feature name text for plotting
X_test_mets.columns = [col.replace('Free_', '') for col in X_test_mets.columns]
X_test_mets.columns = [col.replace('Mono_', '') for col in X_test_mets.columns]
X_test_crp.columns = [col.replace('Free_', '') for col in X_test_crp.columns]
X_test_crp.columns = [col.replace('Mono_', '') for col in X_test_crp.columns]
X_test_ir.columns = [col.replace('Free_', '') for col in X_test_ir.columns]
X_test_ir.columns = [col.replace('Mono_', '') for col in X_test_ir.columns]

print("Columns in X_test_mets:", list(X_test_mets.columns))
print("Features to plot:", mets_feature_plot)
print("Missing features:", [f for f in mets_feature_plot if f not in X_test_mets.columns])

# Dataset configurations
datasets = [
    {'name': 'IR', 'shap_values': shap_values_ir, 'X_test': X_test_ir, 'features': ir_feature_plot},
    {'name': 'MetS', 'shap_values': shap_values_mets, 'X_test': X_test_mets, 'features': mets_feature_plot},
    {'name': 'CRP', 'shap_values': shap_values_crp, 'X_test': X_test_crp, 'features': crp_feature_plot}
]

# Create figure with 5 rows, 3 columns (15 total subplots for 7+7+1=15 plots)
fig, axes = plt.subplots(5, 3, figsize=(15, 20))
axes = axes.flatten()

plot_idx = 0
for dataset in datasets:
    for feature in dataset['features']:
        ax = axes[plot_idx]
        
        # Create the SHAP dependence plot
        shap.dependence_plot(feature, dataset['shap_values'][:, :, 1], 
                           dataset['X_test'], 
                           interaction_index=None,
                           show=False, ax=ax)
        
        # Add dataset label to title
        ax.set_title(f"{dataset['name']}", fontsize=10)
        
        # Add horizontal line at y=0
        ax.axhline(y=0, color='gray', linestyle='--', linewidth=1, alpha=0.5, zorder=1)

        # Append unit to x-axis label
        current_xlabel = ax.get_xlabel()
        ax.set_xlabel(f"{current_xlabel} (g per 1000 kcal per day)")
        
        # Get values for LOWESS
        feature_values = dataset['X_test'][feature].values
        shap_vals = dataset['shap_values'][:, :, 1][:, dataset['X_test'].columns.get_loc(feature)]
        
        plot_idx += 1

# Hide unused subplots (should be 0 with 15 plots in 15 spaces)
for idx in range(plot_idx, len(axes)):
    axes[idx].set_visible(False)

# Remove extra colorbars (shap.dependence_plot creates colorbars)
# Keep only the subplot axes, remove colorbar axes
original_axes_count = len(axes)
for i in range(len(fig.axes)-1, original_axes_count-1, -1):
    if i >= original_axes_count:
        fig.axes[i].remove()

plt.tight_layout()
plt.savefig(output_dir / 'figure_5.png', 
            dpi=300, bbox_inches='tight')

# plot for supplemental of features that ranked higher than fiber and pulse-enriched, but not in the top 15 shown in the SHAP plot (fig 4)
ir_feature_plot = ['Stachyose', 'Arabinose', 'Fucose', 'Galactose']
mets_feature_plot = ['Verbascose', 'Arabinose', 'Fucose', 'Galactose']
# no features for crp that pulse-enriched/outranking fiber not aren't displayed in fig 4.

# Dataset configurations
datasets = [
    {'name': 'IR', 'shap_values': shap_values_ir, 'X_test': X_test_ir, 'features': ir_feature_plot},
    {'name': 'MetS', 'shap_values': shap_values_mets, 'X_test': X_test_mets, 'features': mets_feature_plot}
]

# Create figure with 2 rows, 4 columns (8 total subplots)
fig, axes = plt.subplots(2, 4, figsize=(18, 10))
axes = axes.flatten()

plot_idx = 0
for dataset in datasets:
    for feature in dataset['features']:
        ax = axes[plot_idx]
        
        # Create the SHAP dependence plot
        shap.dependence_plot(feature, dataset['shap_values'][:, :, 1], 
                           dataset['X_test'], 
                           interaction_index=None,
                           show=False, ax=ax)
        
        # Add dataset label to title
        ax.set_title(f"{dataset['name']}", fontsize=10)
        
        # Add horizontal line at y=0
        ax.axhline(y=0, color='gray', linestyle='--', linewidth=1, alpha=0.5, zorder=1)

        # Append unit to x-axis label
        current_xlabel = ax.get_xlabel()
        ax.set_xlabel(f"{current_xlabel} (g per 1000 kcal per day)")
        
        # Get values for LOWESS
        feature_values = dataset['X_test'][feature].values
        shap_vals = dataset['shap_values'][:, :, 1][:, dataset['X_test'].columns.get_loc(feature)]
        
        plot_idx += 1

# Hide unused subplots (should be 0 with 15 plots in 15 spaces)
for idx in range(plot_idx, len(axes)):
    axes[idx].set_visible(False)

# Remove extra colorbars (shap.dependence_plot creates colorbars)
# Keep only the subplot axes, remove colorbar axes
original_axes_count = len(axes)
for i in range(len(fig.axes)-1, original_axes_count-1, -1):
    if i >= original_axes_count:
        fig.axes[i].remove()

plt.tight_layout()
plt.savefig(output_dir / 'supplemental_figure_2.png', 
            dpi=300, bbox_inches='tight')