#!/usr/bin/env python
# coding: utf-8

# SHAP Feature Importance Analysis for IR Prediction with Random Forest Model

import joblib
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import shap
from pathlib import Path

# Set up output directory for plots
output_dir = Path('../../output/05/shap/ir')
output_dir.mkdir(exist_ok=True, parents=True)

# Load the model and data
print("Loading model and data...")
model = joblib.load('../../output/04/ir/best_rf_model.pkl')
X_test = joblib.load('../../output/04/ir/X_test.pkl')

# Extract preprocessor and classifier from the pipeline
# Check if the model is a pipeline
if hasattr(model, 'steps'):
    print("Model is a pipeline. Extracting components...")
    preprocessor = model.named_steps['preprocessor'] if 'preprocessor' in model.named_steps else model.steps[0][1]
    classifier = model.named_steps['classifier'] if 'classifier' in model.named_steps else model.steps[-1][1]
    print(f"Extracted preprocessor and classifier: {type(classifier).__name__}")
else:
    print("Model is not a pipeline. Using as is...")
    preprocessor = None
    classifier = model

# ============= EXTRACT FEATURE NAMES =============
print("\nExtracting feature names after preprocessing...")
feature_names = X_test.columns

print(f"Final shape of test data for SHAP analysis: {X_test.shape}")
print(f"Number of features for SHAP analysis: {len(feature_names)}")

# Create the TreeExplainer for Random Forest models
print("Creating SHAP TreeExplainer...")
# Use the classifier component rather than the full pipeline
explainer = shap.TreeExplainer(classifier)

# Calculate SHAP values
print("Calculating SHAP values...")
# Use transformed data for SHAP calculations
shap_values = explainer.shap_values(X_test)

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

#Calculate mean absolute SHAP values for the positive class
print("\nCalculating feature importance...")

if isinstance(shap_values, list) and len(shap_values) == 2:
    # Use positive class SHAP values
    mean_abs_shap = np.abs(shap_values[1]).mean(axis=0)
elif shap_values.ndim == 3:
    # If 3D array, use positive class
    mean_abs_shap = np.abs(shap_values[:, :, 1]).mean(axis=0)
else:
    mean_abs_shap = np.abs(shap_values).mean(axis=0)

# Create DataFrame with feature importance
feature_importance = pd.DataFrame({
    'feature': feature_names,
    'importance': mean_abs_shap
})
feature_importance = feature_importance.sort_values('importance', ascending=False)

# Save feature importance to CSV
feature_importance.to_csv(output_dir / 'shap_feature_importance.csv', index=False)

print(f"SHAP analysis completed. Results saved to {output_dir}")

# Summary plot with distribution for positive class
print("Generating summary plot with distribution...")
plt.figure(figsize=(10, 4.5))
X_test.columns = [col.replace('Free_', '') for col in X_test.columns]
X_test.columns = [col.replace('Mono_', 'Monomeric ') for col in X_test.columns]

# For binary classification, use shap_values for the positive class (index 1)
if isinstance(shap_values, list) and len(shap_values) == 2:
    shap.summary_plot(
        shap_values[1], 
        X_test, 
        show=False,
        alpha=0.5
    )
elif shap_values.ndim == 3:
    shap.summary_plot(
        shap_values[:, :, 1], 
        X_test, 
        max_display=15, 
        show=False,
        alpha=0.5
    )
else:
    shap.summary_plot(
        shap_values, 
        X_test, 
        show=False,
        alpha=0.5
    )

# Get axes and set all font sizes to 10
ax = plt.gca()

# Set tick label font sizes
ax.tick_params(axis='both', labelsize=10)

# Set axis label font sizes (for colorbar if present)
ax.xaxis.label.set_fontsize(10)
ax.yaxis.label.set_fontsize(10)

# Manually adjust point sizes
for collection in ax.collections:
    collection.set_sizes([4])

# Custom x-axis label with two lines
ax.set_xlabel('')  # Remove default label

# Line 1: SHAP value (changed to fontsize 10)
ax.text(0.4, -0.1, 'SHAP value', 
        ha='center', va='top', transform=ax.transAxes, fontsize=10)

# Line 2: Text on either side of x=0
ax.text(-0.01, -0.17, '← Predictive of no IR', 
        ha='right', va='top', transform=ax.get_xaxis_transform(), fontsize=10)
ax.text(0.01, -0.17, 'Predictive of IR →', 
        ha='left', va='top', transform=ax.get_xaxis_transform(), fontsize=10)

# Bold specific y-axis labels
y_labels = ax.get_yticklabels()
features_to_bold = ['Raffinose', 'Verbascose', 'Stachyose', 'Monomeric Mannose', 'Monomeric Fucose', 'Monomeric Galactose', 'Monomeric Arabinose']

for label in y_labels:
    if label.get_text() in features_to_bold:
        label.set_fontweight('bold')
    label.set_fontsize(10)  # Ensure font size is 10

# Change plot size
print(f'Original size: {plt.gcf().get_size_inches()}')
plt.gcf().set_size_inches(8, 3.5)
plt.tight_layout()
print(f'New size: {plt.gcf().get_size_inches()}')

plt.savefig('../../plot/figure4a.png', dpi=300, bbox_inches='tight')
plt.close()