#!/usr/bin/env python
# coding: utf-8

### plot combined roc-auc curves with overlaid lines

# import packages
import joblib
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.metrics import roc_curve, roc_auc_score
from pathlib import Path

# create output directory
output_dir = Path('../../plot')
output_dir.mkdir(exist_ok=True, parents=True)

# Set font sizes
plt.rcParams['font.size'] = 10
plt.rcParams['axes.labelsize'] = 10
plt.rcParams['axes.titlesize'] = 11
plt.rcParams['xtick.labelsize'] = 10
plt.rcParams['ytick.labelsize'] = 10
plt.rcParams['legend.fontsize'] = 10

# Create single combined figure
plt.figure(figsize=(6, 5), dpi=300)

conditions = [
    ('../../output/04/crp',  'C-reactive protein'),
    ('../../output/04/ir',   'Insulin resistance'),
    ('../../output/04/mets', 'Metabolic syndrome')
]

for path, label in conditions:
    model  = joblib.load(f'{path}/best_rf_model.pkl')
    y_test = joblib.load(f'{path}/y_test.pkl')
    X_test = joblib.load(f'{path}/X_test.pkl')

    y_pred_proba = model.predict_proba(X_test)[:, 1]
    test_auc = roc_auc_score(y_test, y_pred_proba)
    fpr, tpr, _ = roc_curve(y_test, y_pred_proba)

    plt.plot(fpr, tpr, linewidth=2, alpha=0.7, label=f'{label} (AUC = {test_auc:.4f})')
    print(f"{label} Test ROC-AUC = {test_auc:.4f}")

plt.plot([0, 1], [0, 1], 'k--', linewidth=1.5)
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.legend()
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig(output_dir / 'figure3.png', dpi=300, bbox_inches='tight')
plt.close()

print("Combined ROC curve saved.")
