import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.preprocessing import StandardScaler
from sklearn.feature_selection import VarianceThreshold
from sklearn.model_selection import train_test_split, GridSearchCV, RepeatedStratifiedKFold
from sklearn.ensemble import RandomForestClassifier
from sklearn.pipeline import Pipeline
from sklearn.compose import ColumnTransformer
from sklearn.metrics import roc_auc_score, confusion_matrix, classification_report, roc_curve, make_scorer, balanced_accuracy_score, average_precision_score, precision_recall_curve
import time
import warnings
import joblib
from pathlib import Path
warnings.filterwarnings('ignore')

# create output dir for ML model outputs
output_dir = Path('../../output/ml/ir/')
output_dir.mkdir(exist_ok=True, parents=True)

# Set random seed for reproducibility
np.random.seed(42)

# ============= DATA LOADING =============
# Load your data with SEQN as index
X = pd.read_csv('../../data/ir/ir_dfg2_intake.tsv', sep='\t', index_col='SEQN')
y = pd.read_csv('../../data/ir/ir_target.tsv', sep='\t', index_col='SEQN')['ir']

# Removing linkages from the model features
X = X.drop(columns=['t-Glucose', '4-Glucose', '6-Glucose', '2-Glucose',
       '4,6-Glucose', '3,4-Glucose', '2,4-Glucose', '3,4,6-Glucose',
       't-Galactose', '6-Galactose', '4-Galactose', '2-Galactose',
       '4,6-Galactose', '3,6-Galactose', '3,4-Galactose', 't-p-Xylose',
       '4-p-Xylose', '3-Xylose ', '2-Xylose', '2,4-p-Xylose', 't-f-Arabinose',
       't-p-Arabinose', '5-f-Arabinose', '3-Arabinose', '2-f-Arabinose',
       '2,3-f-Arabinose', 't-Fucose', 't-Rhamnose', '4-Rhamnose ',
       '2-Rhamnose', '2,4-Rhamnose', 't-Mannose', '4-Mannose', '3-Mannose',
       '2-Mannose', '4,6-Mannose', '2,X-Mannose', '3,4,6-Mannose'])

# Removing the digestible sugars, the focus is to determine what non-digestible carbohydrates are related to metabolic health
X = X.drop(columns=['Starch', 'Free_Fructose', 'Free_Glucose', 'Free_Sucrose',
       'Free_Maltose', 'Free_Lactose(trehalose)', 'Mono_Glucose'])

# Ensure X and y have the same subjects by aligning on index
common_indices = X.index.intersection(y.index)
X = X.loc[common_indices]
y = y.loc[common_indices]

# Clean up the X features and OHE the categorical features 
X = X.drop(columns=['diet_wts_combined', 'CYCLE'])
X = pd.get_dummies(X, columns= ['Sex', 'education', 'ethnicity', 'ever_smoker'], drop_first=True, dtype='int')

print(f"Dataset shape after alignment: {X.shape}")
print(f"Outcome distribution: {y.value_counts()}")
print(f"Feature set: {X.columns}")

# ============= SEPARATE COVARIATES BY TYPE =============
binary_covariates = [
    'Sex_Male', 'education_high school graduate or equivalent', 'education_less than high school graduate', 'education_some college', 'ethnicity_Non-Hispanic_Black', 'ethnicity_Non-Hispanic_White', 'ethnicity_Other_Hispanic', 'ethnicity_Other_Multi-Racial', 'ever_smoker_yes'
]

continuous_covariates = [
    'family_pir', 'Age', 'BMI'  # Replace with your actual continuous variables
]

# Verify we have all covariates
covariates = binary_covariates + continuous_covariates

# Separate dietary features
dietary_features = [col for col in X.columns if col not in covariates]

print(f"Number of binary covariates: {len(binary_covariates)}")
print(f"Number of continuous covariates: {len(continuous_covariates)}")
print(f"Number of dietary features: {len(dietary_features)}")

# ============= DATA SPLITTING =============
# Split data into training and test sets (80/20)
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, random_state=42, stratify=y
)
print(f"Training set shape: {X_train.shape}")
print(f"Test set shape: {X_test.shape}")

# ============= FEATURE VARIANCE ANALYSIS =============
start_time = time.time()
print("\nAnalyzing dietary feature variance distribution...")

# Calculate variance of dietary features only
dietary_variances = X_train[dietary_features].var(axis=0)
print(f"Dietary feature variance statistics: min={dietary_variances.min():.6f}, median={dietary_variances.median():.6f}, max={dietary_variances.max():.6f}")

# Check number of features retained at different thresholds
thresholds = [0]
retained_features = {}

for threshold in thresholds:
    selector = VarianceThreshold(threshold=threshold)
    selector.fit(X_train[dietary_features])
    n_retained = selector.get_support().sum()
    retained_features[threshold] = n_retained
    print(f"Threshold {threshold:.4f}: {n_retained} dietary features ({100*n_retained/len(dietary_features):.1f}%) remaining")

# Apply variance threshold (remove zero variance features)
print(f"\nApplying variance threshold of {thresholds[0]}...")
variance_selector = VarianceThreshold(threshold=thresholds[0])
variance_selector.fit(X_train)

# Get features that pass variance threshold
variance_features_mask = variance_selector.get_support()
variance_features = X_train.columns[variance_features_mask]

# Apply variance filtering to both train and test sets
X_train_variance = X_train[variance_features]
X_test_variance = X_test[variance_features]

removed_variance_features = set(X_train.columns) - set(variance_features)
if removed_variance_features:
    print(f"Removed {len(removed_variance_features)} zero/low variance features:")
    for feature in sorted(removed_variance_features):
        print(f"  - {feature}")
else:
    print("No zero/low variance features found.")

# ============= CORRELATION ANALYSIS =============
# Remove highly correlated features (95% threshold)
print("\nRemoving highly correlated features (correlation > 0.95)...")
corr_matrix = X_train_variance.corr().abs()

# Calculate correlation with target variable for tie-breaking
target_corr = X_train_variance.corrwith(y_train).abs()

# Find pairs of highly correlated features
high_corr_pairs = []
features_to_remove = set()
for i in range(len(corr_matrix.columns)):
    for j in range(i+1, len(corr_matrix.columns)):
        if corr_matrix.iloc[i, j] > 0.95:
            feature1 = corr_matrix.columns[i]
            feature2 = corr_matrix.columns[j]
            high_corr_pairs.append((feature1, feature2, corr_matrix.iloc[i, j]))
            
            # Remove the feature with higher mean correlation with other features
            mean_corr_1 = corr_matrix[feature1].drop(feature1).mean()
            mean_corr_2 = corr_matrix[feature2].drop(feature2).mean()
            
            if mean_corr_1 > mean_corr_2:
                features_to_remove.add(feature1)
            elif mean_corr_1 < mean_corr_2:
                features_to_remove.add(feature2)
            else:
                # Tie-breaker: keep the feature with higher correlation to target
                target_corr_1 = target_corr[feature1]
                target_corr_2 = target_corr[feature2]
                
                if target_corr_1 >= target_corr_2:
                    features_to_remove.add(feature2)
                else:
                    features_to_remove.add(feature1)

# Apply correlation filtering
final_features = [f for f in X_train_variance.columns if f not in features_to_remove]
X_train_final = X_train_variance[final_features]
X_test_final = X_test_variance[final_features]

# Print correlation removal results
if features_to_remove:
    print(f"\nRemoved {len(features_to_remove)} highly correlated features:")
    for feature in sorted(features_to_remove):
        target_corr_val = target_corr[feature]
        print(f"  - {feature} (target corr: {target_corr_val:.3f})")
    
    print(f"\nHigh correlation pairs (>0.95):")
    for feat1, feat2, corr_val in high_corr_pairs[:20]:  # Show first 20 pairs
        kept_feature = feat1 if feat2 in features_to_remove else feat2
        removed_feature = feat2 if feat2 in features_to_remove else feat1
        print(f"  {feat1} <-> {feat2}: {corr_val:.3f} (kept: {kept_feature})")
    if len(high_corr_pairs) > 20:
        print(f"  ... and {len(high_corr_pairs)-20} more pairs")
else:
    print("No features with correlation > 0.95 found.")

# Summary
print(f"\n============= FEATURE SELECTION SUMMARY =============")
print(f"Original features: {X_train.shape[1]}")
print(f"After variance threshold: {X_train_variance.shape[1]} (-{len(removed_variance_features)})")
print(f"After correlation removal: {X_train_final.shape[1]} (-{len(features_to_remove)})")
print(f"Total features removed: {X_train.shape[1] - X_train_final.shape[1]}")

# ============= PIPELINE FUNCTION =============
def create_model_pipeline():
    """Create a simple pipeline with just the classifier"""
    return Pipeline([
        ('classifier', RandomForestClassifier(
            random_state=42,
            n_jobs=-1,
        ))
    ])

# ============= GRID SEARCH =============
print("\nRunning grid search on pre-processed features...")

# Parameter grid for classifier (removed feature_selector since we did it manually)
param_grid = {
    'classifier__n_estimators': [100, 200, 300],
    'classifier__max_depth': [10, 15, 20, None],
    'classifier__min_samples_split': [2, 5, 10],
    'classifier__min_samples_leaf': [1, 2, 4],
    'classifier__max_features': ['sqrt', 'log2', None]
}

print(f"\nFeature selection summary before grid search:")
print(f"Final feature count: {X_train_final.shape[1]}")
print(f"Training samples: {X_train_final.shape[0]}")

# Set up cross validation
cv = RepeatedStratifiedKFold(
    n_splits=5,
    n_repeats=1,
    random_state=42
)

# Create pipeline 
pipeline = create_model_pipeline()

# Run grid search on the pre-processed data
print(f"\nStarting grid search with {len(param_grid['classifier__n_estimators']) * len(param_grid['classifier__max_depth']) * len(param_grid['classifier__min_samples_split']) * len(param_grid['classifier__min_samples_leaf']) * len(param_grid['classifier__max_features'])} parameter combinations...")

grid_search = GridSearchCV(
    pipeline,
    param_grid,
    cv=cv,
    scoring='roc_auc',
    n_jobs=-1,
    verbose=1,
    return_train_score=True
)

# Fit on the final processed training data
grid_search.fit(X_train_final, y_train)

# Store results
best_model_results = {
    'best_score': grid_search.best_score_,
    'best_params': grid_search.best_params_,
    'best_estimator': grid_search.best_estimator_,
    'cv_results': grid_search.cv_results_
}

print(f"\n=== BEST MODEL RESULTS ===")
print(f"Best cross-validation score: {best_model_results['best_score']:.4f}")
print(f"Best parameters: {best_model_results['best_params']}")

# Get the best model
best_model = grid_search.best_estimator_

print(f"\nBest model selected with CV score: {best_model_results['best_score']:.4f}")

# Execution time
print(f"Grid search completed in {(time.time() - start_time) / 60:.2f} minutes")

# ============= MODEL EVALUATION =============
# Evaluate on test set using the final processed features
y_pred_proba = best_model.predict_proba(X_test_final)[:, 1]
y_pred = best_model.predict(X_test_final)

# Calculate test metrics
test_auc = roc_auc_score(y_test, y_pred_proba)
test_pr_auc = average_precision_score(y_test, y_pred_proba)
test_balanced_acc = balanced_accuracy_score(y_test, y_pred)

print(f"\n=== FINAL TEST SET RESULTS ===")
print(f"Test ROC AUC: {test_auc:.4f}")
print(f"Test PR AUC: {test_pr_auc:.4f}")
print(f"Test Balanced Accuracy: {test_balanced_acc:.4f}")

# Print classification report for detailed metrics
print(f"\nClassification Report:")
print(classification_report(y_test, y_pred, target_names=['Non-IR', 'IR']))

# Print confusion matrix
print(f"\nConfusion Matrix:")
cm = confusion_matrix(y_test, y_pred)
print(cm)
print(f"True Negatives: {cm[0,0]}, False Positives: {cm[0,1]}")
print(f"False Negatives: {cm[1,0]}, True Positives: {cm[1,1]}")

# Plot ROC curve
fpr, tpr, _ = roc_curve(y_test, y_pred_proba)
plt.figure(figsize=(12, 5))

plt.subplot(1, 2, 1)
plt.plot(fpr, tpr, label=f'ROC AUC = {test_auc:.4f}')
plt.plot([0, 1], [0, 1], 'k--')
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curve')
plt.legend()
plt.grid(alpha=0.3)

# Plot Precision-Recall curve
precision, recall, _ = precision_recall_curve(y_test, y_pred_proba)
plt.subplot(1, 2, 2)
plt.plot(recall, precision, label=f'PR AUC = {test_pr_auc:.4f}')
plt.xlabel('Recall (Sensitivity)')
plt.ylabel('Precision')
plt.title('Precision-Recall Curve')
plt.legend()
plt.grid(alpha=0.3)

plt.tight_layout()
plt.savefig('../../output/ml/ir/roc_and_pr_curves.png', dpi=300)
plt.close()

# ============= SAVE MODEL FOR SHAP ANALYSIS =============
# Save the model and data for later SHAP analysis
joblib.dump(best_model_results, '../../output/ml/ir/best_rf_model_results.pkl')
joblib.dump(best_model, '../../output/ml/ir/best_rf_model.pkl')
joblib.dump(X_test_final, '../../output/ml/ir/X_test.pkl')
joblib.dump(y_test, '../../output/ml/ir/y_test.pkl')
X_all = pd.concat([X_test_final, X_train_final])
joblib.dump(X_all, '../../output/ml/ir/X_all.pkl')

print("\nAnalysis complete! Model and data saved for SHAP analysis.")