import streamlit as st
import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import classification_report, confusion_matrix, accuracy_score

df4 = pd.read_csv('ufcdata.csv')

st.header("Faraz UFC Prediction APP")

nameoptions = df4['Fighter_1'].unique().tolist()

with st.sidebar:
    st.header("Fighter Selection")
    
    # Add placeholder options
    nameoptions_with_placeholder = ["-- Select a fighter --"] + nameoptions
    
    fighter1_name = st.selectbox("Pick Fighter 1", options=nameoptions_with_placeholder, key="fighter1")
    fighter2_name = st.selectbox("Pick Fighter 2", options=nameoptions_with_placeholder, key="fighter2")
    
    st.divider()
    st.caption("Select two fighters to compare")
    
    # Add validation
    if fighter1_name == "-- Select a fighter --" or fighter2_name == "-- Select a fighter --":
        st.warning("⚠️ Please select both fighters")
    elif fighter1_name == fighter2_name:
        st.warning("⚠️ Please select two different fighters")


# Prepare features (X) and target (y)
columns_to_drop = ['Winner1', 'Fighter_1', 'Fighter_2', 'Winner', 'Method', 
                   'Event_Date', 'Time_Format', 'Weight_Class']  # Add your columns here
X = df4.drop(columns_to_drop, axis=1)

y = df4['Winner1']                # Target variable

from sklearn.impute import KNNImputer

# KNN Imputation
X = X.fillna(X.median())

# Split the data
split_idx = int(len(X) * 0.8)  # 80% training, 20% testing (bottom)

# Use last 20% for testing
X_train = X.iloc[:split_idx]    # Top 80%
y_train = y.iloc[:split_idx]

X_test = X.iloc[split_idx:]     # Bottom 20%
y_test = y.iloc[split_idx:]


scaler = StandardScaler()
X_train_scaled = scaler.fit_transform(X_train)
X_test_scaled = scaler.transform(X_test)


log_reg = LogisticRegression(
    C=.1,
    penalty='l1',
    solver='liblinear',
    max_iter=4000)

log_reg.fit(X_train_scaled, y_train)


# Make predictions
y_pred = log_reg.predict(X_test_scaled)

# Evaluate the model
print("Accuracy:", accuracy_score(y_test, y_pred))
print("\nClassification Report:")
print(classification_report(y_test, y_pred))
print("\nConfusion Matrix:")
print(confusion_matrix(y_test, y_pred))

# Feature importance (coefficients) - using best model
feature_importance = pd.DataFrame({
    'feature': X.columns,
    'coefficient': log_reg.coef_[0]
}).sort_values('coefficient', key=abs, ascending=False)
print("\nTop 20 Feature Importance (coefficients):")
print(feature_importance.head(30))


import pandas as pd
import numpy as np

numeric_cols = df4.select_dtypes(include=['number']).columns# Remove 'Winner1' from the list if it exists
exclude_cols = ['Winner1']
numeric_cols_to_scale = [col for col in numeric_cols if col not in exclude_cols]

df4[numeric_cols_to_scale] = scaler.transform(df4[numeric_cols_to_scale])


# Properly corrected stats
def get_fighter_stats_correct(df, fighter_name):
    fights = df[(df['Fighter_1'] == fighter_name) | (df['Fighter_2'] == fighter_name)].copy()
    fights['is_F1'] = fights['Fighter_1'] == fighter_name

    diff_cols = [col for col in df.columns if col.startswith('Diff_')]
    
    corrected_stats = {}
    for col in diff_cols:
        fights[f'{col}_corrected'] = fights.apply(
            lambda row: row[col] if row['is_F1'] else -row[col], 
            axis=1
        )
        corrected_stats[col] = fights[f'{col}_corrected'].mean()
    
    for col in ['End_Round', 'Total_Fight_Time_Sec']:
        if col in df.columns:
            corrected_stats[col] = fights[col].mean()
    
    # Add fight count
    corrected_stats['fight_count'] = len(fights)
    
    return corrected_stats


# Get stats for the actual fighters
fighter1_stats = get_fighter_stats_correct(df4, fighter1_name)
fighter2_stats = get_fighter_stats_correct(df4, fighter2_name)



# Feature importance coefficients
# Create coefficients dictionary directly from the model
coefficients = {feature: coef for feature, coef in zip(X.columns, log_reg.coef_[0])}


# Build comparison dataframe
comparison = pd.DataFrame({
    'Feature': list(coefficients.keys()),
    'Coefficient': list(coefficients.values())
})

comparison[f'{fighter1_name}_Corrected'] = comparison['Feature'].map(fighter1_stats)
comparison[f'{fighter2_name}_Corrected'] = comparison['Feature'].map(fighter2_stats)
comparison['Diff'] = comparison[f'{fighter1_name}_Corrected'] - comparison[f'{fighter2_name}_Corrected']
comparison['Impact'] = comparison['Coefficient'] * comparison['Diff']

# Determine who each feature favors
comparison['Favors'] = comparison['Diff'].apply(lambda x: fighter1_name.upper() if x > 0 else (fighter2_name.upper() if x < 0 else 'EVEN'))

# Summary
total_score = comparison['Impact'].sum()
prob_fighter1 = 1 / (1 + np.exp(-total_score))

import streamlit as st

st.markdown(f"{fighter1_name.upper()} ({fighter1_stats['fight_count']} fights) vs {fighter2_name.upper()} ({fighter2_stats['fight_count']} fights)")
st.markdown(f"\nFEATURE-BY-FEATURE BREAKDOWN:")

# Display comparison dataframe
st.dataframe(
    comparison[['Feature', f'{fighter1_name}_Corrected', f'{fighter2_name}_Corrected', 'Diff', 'Coefficient', 'Impact', 'Favors']],
    height=400,  # Fixed height with scrollbar
    use_container_width=True
)
st.markdown(f"SUMMARY")
st.markdown(f"Features favoring {fighter1_name}:   {len(comparison[comparison['Favors'] == fighter1_name.upper()])}")
st.markdown(f"Features favoring {fighter2_name}: {len(comparison[comparison['Favors'] == fighter2_name.upper()])}")
st.markdown(f"Features even:             {len(comparison[comparison['Favors'] == 'EVEN'])}")
st.markdown(f"\nTOTAL SCORE: {total_score:.4f}")

# Use metric display for probabilities
col1, col2 = st.columns(2)
with col1:
    st.metric(f"{fighter1_name.upper()} WIN PROBABILITY", f"{prob_fighter1:.1%}")
with col2:
    st.metric(f"{fighter2_name.upper()} WIN PROBABILITY", f"{1-prob_fighter1:.1%}")

st.markdown(f"\n🏆 PREDICTED WINNER: {fighter1_name.upper() if total_score > 0 else fighter2_name.upper()} 🏆")
