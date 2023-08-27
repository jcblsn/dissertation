import pandas as pd
import GPy
from datetime import datetime
import numpy as np
from sklearn.metrics import mean_squared_error, r2_score

path_data = '../data/clean/ffg_monthly.csv'
DATE_FORMAT = '%Y-%m-%d'
START_DATE = '2015-01-01'
END_DATE = '2023-01-01'
TRAIN_RATIO = 4 / 5
THRESHOLD = 0.85

params_a = {
    'lengthscale_max': 22500,
    'exp_var_min': 15,
    'white_var_min': .0225
}

params_b = {
    'lengthscale_max': 17000,
    'exp_var_min': 40,
    'white_var_min': .0225
}

def char_to_days_since_1970(d):
    return (datetime.strptime(d, DATE_FORMAT) - datetime(1970, 1, 1)).days

def load_and_preprocess_data(data):
    germany_data = data[data['country'] == 'germany'][['t', 'treatment_period', 'x_temp']]
    data_pivot = data.pivot(index='t', columns='country', values='yit').reset_index()
    data_pivot.columns = [f'y_{col}' if col != 't' else col for col in data_pivot.columns]
    data_merged = data_pivot.rename(columns={'y_germany': 'y'}).merge(germany_data, on='t')
    data_filtered = data_merged[(data_merged['t'] >= char_to_days_since_1970(START_DATE)) & (data_merged['t'] <= char_to_days_since_1970(END_DATE))].dropna(axis=1)
    data_filtered['dt'] = pd.to_datetime(data_filtered['t'], unit='D', origin='1970-01-01')
    return data_filtered, ((data_filtered - data_filtered.min()) / (data_filtered.max() - data_filtered.min())).drop(columns=['dt'])

def split_data(data_scaled):
    train_full = data_scaled[data_scaled['treatment_period'] == 0.0].drop(columns=['treatment_period'])
    train_part = train_full.iloc[:int(len(train_full) * TRAIN_RATIO)]
    val_data = train_full.iloc[int(len(train_full) * TRAIN_RATIO):]
    test_data = data_scaled[data_scaled['treatment_period'] == 1.0].drop(columns=['treatment_period'])
    return train_full, train_part, val_data, test_data

def build_gp_model(data, features, params):
    kernel = GPy.kern.Exponential(input_dim=len(features), ARD=True) + GPy.kern.White(input_dim=len(features))
    kernel.Exponential.lengthscale.constrain_bounded(1e-1, params['lengthscale_max'])
    kernel.Exponential.variance.constrain_bounded(params['exp_var_min'], 1e3)
    kernel.white.variance.constrain_bounded(params['white_var_min'], 1e3)
    gp_model = GPy.models.GPRegression(data[features].values, data['y'].values.reshape(-1, 1), kernel)
    gp_model.optimize(optimizer='lbfgs')
    return gp_model

def get_feature_importance(gp_model, features):
    length_scales = gp_model.kern.Exponential.lengthscale.values
    importance_df = pd.DataFrame({'Feature': features, 'Length Scale': length_scales})
    importance_df['Relative Importance'] = 1 / importance_df['Length Scale']
    importance_df['Relative Importance'] /= importance_df['Relative Importance'].sum()
    importance_df = importance_df.sort_values(by='Relative Importance', ascending=False)
    importance_df['Cumulative Importance'] = importance_df['Relative Importance'].cumsum()
    return importance_df.sort_values(by='Relative Importance', ascending=False)

def get_predictions(gp_model, data, features):
    return gp_model.predict(data[features].values)

def rescale_data(scaled_data, original_data):
    return scaled_data * (original_data.max() - original_data.min()) + original_data.min()

def get_metrics(model, data_scaled, features):
    pred = model.predict(data_scaled[features].values)[0][:, 0]
    pred = rescale_data(pred, data_scaled['y'])
    return mean_squared_error(data_scaled['y'], pred), r2_score(data_scaled['y'], pred)

def print_results(features, model, train, test):
    train_mse, train_r2 = get_metrics(model, train, features)
    test_mse, test_r2 = get_metrics(model, test, features)
    print(
        "Training MSE:", round(train_mse, 3), '\n',
        "Testing MSE:", round(test_mse, 3), '\n',
        "Training R2:", round(train_r2, 3), '\n',
        "Testing R2:", round(test_r2, 3)
    )

def define_plot_data(model, features_list, data_scaled, data_original):
    
    pred = model.predict(data_scaled[features_list].values)[0][:, 0]
    pred_lower = model.predict_quantiles(data_scaled[features_list].values, quantiles=(2.5,))[0][:, 0]
    pred_median = model.predict_quantiles(data_scaled[features_list].values, quantiles = (50,))[0][:, 0]
    pred_upper = model.predict_quantiles(data_scaled[features_list].values, quantiles=(97.5,))[0][:, 0]
    pred = rescale_data(pred, data_original['y'])
    pred_lower = rescale_data(pred_lower, data_original['y'])
    pred_median = rescale_data(pred_median, data_original['y'])
    pred_upper = rescale_data(pred_upper, data_original['y'])

    return pd.DataFrame({
        'dt': data_original['dt'], 
        'y': data_original['y'], 
        'y_pred': pred, 
        'y_pred_lower': pred_lower, 
        'y_pred_upper': pred_upper, 
        'y_pred_median': pred_median
        })

ffg = pd.read_csv(path_data)
data_original, data_scaled = load_and_preprocess_data(ffg)
train_full, train_part, val_data, test = split_data(data_scaled)
features = [col for col in train_full.columns if col not in ['y', 't']]

initial_model = build_gp_model(train_full, features, params_a)
print_results(features, initial_model, train_full, test)

feature_importance_df = get_feature_importance(initial_model, features)
features_new = feature_importance_df[feature_importance_df['Cumulative Importance'] <= THRESHOLD]['Feature'].values.tolist()

final_model = build_gp_model(train_full, features_new, params_b)
print_results(features_new, final_model, train_full, test)

plot_data = define_plot_data(initial_model, features, data_scaled, data_original)
plot_data.to_csv('../data/clean/gpr_predictions_monthly.csv', index=False)

# performance metrics (rmse by treatment period)

plot_data = plot_data.merge(data_original[['dt', 'treatment_period']], on='dt')
plot_data['treatment_period'] = plot_data['treatment_period'].apply(lambda x: 'pre' if x == 0 else 'post')
plot_data['sq.er'] = (plot_data['y'] - plot_data['y_pred']) ** 2
gpr_performance = plot_data.groupby(['treatment_period'])['sq.er'].mean().reset_index()
gpr_performance['rmse'] = np.sqrt(gpr_performance['sq.er'])
gpr_performance = gpr_performance.drop(columns=['sq.er'])
gpr_performance = gpr_performance.rename(columns={'treatment_period': 'Treatment'})
gpr_performance.to_csv('../data/clean/gpr_performance.csv', index=False)

# --- causal effect estimates -----------------------------------------

plot_data['diff'] = plot_data['y'] - plot_data['y_pred']

# causal effect estimate
causal_effect = plot_data.groupby(['treatment_period'])['diff'].mean().reset_index()
causal_effect = causal_effect.rename(columns={'treatment_period': 'Treatment', 'diff': 'value'})
causal_effect = causal_effect[causal_effect['Treatment'] == 'post']
causal_effect['Metric'] = 'Causal Effect'

# sum
sum_effect = plot_data.groupby(['treatment_period'])['diff'].sum().reset_index()
sum_effect = sum_effect.rename(columns={'treatment_period': 'Treatment', 'diff': 'value'})
sum_effect = sum_effect[sum_effect['Treatment'] == 'post']
sum_effect['Metric'] = 'Sum Effect'

out = pd.concat([causal_effect, sum_effect])
out.to_csv('../data/clean/gpr_estimates.csv', index=False)
