import pandas as pd
import GPy
from datetime import datetime
import numpy as np
import matplotlib.pyplot as plt
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.base import BaseEstimator, RegressorMixin
from sklearn.model_selection import GridSearchCV, KFold, TimeSeriesSplit

def char_to_days_since_1970(d):
    return (datetime.strptime(d, '%Y-%m-%d')-datetime(1970, 1, 1)).days

# Read the data
ffg = pd.read_csv('../data/clean/ffg_monthly.csv')

germany = ffg[ffg['country'] == 'germany'].copy()
germany = germany[['t', 'treatment_period', 'x_temp']]

ffg = ffg.pivot(index='t', columns='country', values='yit').reset_index()
ffg.columns = [f'y_{col}' if col != 't' else col for col in ffg.columns]
ffg = ffg.rename(columns={'y_germany': 'y'})

ffg = ffg.merge(germany, on='t')

ffg = ffg[(ffg['t'] <= char_to_days_since_1970('2023-01-01'))]

ffg = ffg.dropna(axis=1)

ffg_scaled = (ffg - ffg.min()) / (ffg.max() - ffg.min())

train = ffg_scaled[ffg_scaled['treatment_period'] == 0]

X_vars = [col for col in train.columns if col not in ['y', 't', 'treatment_period']]
# X_vars = ['y_poland', 'y_estonia', 'y_belgium', 'y_czech_republic']

lengthscale_max_vals = np.linspace(500, 50000, 10)
exp_var_min_vals = np.linspace(.01, 50, 10)
white_var_min_vals = np.linspace(.0001, 0.5, 10)

best_score = float('inf')
best_params = None

tscv = TimeSeriesSplit(n_splits=5) 

results = pd.DataFrame(columns=['lengthscale_max', 'exp_var_min', 'white_var_min', 
                                'score_1', 'score_2', 'score_3', 'score_4', 'score_5'])

for lengthscale_max in lengthscale_max_vals:
    for exp_var_min in exp_var_min_vals:
        for white_var_min in white_var_min_vals:
            scores = []
            train_test_diffs = []
            
            for train_index, test_index in tscv.split(train):
                train_fold, test_fold = train.iloc[train_index], train.iloc[test_index]
                
                kernel = GPy.kern.Exponential(input_dim=len(X_vars), ARD=True)
                kernel += GPy.kern.White(input_dim=len(X_vars))
                kernel.Exponential.lengthscale.constrain_bounded(0.01, lengthscale_max)
                kernel.Exponential.variance.constrain_bounded(exp_var_min, 1e3)
                kernel.white.variance.constrain_bounded(white_var_min, 1e3)
                
                model = GPy.models.GPRegression(train_fold[X_vars].values, train_fold['y'].values.reshape(-1, 1), kernel)
                model.optimize()
                
                test_y_pred, _ = model.predict(test_fold[X_vars].values)
                train_y_pred, _ = model.predict(train_fold[X_vars].values)

                mse_test = mean_squared_error(test_fold['y'], test_y_pred) 
                mse_train = mean_squared_error(train_fold['y'], train_y_pred)
                r2_test = r2_score(test_fold['y'], test_y_pred)
                r2_train = r2_score(train_fold['y'], train_y_pred)
                scores.append(r2_test)
                train_test_diff = r2_train - r2_test
                train_test_diffs.append(train_test_diff)
        
            avg_score = np.median(scores)
            results = results.append({'lengthscale_max': lengthscale_max, 'exp_var_min': exp_var_min, 'white_var_min': white_var_min, 
                                        'score_1': scores[0], 'score_2': scores[1], 'score_3': scores[2], 'score_4': scores[3], 'score_5': scores[4],
                                        'med_score': avg_score,
                                        'med_train_test_diff': np.median(train_test_diffs)
                                        }, ignore_index=True)
            
            if avg_score > best_score:
                best_score = avg_score
                best_params = (lengthscale_max, exp_var_min, white_var_min)

print("Best score: {}".format(best_score))
print("Best params: {}".format(best_params))
results.sort_values(by='med_score')

# optimize highest med_score and lowest med_train_test_diff
results['percentile_score'] = results['med_score'].rank(pct=True)
results['percentile_train_test_diff'] = results['med_train_test_diff'].rank(pct=True)
results['combined'] = results['percentile_score'] + (1 - results['percentile_train_test_diff'])
results = results.drop(columns=['percentile_score', 'percentile_train_test_diff']).sort_values(by='combined', ascending=False)