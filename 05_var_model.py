#Il faut prendre en input une base de données ayant les informations suivantes : topic, jour, parti, et proportion du topic ce jour par ce parti. 
# En gros, on reprendrait les bases de données utilisées pour tracer les time series, mais il faudrait les isoler par parti / groupe de public
# Aide pour générer ce code trouvée sur https://github.com/LeCoinStat/100JoursDeML/blob/main/07_Series_Temporelles/02_VAR/02_VAR_MODEL.ipynb
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from statsmodels.tsa.vector_ar.var_model import VAR
from statsmodels.tsa.stattools import adfuller
from statsmodels.tools.eval_measures import rmse, aic
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
import argparse
import os
import sys
from functools import reduce

parser = argparse.ArgumentParser()
parser.add_argument(
    "input_path",
    help="Path to a folder with all dataset",
    type=existing_dir_path,
)

parser.add_argument(
    "output_folder",
    help="Path to a folder that will be created and contain the results of VAR model",
    type=create_dir,
)

parser.add_argument(
    "group_name"
    help = "name of the considered group for a period : lr, nupes, rn, lrem" #Il faudra voir comment insérer les publics aussi
)

parser.add_argument(
    "topic_number"
    help = "the topic number we want to work about" 
)

args = parser.parse_args()

df_list = [pd.read_csv(df) for df in input_path] #Ranger dans un même dossier les TS de chaque groupe : supporters par groupe pol, députés, attentive, general, media 

def log_odd(y):
    if y<=0 or y >=1:
        raise ValueError("The proportion is <=0 or >= 1")
    else:
        return np.log(y/(1-y))

for df in df_list: 
    #Formatage des bases de données pour avoir les dates en index associées à la proportion du topic considéré. On passe également la proportion en log-odd 
    df = df[df['topic'] == topic_number]
    df['date'] = pd.to_datetime(df['date'])
    df = df.set_index('date')
    df['logodd_prop'] = df['prop'].apply(log_odd)
    df.drop(['topic', 'prop'], axis=1, inplace=True)

    #Vérification de la stationnarité et différenciation 
    if adfuller(df['logodd_prop'])[1] <=0.05: #On vérifie la stationnarité au seuil de 5% 
        print('The time series of ... is stationnary') #Il faudra réfléchir à un moyen de préciser de quelle time serie on parle à chaque fois
    else: #On différencie en cas de non stationnarité
        print('The time series of ... is not stationnary, a differentiation is performed') #Il faudra réfléchir à un moyen de préciser de quelle time serie on parle à chaque fois
        df['logodd_prop_diff']= df['logodd_prop'].diff() 
        if adfuller(df['logodd_prop_diff'])[1] <=0.05: 
            print('The differentiation of the time series of... is stationnary') #Rename
        else:
            sys.exit("Stopping the script because the differentiation of time series is not stationary.")
    #On verra si on redifférencie encore + : est ce que ça aurait du sens en terme d'interprétabilité ??? 

df_dep = #Trouver la base de données dépendante selon le group défini plus tot
df_list.remove(df_dep)

exog = reduce(lambda left, right: pd.merge(left, right, how='outer', left_index=True, right_index=True), df_list)

model = VAR(df_dep, exog) #Faudra voir comment ajouter les effets fixes

order_selection = model.select_order(maxlags=12)
print(order_selection.summary())

p = input("Choose the number of lags of VAR model")

results = model.fit(p)


