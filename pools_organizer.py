# %%
import glob
import os
import shutil

import pandas as pd

os.chdir('pools_results/manual_input/')
# %%
path_gen1 = 'gen1/'
for f in glob.glob("*.xlsx"):
    temp = pd.read_excel(f)
    temp.to_csv(path_gen1 + f[:-5] + '.csv', index=False)

for f in glob.glob('*.csv'):
    shutil.copy(f, path_gen1)

for file in glob.glob('gen1/*.csv'):
    bad_words = ['Unnamed', 'e1c8d6347c0c']
    with open(file) as oldfile, open('gen2/gen_' + os.path.basename(file),
                                     'w') as newfile:
        for line in oldfile:
            if not any(bad_word in line for bad_word in bad_words):
                newfile.write(line)
# %%
dfs = {}
for f in glob.glob('gen2/*.csv'):
    dfs[f] = pd.read_csv(f)
# %%
for _key, df in dfs.items():
    df.columns = df.columns.str.lower()
    # df.dropna(how='all', axis='columns')
    # df.dropna(how='all', axis=0)
    df.rename(columns={
        'nr_protocolo_registro': 'tse_id',
        'id': 'tse_id',
        'f_id': 'tse_id',
        'tse_id#string': 'tse_id',
        'obs': 'obs',
        'observação': 'obs',
        'observaçoes': 'obs',
        'observações': 'obs',
        'util (1 ou 0)': 'util',
        'util#int': 'util',
        'cargo(pr, s, df, de, g, p ou v)': 'position',
        'cargo': 'position',
        'cargo#string': 'position',
        'estimulado (1 ou 0)': 'estimulada',
        'estimulado#int': 'estimulada',
        'nome': 'feito_por',
        'url#url': 'url',
        'apenas válidos (se a pesquisa é só de votos válidos)': 'vv',
        'apenas válidos#bool': 'vv',
        'considerou apenas votos válidos#bool': 'vv',
        'validos': 'vv',
        'válidos': 'vv',
        'vv': 'vv',
        'candidate': 'cand1',
        'value': 'resul1',
        'município': 'city',
        'susp': 'suspensa',
        'suspensa (1 apenas se sim)': 'suspensa',
        'suspensa#bool': 'suspensa',
        'resil16': 'resul16'
    },
              inplace=True)
# %%
for _key, df in dfs.items():
    df.columns = [
        val for sublist in df.columns.str.extract('(^[^#]*)').values.tolist()
        for val in sublist
    ]
# %%
for key, df in dfs.items():
    df.to_csv('gen3/' + os.path.basename(key), index=False)
