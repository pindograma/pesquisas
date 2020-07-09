# %%
import glob
import os

import numpy as np
import pandas as pd
# %%
for f in glob.glob('manual/completed/*.xlsx'):
    temp = pd.read_excel(f)
    temp.to_csv('manual/completed/converted_xlsx/' + os.path.basename(f)[:-5] +
                '.csv',
                index=False)

for file in glob.glob('manual/completed/converted_xlsx/*.csv'):
    bad_words = ['Unnamed', 'e1c8d6347c0c']
    with open(file) as oldfile, open(
            'manual/completed/converted_xlsx/fixed/' + os.path.basename(file),
            'w') as newfile:
        for line in oldfile:
            if not any(bad_word in line for bad_word in bad_words):
                newfile.write(line)
# %%
dfs = {}
for f in glob.glob('from_pdf/*.csv'):
    dfs[f] = pd.read_csv(f)
for f in glob.glob('manual/completed/*.csv'):
    dfs[f] = pd.read_csv(f)
for f in glob.glob('manual/completed/converted_xlsx/fixed/*.csv'):
    dfs[f] = pd.read_csv(f)
# %%
for _key, df in dfs.items():
    df.reset_index(drop=True)
    df.columns = df.columns.str.lower()
    df.columns = [
        val for sublist in df.columns.str.extract('(^[^#]*)').values.tolist()
        for val in sublist
    ]
    df.rename(columns={
        'nr_protocolo_registro': 'tse_id',
        'id': 'tse_id',
        'f_id': 'tse_id',
        'observação': 'obs',
        'observaçoes': 'obs',
        'observações': 'obs',
        'util (1 ou 0)': 'util',
        'cargo(pr, s, df, de, g, p ou v)': 'position',
        'cargo': 'position',
        'estimulado (1 ou 0)': 'estimulada',
        'estimulado': 'estimulada',
        'nome': 'feito_por',
        'apenas válidos (se a pesquisa é só de votos válidos)': 'vv',
        'apenas válidos': 'vv',
        'considerou apenas votos válidos': 'vv',
        'validos': 'vv',
        'válidos': 'vv',
        'candidate': 'cand1',
        'value': 'resul1',
        'município': 'cidade',
        'nm_ue': 'cidade',
        'sg_uf': 'estado',
        'susp': 'suspensa',
        'suspensa (1 apenas se sim)': 'suspensa',
        'resil16': 'resul16',
        'cand28nd': 'cand28'
    },
              inplace=True)
    df.drop([
        'tamanho', '1', 'aa_eleicao', 'ds_cargos', 'dt_fim_pesquisa',
        'dt_inicio_pesquisa', 'dt_registro', 'nm_empresa', 'instituto', 'name',
        'title', 'author', 'description', 'copyright', 'created', 'creator',
        'position', 'item link', 'keywords', 'kind', 'language', 'modified',
        'producer'
    ],
            axis=1,
            errors='ignore',
            inplace=True)
    df.reset_index(drop=True)
# %%
for key, df in dfs.items():
    df.to_csv('parsed/' + os.path.basename(key), index=False)
# %%
df2 = pd.concat(dfs)
df2 = df2.reset_index()
df2 = df2[[
    'level_0', 'obs', 'feito_por', 'tse_id', 'url', 'vv', 'suspensa', 'util',
    'estimulada', 'scenario', 'estado', 'cidade', 'sg_ue', 'cand1', 'resul1',
    'cand2', 'resul2', 'cand3', 'resul3', 'cand4', 'resul4', 'cand5', 'resul5',
    'cand6', 'resul6', 'cand7', 'resul7', 'cand8', 'resul8', 'cand9', 'resul9',
    'cand10', 'resul10', 'cand11', 'resul11', 'cand12', 'resul12', 'cand13',
    'resul13', 'cand14', 'resul14', 'cand15', 'resul15', 'cand16', 'resul16',
    'cand17', 'resul17', 'cand18', 'resul18', 'cand19', 'resul19', 'cand20',
    'resul20', 'cand21', 'resul21', 'cand22', 'resul22', 'cand23', 'resul23',
    'cand24', 'resul24', 'cand25', 'resul25', 'cand26', 'resul26', 'cand27',
    'resul27', 'cand28', 'resul28', 'cand29', 'resul29', 'cand30', 'resul30',
    'cand31', 'resul31', 'cand32', 'resul32', 'cand33', 'resul33', 'cand34',
    'resul34', 'cand35', 'resul35', 'cand36', 'resul36', 'cand37', 'resul37',
    'cand38', 'resul38', 'cand39', 'resul39', 'cand40', 'resul40', 'cand41',
    'resul41', 'cand42', 'resul42', 'cand43', 'resul43'
]]
df2.rename(columns={'level_0': 'source'}, inplace=True)
df2["tse_id"] = df2["tse_id"].str.replace('-', '')
df2["tse_id"] = df2["tse_id"].str.replace('/', '')
df2 = df2.sort_values(by=['tse_id', 'source'], ascending=[True, True])
# %%
df2_3 = df2.groupby(['tse_id', 'source'
                     ]).agg(tse_id_count=('tse_id', 'count'),
                            source_count=('source', 'count')).reset_index()
df2_3 = df2_3.merge(df2_3.groupby(['tse_id'
                                   ]).agg(count=('tse_id',
                                                 'count')).reset_index(),
                    on=['tse_id'])
df2_3.to_csv('group_by.csv',
             columns=['tse_id', 'count', 'source'],
             index=False)
