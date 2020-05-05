# p360_benchmark.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This program is licensed under the GNU General Public License, version 3.
# See the LICENSE file for details.

# NOTE: `df` is generated in polls.R.

# WARNING: When this is on a nice Docker container, this will no longer be needed,
# but alas. The default installation of sqlite3 on macOS does not support
# extensions. As a result, loading sqlite3 dylibs will not work. To address
# this issue, simply download SQLite3 from source and install it manually.
options('sqldf.dll' = '/Users/Daniel/Downloads/sqlite/spellfix.so')

library(readr)
library(dplyr)
library(stringr)
library(stringi)
library(fuzzyjoin)
library(sqldf)

all_p3 = read_delim('poder360/all.csv', ';', escape_double = F, trim_ws = T)
df$cmp_ue = stri_trans_general(str = df$NM_UE, id = 'Latin-ASCII')

# NOTE: The Poder360 database does not seem to account for supplementary
# elections. If it did, this line would warrant a slight adjustment.
dfp3 = all_p3 %>% filter(format(data_pesquisa, '%Y') == ano)
dfp3 = dfp3 %>% filter(ambito != 'RE')

dfp3$num_registro = stri_trans_general(str = dfp3$num_registro, id = 'Latin-ASCII')
dfp3$raw_nr = str_replace_all(dfp3$num_registro, '[\\s\\-\\/]', '')

dfp3$raw_nr = recode(dfp3$raw_nr,
  `RN00032014` = 'RN000032014',
  `PI00782014` = 'PI000782014',
  `PI00852014` = 'PI000852014',
  `PI00932014` = 'PI000932014',
  `DF00043214` = 'DF000432014',
  `BR10372014` = 'BR010372014',
  `GO14952016` = 'GO014952016',
  `C055462018` = 'AC055462018',
  `0001362012` = 'MG001362012'
)

concatenate_id = function(x, y) {
  paste0(ifelse(x == 'TSE', 'BR', ifelse(
    nchar(x) == 6, substring(x, first=5, last=6), NA
  )), y)
}

dfp3$raw_nr = ifelse(
  nchar(dfp3$raw_nr) == 9,
  concatenate_id(dfp3$orgao_registro, dfp3$raw_nr),
  dfp3$raw_nr)

dfp3$cmp_ue = stri_trans_general(str = toupper(
  ifelse(!is.na(dfp3$cidade),
    dfp3$cidade,
    dfp3$unidade_federativa_nome
  )), id = 'Latin-ASCII')

p360_empresa_correspondence <- read_csv("../dados/polling/p360_empresa_correspondence.csv")

# ----

dfp3$has_problematic_id = is.na(dfp3$raw_nr) | (dfp3$ambito != 'BR' & dfp3$ambito != substring(dfp3$raw_nr, first = 1, last = 2))

without_id = dfp3 %>% filter(has_problematic_id) %>%
  select(pesquisa_id, data_pesquisa, instituto, unidade_federativa_nome, cidade, cmp_ue) %>%
  distinct(data_pesquisa, instituto, unidade_federativa_nome, cidade, .keep_all = T)

without_id = left_join(without_id,
  p360_empresa_correspondence,
  by=c('instituto'='instituto'))

# NOTE: At the risk of ambiguous double-matches, we are not JOINing by office sought.
# While technically possible, that field is a fucking mess in the TSE database, and
# it is very likely that we will lose good matches if we do so. This seems to be an
# acceptable tradeoff, since this seems to work pretty well as-is.
matches = sqldf('SELECT * FROM without_id LEFT JOIN df ON
  without_id.cmp_ue = df.cmp_ue AND
  without_id.cnpj = df.NR_CNPJ_EMPRESA AND
  ABS(julianday(without_id.data_pesquisa) - julianday(df.DT_FIM_PESQUISA)) < 5')

matches = subset(matches, select = -c(cmp_ue))
gmatches = matches %>% group_by(NM_UE, data_pesquisa, DT_FIM_PESQUISA, instituto, NM_EMPRESA)

matches = gmatches %>% filter(n() == 1) %>% ungroup()
anti_matches = matches %>% filter(is.na(NM_EMPRESA))
matches = matches %>% filter(!is.na(NM_EMPRESA))
ambiguous_matches = gmatches %>% filter(n() > 1) %>% ungroup() %>%
  distinct(NM_UE, data_pesquisa, DT_FIM_PESQUISA, instituto, NM_EMPRESA, .keep_all = T)
rm(gmatches)

dfp3 = left_join(dfp3,
                 matches %>% select(pesquisa_id, NR_IDENTIFICACAO_PESQUISA),
                 by=c('pesquisa_id'='pesquisa_id'))
dfp3$raw_nr = ifelse(
  dfp3$has_problematic_id,
  dfp3$NR_IDENTIFICACAO_PESQUISA,
  dfp3$raw_nr)

dfp3 = dfp3 %>% filter(!is.na(raw_nr) & nchar(raw_nr) == 11)
dfp3$raw_tipo = ifelse(dfp3$tipo == 'Estimulada', 1, ifelse(dfp3$tipo == 'Espontânea', 0, NA))
dfp3$raw_cand = stri_trans_general(str = dfp3$candidato, id = 'Latin-ASCII')

get_missing_polls = function(df_filtered_polls, imported_polls) {
  anti_join(
    df_filtered_polls,
    imported_polls,
    by=c('NR_IDENTIFICACAO_PESQUISA'='tse_id'))
}

get_success_rate = function(df_filtered_polls, imported_polls) {
  successful = inner_join(
    df_filtered_polls,
    imported_polls,
    by=c('NR_IDENTIFICACAO_PESQUISA'='tse_id')) %>% distinct(NR_IDENTIFICACAO_PESQUISA)
  
  successful %>% nrow() / (successful %>% nrow() + get_missing_polls(df_filtered_polls, imported_polls) %>% nrow())
}

get_benchmark = function(p3_f, imported_polls) {
  f1 = imported_polls %>%
    filter(candidate != 'BASE' & candidate != 'Branco/ Nulo' & candidate != 'Nao sabe/ Nao respondeu' & candidate != 'Citou outro nome')
  f2 = f1 %>% group_by(tse_id, estimulada, position) %>% filter(sum(scenario)/n() == first(scenario))
  print(sprintf('We are verifying %d imported polls...', f2 %>% distinct(tse_id) %>% nrow()))
  
  p3_f = p3_f %>%
    filter(voto_tipo != 'Votos Válidos') %>%
    filter(cenario_descricao == 'Diversos candidatos') %>%
    group_by(raw_nr) %>%
    filter(n_distinct(ambito) == 1) %>%
    ungroup() %>%
    group_by(raw_nr, raw_tipo, raw_cand) %>%
    filter(n() == 1 | turno == 1) %>%
    ungroup()
  print(sprintf('...against %d Poder360 polls.', p3_f %>% distinct(raw_nr) %>% nrow()))
  
  sqldf('SELECT tse_id, estimulada, position, cargo, candidate, raw_cand, value, percentual, ambito, editdist3(f2.candidate, p3_f.raw_cand), turno FROM f2
             INNER JOIN p3_f ON
             f2.tse_id = p3_f.raw_nr AND
             f2.estimulada = p3_f.raw_tipo AND
             editdist3(f2.candidate, p3_f.raw_cand) < 650') %>% filter(abs(percentual - value) > 2)
}