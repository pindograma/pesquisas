library(tidyverse)

source('polls_registry.R')
tse_2022 = open_tse_file('./data/tse/tse_2022.csv') %>%
  rename(NR_IDENTIFICACAO_PESQUISA = NR_PROTOCOLO_REGISTRO) %>%
  select(-DT_GERACAO, -HH_GERACAO) %>%
  distinct() %>%
  mutate(f_id = paste0(
    substring(NR_IDENTIFICACAO_PESQUISA, 1, 2), '-',
    substring(NR_IDENTIFICACAO_PESQUISA, 3, 7), '/',
    substring(NR_IDENTIFICACAO_PESQUISA, 8, 12))) %>%
  select(f_id, SG_UF, NM_UE, DS_CARGOS)

read_csv('bing_output.csv') %>%
  mutate(tse_id = str_replace_all(tse_id, '"', '')) %>%
  left_join(tse_2022, c('tse_id' = 'f_id')) %>%
  group_split(tse_id) %>%
  map_dfr(function(x) { add_row(x, tse_id='', url='', name='', snippet='', SG_UF='', NM_UE='', DS_CARGOS='') }) %>%
  write.csv('leva_new.csv', row.names = F)
