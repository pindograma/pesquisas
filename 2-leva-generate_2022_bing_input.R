library(tidyverse)
library(lubridate)
library(readxl)

source('polls_registry.R')

tse_2022 = open_tse_file('./data/tse/tse_2022.csv') %>%
  rename(NR_IDENTIFICACAO_PESQUISA = NR_PROTOCOLO_REGISTRO) %>%
  select(-DT_GERACAO, -HH_GERACAO) %>%
  distinct() %>%
  mutate(f_id = paste0(
    substring(NR_IDENTIFICACAO_PESQUISA, 1, 2), '-',
    substring(NR_IDENTIFICACAO_PESQUISA, 3, 7), '/',
    substring(NR_IDENTIFICACAO_PESQUISA, 8, 12)))

previous_levas = read_csv('previous_levas_newgen.csv')

potential_polls = tse_2022 %>%
  filter(DT_INICIO_PESQUISA >= (now() - days(14)) & DT_REGISTRO <= (now() - days(4))) %>%
  filter(!(f_id %in% previous_levas$tse_id))

potential_polls %>%
  select(f_id) %>%
  rename(tse_id = f_id) %>%
  write.csv('bing_input.txt', row.names = F)
