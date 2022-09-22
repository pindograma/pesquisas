# parse_2022.R
# (c) 2022 CincoNoveSeis Jornalismo Ltda.

library(tidyverse)
library(stringi)
library(lubridate)
library(mgsub)
library(fastdigest)

library(sqldf)
options('sqldf.dll' = paste0(getwd(), '/', 'spellfix.so'))

Rcpp::sourceCpp('src/wordmatch.cpp')
source('polling_utils.R')
source('poll_to_candidate_matcher.R')
source('polls_registry.R')

estatisticos_ids = read_csv('data/manual-data/estatisticos_ids.csv')
df_2022 = load_poll_registry_data(estatisticos_ids = estatisticos_ids, old = F, year__ = 2022)

levas = list.files('data/manual-data/manual-2022/', pattern = 'leva.*csv', full.names = T)
X2022 = map_dfr(levas, function(x) read_csv(x, col_types = rtypes) %>% mutate(filename = x)) %>%
  mutate(tse_id = str_replace_all(tse_id, '"', ''))
X2022_2 = normalize_input(X2022, year__ = 2022)

# X2022_2 %>% distinct(tse_id) %>% write.csv('previous_levas_newgen.csv', row.names = F)

cur = X2022_2 %>%
  mutate(scenario_id = row_number()) %>%
  rowwise() %>%
  mutate(total = sum(c_across(contains('resul')), na.rm = T)) %>%
  mutate(for_patch = (total > 98 & is.na(vv)) | (total < 98 & !is.na(vv)) | total > 102) %>%
  ungroup()

lhs = cur %>%
  pivot_longer(cols = starts_with('cand'),
               names_to = 'index',
               names_prefix = 'cand',
               values_to = 'candidate',
               values_drop_na = T) %>%
  select(-matches('resul'), -matches('part'))

rhs = cur %>%
  pivot_longer(cols = starts_with('resul'),
               names_to = 'index',
               names_prefix = 'resul',
               values_to = 'result',
               values_drop_na = T) %>%
  select(-matches('cand'), -matches('part'))

manual = inner_join(lhs, rhs, by = c(
  'index' = 'index',
  'scenario_id' = 'scenario_id'
)) %>%
  select(-matches('\\.y')) %>%
  rename_at(vars(matches('\\.x')), function(x) { str_sub(x, end = -3) }) %>%
  mutate(tse_id = ifelse(is.na(tse_id), id_pesq, tse_id)) %>%
  mutate(NM_UE = ifelse(is.na(NM_UE), info_uf, NM_UE))

correction_of_candidate_name = read_csv('data/manual-data/cand_correction_22.csv')

manual_tse = manual %>%
  inner_join(df_2022, by = c('tse_id' = 'f_id')) %>%
  mutate(main_source = 'Pindograma-Manual') %>%
  rename(source = url) %>%
  mutate(CD_CARGO = recode(position,
    `pr` = 1,
    `g` = 3,
    `s` = 5,
    `df` = 6,
    `de` = 7
  )) %>%
  mutate(turno = ifelse(DT_FIM_PESQUISA <= make_date(2022, 10, 2), 1, 2)) %>%
  left_join(correction_of_candidate_name, 'candidate') %>%
  mutate(candidate = ifelse(is.na(corrected_candidate), candidate, corrected_candidate)) %>%
  select(-corrected_candidate) %>%
  mutate(candidate_without_title = normalize_cand_rm_titles(candidate))

# -- Candidates
cands_ = read_csv2('data/tse/consulta_cand_2022_BRASIL.csv', locale = locale(encoding = 'ISO-8859-1')) %>%
  filter(CD_CARGO %in% c(1, 3, 5))

fake_cands = read_csv('data/fake_cands_atlas.csv')

cands_2 = cands_ %>%
  rename(NOME_CANDIDATO = NM_CANDIDATO, NOME_URNA_CANDIDATO = NM_URNA_CANDIDATO,
         CODIGO_CARGO = CD_CARGO, SIGLA_UE = SG_UE, NUMERO_CANDIDATO = NR_CANDIDATO,
         NUM_TURNO = NR_TURNO) %>%
  select(NOME_CANDIDATO, NOME_URNA_CANDIDATO, CODIGO_CARGO, SIGLA_UE, NUMERO_CANDIDATO, NUM_TURNO, ANO_ELEICAO) %>%
  mutate(NOME_CANDIDATO = normalize_cand(NOME_CANDIDATO)) %>%
  mutate(NOME_URNA_CANDIDATO = normalize_cand(NOME_URNA_CANDIDATO)) %>%
  mutate(CODIGO_CARGO = ifelse(CODIGO_CARGO == 8, 7, CODIGO_CARGO)) %>%
  mutate(SIGLA_UF = NA) %>%
  distinct() %>%
  distinct(NOME_CANDIDATO, NOME_URNA_CANDIDATO, .keep_all = T) %>%
  bind_rows(fake_cands)

cands = bind_rows(cands_2, cands_2) %>%
  mutate(NUM_TURNO = c(rep(1, nrow(cands_2)), rep(2, nrow(cands_2))))

rm(cands_, cands_2)
#-- End Canddates

rm(lhs, rhs)
manual_matches = match_polls_with_candidates(manual_tse)

company_conversion = read_csv('data/manual-data/empresas_multiplos_cnpjs.csv')
company_names = read_csv('data/manual-data/nomes_empresas.csv')

all_polls = manual_matches %>%
  left_join(company_conversion, 'NR_CNPJ_EMPRESA') %>%
  mutate(company_id = ifelse(is.na(company_id), NR_CNPJ_EMPRESA, company_id)) %>%
  # TODO readd hirer and partisan and self_hired
  distinct(year, NR_IDENTIFICACAO_PESQUISA, NR_CNPJ_EMPRESA, SG_UE, CD_CARGO, company_id, suspensa,
           estimulada, NUMERO_CANDIDATO, NOME_URNA_CANDIDATO, result, DT_FIM_PESQUISA, vv, turno,
           is_fluxo, is_phone, QT_ENTREVISTADO, main_source, source, scenario_id, is_complete,
           DT_INICIO_PESQUISA, confidence_interval_final, error_final, candidate, ambito) %>%
  left_join(company_names, 'company_id') %>%
  rename(polled_UE = ambito) %>%
  mutate(polled_UE = toupper(polled_UE)) %>%
  group_by(NR_IDENTIFICACAO_PESQUISA, NR_CNPJ_EMPRESA, SG_UE, CD_CARGO, estimulada, scenario_id) %>%
  mutate(undecided = 100 - sum(result)) %>%
  ungroup()

all_polls_2 = all_polls %>%
  group_by(NR_IDENTIFICACAO_PESQUISA, estimulada, CD_CARGO, scenario_id) %>%
  filter(n_distinct(candidate) == n()) %>%
  mutate(scenario_count = n()) %>%
  ungroup() %>%
  group_by(NR_IDENTIFICACAO_PESQUISA, estimulada, CD_CARGO, NR_CNPJ_EMPRESA) %>%
  mutate(multiple_scenarios = n_distinct(scenario_id) > 1) %>%
  filter(scenario_count == max(scenario_count)) %>%
  ungroup() %>%
  select(-scenario_count)

all_polls_3 = all_polls_2 %>%
  group_by(NR_IDENTIFICACAO_PESQUISA, NR_CNPJ_EMPRESA, SG_UE, polled_UE, estimulada, CD_CARGO, vv, scenario_id) %>%
  mutate(group_digest = fastdigest(cur_data() %>% arrange(NUMERO_CANDIDATO) %>% select(NUMERO_CANDIDATO, result))) %>%
  ungroup() %>%
  group_by(NR_IDENTIFICACAO_PESQUISA, NR_CNPJ_EMPRESA, SG_UE, polled_UE, estimulada, CD_CARGO, vv, group_digest) %>%
  filter(scenario_id == first(scenario_id)) %>%
  ungroup()

saveRDS(all_polls_3, 'all_polls_2022.rda')

no_vv_polls = all_polls_3 %>%
  filter(is.na(vv)) %>%
  group_by(NR_IDENTIFICACAO_PESQUISA, scenario_id, NUMERO_CANDIDATO) %>%
  filter(all(n() == 1)) %>%
  ungroup() %>%
  select(-NOME_URNA_CANDIDATO)

early_polls_2 = no_vv_polls %>%
  left_join(election_dates, 'year') %>%
  filter(DT_FIM_PESQUISA <= candidate_registry_date) %>%
  filter(estimulada == 0)

recent_polls = no_vv_polls %>%
  left_join(election_dates, 'year') %>%
  group_by(NR_IDENTIFICACAO_PESQUISA, NR_CNPJ_EMPRESA, SG_UE, polled_UE, CD_CARGO, vv) %>%
  filter(n_distinct(estimulada) == 1 | estimulada == 1) %>%
  ungroup() 

