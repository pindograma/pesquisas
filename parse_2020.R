library(tidyverse)
library(stringi)

df2020 = read_csv('data/tse/df2020_12sep.csv') %>%
  mutate(norm_met = tolower(normalize_simple(txt_method))) %>%
  mutate(norm_pa = tolower(normalize_simple(txt_plan))) %>%
  mutate(is_fluxo = as.vector(!is.na(str_match(norm_met, 'fluxo')) | !is.na(str_match(norm_pa, 'fluxo')))) %>%
  mutate(is_phone = text_is_phone(norm_met) | text_is_phone(norm_pa)) %>%
  mutate(partisan = grepl(party_pattern, normalize_simple(pesq_contractors))) %>%
  mutate(confidence_interval_1 = get_quantity_1(norm_pa, rgx_confidence, 95)) %>%
  mutate(confidence_interval_1_met = get_quantity_1(norm_met, rgx_confidence, 95)) %>%
  mutate(confidence_interval_2 = get_quantity_2(norm_pa, 'confian.a|confiabilidade', '.*?%|.*?cento', 95)) %>%
  mutate(confidence_interval_2_met = get_quantity_2(norm_met, 'confian.a|confiabilidade', '.*?%|.*?cento', 95)) %>%
  mutate(error_1 = get_quantity_1(norm_pa, rgx_error, 4)) %>%
  mutate(error_1_met = get_quantity_1(norm_met, rgx_error, 4)) %>%
  mutate(error_2 = get_quantity_2(norm_pa, 'erro', '.*?%|.*?cento|.*?pontos|.*?p.*?p', 4)) %>%
  mutate(error_2_met = get_quantity_2(norm_met, 'erro', '.*?%|.*?cento|.*?pontos|.*?p.*?p', 4)) %>%
  mutate(confidence_interval = select_closest(confidence_interval_1, confidence_interval_2, 95)) %>%
  mutate(confidence_interval_met = select_closest(confidence_interval_1_met, confidence_interval_2_met, 95)) %>%
  mutate(error = select_closest(error_1, error_2, 4)) %>%
  mutate(error_met = select_closest(error_1_met, error_2_met, 4)) %>%
  mutate(confidence_interval_final = select_closest(confidence_interval, confidence_interval_met, 95)) %>%
  mutate(error_final = select_closest(error, error_met, 4)) %>%
  select(-confidence_interval_1, -confidence_interval_1_met,
   -confidence_interval_2, -confidence_interval_2_met,
   -error_1, -error_1_met, -error_2, -error_2_met,
   -confidence_interval, -confidence_interval_met,
   -error, -error_met)

names_correction = read_csv('data/manual-data/normalized_names_2020_2.csv')

leva1 = read_csv('data/manual-data/manual-2020/pedro-fixed-pedro_leva1_2020_orig.csv', col_types = rtypes)
leva2 = read_csv('data/manual-data/manual-2020/pedro-fixed-pedro_leva2_2020_orig.csv', col_types = rtypes)
leva3 = read_csv('data/manual-data/manual-2020/pedro_leva3_2020.csv', col_types = rtypes)
leva3_ex = read_csv('data/manual-data/manual-2020/pedro_leva_3_extra_.csv', col_types = rtypes)
leva4 = read_csv('data/manual-data/manual-2020/pedro_leva4_2020.csv', col_types = rtypes)
leva5 = read_csv('data/manual-data/manual-2020/pedro_leva5_2020.csv', col_types = rtypes)

X2020 = bind_rows(leva1, leva2, leva3, leva4, leva5)

X2020_2 = X2020 %>%
  select(-contains('unnamed')) %>%
  mutate_at(vars(matches('resul')), str_to_dbl) %>%
  filter(util == 1 & !is.na(cand1)) %>%
  mutate(position = tolower(position)) %>%
  rowwise() %>%
  mutate(total = sum(c_across(matches('resul')), na.rm = T)) %>%
  ungroup() %>%
  mutate_at(vars(matches('cand')), normalize_cand) %>%
  mutate_at(vars(matches('resul')), function(x, t) {
    case_when(
      x < 1 ~ ifelse(t <= 1, x * 100, x),
      T ~ x
    )
  }, .$total) %>%
  mutate(year = 2020)

# MUST VERIFY:
# - dup_polls
# - multiple scenarios
# - total and relation to vv

cur = X2020_2 %>%
  mutate(scenario_id = row_number()) %>%
  rowwise() %>%
  mutate(total = sum(c_across(contains('resul')), na.rm = T)) %>%
  mutate(for_patch = (total > 98 & is.na(vv)) | (total < 98 & !is.na(vv))) %>%
  ungroup()

patch_1 = read_csv('data/manual-data/manual-2020/patch_1_2020.csv')

cur = bind_rows(
  cur %>% filter(!for_patch),
  patch_1
)

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
  left_join(names_correction, by = c('info_muni', 'candidate')) %>%
  mutate(candidate = ifelse(is.na(correction), candidate, correction))

manual_tse = manual %>%
  left_join(df2020, by = c('tse_id' = 'id_pesq', 'info_muni')) %>%
  mutate(NR_IDENTIFICACAO_PESQUISA = str_replace_all(tse_id, '[\\-\\/]', '')) %>%
  mutate(main_source = 'Pindograma-Manual') %>%
  rename(source = url) %>%
  rename(NR_CNPJ_EMPRESA = comp_cnpj) %>%
  rename(SG_UE = id_muni) %>%
  mutate(CD_CARGO = recode(position,
    `p` = 11,
    `v` = 13,
  )) %>%
  rename(DT_INICIO_PESQUISA = dt_start, DT_FIM_PESQUISA = dt_end) %>%
  rename(QT_ENTREVISTADOS = pesq_n) %>%
  mutate(self_hired = ifelse(comp_contract_same == 'Sim', T, F)) %>%
  mutate(is_complete = T) %>%
  mutate(hirer = word(word(pesq_contractors, 2, sep = ' - '), 1, sep = 'Origem|\\(')) %>%
  mutate(company_id = case_when(
    NR_CNPJ_EMPRESA %in% c('24776969000117', '07742623000189', '67662494000140', '14931054000185', '26195312000191') ~ 'REALIDADE',
    NR_CNPJ_EMPRESA %in% c('23254436000102', '00852438000106', '00852501000104') ~ 'VOX POPULI',
    NR_CNPJ_EMPRESA %in% c('05939922000263', '32208779000121') ~ 'GERP',
    NR_CNPJ_EMPRESA %in% c('10828442000184', '22198794000182') ~ 'MULTIPLA',
    NR_CNPJ_EMPRESA %in% c('17070395000100', '06154093000195') ~ 'MULTIDADOS',
    NR_CNPJ_EMPRESA %in% c('09656847000101', '09283689000183') ~ 'PROMIDIA',
    NR_CNPJ_EMPRESA %in% c('03622028000159', '39795414000190') ~ 'FUTURA',
    NR_CNPJ_EMPRESA %in% c('26968293000199', '11785871000184') ~ 'RANKING',
    NR_CNPJ_EMPRESA %in% c('57541377000175', '16623147000178') ~ 'DGABC',
    NR_CNPJ_EMPRESA %in% c('01077145000153', '10575983000148') ~ 'DATAMETRICA',
    NR_CNPJ_EMPRESA %in% c('13776203000116', '12784563000105') ~ 'ARBEIT',
    NR_CNPJ_EMPRESA %in% c('14263830000116', '16684996000131') ~ 'AGORASEI',
    NR_CNPJ_EMPRESA %in% c('11509901000120', '20450146000146') ~ 'LONDON',
    NR_CNPJ_EMPRESA %in% c('17110229000181', '29880121000157') ~ 'AMPLACE',
    NR_CNPJ_EMPRESA %in% c('03397255000128', '33030852000180') ~ 'TDL',
    NR_CNPJ_EMPRESA %in% c('04996040000196', '09439784000123') ~ 'ANGULO',
    NR_CNPJ_EMPRESA %in% c('03490620000144', '32980640000100') ~ 'TENDENCIAMS',
    T ~ NR_CNPJ_EMPRESA
  )) %>%
  mutate(turno = 1) %>%
  group_by(SG_UE, candidate) %>%
  mutate(NUMERO_CANDIDATO = cur_group_id() + 1000) %>%
  ungroup()

company_names = read_csv('data/manual-data/nomes_empresas.csv')

all_polls = manual_tse %>%
  distinct(year, NR_IDENTIFICACAO_PESQUISA, NR_CNPJ_EMPRESA, SG_UE, CD_CARGO, company_id,
           estimulada, NUMERO_CANDIDATO, result, DT_FIM_PESQUISA, vv, turno,
           is_fluxo, is_phone, self_hired, QT_ENTREVISTADOS, main_source, source, scenario_id, is_complete,
           DT_INICIO_PESQUISA, hirer, confidence_interval_final, error_final, candidate) %>%
  mutate(polled_UE = SG_UE) %>%
  left_join(company_names, 'company_id') %>%
  group_by(NR_IDENTIFICACAO_PESQUISA, NR_CNPJ_EMPRESA, SG_UE, CD_CARGO, estimulada, scenario_id) %>%
  mutate(undecided = 100 - sum(result)) %>%
  ungroup()

rm(lhs, rhs)

no_vv_polls = all_polls %>%
  filter(is.na(vv))

early_polls = no_vv_polls %>%
  left_join(election_dates, 'year') %>%
  filter(DT_FIM_PESQUISA <= candidate_registry_date) %>%
  filter(estimulada == 0) %>%
  filter(CD_CARGO == 11)

#late_polls = no_vv_polls %>%
#  left_join(election_dates, 'year') %>%
#  filter(DT_FIM_PESQUISA > candidate_registry_date) %>%
#  group_by(NR_IDENTIFICACAO_PESQUISA, NR_CNPJ_EMPRESA, SG_UE, polled_UE, CD_CARGO, vv) %>%
#  filter(n_distinct(estimulada) == 1 | estimulada == 1) %>%
#  ungroup()

early_polls %>%
  write.csv('output/early_polls_2020.csv', row.names = F)

#late_polls %>%
#  write.csv('output/late_polls.csv', row.names = F)
