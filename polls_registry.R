text_is_phone = function(x) {
  as.vector(
    !is.na(str_match(x, 'telefon')) &
    is.na(str_match(x, 'verificacao')) &
    is.na(str_match(x, 'monitoramento')) &
    is.na(str_match(x, 'endereco')) &
    is.na(str_match(x, 'telefone[\\s\\(:]*[\\d]')) &
    is.na(str_match(x, 'wi-fi'))
  )
}

party_pattern = 'PARTIDO|DIRETORIO| PT |PMDB|PSDB|ELEICOES|PREFEITO|GOVERNADOR|PSB|PDT|ELEICAO|PC DO B|PCDOB| PP | PR | PSL | PSC | PV'

rgx_confidence = '(?:confian.a|confiabilidade|coeficiente de seguranca).*?(\\d[\\d\\,\\.]*)(?=.*?%|.*?cento)'
rgx_error = '(?:erro).*?(\\d[\\d\\,\\.]*)(?=.*?%|.*?cento|.*?pontos|.*?p.*?p)'

open_tse_file = function(path) {
  read_delim(path, ';',
    escape_double = F,
    locale = locale(date_format = '%d/%m/%Y', decimal_mark = ',', encoding = 'ISO-8859-1'),
    trim_ws = T)
}

get_quantity_1 = function(vec, rgx, val) {
  matches = str_match_all(vec, rgx)
  
  map_dbl(matches, function(x) {
    match_num = str_to_dbl(x[,2])
    r = match_num[which.min(abs(match_num - val))]
    if (identical(r, numeric(0))) NA else r
  })
}

gol = function(x, idx) {
  lapply(x, '[[', idx)
}

get_quantity_2 = function(vec, rgx, suffix_rgx, val) {
  pattern_locations = str_locate_all(vec, rgx)
  number_locations = str_locate_all(vec, paste0('\\d[\\d\\,\\.]*(?=', suffix_rgx, ')'))
  
  pmap_dbl(
    list(pattern_locations, number_locations, vec),
    function(x, y, z) {
      combinations = expand.grid(x[,1], split(y, row(y)))
      combinations$dist = abs(as.numeric(gol(combinations$Var2, 1)) - combinations$Var1)
      combinations = combinations %>% group_by(Var1) %>% arrange(dist) %>% slice(1:2)
      
      if (nrow(combinations) > 0) {
        subs = str_to_dbl(substring(
          rep(z, nrow(combinations)),
          first = gol(combinations$Var2, 1),
          last = gol(combinations$Var2, 2)))
        
        r = subs[which.min(abs(subs - val))]
        if (identical(r, numeric(0))) NA else r
      } else NA })
}

select_closest = function(vec1, vec2, val) {
  ifelse(is.na(vec1), vec2, ifelse(
    is.na(vec2), vec1, ifelse(
      abs(vec1 - val) < abs(vec2 - val), vec1, vec2)))
}

load_poll_registry_data = function(path = './data/tse', estatisticos_ids, old = T) {
  if (old) {
    X2012 <- open_tse_file(paste0(path, '/tse_2012.csv'))
    X2014 <- open_tse_file(paste0(path, '/tse_2014.csv'))
    X2016 <- open_tse_file(paste0(path, '/tse_2016.csv'))
    X2018 <- open_tse_file(paste0(path, '/tse_2018.csv'))
    
    df1 = rbind(X2012, X2014)
    df2 = rbind(X2016, X2018) %>% rename(NR_IDENTIFICACAO_PESQUISA = NR_PROTOCOLO_REGISTRO)
    df_orig = rbind(df1, df2) %>% select(-DT_GERACAO, -HH_GERACAO)
    rm(X2012, X2014, X2016, X2018, df1, df2)
  } else {
    df_orig = open_tse_file(paste0(path, '/tse_2020.csv')) %>%
      rename(NR_IDENTIFICACAO_PESQUISA = NR_PROTOCOLO_REGISTRO) %>%
      select(-DT_GERACAO, -HH_GERACAO)
  }
  
  df_orig %>%
    distinct() %>%
    mutate(f_id = paste0(
      substring(NR_IDENTIFICACAO_PESQUISA, 1, 2), '-',
      substring(NR_IDENTIFICACAO_PESQUISA, 3, 7), '/',
      substring(NR_IDENTIFICACAO_PESQUISA, 8, 12))) %>%
    mutate_if(is.Date, function(x, e, a) {
      if_else(grepl('Suplementares', e), x, make_date(
        year = a,
        month = month(x),
        day = day(x)
      ))
    }, .$NM_ELEICAO, .$AA_ELEICAO) %>%
    mutate(cmp_ue = normalize_simple(NM_UE)) %>%
    mutate(norm_est = normalize_estatistico(NM_ESTATISTICO_RESP)) %>%
    left_join(estatisticos_ids, by = c('norm_est' = 'norm_est')) %>%
    group_by(AA_ELEICAO, est_id) %>%
    mutate(inhouse_stat = n_distinct(NR_CNPJ_EMPRESA) == 1) %>%
    mutate(stat_number = n()) %>%
    ungroup() %>%
    group_by(NR_CNPJ_EMPRESA) %>%
    mutate(pollster_size = n()) %>%
    mutate(pollster_stat_number = n_distinct(est_id)) %>%
    mutate(pollster_inhouse_stat = all(inhouse_stat)) %>%
    ungroup() %>%
    mutate(norm_met = tolower(normalize_simple(DS_METODOLOGIA_PESQUISA))) %>%
    mutate(norm_pa = tolower(normalize_simple(DS_PLANO_AMOSTRAL))) %>%
    mutate(is_fluxo = as.vector(!is.na(str_match(norm_met, 'fluxo')) | !is.na(str_match(norm_pa, 'fluxo')))) %>%
    mutate(is_phone = text_is_phone(norm_met) | text_is_phone(norm_pa)) %>%
    mutate(self_hired = (
      normalize_company(NM_CONTRATANTE) == normalize_company(NM_EMPRESA) |
      NR_CPF_CNPJ_CONTRATANTE == NR_CNPJ_EMPRESA |
      grepl('PROPRIO|PROPRIA', normalize_simple(NM_CONTRATANTE))
    )) %>%
    mutate(partisan = grepl(party_pattern, normalize_simple(NM_CONTRATANTE))) %>%
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
}

get_poll_registry_for_merge = function(df) {
  df %>%
    group_by_at(vars(-NR_CPF_CNPJ_CONTRATANTE, -NM_CONTRATANTE, -NM_PAGANTE_PESQUISA, -NR_CPF_CNPJ_PAGANTE, -DS_ORIGEM_RECURSO)) %>%
    summarize(hirer = paste(NM_CONTRATANTE, collapse = '/')) %>%
    ungroup()
}

translate_html_parser_output = function(df) {
  df %>%
    mutate(NR_IDENTIFICACAO_PESQUISA = str_replace_all(id_pesq, '[\\-\\/]', '')) %>%
    rename(f_id = id_pesq) %>%
    rename(NM_UE = info_muni) %>%
    rename(NR_CNPJ_EMPRESA = comp_cnpj) %>%
    rename(SG_UE = id_muni) %>%
    rename(DT_INICIO_PESQUISA = dt_start, DT_FIM_PESQUISA = dt_end) %>%
    rename(QT_ENTREVISTADOS = pesq_n) %>%
    mutate(self_hired = ifelse(comp_contract_same == 'Sim', T, F)) %>%
    mutate(hirer = word(word(pesq_contractors, 2, sep = ' - '), 1, sep = 'Origem|\\(')) %>%
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
}
