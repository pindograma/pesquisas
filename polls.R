# polls.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This program is licensed under the GNU General Public License, version 3.
# See the LICENSE file for details.

library(stringi)
library(stringr)
library(purrr)
library(dplyr)
library(readr)

# gol: go over list
gol = function(x, idx) {
  lapply(x, '[[', idx)
}

str_to_dbl = function(x) {
  as.double(sub(',', '.', x, fixed=T))
}

X2012 <- read_delim("2012.csv", ";", escape_double = FALSE, 
  locale = locale(
    date_format = "%d/%m/%Y", 
    decimal_mark = ",", encoding = "ISO-8859-1"), 
  trim_ws = TRUE)

X2014 <- read_delim("2014.csv", ";", escape_double = FALSE, 
  locale = locale(
    date_format = "%d/%m/%Y", 
    decimal_mark = ",", encoding = "ISO-8859-1"), 
  trim_ws = TRUE)

X2016 <- read_delim("2016.csv", ";", escape_double = FALSE, 
  locale = locale(
    date_format = "%d/%m/%Y", 
    decimal_mark = ",", encoding = "ISO-8859-1"), 
  trim_ws = TRUE)

X2018 <- read_delim("2018.csv", ";", escape_double = FALSE, 
  locale = locale(
    date_format = "%d/%m/%Y", 
    decimal_mark = ",", encoding = "ISO-8859-1"), 
  trim_ws = TRUE)

df1 = rbind(X2012, X2014)
df2 = rbind(X2016, X2018) %>% rename(NR_IDENTIFICACAO_PESQUISA = NR_PROTOCOLO_REGISTRO)
df = rbind(df1, df2)
rm(X2012, X2014, X2016, X2018, df1, df2)

df = df %>% select(-DT_GERACAO, -HH_GERACAO)
df = df %>% distinct()

normalize = function(x) {
  tolower(stri_trans_general(str = x, id = 'Latin-ASCII'))
}

df$norm_met = normalize(df$DS_METODOLOGIA_PESQUISA)
df$norm_pa = normalize(df$DS_PLANO_AMOSTRAL)

get_quantity_1 = function(vec, rgx, val) {
  matches = str_match_all(vec, rgx)
  
  map_dbl(matches, function(x) {
    match_num = str_to_dbl(x[,2])
    r = match_num[which.min(abs(match_num - val))]
    if (identical(r, numeric(0))) NA else r
  })
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
          first=gol(combinations$Var2, 1),
          last=gol(combinations$Var2, 2)))
        
        r = subs[which.min(abs(subs - val))]
        if (identical(r, numeric(0))) NA else r
      } else NA })
}

rgx_confidence = '(?:confian.a|confiabilidade|coeficiente de seguranca).*?(\\d[\\d\\,\\.]*)(?=.*?%|.*?cento)'
rgx_error = '(?:erro).*?(\\d[\\d\\,\\.]*)(?=.*?%|.*?cento|.*?pontos|.*?p.*?p)'

df$confidence_interval_1 = get_quantity_1(df$norm_pa, rgx_confidence, 95)
df$confidence_interval_1_met = get_quantity_1(df$norm_met, rgx_confidence, 95)
df$confidence_interval_2 = get_quantity_2(df$norm_pa, 'confian.a|confiabilidade', '.*?%|.*?cento', 95)
df$confidence_interval_2_met = get_quantity_2(df$norm_met, 'confian.a|confiabilidade', '.*?%|.*?cento', 95)

df$error_1 = get_quantity_1(df$norm_pa, rgx_error, 4)
df$error_1_met = get_quantity_1(df$norm_met, rgx_error, 4)
df$error_2 = get_quantity_2(df$norm_pa, 'erro', '.*?%|.*?cento|.*?pontos|.*?p.*?p', 4)
df$error_2_met = get_quantity_2(df$norm_met, 'erro', '.*?%|.*?cento|.*?pontos|.*?p.*?p', 4)

select_closest = function(vec1, vec2, val) {
  ifelse(is.na(vec1), vec2, ifelse(
    is.na(vec2), vec1, ifelse(
      abs(vec1 - val) < abs(vec2 - val), vec1, vec2)))
}

df$confidence_interval = select_closest(df$confidence_interval_1, df$confidence_interval_2, 95)
df$confidence_interval_met = select_closest(df$confidence_interval_1_met, df$confidence_interval_2_met, 95)
df$confidence_interval_final = select_closest(df$confidence_interval, df$confidence_interval_met, 95)

df$error = select_closest(df$error_1, df$error_2, 4)
df$error_met = select_closest(df$error_1_met, df$error_2_met, 4)
df$error_final = select_closest(df$error, df$error_met, 4)

is_phone = function(x) {
  !is.na(str_match(x, 'telefon')) &
  is.na(str_match(x, 'verificacao')) &
  is.na(str_match(x, 'monitoramento')) &
  is.na(str_match(x, 'endereco')) &
  is.na(str_match(x, 'telefone[\\s\\(:]*[\\d]')) &
  is.na(str_match(x, 'wi-fi'))
}

df$is_fluxo = !is.na(str_match(df$norm_met, 'fluxo')) | !is.na(str_match(df$norm_pa, 'fluxo'))
df$is_phone = is_phone(df$norm_met) | is_phone(df$norm_pa)

df$self_hired = df$NR_CPF_CNPJ_CONTRATANTE == '#NE'
df$adj_cnpj_contratante = ifelse(df$self_hired, '', df$NR_CPF_CNPJ_CONTRATANTE)
df$adj_cnpj_pagante = ifelse(df$self_hired, '', df$NR_CPF_CNPJ_PAGANTE)
df$adj_contratante = ifelse(df$self_hired, '', df$NM_CONTRATANTE)
df$adj_pagante = ifelse(df$self_hired, '', df$NM_PAGANTE_PESQUISA)

df$adj_confidence_interval = ifelse(is.na(df$confidence_interval_final), '', df$confidence_interval_final)
df$adj_error = ifelse(is.na(df$error_final), '', df$error_final)

write.csv(df %>% select(
  NR_IDENTIFICACAO_PESQUISA,
  DT_REGISTRO,
  NR_CNPJ_EMPRESA,
  DT_INICIO_PESQUISA,
  DT_FIM_PESQUISA,
  QT_ENTREVISTADOS,
  NM_ESTATISTICO_RESP,
  CD_CONRE,
  VR_PESQUISA,
  adj_contratante,
  self_hired,
  adj_pagante,
  adj_cnpj_contratante,
  confidence_interval_final,
  error_final,
  adj_cnpj_pagante,
  is_phone,
  is_fluxo), './polls.csv', row.names=F)
