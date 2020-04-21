library(readr)
library(dplyr)
library(digest)

names = read_delim('replc.csv', ";",
  trim_ws = TRUE,
  locale = locale(date_format = "%d/%m/%Y"),
  col_types=c('DATA'='D'))

X2012 = read_delim('csv/2012.csv', ";",
  escape_double = FALSE,
  locale = locale(
    date_format = "%d/%m/%Y",
    decimal_mark = ",",
    encoding = "ISO-8859-1"), 
  trim_ws = TRUE)

X2014 = read_delim('csv/consulta_cand_2014_BRASIL.csv', ";",
  escape_double = FALSE,
  locale = locale(
    date_format = "%d/%m/%Y",
    decimal_mark = ",",
    encoding = "ISO-8859-1"), 
  trim_ws = TRUE)

X2016 = read_delim('csv/consulta_cand_2016_BRASIL.csv', ";",
  escape_double = FALSE,
  locale = locale(
    date_format = "%d/%m/%Y",
    decimal_mark = ",",
    encoding = "ISO-8859-1"), 
  trim_ws = TRUE)

X2018 = read_delim('csv/consulta_cand_2018_BRASIL.csv', ";",
  escape_double = FALSE,
  locale = locale(
    date_format = "%d/%m/%Y",
    decimal_mark = ",",
    encoding = "ISO-8859-1"), 
  trim_ws = TRUE)

post_2012 = rbind(X2014, X2016, X2018)
rm(X2014, X2016, X2018)

X2012_wd = left_join(X2012, names, by = c('DESCRICAO_ELEICAO' = 'NOME NA PLANILHA'))
rm(X2012)

X2012_wd$DT_ELEICAO = ifelse(
  X2012_wd$NUM_TURNO == 2,
  ifelse(
    X2012_wd$DATA == as.Date('2012-10-07'),
    as.Date('2012-10-28'),
    X2012_wd$DATA),
  X2012_wd$DATA)
X2012_wd$DT_ELEICAO = as.Date(X2012_wd$DT_ELEICAO, origin='1970-01-01')

X2012_wd = X2012_wd %>% filter(
  CODIGO_CARGO == 1 |
  CODIGO_CARGO == 3 |
  CODIGO_CARGO == 5 |
  CODIGO_CARGO == 6 |
  CODIGO_CARGO == 7 |
  CODIGO_CARGO == 11 |
  CODIGO_CARGO == 13 |
  CODIGO_CARGO == 8)

post_2012 = post_2012 %>% filter(
  CD_CARGO == 1 |
  CD_CARGO == 3 |
  CD_CARGO == 5 |
  CD_CARGO == 6 |
  CD_CARGO == 7 |
  CD_CARGO == 11 |
  CD_CARGO == 13 |
  CD_CARGO == 8)

X2012_wd$CARGO = recode(
  X2012_wd$CODIGO_CARGO,
  `11`='p',
  `13`='v',
  `7`='de',
  `8`='de',
  `6`='df',
  `5`='s',
  `3`='g',
  `1`='pr')

post_2012$CARGO = recode(
  post_2012$CD_CARGO,
  `11`='p',
  `13`='v',
  `7`='de',
  `8`='de',
  `6`='df',
  `5`='s',
  `3`='g',
  `1`='pr')

X2012_wd$STUB1 = NA
X2012_wd$STUB2 = NA

ca_2012 = X2012_wd %>% select(
  SIGLA_PARTIDO,
  NOME_URNA_CANDIDATO,
  COD_SITUACAO_CANDIDATURA,
  STUB1,
  SEQUENCIAL_CANDIDATO,
  STUB2,
  COMPOSICAO_LEGENDA,
  NOME_CANDIDATO,
  CPF_CANDIDATO,
  SIGLA_UF,
  SIGLA_UE,
  DESCRICAO_UE,
  CARGO,
  DT_ELEICAO,
  NUM_TURNO)

ca_post_2012 = post_2012 %>% select(
  SG_PARTIDO,
  NM_URNA_CANDIDATO,
  CD_SITUACAO_CANDIDATURA,
  CD_DETALHE_SITUACAO_CAND,
  NR_CANDIDATO,
  ST_REELEICAO,
  DS_COMPOSICAO_COLIGACAO,
  NM_CANDIDATO,
  NR_CPF_CANDIDATO,
  SG_UF,
  SG_UE,
  NM_UE,
  CARGO,
  DT_ELEICAO,
  NR_TURNO)

colnames(ca_2012) = colnames(ca_post_2012)
ca = rbind(ca_2012, ca_post_2012)
rm(X2012_wd, post_2012, ca_2012, ca_post_2012)

vdigest = Vectorize(digest)
ca$elec_hash = vdigest(paste0(ca$SG_UF, ca$SG_UE, ca$NM_UE, ca$CARGO, ca$DT_ELEICAO, ca$NR_TURNO), algo='md5')
el = ca %>% select(
  elec_hash,
  SG_UF,
  SG_UE,
  NM_UE,
  CARGO,
  DT_ELEICAO,
  NR_TURNO) %>% distinct()
el = el %>% filter(!is.na(DT_ELEICAO)) # Suplementares Suspensas

write.csv(el, 'el.csv', row.names=F)

ca_oup = ca %>% select(
  SG_PARTIDO,
  NM_URNA_CANDIDATO,
  CD_SITUACAO_CANDIDATURA,
  CD_DETALHE_SITUACAO_CAND,
  NR_CANDIDATO,
  ST_REELEICAO,
  DS_COMPOSICAO_COLIGACAO,
  NM_CANDIDATO,
  NR_CPF_CANDIDATO,
  elec_hash)
write.csv(ca_oup, 'ca.csv', row.names=F)