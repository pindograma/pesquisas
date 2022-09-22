election_dates = tibble(
  year = c(2012, 2014, 2016, 2018, 2020),
  first_round_date = c(
    make_date(year = 2012, month = 10, day = 7),
    make_date(year = 2014, month = 10, day = 5),
    make_date(year = 2016, month = 10, day = 2),
    make_date(year = 2018, month = 10, day = 7),
    make_date(2020, 11, 15)
  ),
  second_round_date = c(
    make_date(year = 2012, month = 10, day = 28),
    make_date(year = 2014, month = 10, day = 26),
    make_date(year = 2016, month = 10, day = 30),
    make_date(year = 2018, month = 10, day = 28),
    make_date(2020, 11, 29)
  ),
  candidate_registry_date = c(
    make_date(2012, 7, 5),
    make_date(2014, 7, 5),
    make_date(2016, 8, 15),
    make_date(2018, 8, 15),
    make_date(2020, 9, 26)
  )
)

normalize_simple = function(x) {
  str_squish(toupper(stri_trans_general(str = x, id = 'Latin-ASCII')))
}

normalize_company = function(x) {
  normalize_simple(x) %>%
    str_replace_all('[\\.\\-\\,\\/]', ' ') %>%
    str_replace_all('&', 'E') %>%
    str_replace_all(' LTDA', ' ') %>%
    str_replace_all(' ME', ' ') %>%
    str_replace_all(' EPP', ' ') %>%
    str_replace_all(' S\\/?A', ' ') %>%
    str_squish()
}

normalize_cand = function(x) {
  normalize_simple(x) %>%
    str_replace_all('[\\.\\-]', ' ') %>%
    str_replace_all('\\sJR', ' JUNIOR') %>%
    str_replace_all('\\sPROF', ' PROFESSOR') %>%
    str_replace_all('\\sDR', ' DOUTOR') %>%
    str_replace_all('\\s+D[AEO]S?\\s+', ' ') %>%
    str_squish()
}

normalize_cand_2 = function(x) {
  word(x, 1, sep = '\\(') %>%
    word(1, sep = '-') %>%
    normalize_cand()
}

normalize_cand_rm_titles = function(x, fun = normalize_cand) {
  titles = c('DR ', 'DRA ', 'PROF ', 'PROFA ', 'PASTOR ', 'PASTORA ',
            'DOUTOR ', 'DOUTORA ', 'PROFESSOR ', 'PROFESSORA ',
            'GENERAL ', 'DELEGADO ', 'DELEGADA ', 'BRIGADEIRO ',
            'COMANDANTE ', 'JORNALISTA ', 'PROMOTOR ', 'JUIZ ',
            'PROMOTORA ', 'JUIZA ')
  
  fun(x) %>%
    mgsub(titles, rep(' ', length(titles))) %>%
    str_squish()
}

normalize_estatistico = function(x) {
  normalize_simple(x) %>%
    str_replace('^: ', '') %>%
    str_replace('CONRE.*$', '') %>%
    str_replace('CPF.*', '') %>%
    str_replace_all(' D[AEO]S? ', ' ') %>%
    str_replace_all('[\\-\\.]', ' ') %>%
    str_replace_all('0', 'O') %>%
    str_squish()
}

str_to_dbl = function(x) {
  as.double(sub(',', '.', x, fixed=T))
}

rtypes = cols(
  'util' = col_number(),
  'estimulada' = col_number(),
  'vv' = col_number(),
  'resul1' = col_character(),
  'resul2' = col_character(),
  'resul3' = col_character(),
  'resul4' = col_character(),
  'resul5' = col_character(),
  'resul6' = col_character(),
  'resul7' = col_character(),
  'resul8' = col_character(),
  'resul9' = col_character(),
  'resul10' = col_character(),
  'resul11' = col_character(),
  'resul12' = col_character(),
  'resul13' = col_character(),
  'resul14' = col_character(),
  'resul15' = col_character(),
  'resul16' = col_character(),
  'resul17' = col_character(),
  'resul18' = col_character(),
  'resul19' = col_character(),
  'resul20' = col_character(),
  'resul21' = col_character(),
  'resul22' = col_character(),
  'resul23' = col_character(),
  'resul24' = col_character(),
  'resul25' = col_character(),
  'resul26' = col_character(),
  'resul27' = col_character(),
  'resul28' = col_character(),
  'resul29' = col_character(),
  'resul30' = col_character(),
  'resul31' = col_character(),
  'resul32' = col_character(),
  'resul33' = col_character(),
  'resul34' = col_character(),
  'resul35' = col_character(),
  'resul36' = col_character(),
  'resul37' = col_character(),
  'resul38' = col_character(),
  'resul39' = col_character(),
  'resul40' = col_character(),
  'resul41' = col_character(),
  'resul42' = col_character(),
  'resul43' = col_character()
)

normalize_input = function(x, year__ = 2020) {
  x %>%
    select(-contains('unnamed')) %>%
    mutate_at(vars(matches('resul')), str_to_dbl) %>%
    filter(util == 1 & !is.na(cand1) & !is.na(cand2)) %>%
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
    mutate(year = year__)
}
