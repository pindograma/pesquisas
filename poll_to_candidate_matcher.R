# poll_to_candidate_matcher.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This file is licensed under the GNU General Public License, version 3.

spread_list = function(pred) {
  imap_dfr(pred, function(l, i) {
      if (length(l) == 0) { return(tibble()) }
      tibble(lhs = rep(i, length(l)), rhs = unlist(l))
  })
}


name_match = function(l, key, candkey = 'candidate', threshold = 0) {
  l %>%
    group_split(year, turno, SG_UE, CD_CARGO) %>%
    map_dfr(function(x) {
      x = x %>% mutate(rn = row_number())
      if (nrow(x) == 0) {
        return(tibble())
      }
      
      d = cands %>%
        filter(ANO_ELEICAO == x$year[1] &
               NUM_TURNO == x$turno[1] &
               SIGLA_UE == x$SG_UE[1] &
               CODIGO_CARGO == x$CD_CARGO[1]) %>%
        mutate(rn = row_number())
      if (nrow(d) == 0) {
        return(tibble())
      }
      
      wm = word_match(str_split(x[[candkey]], ' '), d[[key]], threshold)
      w = spread_list(wm)
      
      if (nrow(w) == 0) {
        return(tibble())
      }
      
      inner_join(x, w, by = c('rn' = 'lhs')) %>%
        inner_join(d, by = c('rhs' = 'rn')) %>%
        group_by(rn) %>%
        filter(n() == 1) %>%
        ungroup() %>%
        select(-rhs, -ANO_ELEICAO, -NUM_TURNO, -SIGLA_UF, -SIGLA_UE, -CODIGO_CARGO)
    }) %>%
    ungroup()
}

match_polls_with_candidates = function(data, use_sql = T) {
  left = data %>%
    mutate(ID = row_number()) %>%
    group_by(scenario_id) %>%
    mutate(scenario_count = n()) %>%
    ungroup()
  
  j1 = inner_join(left, cands, by = c(
    'year' = 'ANO_ELEICAO',
    'turno' = 'NUM_TURNO',
    'SG_UE' = 'SIGLA_UE',
    'CD_CARGO' = 'CODIGO_CARGO',
    'candidate' = 'NOME_URNA_CANDIDATO'
  ))
  left = left %>% filter(!(ID %in% j1$ID))
  
  j1_1 = inner_join(left, cands, by = c(
    'year' = 'ANO_ELEICAO',
    'turno' = 'NUM_TURNO',
    'SG_UE' = 'SIGLA_UE',
    'CD_CARGO' = 'CODIGO_CARGO',
    'candidate_without_title' = 'NOME_URNA_CANDIDATO'
  ))
  left = left %>% filter(!(ID %in% j1_1$ID))
  
  j2 = inner_join(left, cands, by = c(
    'year' = 'ANO_ELEICAO',
    'turno' = 'NUM_TURNO',
    'SG_UE' = 'SIGLA_UE',
    'CD_CARGO' = 'CODIGO_CARGO',
    'candidate' = 'NOME_CANDIDATO'
  ))
  left = left %>% filter(!(ID %in% j2$ID))
  
  j2_1 = inner_join(left, cands, by = c(
    'year' = 'ANO_ELEICAO',
    'turno' = 'NUM_TURNO',
    'SG_UE' = 'SIGLA_UE',
    'CD_CARGO' = 'CODIGO_CARGO',
    'candidate_without_title' = 'NOME_CANDIDATO'
  ))
  left = left %>% filter(!(ID %in% j2_1$ID))
  
  if (use_sql) {
    j3 = sqldf('
    SELECT * FROM left INNER JOIN cands ON
    left.year = cands.ANO_ELEICAO AND
    left.turno = cands.NUM_TURNO AND
    left.SG_UE = cands.SIGLA_UE AND
    left.CD_CARGO = cands.CODIGO_CARGO AND
    editdist3(left.candidate, cands.NOME_URNA_CANDIDATO) <= 200') %>%
      select(-ANO_ELEICAO, -NUM_TURNO, -SIGLA_UF, -SIGLA_UE, -CODIGO_CARGO)
    left = left %>% filter(!(ID %in% j3$ID))
    
    j3_1 = sqldf('
    SELECT * FROM left INNER JOIN cands ON
    left.year = cands.ANO_ELEICAO AND
    left.turno = cands.NUM_TURNO AND
    left.SG_UE = cands.SIGLA_UE AND
    left.CD_CARGO = cands.CODIGO_CARGO AND
    editdist3(left.candidate_without_title, cands.NOME_URNA_CANDIDATO) <= 200') %>%
      select(-ANO_ELEICAO, -NUM_TURNO, -SIGLA_UF, -SIGLA_UE, -CODIGO_CARGO)
    left = left %>% filter(!(ID %in% j3_1$ID))
    
    j4 = sqldf('
    SELECT * FROM left INNER JOIN cands ON
    left.year = cands.ANO_ELEICAO AND
    left.turno = cands.NUM_TURNO AND
    left.SG_UE = cands.SIGLA_UE AND
    left.CD_CARGO = cands.CODIGO_CARGO AND
    editdist3(left.candidate, cands.NOME_CANDIDATO) <= 200') %>%
      select(-ANO_ELEICAO, -NUM_TURNO, -SIGLA_UF, -SIGLA_UE, -CODIGO_CARGO)
    left = left %>% filter(!(ID %in% j4$ID))
    
    j4_1 = sqldf('
    SELECT * FROM left INNER JOIN cands ON
    left.year = cands.ANO_ELEICAO AND
    left.turno = cands.NUM_TURNO AND
    left.SG_UE = cands.SIGLA_UE AND
    left.CD_CARGO = cands.CODIGO_CARGO AND
    editdist3(left.candidate_without_title, cands.NOME_CANDIDATO) <= 200') %>%
      select(-ANO_ELEICAO, -NUM_TURNO, -SIGLA_UF, -SIGLA_UE, -CODIGO_CARGO)
    left = left %>% filter(!(ID %in% j4_1$ID))
  } else {
    j3 = tibble()
    j3_1 = tibble()
    j4 = tibble()
    j4_1 = tibble()
  }
  
  j5 = name_match(left, 'NOME_CANDIDATO')
  left = left %>% filter(!(ID %in% j5$ID))
  
  j6 = name_match(left, 'NOME_URNA_CANDIDATO')
  left = left %>% filter(!(ID %in% j6$ID))
  
  j7 = name_match(left, 'NOME_CANDIDATO', threshold = 2)
  left = left %>% filter(!(ID %in% j7$ID))
  
  j8 = name_match(left, 'NOME_URNA_CANDIDATO', threshold = 2)
  left = left %>% filter(!(ID %in% j8$ID))
  
  j5_1 = name_match(left, 'NOME_CANDIDATO', candkey = 'candidate_without_title')
  left = left %>% filter(!(ID %in% j5_1$ID))
  
  j6_1 = name_match(left, 'NOME_URNA_CANDIDATO', candkey = 'candidate_without_title')
  left = left %>% filter(!(ID %in% j6_1$ID))
  
  bind_rows(j1, j1_1, j2, j2_1, j3, j3_1, j4, j4_1, j5, j6, j7, j8, j5_1, j6_1) %>%
    group_by(scenario_id) %>%
    mutate(is_complete = n() == scenario_count) %>%
    ungroup()
}
