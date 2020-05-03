generate_bing_csv = function(name, filtered) {
  filtered %>% %>% distinct(f_id)
}

generate_devonthink_csv = function(name, filtered, bing_df) {
  left_join(bing_df, filtered %>% select(f_id, SG_UF, NM_UE, DS_CARGOS), by=c('tse_id'='f_id'))
}