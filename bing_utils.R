# bing_utils.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This program is licensed under the GNU General Public License, version 3.
# See the LICENSE file for details.

generate_bing_csv = function(name, filtered) {
  filtered %>% distinct(id_pesq) %>% write.csv(paste0(name, '.csv'), row.names=F)
}

generate_devonthink_csv = function(name, filtered, bing_df) {
  left_join(bing_df, filtered %>% select(f_id, SG_UF, NM_UE, DS_CARGOS), by=c('tse_id'='f_id')) %>%
    write.csv(paste0(name, '.csv'), row.names=F)
}

generate_devonthink_csv_2020 = function(name, filtered, bing_df) {
  bing_df %>%
    mutate(tse_id = str_replace_all(tse_id, '"', '')) %>%
    left_join(filtered %>% select(id_pesq, info_uf, info_muni, info_position), by = c('tse_id' = 'id_pesq')) %>%
      select(-name, -snippet) %>%
      write.csv(paste0(name, '.csv'), row.names = F)
}
