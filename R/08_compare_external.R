# Compare EB estimates against external benchmarks and across survey rounds.

compare_to_benchmarks <- function(eb_tbl, wpp_tbl, gbd_tbl) {
  eb_tbl %>%
    left_join(wpp_tbl, by = c("sib.sex", "sib.age")) %>%
    left_join(gbd_tbl, by = c("sib.sex", "sib.age"))
}

compare_rounds <- function(eb_new, eb_old, label_new = "2024", label_old = "2018") {
  eb_new %>%
    select(sib.sex, sib.age, M_EB_new = M_EB) %>%
    left_join(eb_old %>% select(sib.sex, sib.age, M_EB_old = M_EB),
              by = c("sib.sex", "sib.age")) %>%
    mutate(pct_change = 100 * (M_EB_new - M_EB_old) / M_EB_old) %>%
    rename(!!paste0("M_EB_", label_new) := M_EB_new,
           !!paste0("M_EB_", label_old) := M_EB_old)
}
