# Age heaping (Whipple's / Myers' indices) and date-telescoping checks,
# applied to the raw SR file before package standardization.

whipple_index <- function(ages) {
  ages <- ages[!is.na(ages) & ages >= 23 & ages <= 47]
  num   <- sum(ages %in% c(25, 30, 35, 40, 45))
  denom <- length(ages) / 5
  100 * num / denom
}

# Simplified two-point blended digit-preference index (0-10 good,
# 10-20 moderate, >20 severe). Restricted to ages 12-49.
myers_index <- function(ages) {
  ages <- ages[!is.na(ages) & ages >= 12 & ages <= 49]
  pct <- sapply(0:9, function(d) mean(ages %% 10 == d) * 100)
  blended <- sapply(1:10, function(i) (pct[i] + pct[i %% 10 + 1]) / 2)
  sum(abs(blended - 10)) / 2
}

date_telescoping_check <- function(sr_raw, boundary_months = 84, window = 12) {
  sr_raw %>%
    filter(mm2 == 0, !is.na(mm8), !is.na(v008)) %>%
    mutate(months_before = v008 - mm8) %>%
    filter(abs(months_before - boundary_months) <= window) %>%
    count(months_before) %>%
    arrange(months_before)
}

run_data_quality <- function(sr_raw) {
  dead <- sr_raw %>% filter(mm2 == 0, !is.na(mm7), mm7 >= 15, mm7 <= 49)

  indices <- tibble(
    whipple_overall = whipple_index(dead$mm7),
    whipple_female  = whipple_index(dead$mm7[dead$mm1 == 2]),
    whipple_male    = whipple_index(dead$mm7[dead$mm1 == 1]),
    myers           = myers_index(dead$mm7)
  )

  list(indices = indices, telescoping = date_telescoping_check(sr_raw))
}
