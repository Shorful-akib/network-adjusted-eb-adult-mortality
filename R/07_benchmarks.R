# External benchmarks: WPP single-year rates (data_raw/wpp_nigeria_raw.csv)
# and GBD 2021 all-cause rates (user-supplied CSV export), both harmonized
# to 5-year age bands, per person-year, averaged over 2017-2023/2024.

build_wpp_benchmark <- function(wpp_csv = file.path(DATA_DIR, "wpp_nigeria_raw.csv")) {
  read.csv(wpp_csv) %>%
    pivot_longer(starts_with("r20"), names_to = "year", values_to = "rate") %>%
    mutate(sib.sex = sex,
           sib.age = AGE_LABELS[pmin(pmax(floor((age - 15) / 5) + 1, 1), 7)]) %>%
    group_by(sib.sex, sib.age) %>%
    summarise(M_WPP = mean(rate), .groups = "drop")
}

build_gbd_benchmark <- function(gbd_csv, years = 2017:2023) {
  read.csv(gbd_csv) %>%
    filter(metric_name == "Rate", cause_name == "All causes",
           sex_name %in% c("Male", "Female"), year %in% years,
           age_name %in% AGE_LABELS) %>%
    mutate(sib.sex = ifelse(sex_name == "Female", "f", "m"),
           rate_py = val / 100000) %>%
    group_by(sib.sex, sib.age = age_name) %>%
    summarise(M_GBD = mean(rate_py), .groups = "drop")
}
