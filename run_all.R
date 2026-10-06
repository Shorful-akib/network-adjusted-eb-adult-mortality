# Full pipeline: source scripts 00-08 (09 is optional), run for the 2024
# and 2018 Nigeria DHS rounds, write all manuscript tables to outputs/.
#
# Required raw files in data_raw/  (see README.md):
#   NGIR8BFL.DTA, NGSR8BFL.DTA   (2024 round)
#   NGIR7BFL.DTA, NGSR7BFL.DTA   (2018 round)
#   wpp_nigeria_raw.csv          (included)
#   gbd_nigeria.csv              (user-supplied IHME GBD export)

for (f in sprintf("R/%02d_%s.R", 0:8,
                   c("setup","data_prep","data_quality","eb_shrinkage",
                     "bootstrap","sensitivity","life_table","benchmarks",
                     "compare_external"))) source(f)

run_survey_round <- function(ir_path, sr_path, label) {

  round <- prep_round(ir_path)
  sr_raw <- read_dta(sr_path)

  dq <- run_data_quality(sr_raw)
  eb <- run_eb(round$asdr.ind)
  boot_ests <- run_bootstrap(round$sib.dat, round$ego.dat, round$cc)
  boot_ci   <- eb_bootstrap_ci(boot_ests)
  eb_final  <- eb %>% left_join(boot_ci$ci, by = c("sib.sex", "sib.age"))

  visibility <- round$asdr.ind %>%
    select(sib.sex, sib.age, num.hat, asdr.hat) %>%
    left_join(round$asdr.agg %>% select(sib.sex, sib.age, M_agg = asdr.hat),
              by = c("sib.sex", "sib.age")) %>%
    rename(M_ind = asdr.hat) %>%
    mutate(ratio = M_ind / M_agg)

  pI <- invisible_fraction(round$sib.dat, round$ego.dat)

  # Descriptive diagnostic (R_alpha, B_alpha, A_alpha; Section 2.3.3) --
  # no bootstrap needed, since the manuscript treats this as a point-estimate
  # ranking across age bands, not a hypothesis test.
  ic <- internal_consistency(round$sib.dat, round$ego.dat)

  lt <- eb_final %>% group_by(sib.sex) %>%
    group_modify(~ build_life_table(.x)) %>% ungroup()
  e35 <- bootstrap_e35_15(boot_ci$replicates)

  out <- list(dq_indices = dq$indices, dq_telescoping = dq$telescoping,
              visibility = visibility, eb = eb_final, invisible = pI,
              internal_consistency = ic, life_table = lt, e35_15 = e35,
              k_sensitivity = k_sensitivity(eb_final, pI),
              gamma_sensitivity = gamma_sensitivity(eb_final))

  saveRDS(out, file.path(OUT_DIR, paste0("results_", label, ".rds")))
  for (nm in names(out)) {
    if (is.data.frame(out[[nm]]))
      write.csv(out[[nm]], file.path(OUT_DIR, paste0(label, "_", nm, ".csv")),
                row.names = FALSE)
  }
  out
}

res_2024 <- run_survey_round(file.path(DATA_DIR, "NGIR8BFL.DTA"),
                              file.path(DATA_DIR, "NGSR8BFL.DTA"), "2024")
res_2018 <- run_survey_round(file.path(DATA_DIR, "NGIR7BFL.DTA"),
                              file.path(DATA_DIR, "NGSR7BFL.DTA"), "2018")

wpp <- build_wpp_benchmark()
gbd <- build_gbd_benchmark(file.path(DATA_DIR, "gbd_nigeria.csv"))

comparison_external <- compare_to_benchmarks(res_2024$eb, wpp, gbd)
comparison_rounds    <- compare_rounds(res_2024$eb, res_2018$eb)

write.csv(comparison_external, file.path(OUT_DIR, "comparison_external.csv"), row.names = FALSE)
write.csv(comparison_rounds,    file.path(OUT_DIR, "comparison_rounds.csv"),    row.names = FALSE)
