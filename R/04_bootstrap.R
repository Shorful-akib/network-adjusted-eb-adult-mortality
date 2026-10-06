# Rao-Wu rescaled bootstrap: recompute the full pipeline (visibility
# estimator + SURE-EB) inside every replicate. Replicate id column is
# "rep" OR "boot_idx".

run_bootstrap <- function(sib.dat, ego.dat, cc, B = N_BOOT) {
  bw <- rescaled.bootstrap.weights(
    survey.design = ~ psu + strata(stratum_design),
    num.reps = B, idvar = "caseid", weights = "wwgt", survey.data = ego.dat
  )

  sibling_estimator(
    sib.dat = sib.dat, ego.id = "caseid", sib.id = "sibid",
    sib.frame.indicator = "in.F", sib.sex = "sib.sex",
    cell.config = cc, weights = "wwgt",
    boot.weights = bw, return.boot = TRUE
  )
}

eb_bootstrap_ci <- function(boot_ests) {
  boot_ind <- boot_ests$boot.asdr.ind

  eb_boot <- boot_ind %>%
    group_by(rep, sib.sex) %>%
    arrange(sib.age, .by_group = TRUE) %>%
    group_modify(~ {
      if (any(.x$num.hat <= 0, na.rm = TRUE) || nrow(.x) < 3) {
        return(mutate(.x, M_EB = NA_real_))
      }
      eb <- sure_eb_heteroscedastic(log(.x$asdr.hat), 1 / .x$num.hat)
      mutate(.x, M_EB = eb$M_EB)
    }) %>%
    ungroup()

  list(
    replicates = eb_boot,
    ci = eb_boot %>%
      group_by(sib.sex, sib.age) %>%
      summarise(
        M_EB_lo = quantile(M_EB, 0.025, na.rm = TRUE),
        M_EB_hi = quantile(M_EB, 0.975, na.rm = TRUE),
        .groups = "drop"
      )
  )
}
