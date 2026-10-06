# Invisibility (p_I), reporting-accuracy (gamma) and internal-consistency
# (Delta_alpha) diagnostics -- Feehan & Borges (2021) sensitivity framework.

invisible_fraction <- function(sib.dat, ego.dat) {
  sib.dat %>%
    group_by(caseid) %>%
    summarise(y_iF = sum(in.F, na.rm = TRUE), .groups = "drop") %>%
    right_join(ego.dat %>% select(caseid, wwgt, age5), by = "caseid") %>%
    mutate(y_iF = coalesce(y_iF, 0L), invisible = as.integer(y_iF == 0)) %>%
    group_by(age5) %>%
    summarise(p_I = weighted.mean(invisible, wwgt), .groups = "drop") %>%
    mutate(sib.age = AGE_LABELS[age5])
}

k_sensitivity <- function(eb_tbl, pI_tbl, K_vals = c(1, 1.5, 2, 3)) {
  eb_tbl %>%
    left_join(pI_tbl %>% select(sib.age, p_I), by = "sib.age") %>%
    crossing(K = K_vals) %>%
    mutate(M_total = M_EB * (1 + p_I * (K - 1)))
}

gamma_sensitivity <- function(eb_tbl,
                              gamma_vals = c(.75, .85, .90, .95, 1.00, 1.05, 1.10, 1.15, 1.25)) {
  eb_tbl %>%
    crossing(gamma = gamma_vals) %>%
    mutate(M_adjusted = M_EB * gamma)
}

# Reciprocal network-reporting diagnostic (Feehan & Cobb 2019; Feehan &
# Borges 2021), Section 2.3.3. Descriptive only, per Reviewer 2: reports
# the symmetry ratio R_alpha = y(F_alpha, F_-alpha) / y(F_-alpha, F_alpha),
# the signed imbalance B_alpha = 100(R_alpha - 1), and its magnitude
# A_alpha = |B_alpha|, contextualized by rank/median/range across the
# seven age bands rather than a hypothesis test.
internal_consistency <- function(sib.dat, ego.dat) {

  sib_age <- sib.dat %>%
    filter(in.F == 1) %>%
    left_join(ego.dat %>% select(caseid, age5, wwgt), by = "caseid") %>%
    mutate(sib_age5 = pmin(pmax(floor((sib.age - 15) / 5) + 1, 1), 7))

  result <- map_dfr(1:7, function(a) {
    y_out <- sum(sib_age$wwgt[sib_age$age5 == a & sib_age$sib_age5 != a], na.rm = TRUE)
    y_in  <- sum(sib_age$wwgt[sib_age$age5 != a & sib_age$sib_age5 == a], na.rm = TRUE)
    tibble(sib.age = AGE_LABELS[a], y_out = y_out, y_in = y_in,
           R_alpha = y_out / y_in, B_alpha = 100 * (y_out / y_in - 1))
  }) %>%
    mutate(A_alpha = abs(B_alpha), rank = rank(-A_alpha))

  result %>%
    mutate(A_alpha_median = median(A_alpha), A_alpha_range = max(A_alpha) - min(A_alpha))
}
