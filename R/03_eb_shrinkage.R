# Heteroscedastic SURE empirical Bayes shrinkage (Xie, Kou & Brown, 2012),
# with the divergence term corrected for a data-estimated prior mean.

sure_eb_heteroscedastic <- function(Z, sigma2) {
  prec  <- 1 / sigma2
  P     <- sum(prec)
  Z_bar <- sum(prec * Z) / P

  sure_objective <- function(A) {
    A  <- max(A, 1e-10)
    w  <- A / (A + sigma2)
    th <- Z_bar + w * (Z - Z_bar)
    divergence <- w + (1 - w) * (prec / P)
    sum(-sigma2 + (Z - th)^2 + 2 * sigma2 * divergence)
  }

  opt   <- optimise(sure_objective, interval = c(1e-10, max(var(Z) * 100, 10)))
  A_hat <- max(opt$minimum, 0)
  w_hat <- A_hat / (A_hat + sigma2)

  list(
    A_hat = A_hat, Z_bar = Z_bar, w_hat = w_hat,
    theta_EB = Z_bar + w_hat * (Z - Z_bar),
    post_var = w_hat * sigma2,
    M_EB     = exp(Z_bar + w_hat * (Z - Z_bar)),
    SURE_min = opt$objective
  )
}

run_eb <- function(asdr_ind) {
  asdr_ind %>%
    group_by(sib.sex) %>%
    arrange(sib.age, .by_group = TRUE) %>%
    group_modify(~ {
      Z      <- log(.x$asdr.hat)
      sigma2 <- 1 / .x$num.hat
      eb <- sure_eb_heteroscedastic(Z, sigma2)
      mutate(.x,
        Z_alpha = Z, sigma2 = sigma2, A_hat = eb$A_hat,
        w_alpha = eb$w_hat, theta_EB = eb$theta_EB,
        post_var = eb$post_var, M_EB = eb$M_EB
      )
    }) %>%
    ungroup()
}
