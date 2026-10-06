# OPTIONAL / supplementary: age-covariate (Fay-Herriot style) linking model,
# shrinking toward a fitted linear age trend instead of a flat grand mean.
# Not the primary model reported in the main results; kept separate.

sure_eb_trend <- function(Z, sigma2, x) {
  X <- cbind(1, x)

  fitted_beta <- function(A) {
    V_inv <- diag(1 / (sigma2 + A))
    solve(t(X) %*% V_inv %*% X) %*% t(X) %*% V_inv %*% Z
  }
  hat_diag <- function(A) {
    V_inv <- diag(1 / (sigma2 + A))
    H <- X %*% solve(t(X) %*% V_inv %*% X) %*% t(X) %*% V_inv
    diag(H)
  }

  sure_objective <- function(A) {
    A <- max(A, 1e-10)
    beta <- fitted_beta(A)
    mu_x <- as.numeric(X %*% beta)
    w    <- A / (A + sigma2)
    th   <- mu_x + w * (Z - mu_x)
    h    <- hat_diag(A)
    divergence <- w + (1 - w) * h
    sum(-sigma2 + (Z - th)^2 + 2 * sigma2 * divergence)
  }

  opt   <- optimise(sure_objective, interval = c(1e-10, max(var(Z) * 100, 10)))
  A_hat <- max(opt$minimum, 0)
  beta  <- fitted_beta(A_hat)
  mu_x  <- as.numeric(X %*% beta)
  w_hat <- A_hat / (A_hat + sigma2)

  list(A_hat = A_hat, beta = beta, w_hat = w_hat,
       theta_EB = mu_x + w_hat * (Z - mu_x),
       M_EB = exp(mu_x + w_hat * (Z - mu_x)))
}

run_eb_trend <- function(asdr_ind) {
  asdr_ind %>%
    group_by(sib.sex) %>%
    arrange(sib.age, .by_group = TRUE) %>%
    group_modify(~ {
      Z      <- log(.x$asdr.hat)
      sigma2 <- 1 / .x$num.hat
      eb <- sure_eb_trend(Z, sigma2, X_CENTERED)
      mutate(.x, A_hat = eb$A_hat, w_alpha = eb$w_hat, M_EB_trend = eb$M_EB)
    }) %>%
    ungroup()
}
