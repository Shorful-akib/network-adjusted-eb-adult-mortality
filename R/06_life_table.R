# Abridged period life table (ages 15-49) from EB-smoothed rates.

build_life_table <- function(eb_one_sex, radix = 100000) {
  df <- eb_one_sex %>% arrange(sib.age) %>%
    mutate(n = 5, a_x = 2.5, nqx = 1 - exp(-n * M_EB))

  lx <- numeric(nrow(df)); lx[1] <- radix
  for (i in 2:nrow(df)) lx[i] <- lx[i - 1] * (1 - df$nqx[i - 1])

  df %>% mutate(
    lx = lx, ndx = lx * nqx, nLx = n * (lx - ndx) + a_x * ndx,
    Tx = rev(cumsum(rev(nLx))), ex = Tx / lx
  )
}

partial_e35_15 <- function(life_table) sum(life_table$nLx) / life_table$lx[1]

bootstrap_e35_15 <- function(eb_boot_replicates) {
  eb_boot_replicates %>%
    group_by(rep, sib.sex) %>%
    arrange(sib.age, .by_group = TRUE) %>%
    summarise(e35_15 = {
      nqx <- 1 - exp(-5 * M_EB)
      lx <- numeric(length(nqx)); lx[1] <- 100000
      for (i in 2:length(nqx)) lx[i] <- lx[i - 1] * (1 - nqx[i - 1])
      ndx <- lx * nqx
      sum(5 * (lx - ndx) + 2.5 * ndx, na.rm = TRUE) / 100000
    }, .groups = "drop") %>%
    group_by(sib.sex) %>%
    summarise(e35_lo = quantile(e35_15, .025, na.rm = TRUE),
              e35_hi = quantile(e35_15, .975, na.rm = TRUE), .groups = "drop")
}
