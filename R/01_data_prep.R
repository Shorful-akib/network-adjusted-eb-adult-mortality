# Load one DHS round (IR file), standardize sibling histories, and
# compute the aggregate and individual-visibility direct estimates.

prep_round <- function(ir_path, ref_window = "7yr_beforeinterview") {

  ir <- read_dta(ir_path)

  prepped <- prep_dhs_sib_histories(
    ir, varmap = sibhist_varmap_dhs8, keep_missing = FALSE
  )

  sib.dat <- prepped$sib.dat %>%
    mutate(
      sib.age   = as.numeric(sib.age),
      sib.alive = as.numeric(sib.alive),
      in.F = as.numeric(sib.alive == 1 & sib.age >= 15 & sib.age <= 49 &
                         sib.sex == "f")
    )
  ego.dat <- prepped$ego.dat

  cc <- cell_config(
    age.groups   = "5yr_to50",
    time.periods = ref_window,
    start.obs    = "sib.dob",
    end.obs      = "sib.endobs",
    event        = "sib.death.date",
    age.offset   = "sib.dob",
    time.offset  = "doi",
    exp.scale    = 1 / 12
  )

  ests <- sibling_estimator(
    sib.dat = sib.dat, ego.id = "caseid", sib.id = "sibid",
    sib.frame.indicator = "in.F", sib.sex = "sib.sex",
    cell.config = cc, weights = "wwgt"
  )

  list(ego.dat = ego.dat, sib.dat = sib.dat, cc = cc,
       asdr.ind = ests$asdr.ind, asdr.agg = ests$asdr.agg)
}
