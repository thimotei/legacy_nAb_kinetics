quantile_fun <- function(x) {
  list(
    me = quantile(x, 0.5),
    lo = quantile(x, 0.025),
    hi = quantile(x, 0.975)
  )
}
