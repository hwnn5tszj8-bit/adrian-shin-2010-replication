# Bartlett HAC standard errors based on actual calendar lags. Sum scores within
# period for panels, allowing contemporaneous dependence across firms.
fit_hac <- function(data, formula, label, period, lags = 4L, date_col = "date") {
  columns <- unique(c(all.vars(formula), period, date_col))
  sample <- as.data.frame(data[stats::complete.cases(data[columns]), ])
  model <- stats::lm(formula, sample, na.action = stats::na.fail)
  x <- stats::model.matrix(model)
  if (model$rank < ncol(x) || stats::df.residual(model) < 1L) {
    stop("Regression is underidentified: ", label)
  }
  scores <- rowsum(x * stats::residuals(model), sample[[period]])
  periods <- as.integer(rownames(scores))
  meat <- crossprod(scores)
  for (lag in seq_len(lags)) {
    previous <- match(periods - lag, periods)
    paired <- !is.na(previous)
    cross <- crossprod(scores[paired, , drop = FALSE], scores[previous[paired], , drop = FALSE])
    meat <- meat + (1 - lag / (lags + 1)) * (cross + t(cross))
  }
  bread <- solve(crossprod(x))
  se <- sqrt(diag(bread %*% meat %*% bread))
  coefs <- stats::coef(model)
  summary <- summary(model)
  statistic <- coefs / se
  tibble::tibble(
    Model = label, Term = names(coefs), Estimate = unname(coefs),
    Std_Error_HAC = unname(se), t_stat = unname(statistic),
    p_value = unname(2 * stats::pt(-abs(statistic), stats::df.residual(model))),
    CI_Low = unname(coefs - stats::qt(0.975, stats::df.residual(model)) * se),
    CI_High = unname(coefs + stats::qt(0.975, stats::df.residual(model)) * se),
    R_squared = summary$r.squared, Adj_R_squared = summary$adj.r.squared,
    N = stats::nobs(model), First = min(sample[[date_col]]), Last = max(sample[[date_col]])
  )
}
