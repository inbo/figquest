#' Generate a time series
#' @param size The effect size of the strongest effect.
#' Must be one of `"stable"`, `"moderate"`, `"strong"` or `"potential"`.
#' @inheritParams scale_data
#' @export
#' @importFrom stats runif
generate_data <- function(
    size = c("stable", "moderate", "strong", "potential"), threshold = log(0.75)
  ) {
  size <- match.arg(size)
  f_down_low <- switch(
    size, stable = -0.95, moderate = 0.05, strong = 1.05, potential = -0.95
  )
  f_down_high <- switch(
    size, stable = -0.05, moderate = 0.5, strong = 1.5, potential = -0.05
  )
  f_down <- runif(1, f_down_low, f_down_high)
  f_up_low <- switch(
    size, stable = 0.05, moderate = f_down_high, strong = f_down_high,
    potential = 1.5
  )
  f_up_high <- switch(
    size, stable = 0.95, moderate = 0.95, strong = 2, potential = 2
  )
  f_up <- runif(1, f_up_low, f_up_high)
  direction <- sample(c(-1, 1), 1)
  ds <- scale_data(
    f_down = min(direction * c(f_down, f_up)),
    f_up = max(direction * c(f_down, f_up)), threshold = threshold
  )
  attr(ds, "direction") <- direction
  return(ds)
}

#' Generate a time series
#' @param f_down Relative location of the lower confidence limit of the
#' strongest effect.
#' Relative to the `threshold`.
#' @param f_up Relative location of the upper confidence limit of the
#' strongest effect.
#' Relative to the `threshold`.
#' @param threshold log threshold value.
#' @export
#' @importFrom assertthat assert_that is.number
#' @importFrom dplyr %>% across arrange transmute
#' @importFrom effectclass classification
#' @importFrom git2rdata read_vc
#' @importFrom rlang .data
#' @importFrom stats qnorm
scale_data <- function(f_down = -0.75, f_up = 0.25, threshold = log(0.75)) {
  assert_that(
    is.number(f_down), is.number(f_up), is.number(threshold), f_down < f_up
  )
  sigma <- abs(threshold) * 0.5 * (f_up - f_down) / qnorm(0.95)
  flip <- sample(c(-1, 1), 1)
  read_vc("base_data", root = system.file(package = "figquest")) %>%
    transmute(
      x = flip * .data$x,
      x = .data$x - min(.data$x) + 2001,
      mu = .data$y * abs(threshold) * 0.5 * (f_up + f_down),
      lcl_90 = qnorm(0.05, mean = .data$mu, sd = sigma),
      ucl_90 = qnorm(0.95, mean = .data$mu, sd = sigma),
      lcl_60 = qnorm(0.2, mean = .data$mu, sd = sigma),
      ucl_60 = qnorm(0.8, mean = .data$mu, sd = sigma),
      lcl_30 = qnorm(0.35, mean = .data$mu, sd = sigma),
      ucl_30 = qnorm(0.65, mean = .data$mu, sd = sigma),
      across(c(-"x", -"y"), exp),
      classification = classification(
        lcl = .data$lcl_90, ucl = .data$ucl_90,
        threshold = exp(c(-1, 1) * threshold), reference = 1
      )
    ) %>%
    arrange(.data$x) -> dataset
  attr(dataset, "threshold") <- threshold
  attr(dataset, "f_down") <- f_down
  attr(dataset, "f_up") <- f_up
  attr(dataset, "flip") <- flip
  return(dataset)
}
