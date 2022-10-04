#' Generate a design with both pairwise comparisons and constant effects
#' @param design_effect A named list of vectors with design effects.
#' One character vector with effect levels per design effect.
#' The output contains paired questions of design effects.
#' @param design_data  A named list of vectors with design data.
#' One character vector with data levels per design data.
#' The output contains a constant value for the data.
#' @param max_question The maximum number of questions.
#' Defaults to 10.
#' @export
#' @importFrom dplyr ends_with inner_join mutate row_number select slice_sample
#' @importFrom purrr map
#' @importFrom tidyselect all_of
#' @importFrom utils combn head
generate_design <- function(design_effect, design_data, max_question = 10) {
  design_element <- vapply(
    seq_along(design_effect), FUN.VALUE = vector(mode = "list", 1),
    design_effect = design_effect,
    FUN = function(i, design_effect) {
      x <- combn(design_effect[[i]], m = 2)
      x |>
        t() |>
        as.data.frame() |>
        mutate(id = row_number()) |>
        `colnames<-`(paste0(names(design_effect[i]), c("_a", "_b", "_id"))) |>
        list()
    }
  )
  map(design_element, select, ends_with("_id")) |>
    unlist(recursive = FALSE) |>
    expand.grid() |>
    mutate(id = row_number()) -> design
  for (x in design_element) {
    design <- merge(design, x)
  }
  list(id = design$id) |>
    c(design_data) |>
    expand.grid() -> combs
  factors <- names(which(vapply(combs, is.factor, logical(1))))
  combs %>%
    mutate(across(all_of(factors), as.character)) -> combs
  if (nrow(combs) <= max_question) {
    combs |>
      inner_join(design, by = "id") |>
      randomize_alternative() -> selection
    return(selection)
  }
  combs_data <- expand.grid(design_data)
  factors <- names(which(vapply(combs_data, is.factor, logical(1))))
  combs_data %>%
    mutate(across(all_of(factors), as.character)) -> combs_data
  sample_data <- sample(nrow(combs_data))
  while (length(sample_data) < max_question) {
    sample_data <- c(sample_data, sample(nrow(combs_data)))
  }
  sample_effect <- sample(design$id)
  while (length(sample_effect) < max_question) {
    sample_effect <- c(sample_effect, sample(design$id))
  }
  combs_data[head(sample_data, max_question), , drop = FALSE] |>
    cbind(id = head(sample_effect, max_question)) |>
    inner_join(design, by = "id") |>
    randomize_alternative()
}

randomize_alternative <- function(z) {
  for (i in grep("_a$", colnames(z))) {
    change <- sample(nrow(z), size = floor(nrow(z) / 2))
    b <- gsub("_a$", "_b", colnames(z)[i])
    old <- z[change, i]
    z[change, i] <- z[change, b]
    z[change, b] <- old
  }
  return(slice_sample(z, prop = 1))
}
