#' Import the results from the app
#' @param root the data folder of the app
#' @param target the folder were to store the aggregated data
#' @importFrom assertthat assert_that is.string
#' @importFrom dplyr bind_rows
#' @importFrom git2rdata write_vc
#' @importFrom purrr map
#' @importFrom utils file_test
#' @export
import_app_data <- function(root, target) {
  assert_that(is.string(root), is.string(target))
  assert_that(file_test("-d", root), file_test("-d", target))
  list.files(root, pattern = "intro.*tsv$") |>
    map(read_vc, root = root) |>
    bind_rows() |>
    write_vc("intro", root = target, sorting = c("session", "timestamp"))
  list.files(root, pattern = "exam.*tsv$") |>
    map(read_vc, root = root) |>
    bind_rows() |>
    write_vc("exam", root = target, sorting = c("session", "timestamp"))
  list.files(root, pattern = "preference_3.*tsv$") |>
    map(read_vc, root = root) |>
    bind_rows() |>
    write_vc(
      "preference_3", root = target, sorting = c("session", "timestamp", "id")
    )
  list.files(root, pattern = "preference_4.*tsv$") |>
    map(read_vc, root = root) |>
    bind_rows() |>
    write_vc(
      "preference_4", root = target, sorting = c("session", "timestamp", "id")
    )
  list.files(root, pattern = "preference_5.*tsv$") |>
    map(read_vc, root = root) |>
    bind_rows() |>
    write_vc(
      "preference_5", root = target, sorting = c("session", "timestamp", "id")
    )
  list.files(root, pattern = "preference_6.*tsv$") |>
    map(read_vc, root = root) |>
    bind_rows() |>
    write_vc(
      "preference_6", root = target, sorting = c("session", "timestamp", "id")
    )
  return(invisible(NULL))
}
