#' Access metadata for all available datasets provided by the Peruvian Central Bank
#' @export
#' @examples
#' # No arguments required, simply call the function.
#' get_bcrp_metadata()
#'
#' @return A data frame with one row per code available for request.
#' @author Julio Collazos
get_bcrp_metadata <- function() {
  url <- "https://estadisticas.bcrp.gob.pe/estadisticas/series/metadata"
  temp <- tempfile(fileext = ".csv")
  on.exit(unlink(temp), add = TRUE)
  httr2::req_perform(
    httr2::request(url),
    path = temp
  )

  suppressMessages(
    readr::read_delim(
      temp,
      locale = readr::locale(encoding = "latin1"),
      delim = ";",
      show_col_types = FALSE
    )
  )
}
