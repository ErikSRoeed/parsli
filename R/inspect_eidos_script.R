
#' Inspect the Eidos script of a SLiM model.
#'
#' @description This functions outputs the script contained in an EidosModel
#' object to the console.
#'
#' @param slim_model An EidosModel object.
#'
#' @export
#'
inspect_eidos_script <- function(slim_model)
{
  TAB = "\t"
  NEWLINE = "\n"
  line_number <- 1

  paste("#", TAB, slim_model$name, NEWLINE, NEWLINE, sep = "") |> cat()

  for (line_string in slim_model$get_lines_as_character())
  {
    paste(line_number, TAB, line_string, NEWLINE, sep = "") |> cat()
    line_number <- line_number + 1
  }
}
