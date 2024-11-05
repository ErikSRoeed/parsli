
#' Add lines to a slimmr SLiM model.
#'
#' @description Adds one/more Eidos line(s) to a slimmr SLiM model.
#'
#' @param slim_model An EidosModel object.
#' @param lines A character string of lines to add, e.g. from readLines().
#' @param after_line An integer index: which line to add line(s) after?
#'
#' @export
#'
add_lines <- function(slim_model, lines, after_line)
{
  eidos_lines <- convert_script_to_eidoslines(lines)

  line_counter <- 0
  for (block in slim_model$blocks)
  {
    which_block <- block
    block_length <- length(which_block$lines)

    line_counter <- line_counter + block_length

    if (line_counter > after_line)
    {
      after_line <- (after_line + block_length) - line_counter
      break
    }
  }

  for (line in eidos_lines)
  {
    which_block$add(line, after = after_line)
    after_line <- after_line + 1
  }
}
