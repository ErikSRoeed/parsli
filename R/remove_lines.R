
#' Remove lines from a slimmr SLiM model.
#'
#' @description Removes one/more Eidos line(s) from a slimmr SLiM model.
#'
#' @param slim_model An EidosModel object.
#' @param line_indices Index/indices of line(s) to be removed.
#'
#' @export
#'
remove_lines <- function(slim_model, line_indices)
{
  model_line_counter <- 0

  for (block in slim_model$blocks)
  {
    block_line_counter <- 1

    for (line in block$lines)
    {
      model_line_counter <- model_line_counter + 1

      if (model_line_counter %in% line_indices)
      {
        block$remove(block_line_counter)
        next
      }

      block_line_counter <- block_line_counter + 1
    }
  }
}
