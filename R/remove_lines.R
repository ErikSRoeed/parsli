
#' Remove lines from a parsli SLiM model.
#'
#' @description Removes one/more Eidos line(s) from a parsli SLiM model.
#'
#' @param slim_model An EidosModel object.
#' @param line_indices Index/indices of line(s) to be removed.
#'
#' @export
#'
remove_lines <- function(slim_model, line_indices, .force = FALSE)
{
  stopifnot((line_indices %in% seq(length(slim_model$lines))))

  model_line_counter <- 0

  for (block in slim_model$blocks)
  {
    block_line_counter <- 1

    for (line in block$lines)
    {
      model_line_counter <- model_line_counter + 1

      if (model_line_counter %in% line_indices)
      {
        if (! .force)
        {
          line_is_callback <- block$lines[[block_line_counter]]$is_callback
          stopifnot(! line_is_callback)
        }
        block$remove(block_line_counter)
        next
      }

      block_line_counter <- block_line_counter + 1
    }
  }
}
