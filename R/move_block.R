
#' Move a block in a slimmr SLiM model to a different index.
#'
#' @description Moves a block in a slimmr SLiM model to a different index.
#'
#' @param slim_model An EidosModel object.
#' @param from_index Original index of block.
#' @param after_index Move block to after block index ... (set 0 to move first!)
#'
#' @export
#'
move_block <- function(slim_model, from_index, after_index)
{
  block <- slim_model$blocks[[from_index]]

  if (from_index <= after_index)
  {
    after_index <- after_index - 1
  }

  slim_model$remove(from_index)
  slim_model$add(block, after_index)
}
