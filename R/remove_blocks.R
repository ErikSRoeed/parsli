
#' Remove blocks from a slimmr SLiM model.
#'
#' @description Removes one/more Eidos block(s) from a slimmr SLiM model.
#'
#' @param slim_model An EidosModel object.
#' @param block_indices Index/indices of block(s) to be removed.
#'
#' @export
#'
remove_blocks <- function(slim_model, block_indices)
{
  slim_model$remove(block_indices)
}
