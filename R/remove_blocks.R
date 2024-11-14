
#' Remove blocks from a parsli SLiM model.
#'
#' @description Removes one/more Eidos block(s) from a parsli SLiM model.
#'
#' @param slim_model An EidosModel object.
#' @param block_indices Index/indices of block(s) to be removed.
#'
#' @export
#'
remove_blocks <- function(slim_model, block_indices)
{
  stopifnot(slim_model$assert_items_exist(block_indices))
  slim_model$remove(block_indices)
}
