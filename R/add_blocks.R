
#' Add blocks to a parsli SLiM model.
#'
#' @description Adds one/more Eidos blocks (in order) to a parsli SLiM model.
#'
#' @param slim_model An EidosModel object.
#' @param blocks_script A string of lines to add, e.g. from readLines().
#' @param after_block An integer index: which block to add block after?
#'
#' @export
#'
add_blocks <- function(slim_model, blocks_script = character(), after_block)
{
  stopifnot(slim_model$assert_items_exist(after_block, allow_zero = TRUE))

  new_blocks <- blocks_script |>
    convert_script_to_eidoslines() |>
    group_eidoslines_in_eidosblocks()

  for (block in new_blocks)
  {
    slim_model$add(block, after_index = after_block)
    after_block <- after_block + 1
  }
}
