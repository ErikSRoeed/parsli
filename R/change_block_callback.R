
#' Change the callback of a block in a slimmr SLiM model.
#'
#' @description Changes the callback of one Eidos block in a slimmr SLiM model.
#' A more convenient alternative to slimmr::replace_eidos_pattern for callback
#' modification, but that function can be used to the same ends if you prefer.
#'
#' @param slim_model An EidosModel object.
#' @param block_index Which block to change callback for?
#' @param new_callback E.g. "1000 late()"
#'
#' @export
#'
change_block_callback <- function(slim_model, block_index, new_callback)
{
  slim_model$blocks[[block_index]]$change_callback(new_callback)
}
