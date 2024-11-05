
#' Branch (i.e. copy) a slimmr SLiM model.
#'
#' @description Branches (copies) a slimmr SLiM model with all its components.
#'
#' @param slim_model An EidosModel object.
#' @param branch_name Name for the branched model.
#' @returns A complete copy of the slim_model, with all references to the
#' original model severed. Branched models are entirely separate.
#'
#' @export
#'
branch_model <- function(slim_model, branch_name)
{
  clone <- slim_model$get_lines_as_character() |>
    convert_script_to_eidoslines() |>
    group_eidoslines_in_eidosblocks() |>
    construct_eidosmodel_from_eidosblocks(name = branch_name)

  return(clone)
}
