
#' Import a SLiM model from an Eidos script.
#'
#' @description This function imports a .slim script file and converts it to a
#' parsli EidosModel object. This object can be manipulated and run with the
#' various functions of parsli.
#'
#' @param script_path Path to .slim script.
#' @param name A name for the model (defaults to script_path)
#' @return An EidosModel object.
#'
#' @export
#'
import_slim_model <- function(script_path, name = script_path)
{
  model <- parse_script(script_path) |>
    convert_script_to_eidoslines() |>
    group_eidoslines_in_eidosblocks() |>
    construct_eidosmodel_from_eidosblocks(name = name)

  return(model)
}
