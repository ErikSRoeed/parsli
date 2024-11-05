
#' Replace eidos patterns at model, block, or line level.
#'
#' @description Replaces an Eidos pattern at model, block, or line level.
#'
#' @param slim_model An EidosModel object.
#' @param pattern Eidos script pattern to replace.
#' @param replacement Eidos script replacement for the pattern.
#' @param in_blocks Which blocks to replace in? Incompatible with in_lines.
#' If in_blocks and in_lines are both NULL, will replace across entire model.
#' @param in_lines Which lines to replace in? Incompatible with in_blocks.
#' If in_blocks and in_lines are both NULL, will replace across entire model.
#'
#' @export
#'
replace_eidos_pattern <- function(
    slim_model,
    pattern,
    replacement,
    in_blocks = NULL,
    in_lines = NULL
)
{
  blocks_provided <- ! is.null(in_blocks)
  lines_provided <- ! is.null(in_lines)

  if (blocks_provided & lines_provided)
  {
    stop("slimmr: Please specify at most one of in_blocks and in_lines.")
  }

  if (blocks_provided)
  {
    slim_model$substitute(pattern, replacement, in_blocks)
    return()
  }

  if (lines_provided)
  {
    for (line in slim_model$lines[in_lines])
    {
      line$substitute(pattern, replacement)
    }
    return()
  }

  slim_model$substitute(pattern, replacement)
}
