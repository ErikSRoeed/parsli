
#' check_list_contains_only_correct_class
#'
#' @description Internal slimmr function.
#'
#' @param list A list() of objects.
#' @param correct_class The desired class of the objects in list
#' @returns TRUE/FALSE
#'
#' @noRd
#'
check_list_contains_only_correct_class <- function(list, correct_class)
{
  check_class <- function(object)
  {
    is_correct <- as.character(correct_class) %in% class(object)
    return(is_correct)
  }

  class_checks <- vapply(list, check_class, FUN.VALUE = logical(1))
  return(class_checks)
}

#' parse_script
#'
#' @description Internal slimmr function.
#'
#' @param script_path Valid character path to a .slim script file.
#' @param drop_empty_lines Boolean, default FALSE. Drop empty lines from script?
#' @returns A character vector with one item per line in the parsed script.
#'
#' @noRd
#'
parse_script <- function(script_path, drop_empty_lines = FALSE)
{
  stopifnot(file.exists(script_path))
  script_lines <- readLines(script_path)

  if (drop_empty_lines)
  {
    empty_lines <- which(script_lines == "")
    script_lines <- script_lines[-empty_lines]
  }

  return(script_lines)
}

#' convert_script_to_eidoslines
#'
#' @description Internal slimmr function.
#'
#' @param script_lines char. vector of script lines from slimmr::parse_script().
#' @returns A list() of slimmr::EidosLine objects.
#'
#' @noRd
#'
convert_script_to_eidoslines <- function(script_lines)
{
  stopifnot(script_lines |> is.character())
  stopifnot(script_lines |> is.null() |> isFALSE())

  line_numbers <- 1 : length(script_lines)
  eidos_lines <- list()

  for (number in line_numbers)
  {
    eidos_lines[[number]] <- script_lines[number] |> EidosLine$new()
  }

  return(eidos_lines)
}

#' group_eidoslines_in_eidosblocks
#'
#' @description Internal slimmr function.
#'
#' @param eidos_lines A list() of slimmr::EidosLine objects.
#' @returns A list of slimmr::EidosBlock objects.
#'
#' @noRd
#'
group_eidoslines_in_eidosblocks <- function(eidos_lines)
{
  stopifnot(eidos_lines |> is.list())
  stopifnot(
    eidos_lines |>
      check_list_contains_only_correct_class("EidosLine") |>
      all()
  )

  empty_first_block <- EidosBlock$new(items = list())
  blocks <- list(empty_first_block)

  current_block_number <- 1
  must_add_new_block <- FALSE

  for (line in eidos_lines)
  {
    possibly_add_new_block <- line$is_comment || line$is_callback

    if (possibly_add_new_block)
    {
      must_add_new_block <- TRUE
    }

    current_block_needs_callback <- blocks[[current_block_number]]$callback |> is.null()

    if (current_block_needs_callback)
    {
      must_add_new_block <- FALSE
    }

    not_add_new_block <- ! line$is_toplevel

    if (not_add_new_block)
    {
      must_add_new_block <- FALSE
    }

    if (must_add_new_block)
    {
      current_block_number <- current_block_number + 1
      blocks[[current_block_number]] <- EidosBlock$new(items = list(line))
      next
    }

    blocks[[current_block_number]]$add(line)
  }

  return(blocks)
}

#' construct_eidosmodel_from_eidosblocks
#'
#' @description Internal slimmr function.
#'
#' @param eidos_blocks A list() of slimmr::EidosBlock objects.
#' @returns An EidosModel object.
#'
#' @noRd
#'
construct_eidosmodel_from_eidosblocks <- function(name, eidos_blocks)
{
  stopifnot(eidos_blocks |> is.list())
  stopifnot(
    eidos_blocks |>
      check_list_contains_only_correct_class("EidosBlock") |>
      all()
  )

  model <- EidosModel$new(name = name, eidos_blocks = eidos_blocks)
  return(model)
}

#' Check SLiM installation.
#'
#' @description Internal slimmr function. Attempt "slim -version". Stop if fail.
#'
#' @param slim_command Full path to SLiM executable, or just "slim" if on PATH.
#' @returns Void.
#'
#' @noRd
#'
check_slim_installation <- function(slim_command = "slim")
{
  SLIM_CALL_FAILED_CODE = 127
  slim_call_attempt <- try(system2(slim_command, "-version"))

  if (slim_call_attempt == SLIM_CALL_FAILED_CODE)
  {
    stop("SLiM call failed. Is SLiM installed and on your PATH?")
  }
}
