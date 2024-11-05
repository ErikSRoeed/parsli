#' Run a SLiM model.
#'
#' @description This function runs a SLiM model.
#'
#' @param model An EidosModel object.
#' @param reps Number of replicate simulations.
#' @param seed Random seed for simulations
#' @param slim_output_verbosity An integer 0 - 2.
#' @param output_parsing_function A function applied to raw output from SLiM
#' @param parallel_nodes Number of load balancing parallel nodes to use.
#' @param slim_command Full path to SLiM executable. Default "slim" if on PATH.
#' @param output_runtime_diagnostics TRUE/FALSE. Output CPU and RAM usage?
#' @param ... Named constant arguments to SLiM (i.e. passed to -d / -define)
#' @return Either the raw output from SLiM, or that output as parsed by
#' output_parse_function.
#'
#' @export
#'
run_slim <- function(
    model,
    reps = 1,
    seed = NULL,
    slim_output_verbosity = 1,
    output_parsing_function = function(x) return(x),
    parallel_nodes = 1,
    slim_command = "slim",
    output_runtime_diagnostics = FALSE,
    ...
)
{
  check_slim_installation(slim_command)
  temporary_script_path <- tempfile(fileext = ".slim")
  model$write_to_file(file_path = temporary_script_path)

  constants <- list(...)
  slim_arguments <- paste(
    paste(
      ifelse(output_runtime_diagnostics, "-t", ""),
      ifelse(output_runtime_diagnostics, "-m", ""),
      paste("-l", slim_output_verbosity),
      ifelse(is.null(seed), "", paste("-s", seed)),
      sep = " "
    ),
    paste(
      "-d ", names(constants), "=", constants,
      collapse = " ", sep = ""
    ),
    paste(
      '"', temporary_script_path, '"',
      sep = ""
    ),
    sep = " "
  )

  output_data_structure <- list(
    script = readLines(temporary_script_path),
    seed = seed,
    constants = constants,
    system_call = paste(slim_command, slim_arguments),
    replicate_runs = reps
  )

  export_to_nodes <- list(
    slim_command = slim_command,
    slim_arguments = slim_arguments,
    output_parsing_function = output_parsing_function
  )

  cluster <- parallel::makeCluster(parallel_nodes)
  cluster |> parallel::clusterExport(
    varlist = names(export_to_nodes),
    envir = list2env(export_to_nodes)
  )

  slim_outputs <- parallel::clusterApplyLB(
    cl = cluster,
    x = seq(reps),
    fun = function(rep)
    {
      slim_output <- system2(
        slim_command,
        slim_arguments,
        stdout = TRUE,
        stderr = TRUE
      ) |> output_parsing_function()

      return(slim_output)
    }
  )

  parallel::stopCluster(cluster)

  output_data_structure$slim_outputs <- slim_outputs
  return(output_data_structure)
}
