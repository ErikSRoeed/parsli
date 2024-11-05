
#' EidosModel
#'
#' @description Internal class for Eidos model objects.
#'
#' @noRd
#'
EidosModel <- R6::R6Class(

  classname = "EidosModel",
  inherit = EidosComposition,

  public = list(

    initialize = function(name, eidos_blocks)
    {
      super$initialize(eidos_blocks)
      self$set_name(name)
    },

    set_name = function(name)
    {
      private$name_private <- name
    },

    print = function()
    {
      TAB = "\t"
      HEAD = paste0("#", TAB)
      NEWLINE = "\n"

      paste0(HEAD, self$name, NEWLINE) |> cat()
      paste0(HEAD, NEWLINE) |> cat()
      paste0(HEAD, "Block count: ", self$block_count, NEWLINE) |> cat()
      paste0(HEAD, "Line count: ", self$line_count, NEWLINE) |> cat()
      paste0(HEAD, NEWLINE) |> cat()
      paste0(HEAD, "Blocks:", NEWLINE) |> cat()
      paste0(HEAD, NEWLINE) |> cat()

      block_number <- 1
      for (block in self$blocks)
      {
        block_header <- paste0("[", block_number, "] ")
        paste0(HEAD, block_header, block$callback, NEWLINE) |> cat()
        block_number <- block_number + 1
      }

      paste0(HEAD, NEWLINE) |> cat()
      paste0(HEAD, "slimmr ", utils::packageVersion("slimmr"), NEWLINE) |> cat()
    },

    write_to_file = function(file_path, overwrite = FALSE)
    {
      if (file.exists(file_path))
      {
        stopifnot(overwrite)
      }
      else
      {
        file.create(file_path)
      }

      file_connection <- file(file_path, open = "wt")

      for (line in self$lines)
      {
        writeLines(text = line$string, con = file_connection)
      }

      close(file_connection)
    }

  ),

  active = list(

    name = function()
    {
      return(private$name_private)
    },

    blocks = function()
    {
      return(private$items)
    },

    lines = function()
    {
      lines <- lapply(
        self$blocks,
        function(block) return(block$lines)
      ) |> unlist()
      return(lines)
    },

    block_count = function()
    {
      return(length(self$blocks))
    },

    line_count = function()
    {
      return(length(self$lines))
    }

  ),

  private = list(

    name_private = ""

  )

)
