
#' EidosBlock
#'
#' @description Internal class for Eidos block objects.
#'
#' @noRd
#'
EidosBlock <- R6::R6Class(

  classname = "EidosBlock",
  inherit = EidosComposition,

  public = list(

    change_callback = function(new_callback)
    {
      super$substitute(self$callback, new_callback)
    }

  ),

  active = list(

    callback = function()
    {
      callback_exists <- length(self$callback_line_number) != 0

      if (callback_exists)
      {
        callback <- self$lines[[self$callback_line_number]]$callback
        return(callback)
      }

      return(NULL)
    },

    callback_line_number = function()
    {
      callback_line_number <- self$lines |>
        sapply(function(line) ! is.null(line$callback)) |>
        which.max()
      return(callback_line_number)
    },

    lines = function()
    {
      return(private$items)
    }

  )

)
