
#' EidosComposition
#'
#' @description Internal superclass for Eidos block and model objects.
#'
#' @noRd
#'
EidosComposition <- R6::R6Class(

  classname = "EidosComposition",

  public = list(

    initialize = function(items)
    {
      for (item in items)
      {
        self$add(item)
      }
    },

    add = function(item, after_index = length(private$items))
    {
      private$items <- append(private$items, item, after_index)
    },

    remove = function(item_index)
    {
      private$items <- private$items[-item_index]
    },

    substitute = function(phrase, substitute, in_items)
    {
      for (item in private$items[in_items])
      {
        item$substitute(phrase, substitute)
      }
    },

    get_lines_as_character = function()
    {
      as_character <- self$lines |>
        vapply(
          function(line) return(line$string),
          character(1)
        )
      return(as_character)
    }

  ),

  private = list(

    items = list()

  )

)
