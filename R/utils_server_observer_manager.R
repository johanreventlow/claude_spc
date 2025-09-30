# server_observer_manager.R
# Administrerer server-observers og sikrer oprydning

#' Opret observer manager
#'
#' @description
#' Giver et sæt hjælpefunktioner til at registrere, fjerne og rydde op i
#' Shiny observers, så vi undgår memory leaks og hængende observers mellem
#' sessioner/tests.
#'
#' @return Liste med funktionerne `add()`, `remove()`, `cleanup_all()` og `count()`
#' @export
observer_manager <- function() {
  observers <- list()

  list(
    add = function(observer, name = NULL) {
      id <- if (is.null(name)) length(observers) + 1 else name
      observers[[id]] <<- observer
      id
    },
    remove = function(id) {
      if (id %in% names(observers)) {
        if (!is.null(observers[[id]]$destroy)) {
          observers[[id]]$destroy()
        }
        observers[[id]] <<- NULL
      }
      invisible()
    },
    cleanup_all = function() {
      for (id in names(observers)) {
        if (!is.null(observers[[id]]$destroy)) {
          tryCatch(
            observers[[id]]$destroy(),
            error = function(e) {
              log_error(
                message = paste("Observer cleanup fejl for", id, ":", e$message),
                component = "[OBSERVER_MGMT]"
              )
            }
          )
        }
      }
      observers <<- list()
      invisible()
    },
    count = function() {
      length(observers)
    }
  )
}
