#' Report time
#' 
#' Report time at end of function.
#' @inheritParams messager
#' @inheritParams base::round
#' @inheritParams base::difftime
#' @keywords internal
#' @importFrom utils capture.output
report_time <- function(start, 
                        prefix=NULL,
                        verbose = TRUE,
                        units = "min",
                        digits = 1) { 
    messager(
        prefix, 
        "Done in",
            round(difftime(time1 = Sys.time(), 
                           time2 = start,
                           units = units),
                  digits = digits),
        units,
        v = verbose
    )
}
