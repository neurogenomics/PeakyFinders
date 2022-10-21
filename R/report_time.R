#' Report time
#' 
#' Report time at end of function.
#' @inheritParams messager
#' @inheritParams base::round
#' @inheritParams base::difftime
#' @keywords internal
#' @importFrom utils capture.output
#' @importFrom stringr str_to_sentence
report_time <- function(start, 
                        end = Sys.time(),
                        prefix = NULL,
                        verbose = TRUE,
                        units = "min",
                        digits = 2) { 
    messager(
        trimws(
            paste(
                stringr::str_to_sentence(paste(prefix, 
                                               "Done in")),
                round(difftime(time1 = end, 
                               time2 = start,
                               units = units),
                      digits = digits),
                units
            )
        ),
        v = verbose
    )
}
