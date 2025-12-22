#' Simple logger function
#'
#' @param msg Character string containing the log message
#' @return NULL (invisibly)
#' @export
log_info <- function(msg, ...) {
  on.exit(flush.console())
  date_time <- strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S")
  log_msg <- sprintf("INFO %s: %s", date_time, sprintf(msg, ...))
  writeLines(log_msg)
}
