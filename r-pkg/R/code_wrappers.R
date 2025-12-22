#' Wrap R code sections in markdown code fences
#'
#' @param lines Character vector of markdown lines
#' @return Modified character vector with code sections wrapped
#' @export
wrap_r_code <- function(lines) {
  lines <- wrap_r_usage(lines)
  lines <- wrap_r_service_syntax(lines)
  lines <- wrap_r_value(lines)
  lines <- wrap_r_request_syntax(lines)
  lines <- wrap_r_examples(lines)
  return(lines)
}

#' Wrap R usage section in code fences
#'
#' @param lines Character vector of markdown lines
#' @return Modified character vector
#' @export
wrap_r_usage <- function(lines) {
  start <- (grep("### Usage", lines, perl = TRUE) + 1)
  end <- (grep("### Arguments", lines, perl = TRUE) - 1)
  if (length(end) == 0) {
    end <- (grep("### Value", lines, perl = TRUE) - 1)
  }
  if (length(start) > 0) {
    idx_range <- start:end
    lines[idx_range] <- gsub("^[ ]{4}", "", lines[idx_range], perl = TRUE)
    lines[start] <- "```r"
    lines[end] <- "```"
  }
  return(lines)
}

#' Wrap R value section in code fences
#'
#' @param lines Character vector of markdown lines
#' @return Modified character vector
#' @export
wrap_r_value <- function(lines) {
  start <- grep("### Value", lines, perl = TRUE)
  end <- (grep("### Request syntax", lines, perl = TRUE) - 1)
  if (length(end) == 0) {
    end <- (grep("### Service syntax", lines, perl = TRUE) - 1)
  }
  if (length(end) == 0) {
    end <- (grep("### Examples", lines, perl = TRUE) - 1)
  }
  if (length(start) > 0) {
    if (length(end) == 0) {
      end <- length(lines) + 1
    }
    idx_range <- start:end
    # format return value
    code_start <- grep("^[ ]{4}list\\(", lines[idx_range], perl = TRUE)
    if (length(code_start) > 0) {
      lines[idx_range][code_start - 1] <- "```r"
      lines[end] <- "```"
    }
    lines[idx_range] <- gsub("^[ ]{4}", "", lines[idx_range], perl = TRUE)
  }
  return(lines)
}

#' Wrap R request syntax section in code fences
#'
#' @param lines Character vector of markdown lines
#' @return Modified character vector
#' @export
wrap_r_request_syntax <- function(lines) {
  # format function syntax
  start <- (grep("### Request syntax", lines, perl = TRUE) + 1)
  end <- (grep("### Examples", lines, perl = TRUE) - 1)
  if (length(end) == 0) {
    end <- length(lines) + 1
  }
  if (length(start) > 0) {
    idx_range <- start:end
    lines[idx_range] <- gsub("^[ ]{4}", "", lines[idx_range], perl = TRUE)
    lines[start] <- "```r"
    lines[end] <- "```"
  }
  return(lines)
}

#' Wrap R examples section in code fences
#'
#' @param lines Character vector of markdown lines
#' @return Modified character vector
#' @export
wrap_r_examples <- function(lines) {
  start <- (grep("### Examples", lines, perl = TRUE) + 1)
  end <- (grep("## End\\(Not run\\)", lines, perl = TRUE) + 1)
  if (length(end) == 0) {
    end <- length(lines) + 1
  }
  if (length(start) > 0) {
    idx_range <- start:end
    lines[idx_range] <- gsub("^[ ]{4}", "", lines[idx_range], perl = TRUE)
    lines[start] <- "```r"
    lines[end] <- "```"
  }
  return(lines)
}

#' Wrap R service syntax section in code fences
#'
#' @param lines Character vector of markdown lines
#' @return Modified character vector
#' @export
wrap_r_service_syntax <- function(lines) {
  # format function syntax
  start <- (grep("### Service syntax", lines, perl = TRUE) + 1)
  end <- (grep("### Operations", lines, perl = TRUE) - 1)
  if (length(start) > 0) {
    idx_range <- start:end
    lines[idx_range] <- gsub("^[ ]{4}", "", lines[idx_range], perl = TRUE)
    lines[start] <- "```r"
    lines[end] <- "```"
  }
  return(lines)
}
