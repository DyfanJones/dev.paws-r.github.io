#' @importFrom roxygen2 update_collate roxygenize

#' @title Build Rd documentation for long documentation packages
#'
#' @param long_doc Character string path to the package directory
#' @return NULL (invisibly)
#' @export
build_long_rd <- function(long_doc = "vendor/paws/paws") {
  suppressMessages({
    update_collate(long_doc)
    roxygenize(long_doc, roclets = c("rd"))
  })
}

#' @title Find and replace operator references in HTML strings
#'
#' @param string Character string to search
#' @param operator Character string operator name
#' @return Modified string with links added
#' @export
find_and_replace <- function(string, operator) {
  m <- regexpr(">[a-z0-9_]+<", string)
  found <- gsub(">|<", "", regmatches(string, m))
  gsub(
    found,
    sprintf('<a href="../%s_%s/"> %s </a>', operator, found, found),
    string
  )
}

#' @title Convert HTML tables to definition lists
#'
#' @param lines Character vector of HTML lines
#' @return Modified character vector with tables converted to definition lists
#' @export
html_table_to_list <- function(lines) {
  # Convert argument tables to definition lists (pkgdown style)
  # Match <table> and </table> tags (with or without attributes)
  idx <- grep("<table[^>]*>|</table>", lines, perl = TRUE)[-1]

  # Only process if we have pairs of tags
  if (length(idx) == 0 || length(idx) %% 2 != 0) {
    return(lines)
  }

  idx_ranges <- split(idx, ceiling(seq_along(idx) / 2))
  idx_ranges <- lapply(idx_ranges, \(x) x[1]:x[2])

  for (idx_range in idx_ranges) {
    # Convert table tags to definition list
    lines[idx_range] <- gsub("<table[^>]*>", "<dl>", lines[idx_range], perl = TRUE)
    lines[idx_range] <- gsub("</table>", "</dl>", lines[idx_range])

    # Process table rows
    # Pattern: <tr><td>argument_name</td>
    #          <td>description...</td></tr>

    # First pass: Convert opening tags
    # <tr><td> becomes <dt> (for argument names)
    lines[idx_range] <- gsub("<tr><td>", "<dt>", lines[idx_range])

    # Second pass: Convert the first </td> on each line that has <dt>
    # This closes the argument name
    for (i in seq_along(idx_range)) {
      if (grepl("<dt>", lines[idx_range[i]], fixed = TRUE)) {
        # This line has <dt>, so the </td> on this line closes the <dt>
        lines[idx_range[i]] <- sub("</td>", "</dt>", lines[idx_range[i]])
      }
    }

    # Third pass: Convert standalone <td> to <dd> (description column)
    lines[idx_range] <- gsub("^<td>", "<dd>", lines[idx_range])
    lines[idx_range] <- gsub("^\\s*<td>", "<dd>", lines[idx_range])

    # Fourth pass: Convert any remaining </td> to </dd>
    lines[idx_range] <- gsub("</td>", "</dd>", lines[idx_range])

    # Remove </tr> tags
    lines[idx_range] <- gsub("</tr>", "", lines[idx_range])

    # Remove tbody, colgroup tags
    rm_tbody <- grep("<tbody>|</tbody>", lines[idx_range])
    rm_colgp <- grep("<colgroup>|</colgroup>", lines[idx_range])
    if (length(rm_colgp) > 0) {
      rm_colgp <- rm_colgp[1]:rm_colgp[2]
    }
    remove <- c(rm_tbody, rm_colgp)
    lines[idx_range][remove] <- "REMOVE LINE"
  }
  return(
    Filter(
      function(x) {
        x != "REMOVE LINE"
      },
      lines
    )
  )
}

#' @title Restore HTML definition lists in markdown for pkgdown-style rendering
#'
#' @param lines Character vector of markdown lines
#' @return Modified character vector with HTML dl/dt/dd tags for Arguments section
#' @export
restore_html_definition_lists <- function(lines) {
  # Find Arguments section
  args_start <- grep("^### Arguments", lines)
  if (length(args_start) == 0) return(lines)

  # Find next section (### something)
  next_section <- grep("^###", lines)
  next_section <- next_section[next_section > args_start[1]]
  if (length(next_section) == 0) {
    args_end <- length(lines)
  } else {
    args_end <- next_section[1] - 1
  }

  # Process the Arguments section
  args_lines <- lines[args_start[1]:args_end]

  # Start building the new section
  new_args <- character()
  new_args[1] <- args_lines[1]  # Keep "### Arguments"
  new_args[2] <- ""
  new_args[3] <- "<dl>"

  i <- 2  # Start after "### Arguments"
  idx <- 4

  while (i <= length(args_lines)) {
    line <- args_lines[i]

    # Match parameter names like `config`
    if (grepl("^`[^`]+`\\s*$", line)) {
      param_name <- gsub("`", "", line)
      param_name <- trimws(param_name)

      # Add <dt> for parameter
      new_args[idx] <- sprintf('<dt id="arg-%s">%s</dt>', param_name, param_name)
      idx <- idx + 1

      # Skip the blank line after param name
      i <- i + 1
      if (i <= length(args_lines) && args_lines[i] == "") {
        i <- i + 1
      }

      # Start <dd> for description
      new_args[idx] <- "<dd>"
      idx <- idx + 1

      # Collect description lines until next param or end
      while (i <= length(args_lines)) {
        line <- args_lines[i]

        # Stop if we hit another parameter or blank line followed by parameter
        if (grepl("^`[^`]+`\\s*$", line)) {
          break
        }

        # Add description line
        if (line != "" || (i < length(args_lines) && !grepl("^`[^`]+`\\s*$", args_lines[i+1]))) {
          new_args[idx] <- line
          idx <- idx + 1
        }

        i <- i + 1
      }

      # Close <dd>
      new_args[idx] <- "</dd>"
      idx <- idx + 1
      new_args[idx] <- ""
      idx <- idx + 1
    } else {
      i <- i + 1
    }
  }

  # Close <dl>
  new_args[idx] <- "</dl>"

  # Replace the Arguments section
  lines <- c(
    lines[1:(args_start[1] - 1)],
    new_args,
    if (args_end < length(lines)) lines[(args_end + 1):length(lines)] else character()
  )

  return(lines)
}
