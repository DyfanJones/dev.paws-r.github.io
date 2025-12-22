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
  # convert table to list
  # Match <table> and </table> tags (with or without attributes)
  idx <- grep("<table[^>]*>|</table>", lines, perl = TRUE)[-1]

  # Only process if we have pairs of tags
  if (length(idx) == 0 || length(idx) %% 2 != 0) {
    return(lines)
  }

  idx_ranges <- split(idx, ceiling(seq_along(idx) / 2))
  idx_ranges <- lapply(idx_ranges, \(x) x[1]:x[2])

  for (idx_range in idx_ranges) {
    lines[idx_range] <- gsub("<table[^>]*>", "<dl>", lines[idx_range], perl = TRUE)
    lines[idx_range] <- gsub("</table>", "</dl>", lines[idx_range])

    lines[idx_range] <- gsub("<td><code", "<dt><code", lines[idx_range])
    lines[idx_range] <- gsub(
      '<td style="text-align: left;"><a',
      "<dt><a",
      lines[idx_range]
    )

    lines[idx_range] <- gsub("</code></td>", "</code></dt>", lines[idx_range])
    lines[idx_range] <- gsub("</a></td>", "</a></dt>", lines[idx_range])

    lines[idx_range] <- gsub("<td><p>", "<dd><p>", lines[idx_range])
    lines[idx_range] <- gsub(
      '<td style="text-align: left;">',
      "<dd>",
      lines[idx_range]
    )

    lines[idx_range] <- gsub("</p></td>", "</p></dd>", lines[idx_range])
    lines[idx_range] <- gsub("</td>", "</dd>", lines[idx_range])

    rm_tbody <- grep("<tbody>|</tbody>", lines[idx_range])
    rm_tr <- grep("<tr|</tr>", lines[idx_range])
    rm_colgp <- grep("<colgroup>|</colgroup>", lines[idx_range])
    if (length(rm_colgp) > 0) {
      rm_colgp <- rm_colgp[1]:rm_colgp[2]
    }
    remove <- c(rm_tbody, rm_tr, rm_colgp)
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
