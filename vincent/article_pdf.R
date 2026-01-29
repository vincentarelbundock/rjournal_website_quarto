# Generate PDF-only index.qmd pages for articles without index.qmd
#
# Uses mirai for parallel processing. Set RJOURNAL_MIRAI_DAEMONS env var
# to control worker count (defaults to cores - 1).

source("vincent/helpers.R")

article_dirs <- list.dirs("articles", recursive = FALSE, full.names = TRUE)

# Configure parallelism
n_workers <- setup_parallel()
on.exit(daemons(0), add = TRUE)

message("Scanning ", length(article_dirs), " articles with ", n_workers, " workers...")

# Process all articles in parallel
map_results <- mirai_map(
  article_dirs,
  function(dir_path) {
    source("vincent/helpers.R", local = TRUE)

    slug <- basename(dir_path)
    qmd_file <- file.path(dir_path, "index.qmd")

    if (file.exists(qmd_file)) {
      return(list(slug = slug, status = "skipped", error = NULL))
    }

    tryCatch({
      ref <- build_article_reference(dir_path, slug)
      if (nchar(ref$pdf_name) == 0) {
        return(list(slug = slug, status = "no_pdf", error = NULL))
      }

      header <- c(
        "---",
        "page-layout: full",
        "format:",
        "  html:",
        "    toc: false",
        "---",
        ""
      )

      pdf_embed <- c(
        "",
        "<div class=\"paper-reader full-bleed\">",
        sprintf(
          "  <embed src=\"%s\" type=\"application/pdf\" height=\"955px\" width=\"100%%\">",
          ref$pdf_name
        ),
        "</div>"
      )

      qmd_lines <- c(header, build_article_card(ref), pdf_embed)
      writeLines(qmd_lines, qmd_file)

      list(slug = slug, status = "created", error = NULL)
    },
    error = function(e) {
      list(slug = slug, status = "error", error = e$message)
    })
  }
)[.progress]

# Collect results
results <- list()
errors <- character(0)
error_messages <- character(0)

for (item in map_results) {
  results[[item$slug]] <- item$status
  if (item$status == "error") {
    errors <- c(errors, item$slug)
    error_messages <- c(error_messages, sprintf("%s: %s", item$slug, item$error))
  }
}

# Write error log
log_file <- "article_pdf.log"
if (length(error_messages) > 0) {
  writeLines(error_messages, log_file)
} else if (file.exists(log_file)) {
  unlink(log_file)
}

# Report
tab <- table(unlist(results))
message("Done!")
message("  Created: ", tab["created"] %||% 0)
message("  Skipped: ", tab["skipped"] %||% 0)
message("  No PDF:  ", tab["no_pdf"] %||% 0)
message("  Errors:  ", length(errors))

if (length(errors) > 0) {
  message("  Failed:")
  for (slug in errors) {
    message("    ", slug)
  }
}
