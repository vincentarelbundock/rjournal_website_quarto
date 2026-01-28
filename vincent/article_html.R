# Convert all article .Rmd files to embedded HTML pages

source("vincent/helpers.R")

write_article_index <- function(article_dir, slug) {
  index_file <- file.path(article_dir, "index.qmd")
  ref <- build_article_reference(article_dir, slug)

  header <- c(
    "---",
    "page-layout: full",
    "---",
    ""
  )

  iframe_block <- c(
    "",
    "<div class=\"paper-reader full-bleed\">",
    sprintf(
      "  <iframe class=\"paper-frame\" src=\"%s.html\" title=\"%s\" loading=\"lazy\"></iframe>",
      slug,
      slug
    ),
    "</div>",
    ""
  )

  lines <- c(header, build_article_card(ref), iframe_block, IFRAME_RESIZE_SCRIPT)
  writeLines(lines, index_file)
  TRUE
}

process_article <- function(article_dir) {
  slug <- basename(article_dir)
  rmd_file <- file.path(article_dir, paste0(slug, ".Rmd"))

  if (!file.exists(rmd_file)) {
    return("skipped")
  }

  message("Processing ", slug, "...")

  # R News articles use PDF-only index

  if (grepl("^RN-", slug)) {
    html_file <- file.path(article_dir, paste0(slug, ".html"))
    index_file <- file.path(article_dir, "index.qmd")
    unlink(c(index_file, html_file)[file.exists(c(index_file, html_file))])
    if (create_pdf_index(article_dir, slug)) {
      return("converted")
    }
    return("skipped")
  }

  render_embed_html(article_dir, slug, rmd_file)
  write_article_index(article_dir, slug)
  "converted"
}

# Main
article_dirs <- list.dirs("articles", recursive = FALSE, full.names = TRUE)
article_dirs <- sort(article_dirs, decreasing = TRUE)

results <- list()
errors <- character(0)

for (article_dir in article_dirs) {
  slug <- basename(article_dir)
  status <- tryCatch(
    process_article(article_dir),
    error = function(e) {
      message("  Error: ", slug, ": ", e$message)
      # Cleanup on error
      temp_rmd <- file.path(article_dir, paste0(slug, "-render.Rmd"))
      html_file <- file.path(article_dir, paste0(slug, ".html"))
      index_file <- file.path(article_dir, "index.qmd")
      cleanup <- c(temp_rmd, html_file, index_file)
      unlink(cleanup[file.exists(cleanup)], recursive = TRUE, force = TRUE)
      errors <<- c(errors, slug)
      "error"
    }
  )
  results[[slug]] <- status
}

# Report
tab <- table(unlist(results))
message("Done!")
message("  Converted: ", tab["converted"] %||% 0)
message("  Skipped:   ", tab["skipped"] %||% 0)
message("  Errors:    ", length(errors))

if (length(errors) > 0) {
  message("  Failed:")
  for (slug in errors) {
    message("    ", slug)
  }
}
