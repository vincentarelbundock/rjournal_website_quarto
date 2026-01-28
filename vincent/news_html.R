# Convert all news .Rmd files to embedded HTML pages

library(knitr)
library(rmarkdown)
library(yaml)

source("vincent/helpers.R")

news_dirs <- list.dirs("news", recursive = FALSE, full.names = TRUE)
news_dirs <- news_dirs[grepl("^R[JN]-", basename(news_dirs))]
news_dirs <- sort(news_dirs, decreasing = TRUE)
errors <- character(0)
converted <- 0
skipped <- 0
pdf_only <- 0

for (news_dir in news_dirs) {
  slug <- basename(news_dir)
  rmd_file <- file.path(news_dir, paste0(slug, ".Rmd"))
  if (!file.exists(rmd_file)) {
    skipped <- skipped + 1
    next
  }

  html_file <- file.path(news_dir, paste0(slug, ".html"))
  index_file <- file.path(news_dir, "index.qmd")
  temp_rmd <- file.path(news_dir, paste0(slug, "-render.Rmd"))

  tryCatch(
    {
      message("Processing ", slug, "...")
      created <- character(0)
      if (!has_body_content(rmd_file)) {
        message("  Empty Rmd: ", slug, " (using PDF embed)")
        unlink(c(index_file, html_file)[file.exists(c(index_file, html_file))])
        if (create_pdf_index(news_dir, slug)) {
          pdf_only <- pdf_only + 1
        } else {
          skipped <- skipped + 1
        }
        next
      }
      render_embed_html(news_dir, slug, rmd_file)
      created <- c(created, html_file)

      # Extract metadata for listing support
      meta <- parse_front_matter(rmd_file)
      title <- clean_text(meta$title %||% "")
      author_info <- normalize_authors(meta$author)
      author <- author_info$display
      date <- clean_text(meta$date %||% "")

      write_iframe_index(news_dir, slug, title, author, date)
      created <- c(created, index_file)
      converted <- converted + 1
    },
    error = function(e) {
      message("  Error: ", slug, ": ", e$message)
      cleanup <- c(created, temp_rmd)
      if (length(cleanup) > 0) {
        unlink(cleanup[file.exists(cleanup)], recursive = TRUE, force = TRUE)
      }
      errors <<- c(errors, slug)
    })
}

message("Done!")
message("  Converted: ", converted)
message("  PDF only:  ", pdf_only)
message("  Skipped:   ", skipped)
message("  Errors:    ", length(errors))
if (length(errors) > 0) {
  message("  Failed:")
  for (slug in errors) {
    message("    ", slug)
  }
}
