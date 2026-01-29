# Convert all news .Rmd files to embedded HTML pages
#
# Uses mirai for parallel processing. Set RJOURNAL_MIRAI_DAEMONS env var
# to control worker count (defaults to cores - 1).

source("vincent/helpers.R")

news_dirs <- list.dirs("news", recursive = FALSE, full.names = TRUE)
news_dirs <- news_dirs[grepl("^R[JN]-", basename(news_dirs))]
news_dirs <- sort(news_dirs, decreasing = TRUE)

# Configure parallelism
n_workers <- setup_parallel()
on.exit(daemons(0), add = TRUE)

message("Processing ", length(news_dirs), " news items with ", n_workers, " workers...")

# Process all news in parallel
map_results <- mirai_map(
  news_dirs,
  function(news_dir) {
    source("vincent/helpers.R", local = TRUE)

    slug <- basename(news_dir)
    rmd_file <- file.path(news_dir, paste0(slug, ".Rmd"))

    if (!file.exists(rmd_file)) {
      return(list(slug = slug, status = "skipped", error = NULL))
    }

    tryCatch({
      if (!has_body_content(rmd_file)) {
        # Empty Rmd: use PDF embed
        html_file <- file.path(news_dir, paste0(slug, ".html"))
        index_file <- file.path(news_dir, "index.qmd")
        unlink(c(index_file, html_file)[file.exists(c(index_file, html_file))])
        if (create_pdf_index(news_dir, slug)) {
          return(list(slug = slug, status = "pdf_only", error = NULL))
        }
        return(list(slug = slug, status = "skipped", error = NULL))
      }

      render_embed_html(news_dir, slug, rmd_file)

      # Extract metadata for listing support
      meta <- parse_front_matter(rmd_file)
      title <- clean_text(meta$title %||% "")
      author_info <- normalize_authors(meta$author)
      author <- author_info$display
      date <- clean_text(meta$date %||% "")

      write_iframe_index(news_dir, slug, title, author, date)
      list(slug = slug, status = "converted", error = NULL)
    },
    error = function(e) {
      # Cleanup on error
      temp_rmd <- file.path(news_dir, paste0(slug, "-render.Rmd"))
      html_file <- file.path(news_dir, paste0(slug, ".html"))
      index_file <- file.path(news_dir, "index.qmd")
      cleanup <- c(temp_rmd, html_file, index_file)
      unlink(cleanup[file.exists(cleanup)], recursive = TRUE, force = TRUE)
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
log_file <- "news_html.log"
if (length(error_messages) > 0) {
  writeLines(error_messages, log_file)
} else if (file.exists(log_file)) {
  unlink(log_file)
}

# Report
tab <- table(unlist(results))
message("Done!")
message("  Converted: ", tab["converted"] %||% 0)
message("  PDF only:  ", tab["pdf_only"] %||% 0)
message("  Skipped:   ", tab["skipped"] %||% 0)
message("  Errors:    ", length(errors))

if (length(errors) > 0) {
  message("  Failed:")
  for (slug in errors) {
    message("    ", slug)
  }
}
