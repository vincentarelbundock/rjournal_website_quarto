# Convert all article .Rmd files to embedded HTML pages
#
# Uses mirai for parallel processing. Set RJOURNAL_MIRAI_DAEMONS env var
# to control worker count (defaults to cores - 1).

# ============================================================================
# Main
# ============================================================================
source("vincent/helpers.R", local = TRUE)

article_dirs <- list.dirs("articles", recursive = FALSE, full.names = TRUE)
article_dirs <- sort(article_dirs, decreasing = TRUE)

# Configure parallelism
n_workers <- setup_parallel()
on.exit(daemons(0), add = TRUE)

message("Processing ", length(article_dirs), " articles with ", n_workers, " workers...")

# Process all articles in parallel
map_results <- mirai_map(
  article_dirs,
  function(article_dir) {
    # Source helpers inside worker (each worker is a fresh R process)
    source("vincent/helpers.R", local = TRUE)

    slug <- basename(article_dir)
    rmd_file <- file.path(article_dir, paste0(slug, ".Rmd"))

    if (!file.exists(rmd_file)) {
      return(list(slug = slug, status = "skipped", error = NULL))
    }

    tryCatch(
      {
        # R News articles use PDF-only index
        if (grepl("^RN-", slug)) {
          html_file <- file.path(article_dir, paste0(slug, ".html"))
          index_file <- file.path(article_dir, "index.qmd")
          unlink(c(index_file, html_file)[file.exists(c(index_file, html_file))])
          if (create_pdf_index(article_dir, slug)) {
            return(list(slug = slug, status = "converted", error = NULL))
          }
          return(list(slug = slug, status = "skipped", error = NULL))
        }

        # R Journal articles: render HTML and create iframe index
        render_embed_html(article_dir, slug, rmd_file)

        # Build index.qmd
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
            slug, slug
          ),
          "</div>",
          ""
        )

        lines <- c(header, build_article_card(ref), iframe_block, IFRAME_RESIZE_SCRIPT)
        writeLines(lines, index_file)

        list(slug = slug, status = "converted", error = NULL)
      },
      error = function(e) {
        # Cleanup on error
        temp_rmd <- file.path(article_dir, paste0(slug, "-render.Rmd"))
        html_file <- file.path(article_dir, paste0(slug, ".html"))
        index_file <- file.path(article_dir, "index.qmd")
        cleanup <- c(temp_rmd, html_file, index_file)
        unlink(cleanup[file.exists(cleanup)], recursive = TRUE, force = TRUE)
        list(slug = slug, status = "error", error = e$message)
      })
  })[.progress]

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
log_file <- "article_html.log"
if (length(error_messages) > 0) {
  writeLines(error_messages, log_file)
} else if (file.exists(log_file)) {
  unlink(log_file)
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
