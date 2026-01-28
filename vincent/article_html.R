# Convert all article .Rmd files to embedded HTML pages

library(knitr)
library(rmarkdown)

source("vincent/helpers.R")

write_article_index <- function(article_dir, slug) {
  index_file <- file.path(article_dir, "index.qmd")
  ref <- build_article_reference(article_dir, slug)
  lines <- character(0)

  lines <- c(
    lines,
    "---",
    "page-layout: full",
    "---",
    "",
    "<div class=\"paper-card full-bleed\">"
  )

  if (nchar(ref$pdf_name) > 0) {
    lines <- c(
      lines,
      "  <p class=\"paper-links\">",
      "    <i class=\"fa-regular fa-file-pdf\"></i>",
      sprintf("    <a href=\"%s\">Download PDF</a>", ref$pdf_name),
      "  </p>"
    )
  }

  if (nchar(ref$issue_link) > 0 && nchar(ref$issue_label) > 0) {
    lines <- c(
      lines,
      "  <p class=\"paper-links\">",
      "    <i class=\"fa-regular fa-bookmark\"></i>",
      sprintf("    <a href=\"%s\">%s</a>", ref$issue_link, ref$issue_label),
      "  </p>"
    )
  }

  lines <- c(lines, build_citation_block(ref$citation, ref$bibtex))

  lines <- c(
    lines,
    "</div>",
    "",
    "<div class=\"paper-reader full-bleed\">",
    sprintf(
      "  <iframe class=\"paper-frame\" src=\"%s.html\" title=\"%s\" loading=\"lazy\"></iframe>",
      slug,
      slug
    ),
    "</div>",
    "",
    "```{=html}",
    "<script>",
    "  (function() {",
    "    var iframe = document.querySelector('.paper-frame');",
    "    if (!iframe) return;",
    "    var resize = function() {",
    "      var doc = iframe.contentDocument || iframe.contentWindow.document;",
    "      if (!doc) return;",
    "      var body = doc.body;",
    "      var html = doc.documentElement;",
    "      var height = Math.max(",
    "        body ? body.scrollHeight : 0,",
    "        body ? body.offsetHeight : 0,",
    "        html ? html.scrollHeight : 0,",
    "        html ? html.offsetHeight : 0",
    "      );",
    "      if (height > 0) {",
    "        iframe.style.height = height + 'px';",
    "      }",
    "    };",
    "    iframe.addEventListener('load', function() {",
    "      resize();",
    "      var doc = iframe.contentDocument || iframe.contentWindow.document;",
    "      if (!doc) return;",
    "      if ('ResizeObserver' in window) {",
    "        var ro = new ResizeObserver(resize);",
    "        ro.observe(doc.documentElement);",
    "        if (doc.body) ro.observe(doc.body);",
    "      } else {",
    "        setInterval(resize, 500);",
    "      }",
    "    });",
    "    window.addEventListener('resize', resize);",
    "  })();",
    "</script>",
    "```"
  )

  writeLines(lines, index_file)
  TRUE
}

article_dirs <- list.dirs("articles", recursive = FALSE, full.names = TRUE)
# article_dirs <- article_dirs[grepl("^RJ-2024-", basename(article_dirs))]
article_dirs <- sort(article_dirs, decreasing = TRUE)
errors <- character(0)
converted <- 0
skipped <- 0

for (article_dir in article_dirs) {
  slug <- basename(article_dir)
  rmd_file <- file.path(article_dir, paste0(slug, ".Rmd"))
  if (!file.exists(rmd_file)) {
    skipped <- skipped + 1
    next
  }

  html_file <- file.path(article_dir, paste0(slug, ".html"))
  index_file <- file.path(article_dir, "index.qmd")
  temp_rmd <- file.path(article_dir, paste0(slug, "-render.Rmd"))

  tryCatch(
    {
      message("Processing ", slug, "...")
      created <- character(0)
      if (grepl("^RN-", slug)) {
        unlink(c(index_file, html_file)[file.exists(c(index_file, html_file))])
        if (create_pdf_index(article_dir, slug)) {
          converted <- converted + 1
          next
        }
      }
      render_embed_html(article_dir, slug, rmd_file)
      created <- c(created, html_file)
      write_article_index(article_dir, slug)
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
message("  Skipped:   ", skipped)
message("  Errors:    ", length(errors))
if (length(errors) > 0) {
  message("  Failed:")
  for (slug in errors) {
    message("    ", slug)
  }
}
