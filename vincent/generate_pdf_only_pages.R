# Generate PDF-only index.qmd pages for articles without index.qmd

source("vincent/convert_helpers.R")

build_pdf_page <- function(dir_path, slug) {
  qmd_file <- file.path(dir_path, "index.qmd")
  if (file.exists(qmd_file)) {
    return(FALSE)
  }

  ref <- build_article_reference(dir_path, slug)
  if (nchar(ref$pdf_name) == 0) {
    message("  Skipping ", slug, " (no PDF found)")
    return(FALSE)
  }

  qmd_lines <- c(
    "---",
    "page-layout: full",
    "format:",
    "  html:",
    "    toc: false",
    "---",
    "",
    build_citation_block(ref$citation, ref$bibtex),
    "<div class=\"paper-reader\">",
    sprintf(
      "  <embed src=\"%s\" type=\"application/pdf\">",
      ref$pdf_name
    ),
    "</div>"
  )

  writeLines(qmd_lines, qmd_file)
  message("  Created ", qmd_file)
  TRUE
}

message("Scanning articles for missing index.qmd files...")
article_dirs <- list.dirs("articles", recursive = FALSE, full.names = TRUE)
created <- 0
skipped <- 0

for (dir_path in article_dirs) {
  slug <- basename(dir_path)
  if (build_pdf_page(dir_path, slug)) {
    created <- created + 1
  } else {
    skipped <- skipped + 1
  }
}

message("Done!")
message("  Created: ", created)
message("  Skipped: ", skipped)
