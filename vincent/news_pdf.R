# Generate PDF-only index.qmd pages for news without index.qmd

source("vincent/helpers.R")

write_issue_news_page <- function(issue_dir, issue_id, entry) {
  year <- as.numeric(sub("-.*", "", issue_id))
  issue_num <- as.numeric(sub(".*-", "", issue_id))
  prefix <- if (year < 2009) "RN" else "RJ"

  slug <- entry$slug %||% ""
  if (nchar(slug) == 0) {
    return(FALSE)
  }

  news_id <- sprintf("%s-%s-%s-%s", prefix, year, issue_num, slug)
  news_dir <- file.path("news", news_id)
  qmd_file <- file.path(news_dir, "index.qmd")

  if (file.exists(qmd_file)) {
    return(FALSE)
  }

  pdf_name <- find_issue_pdf(issue_dir, issue_id)
  if (nchar(pdf_name) == 0) {
    message("  Skipping ", news_id, " (no issue PDF found)")
    return(FALSE)
  }

  dir.create(news_dir, recursive = TRUE, showWarnings = FALSE)

  title <- clean_text(entry$title %||% slug)
  author_info <- normalize_authors(entry$author)
  pages <- pages_to_string(entry$pages)
  vol <- get_volume(year)

  journal_title <- if (prefix == "RN") "R News" else "R Journal"
  citation <- build_citation(
    author_info$display,
    title,
    journal_title,
    as.character(year),
    as.character(vol),
    as.character(issue_num),
    pages
  )

  canonical_url <- paste0("https://journal.r-project.org/news/", news_id, "/")
  bibtex <- build_bibtex(
    news_id,
    title,
    author_info$bibtex,
    journal_title,
    as.character(year),
    as.character(vol),
    as.character(issue_num),
    "",
    pages,
    canonical_url
  )

  embed_path <- file.path("..", "..", "issues", issue_id, pdf_name)

  # Build date from year and issue number (March, June, Sept, Dec for issues 1-4)
  issue_months <- c("03", "06", "09", "12")
  month_num <- issue_months[min(issue_num, 4)]
  date_str <- sprintf("%s-%s-01", year, month_num)

  # Build YAML with metadata
  yaml_lines <- c(
    "---",
    sprintf("title: %s", yaml_quote(title))
  )
  if (nchar(author_info$display) > 0) {
    yaml_lines <- c(yaml_lines, sprintf("author: %s", yaml_quote(author_info$display)))
  }
  yaml_lines <- c(
    yaml_lines,
    sprintf("date: %s", yaml_quote(date_str)),
    "page-layout: full",
    "format:",
    "  html:",
    "    toc: false",
    "---"
  )

  qmd_lines <- c(
    yaml_lines,
    "",
    build_citation_block(citation, bibtex),
    sprintf(
      "<embed src=\"%s\" type=\"application/pdf\" height=\"955px\" width=\"100%%\">",
      embed_path
    )
  )

  writeLines(qmd_lines, qmd_file)
  message("  Created ", qmd_file)
  TRUE
}

scan_existing_news <- function() {
  news_dirs <- list.dirs("news", recursive = FALSE, full.names = TRUE)
  news_dirs <- news_dirs[grepl("^R[JN]-", basename(news_dirs))]
  created <- 0
  skipped <- 0

  for (dir_path in news_dirs) {
    slug <- basename(dir_path)
    if (create_pdf_index(dir_path, slug)) {
      created <- created + 1
    } else {
      skipped <- skipped + 1
    }
  }

  list(created = created, skipped = skipped)
}

scan_issue_news <- function() {
  issue_dirs <- list.dirs("issues", recursive = FALSE, full.names = TRUE)
  created <- 0
  skipped <- 0

  for (issue_dir in issue_dirs) {
    issue_id <- basename(issue_dir)
    yml_file <- file.path(issue_dir, paste0(issue_id, ".yml"))
    if (!file.exists(yml_file)) {
      next
    }

    yml_data <- read_yaml(yml_file)
    articles_list <- yml_data$articles
    if (is.null(articles_list) || length(articles_list) == 0) {
      next
    }

    heading_idx <- which(vapply(
      articles_list,
      function(x) {
        !is.null(x$heading) && grepl("News", x$heading, ignore.case = TRUE)
      },
      logical(1)
    ))

    if (length(heading_idx) == 0) {
      next
    }

    start_idx <- heading_idx[1] + 1
    for (i in start_idx:length(articles_list)) {
      entry <- articles_list[[i]]
      if (!is.null(entry$heading)) {
        break
      }
      if (is.null(entry$title)) {
        next
      }

      if (write_issue_news_page(issue_dir, issue_id, entry)) {
        created <- created + 1
      } else {
        skipped <- skipped + 1
      }
    }
  }

  list(created = created, skipped = skipped)
}

message("Scanning news for missing index.qmd files...")
existing <- scan_existing_news()
issue_news <- scan_issue_news()

message("Done!")
message("  Created: ", existing$created + issue_news$created)
message("  Skipped: ", existing$skipped + issue_news$skipped)
