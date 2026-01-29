# Generate PDF-only index.qmd pages for news without index.qmd
#
# Uses mirai for parallel processing. Set RJOURNAL_MIRAI_DAEMONS env var
# to control worker count (defaults to cores - 1).

source("vincent/helpers.R")

# ============================================================================
# Scan existing news directories
# ============================================================================

scan_existing_news <- function() {
  news_dirs <- list.dirs("news", recursive = FALSE, full.names = TRUE)
  news_dirs <- news_dirs[grepl("^R[JN]-", basename(news_dirs))]

  if (length(news_dirs) == 0) {
    return(list(created = 0, skipped = 0))
  }

  n_workers <- setup_parallel()
  on.exit(daemons(0), add = TRUE)

  message("Scanning ", length(news_dirs), " news directories with ", n_workers, " workers...")

  map_results <- mirai_map(
    news_dirs,
    function(dir_path) {
      source("vincent/helpers.R", local = TRUE)
      slug <- basename(dir_path)
      if (create_pdf_index(dir_path, slug)) {
        list(status = "created")
      } else {
        list(status = "skipped")
      }
    }
  )[.progress]

  created <- sum(sapply(map_results, function(x) x$status == "created"))
  skipped <- sum(sapply(map_results, function(x) x$status == "skipped"))

  list(created = created, skipped = skipped)
}

# ============================================================================
# Scan issue yml files for news entries
# ============================================================================

scan_issue_news <- function() {
  issue_dirs <- list.dirs("issues", recursive = FALSE, full.names = TRUE)

  # Collect all news entries from yml files
  news_entries <- list()

  for (issue_dir in issue_dirs) {
    issue_id <- basename(issue_dir)
    yml_file <- file.path(issue_dir, paste0(issue_id, ".yml"))
    if (!file.exists(yml_file)) next

    yml_data <- read_yaml(yml_file)
    articles_list <- yml_data$articles
    if (is.null(articles_list) || length(articles_list) == 0) next

    heading_idx <- which(vapply(
      articles_list,
      function(x) !is.null(x$heading) && grepl("News", x$heading, ignore.case = TRUE),
      logical(1)
    ))

    if (length(heading_idx) == 0) next

    start_idx <- heading_idx[1] + 1
    for (i in start_idx:length(articles_list)) {
      entry <- articles_list[[i]]
      if (!is.null(entry$heading)) break
      if (is.null(entry$title)) next

      news_entries[[length(news_entries) + 1]] <- list(
        issue_dir = issue_dir,
        issue_id = issue_id,
        entry = entry
      )
    }
  }

  if (length(news_entries) == 0) {
    return(list(created = 0, skipped = 0))
  }

  n_workers <- setup_parallel()
  on.exit(daemons(0), add = TRUE)

  message("Processing ", length(news_entries), " issue news entries with ", n_workers, " workers...")

  map_results <- mirai_map(
    news_entries,
    function(item) {
      source("vincent/helpers.R", local = TRUE)

      issue_dir <- item$issue_dir
      issue_id <- item$issue_id
      entry <- item$entry

      year <- as.numeric(sub("-.*", "", issue_id))
      issue_num <- as.numeric(sub(".*-", "", issue_id))
      prefix <- if (year < 2009) "RN" else "RJ"

      slug <- entry$slug %||% ""
      if (nchar(slug) == 0) {
        return(list(status = "skipped"))
      }

      news_id <- sprintf("%s-%s-%s-%s", prefix, year, issue_num, slug)
      news_dir <- file.path("news", news_id)
      qmd_file <- file.path(news_dir, "index.qmd")

      if (file.exists(qmd_file)) {
        return(list(status = "skipped"))
      }

      pdf_name <- find_issue_pdf(issue_dir, issue_id)
      if (nchar(pdf_name) == 0) {
        return(list(status = "skipped"))
      }

      dir.create(news_dir, recursive = TRUE, showWarnings = FALSE)

      title <- clean_text(entry$title %||% slug)
      author_info <- normalize_authors(entry$author)
      pages <- pages_to_string(entry$pages)
      vol <- get_volume(year)

      journal_title <- if (prefix == "RN") "R News" else "R Journal"
      citation <- build_citation(
        author_info$display, title, journal_title,
        as.character(year), as.character(vol), as.character(issue_num), pages
      )

      canonical_url <- paste0("https://journal.r-project.org/news/", news_id, "/")
      bibtex <- build_bibtex(
        news_id, title, author_info$bibtex, journal_title,
        as.character(year), as.character(vol), as.character(issue_num),
        "", pages, canonical_url
      )

      embed_path <- file.path("..", "..", "issues", issue_id, pdf_name)

      issue_months <- c("03", "06", "09", "12")
      month_num <- issue_months[min(issue_num, 4)]
      date_str <- sprintf("%s-%s-01", year, month_num)

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
        yaml_lines, "",
        build_citation_block(citation, bibtex),
        sprintf(
          "<embed src=\"%s\" type=\"application/pdf\" height=\"955px\" width=\"100%%\">",
          embed_path
        )
      )

      writeLines(qmd_lines, qmd_file)
      list(status = "created")
    }
  )[.progress]

  created <- sum(sapply(map_results, function(x) x$status == "created"))
  skipped <- sum(sapply(map_results, function(x) x$status == "skipped"))

  list(created = created, skipped = skipped)
}

# ============================================================================
# Main
# ============================================================================

message("Scanning news for missing index.qmd files...")
existing <- scan_existing_news()
issue_news <- scan_issue_news()

message("Done!")
message("  Created: ", existing$created + issue_news$created)
message("  Skipped: ", existing$skipped + issue_news$skipped)
