# Generate Quarto pages for all issues in issues/
#
# Requires: Run 'make yml' first to generate .yml files for all issues

source("vincent/helpers.R")

# Pattern for identifying news items by title
NEWS_PATTERN <- "News|CRAN|Changes|Foundation|Bioconductor"

# LaTeX character replacements
LATEX_REPLACEMENTS <- c(
  "\\{|\\}" = "",
  "\\\\k\\{e\\}" = "e",
  "\\\\L\\{\\}" = "L",
  "\\\\'a" = "a",
  "\\\\\"a" = "a",
  "\\\\\"o" = "o",
  "\\\\\"u" = "u"
)

clean_latex <- function(text) {
  if (is.null(text) || length(text) == 0) {
    return("")
  }
  for (pattern in names(LATEX_REPLACEMENTS)) {
    text <- gsub(pattern, LATEX_REPLACEMENTS[[pattern]], text)
  }
  text
}

# Resolve link path: prefer index.qmd, fallback to PDF
resolve_link_path <- function(base_dir, slug) {
  qmd_file <- file.path(base_dir, slug, "index.qmd")
  if (file.exists(qmd_file)) {
    sprintf("../../%s/%s/", base_dir, slug)
  } else {
    pdf_file <- file.path(base_dir, slug, paste0(slug, ".pdf"))
    if (file.exists(pdf_file)) {
      sprintf("../../%s/%s/%s.pdf", base_dir, slug, slug)
    } else {
      NULL
    }
  }
}

# Format article entry for output
format_article <- function(art, issue_year, issue_num, prefix) {
  title <- clean_latex(art$title %||% art$slug)
  if (!nzchar(title)) title <- art$slug

  # Normalize author
  author <- if (is.list(art$author) && length(art$author) > 0) {
    paste(unlist(art$author), collapse = ", ")
  } else {
    paste(art$author, collapse = ", ")
  }
  author <- clean_latex(author)

  # Format pages
  pages_str <- ""
  if (!is.null(art$pages)) {
    p <- unlist(art$pages)
    if (length(p) == 2 && p[1] != p[2]) {
      pages_str <- sprintf(" (pp. %s-%s)", p[1], p[2])
    } else if (length(p) >= 1) {
      pages_str <- sprintf(" (p. %s)", p[1])
    }
  }

  # Resolve link path
  slug <- art$slug
  link_path <- NULL

  if (!is.null(slug) && nzchar(slug)) {
    is_full_id <- grepl("^R[JN]-", slug)
    is_news_item <- grepl("^RJ-\\d{4}-\\d+-", slug)

    if (is_full_id) {
      base_dir <- if (is_news_item) "news" else "articles"
      link_path <- resolve_link_path(base_dir, slug)
    } else {
      # Construct news ID from components
      news_id <- sprintf("%s-%s-%s-%s", prefix, issue_year, issue_num, slug)
      link_path <- resolve_link_path("news", news_id)
    }
  }

  # Build output
  title_line <- if (!is.null(link_path)) {
    sprintf("[%s](%s)%s", title, link_path, pages_str)
  } else {
    sprintf("**%s**%s", title, pages_str)
  }

  if (nzchar(author)) {
    sprintf("%s<br>*%s*", title_line, author)
  } else {
    title_line
  }
}

generate_issue_qmd <- function(issue_dir) {
  # --- Metadata & issue identification ---
  issue_id <- basename(issue_dir)
  year <- as.numeric(sub("-.*", "", issue_id))
  iss <- as.numeric(sub(".*-", "", issue_id))
  vol <- get_volume(year)
  prefix <- if (year < 2009) "RN" else "RJ"

  # --- Load issue data from yml ---
  yml_file <- file.path(issue_dir, paste0(issue_id, ".yml"))
  if (!file.exists(yml_file)) {
    message("Skipping ", issue_id, " (no .yml file - run 'make yml' first)")
    return(NULL)
  }
  yml_data <- read_yaml(yml_file)

  pdf_file <- find_issue_pdf(issue_dir, issue_id)
  if (!nzchar(pdf_file)) pdf_file <- NULL

  # --- Build date ---
  issue_months <- c("03", "06", "09", "12")
  month_num <- issue_months[min(iss, 4)]
  date_str <- sprintf("%s-%s-01", year, month_num)

  month_name <- ISSUE_MONTHS[min(iss, 4)]
  if (!is.null(yml_data$month)) {
    month_name <- yml_data$month
  }

  # --- Header ---
  qmd_lines <- c(
    "---",
    sprintf('title: "Volume %s, Issue %s"', vol, iss),
    sprintf('date: "%s"', date_str),
    "page-layout: full",
    "title-block-banner: true",
    "toc: true",
    "---",
    ""
  )

  # --- Issue card ---
  card_lines <- "<div class=\"paper-card full-bleed\">"
  if (!is.null(pdf_file)) {
    card_lines <- c(
      card_lines,
      "  <p class=\"paper-links\">",
      "    <i class=\"fa-regular fa-file-pdf\"></i>",
      sprintf('    <a href="%s">Download Complete Issue (PDF)</a>', pdf_file),
      "  </p>"
    )
  }
  card_lines <- c(
    card_lines,
    sprintf('  <div class="paper-citation">Published %s %s</div>', month_name, year),
    "</div>",
    ""
  )
  qmd_lines <- c(qmd_lines, card_lines)

  # --- Table of contents ---
  article_count <- 0
  news_count <- 0

  if (!is.null(yml_data$articles) && length(yml_data$articles) > 0) {
    qmd_lines <- c(qmd_lines, "## Table of Contents", "")

    for (art in yml_data$articles) {
      if (is.null(art)) next

      if (!is.null(art$heading)) {
        qmd_lines <- c(qmd_lines, sprintf("### %s", art$heading), "")
      } else if (!is.null(art$title) || !is.null(art$slug)) {
        qmd_lines <- c(qmd_lines, format_article(art, year, iss, prefix), "")

        is_news <- grepl(NEWS_PATTERN, art$title %||% "", ignore.case = TRUE)
        if (is_news) {
          news_count <- news_count + 1
        } else {
          article_count <- article_count + 1
        }
      }
    }
  }

  # --- Write output ---
  qmd_file <- file.path(issue_dir, "index.qmd")
  writeLines(qmd_lines, qmd_file)
  message("Generated: ", qmd_file, " (", article_count, " articles, ", news_count, " news)")

  qmd_file
}

# --- Main ---
issue_dirs <- list.dirs("issues", recursive = FALSE, full.names = TRUE)
message("Found ", length(issue_dirs), " issue directories\n")

for (issue_dir in issue_dirs) {
  tryCatch(
    {
      generate_issue_qmd(issue_dir)
    },
    error = function(e) {
      message("Error processing ", issue_dir, ": ", e$message)
    }
  )
}

message("\nDone!")
