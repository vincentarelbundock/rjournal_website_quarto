# Generate Quarto pages for all issues in issues/
#
# DATA SOURCES:
# - .yml files (issues/YYYY-N/YYYY-N.yml): Primary source for all issues
#   Run 'make yml' first to generate missing .yml files
# - Article .Rmd files (articles/RN-*/RN-*.Rmd): Fallback for R News era (2001-2008)
#   if no .yml file exists

library(yaml)

source("vincent/helpers.R")

# Parse R News article metadata from articles directory (2001-2008)
parse_rnews_articles <- function() {
  articles <- list()
  art_dirs <- list.dirs("articles", recursive = FALSE, full.names = TRUE)
  rn_dirs <- art_dirs[grepl("/RN-", art_dirs)]

  for (art_dir in rn_dirs) {
    art_id <- basename(art_dir)
    rmd_file <- file.path(art_dir, paste0(art_id, ".Rmd"))
    if (!file.exists(rmd_file)) {
      next
    }

    tryCatch(
      {
        lines <- readLines(rmd_file, warn = FALSE)
        yaml_end <- which(lines == "---")[2]
        if (is.na(yaml_end) || yaml_end <= 2) {
          next
        }

        meta <- yaml.load(paste(lines[2:(yaml_end - 1)], collapse = "\n"))

        author_str <- ""
        if (!is.null(meta$author)) {
          if (is.list(meta$author)) {
            author_names <- sapply(meta$author, function(a) {
              if (is.list(a)) {
                paste(a$first_name %||% "", a$last_name %||% a$name %||% "")
              } else {
                a
              }
            })
            author_str <- paste(trimws(author_names), collapse = ", ")
          } else {
            author_str <- meta$author
          }
        }

        pages <- NULL
        if (!is.null(meta$journal$firstpage)) {
          if (
            !is.null(meta$journal$lastpage) &&
              meta$journal$lastpage != meta$journal$firstpage
          ) {
            pages <- c(meta$journal$firstpage, meta$journal$lastpage)
          } else {
            pages <- meta$journal$firstpage
          }
        }

        entry <- list(
          slug = art_id,
          title = meta$title,
          author = author_str,
          volume = meta$volume,
          number = meta$issue,
          pages = pages
        )
        articles[[art_id]] <- entry
      },
      error = function(e) NULL
    )
  }
  articles
}

# Clean LaTeX from text
clean_latex <- function(text) {
  if (is.null(text) || length(text) == 0) {
    return("")
  }
  text <- gsub("\\{|\\}", "", text)
  text <- gsub("\\\\k\\{e\\}", "e", text)
  text <- gsub("\\\\L\\{\\}", "L", text)
  text <- gsub("\\\\'a", "a", text)
  text <- gsub("\\\\\"a", "a", text)
  text <- gsub("\\\\\"o", "o", text)
  text <- gsub("\\\\\"u", "u", text)
  text
}

# Format article entry for output
format_article <- function(art, issue_year, issue_num, prefix) {
  title <- clean_latex(art$title %||% art$slug)

  # Handle author - could be string or list
  if (is.list(art$author) && length(art$author) > 0) {
    author <- paste(unlist(art$author), collapse = ", ")
  } else {
    author <- paste(art$author, collapse = ", ")
  }
  author <- clean_latex(author)

  # Pages
  pages_str <- ""
  if (!is.null(art$pages)) {
    p <- unlist(art$pages)
    if (length(p) == 2 && p[1] != p[2]) {
      pages_str <- sprintf(" (pp. %s-%s)", p[1], p[2])
    } else {
      pages_str <- sprintf(" (p. %s)", p[1])
    }
  }

  # Determine link path - prefer .qmd (renders as HTML) over PDF
  slug <- art$slug
  link_path <- NULL
  if (!is.null(slug) && grepl("^R[JN]-", slug)) {
    if (grepl("^RJ-\\d{4}-\\d+-", slug)) {
      # News item
      qmd_file <- file.path("news", slug, "index.qmd")
      if (file.exists(qmd_file)) {
        link_path <- sprintf("../../news/%s/", slug)
      } else {
        link_path <- sprintf("../../news/%s/%s.pdf", slug, slug)
      }
    } else {
      # Article
      qmd_file <- file.path("articles", slug, "index.qmd")
      if (file.exists(qmd_file)) {
        link_path <- sprintf("../../articles/%s/", slug)
      } else {
        link_path <- sprintf("../../articles/%s/%s.pdf", slug, slug)
      }
    }
  } else if (!is.null(slug) && nchar(slug) > 0) {
    news_id <- sprintf("%s-%s-%s-%s", prefix, issue_year, issue_num, slug)
    qmd_file <- file.path("news", news_id, "index.qmd")
    if (file.exists(qmd_file)) {
      link_path <- sprintf("../../news/%s/", news_id)
    } else {
      pdf_file <- file.path("news", news_id, paste0(news_id, ".pdf"))
      if (file.exists(pdf_file)) {
        link_path <- sprintf("../../news/%s/%s.pdf", news_id, news_id)
      }
    }
  }

  if (!is.null(link_path)) {
    title_line <- sprintf("[%s](%s)%s", title, link_path, pages_str)
  } else {
    title_line <- sprintf("**%s**%s", title, pages_str)
  }

  if (nchar(author) > 0) {
    sprintf("%s<br>*%s*", title_line, author)
  } else {
    title_line
  }
}

generate_issue_qmd <- function(issue_dir, rnews_articles) {
  issue_id <- basename(issue_dir)
  year <- as.numeric(sub("-.*", "", issue_id))
  iss <- as.numeric(sub(".*-", "", issue_id))

  # Check for .yml file
  yml_file <- file.path(issue_dir, paste0(issue_id, ".yml"))
  yml_data <- NULL
  if (file.exists(yml_file)) {
    yml_data <- read_yaml(yml_file)
  }

  vol <- get_volume(year)

  pdf_file <- find_issue_pdf(issue_dir, issue_id)
  if (nchar(pdf_file) == 0) pdf_file <- NULL

  # Build date from year and issue number (March, June, Sept, Dec for issues 1-4)
  issue_months <- c("03", "06", "09", "12")
  month_num <- issue_months[min(iss, 4)]
  date_str <- sprintf("%s-%s-01", year, month_num)

  # Build .qmd content with metadata for listing
  qmd_lines <- c(
    "---",
    sprintf('title: "Volume %s, Issue %s"', vol, iss),
    sprintf("date: \"%s\"", date_str),
    "page-layout: full",
    "title-block-banner: true",
    "toc: true",
    "---",
    ""
  )

  month_name <- ISSUE_MONTHS[min(iss, 4)]
  if (!is.null(yml_data) && !is.null(yml_data$month)) {
    month_name <- yml_data$month
  }
  published_note <- sprintf("Published %s %s", month_name, year)

  card_lines <- c("<div class=\"paper-card full-bleed\">")
  if (!is.null(pdf_file)) {
    card_lines <- c(
      card_lines,
      "  <p class=\"paper-links\">",
      "    <i class=\"fa-regular fa-file-pdf\"></i>",
      sprintf(
        "    <a href=\"%s\">Download Complete Issue (PDF)</a>",
        pdf_file
      ),
      "  </p>"
    )
  }
  card_lines <- c(
    card_lines,
    sprintf("  <div class=\"paper-citation\">%s</div>", published_note),
    "</div>",
    ""
  )
  qmd_lines <- c(qmd_lines, card_lines)

  article_count <- 0
  news_count <- 0

  if (!is.null(yml_data)) {
    # Use .yml file for structure
    articles_list <- yml_data$articles

    if (!is.null(articles_list) && length(articles_list) > 0) {
      qmd_lines <- c(qmd_lines, "## Table of Contents", "")

      prefix <- if (year < 2009) "RN" else "RJ"

      for (art in articles_list) {
        if (is.null(art)) {
          next
        }

        if (!is.null(art$heading)) {
          qmd_lines <- c(qmd_lines, sprintf("### %s", art$heading), "")
        } else if (!is.null(art$title) || !is.null(art$slug)) {
          qmd_lines <- c(qmd_lines, format_article(art, year, iss, prefix), "")
          if (
            grepl(
              "News|CRAN|Changes|Foundation|Bioconductor",
              art$title %||% "",
              ignore.case = TRUE
            )
          ) {
            news_count <- news_count + 1
          } else {
            article_count <- article_count + 1
          }
        }
      }
    }
  } else if (year < 2009) {
    # R News era - use rnews_articles
    issue_arts <- Filter(
      function(a) {
        !is.null(a$volume) &&
          !is.null(a$number) &&
          a$volume == vol &&
          a$number == iss
      },
      rnews_articles
    )

    if (length(issue_arts) > 0) {
      # Sort by pages
      pages <- sapply(issue_arts, function(a) {
        if (is.null(a$pages)) {
          return(9999)
        }
        as.numeric(a$pages[1])
      })
      issue_arts <- issue_arts[order(pages)]

      qmd_lines <- c(qmd_lines, "## Table of Contents", "")
      qmd_lines <- c(qmd_lines, "### Contributed Research Articles", "")

      prefix <- "RN"
      for (art in issue_arts) {
        qmd_lines <- c(qmd_lines, format_article(art, year, iss, prefix), "")
        article_count <- article_count + 1
      }
    }
  }

  # Write file
  qmd_file <- file.path(issue_dir, "index.qmd")
  writeLines(qmd_lines, qmd_file)
  message(
    "Generated: ",
    qmd_file,
    " (",
    article_count,
    " articles, ",
    news_count,
    " news)"
  )

  return(qmd_file)
}

# Main
message("Parsing R News articles from articles/ directory...")
rnews_articles <- parse_rnews_articles()
message("Found ", length(rnews_articles), " R News articles\n")

# Process all issues
issue_dirs <- list.dirs("issues", recursive = FALSE, full.names = TRUE)
message("Found ", length(issue_dirs), " issue directories\n")

for (dir in issue_dirs) {
  tryCatch(
    generate_issue_qmd(dir, rnews_articles),
    error = function(e) message("Error processing ", dir, ": ", e$message)
  )
}

message("\nDone!")
