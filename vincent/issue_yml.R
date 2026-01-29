# Generate missing .yml files for all issues
#
# This script generates .yml files for issues that don't already have them.
# It handles two eras differently:
#   - R News (2001-2008): Extracts metadata from articles/RN-*.Rmd files
#   - R Journal (2009+): Extracts metadata from RJournal.bib and news/ directories
#
# Output: Writes .yml files directly to issues/{year}-{num}/ folders
# Note: Issues that already have .yml files are skipped

source("vincent/helpers.R")

# ============================================================================
# Common utilities
# ============================================================================

# Parse pages string to list
parse_pages <- function(pages_str) {
  if (is.null(pages_str) || pages_str == "") {
    return(NULL)
  }
  parts <- strsplit(as.character(pages_str), "-")[[1]]
  if (length(parts) == 2) {
    list(as.integer(parts[1]), as.integer(parts[2]))
  } else {
    as.integer(parts[1])
  }
}

# Write yml file with clean formatting
write_yml <- function(yml_data, output_file) {
  lines <- c(
    sprintf("issue: %s", yml_data$issue),
    sprintf("year: %d", yml_data$year),
    sprintf("volume: %d", yml_data$volume),
    sprintf("num: %d", yml_data$num),
    sprintf("month: %s", yml_data$month),
    "articles:"
  )

  for (art in yml_data$articles) {
    if (!is.null(art$heading)) {
      lines <- c(lines, sprintf("- heading: %s", art$heading))
    } else {
      if (!is.null(art$slug)) {
        lines <- c(lines, sprintf("- slug: %s", art$slug))
      }
      if (!is.null(art$title)) {
        # Escape quotes properly in YAML
        if (grepl('"', art$title)) {
          lines <- c(lines, sprintf("  title: '%s'", art$title))
        } else if (grepl(":", art$title)) {
          lines <- c(lines, sprintf('  title: "%s"', art$title))
        } else {
          lines <- c(lines, sprintf("  title: %s", art$title))
        }
      }
      if (!is.null(art$author) && length(art$author) > 0) {
        lines <- c(lines, "  author:")
        for (auth in art$author) {
          if (!is.null(auth) && nchar(trimws(auth)) > 0) {
            lines <- c(lines, sprintf("  - %s", trimws(auth)))
          }
        }
      }
      if (!is.null(art$pages)) {
        if (is.list(art$pages) && length(art$pages) == 2) {
          lines <- c(lines, "  pages:")
          lines <- c(lines, sprintf("  - %s", art$pages[[1]]))
          lines <- c(lines, sprintf("  - %s", art$pages[[2]]))
        } else {
          lines <- c(lines, sprintf("  pages: %s", art$pages))
        }
      }
    }
  }

  writeLines(lines, output_file)
}

needs_news_slug_fix <- function(yml_file, year, issue_num) {
  if (!file.exists(yml_file)) {
    return(FALSE)
  }

  yml_data <- tryCatch(
    yaml.load(paste(readLines(yml_file, warn = FALSE), collapse = "\n")),
    error = function(e) NULL
  )
  if (is.null(yml_data) || is.null(yml_data$articles)) {
    return(FALSE)
  }

  prefix <- if (year < 2009) "RN" else "RJ"
  for (entry in yml_data$articles) {
    if (is.null(entry) || is.null(entry$slug)) {
      next
    }
    if (grepl("^R[JN]-", entry$slug)) {
      next
    }
    news_id <- sprintf("%s-%d-%d-%s", prefix, year, issue_num, entry$slug)
    if (dir.exists(file.path("news", news_id))) {
      return(TRUE)
    }
  }

  FALSE
}

# Convert bibtex entry to normalized article data
bib_entry_to_article <- function(entry) {
  # Handle person objects from bibtex package

  author <- entry$author
  if (inherits(author, "person")) {
    author <- format(author, include = c("given", "family"))
  } else if (is.character(author)) {
    author <- trimws(strsplit(author, " and ")[[1]])
  } else {
    author <- character(0)
  }

  list(
    key = entry$key,
    title = gsub("[{}]", "", entry$title %||% ""),
    author = author,
    pages = entry$pages,
    volume = as.integer(gsub("[^0-9]", "", entry$volume %||% "")),
    number = as.integer(gsub("[^0-9]", "", entry$number %||% "")),
    year = as.integer(gsub("[^0-9]", "", entry$year %||% ""))
  )
}

# ============================================================================
# R News era (2001-2008) - reads from articles/RN-*.Rmd
# ============================================================================

generate_rnews_yml <- function(year, issue_num) {
  vol <- get_volume(year)
  issue_id <- sprintf("%d-%d", year, issue_num)

  # Find all RN articles for this year
  pattern <- sprintf("^RN-%d-", year)
  all_article_dirs <- list.dirs(
    "articles",
    recursive = FALSE,
    full.names = FALSE
  )
  matching_articles <- sort(all_article_dirs[grepl(pattern, all_article_dirs)])

  articles_list <- list()

  if (length(matching_articles) > 0) {
    article_entries <- list()

    for (art_id in matching_articles) {
      rmd_file <- file.path("articles", art_id, paste0(art_id, ".Rmd"))
      meta <- parse_front_matter(rmd_file)
      if (is.null(meta)) next

      meta_volume <- meta$volume %||% meta$journal$volume
      meta_issue <- meta$issue %||% meta$journal$issue
      if (is.null(meta_volume) || is.null(meta_issue)) next
      if (as.integer(meta_volume) != vol || as.integer(meta_issue) != issue_num) {
        next
      }

      # Use helpers for author and pages extraction
      author_info <- normalize_authors(meta$author)
      pages <- extract_pages(meta)

      first_page <- if (!is.null(meta$journal$firstpage)) {
        as.integer(meta$journal$firstpage)
      } else {
        9999
      }

      article_entries[[length(article_entries) + 1]] <- list(
        title = meta$title,
        author = if (nchar(author_info$display) > 0) {
          strsplit(author_info$display, ", ")[[1]]
        } else {
          NULL
        },
        slug = art_id,
        pages = pages,
        first_page = first_page
      )
    }

    # Sort by first page
    if (length(article_entries) > 0) {
      pages_order <- sapply(article_entries, function(a) a$first_page)
      article_entries <- article_entries[order(pages_order)]

      articles_list[[length(articles_list) + 1]] <- list(
        heading = "Contributed Research Articles"
      )

      for (entry in article_entries) {
        articles_list[[length(articles_list) + 1]] <- list(
          slug = entry$slug,
          title = entry$title,
          author = as.list(entry$author),
          pages = entry$pages
        )
      }
    }
  }

  # Determine month from issue number
  month_name <- if (issue_num <= 4) ISSUE_MONTHS[issue_num] else "December"

  list(
    issue = issue_id,
    year = year,
    volume = vol,
    num = issue_num,
    month = month_name,
    articles = articles_list
  )
}

# ============================================================================
# R Journal era (2009+) - reads from RJournal.bib and news/
# ============================================================================

# Read news item metadata from .Rmd
read_news_meta <- function(news_id) {
  news_rmd <- file.path("news", news_id, paste0(news_id, ".Rmd"))
  meta <- parse_front_matter(news_rmd)
  if (is.null(meta)) return(NULL)

  author_info <- normalize_authors(meta$author)
  pages <- extract_pages(meta)

  list(
    title = meta$title,
    author = if (nchar(author_info$display) > 0) {
      strsplit(author_info$display, ", ")[[1]]
    } else {
      NULL
    },
    slug = news_id,
    pages = pages
  )
}

generate_rjournal_yml <- function(year, issue_num, bib_articles) {
  vol <- get_volume(year)
  issue_id <- sprintf("%d-%d", year, issue_num)

  # Filter articles for this issue
  issue_arts <- Filter(
    function(a) {
      !is.na(a$volume) && !is.na(a$number) && !is.na(a$year) &&
        a$volume == vol && a$number == issue_num && a$year == year
    },
    bib_articles
  )

  issue_arts <- Filter(
    function(a) !is.null(a$key) && grepl("^RJ-\\d{4}-\\d{3}$", a$key),
    issue_arts
  )

  # Sort by pages
  if (length(issue_arts) > 0) {
    pages_num <- sapply(issue_arts, function(a) {
      if (is.null(a$pages)) return(9999)
      as.numeric(sub("-.*", "", a$pages))
    })
    issue_arts <- issue_arts[order(pages_num)]
  }

  # Find news items
  news_pattern <- sprintf("^RJ-%d-%d-", year, issue_num)
  all_news_dirs <- list.dirs("news", recursive = FALSE, full.names = FALSE)
  matching_news <- sort(all_news_dirs[grepl(news_pattern, all_news_dirs)])

  # Separate editorial from other news
  editorial_ids <- matching_news[grepl("editorial", matching_news, ignore.case = TRUE)]
  other_news_ids <- matching_news[!grepl("editorial", matching_news, ignore.case = TRUE)]

  # Read .Rmd for date info
  rmd_file <- file.path("issues", issue_id, paste0(issue_id, ".Rmd"))
  rmd_meta <- parse_front_matter(rmd_file)
  rmd_date <- rmd_meta$date

  # Build articles list
  articles_list <- list()

  # Add editorial if present
  for (ed_id in editorial_ids) {
    ed_meta <- read_news_meta(ed_id)
    if (!is.null(ed_meta)) {
      articles_list[[length(articles_list) + 1]] <- list(
        title = ed_meta$title %||% "Editorial",
        author = ed_meta$author,
        slug = ed_id,
        pages = ed_meta$pages
      )
    }
  }

  # Add section heading for articles
  if (length(issue_arts) > 0) {
    articles_list[[length(articles_list) + 1]] <- list(
      heading = "Contributed Research Articles"
    )

    for (art in issue_arts) {
      articles_list[[length(articles_list) + 1]] <- list(
        slug = art$key,
        title = art$title,
        author = as.list(art$author),
        pages = parse_pages(art$pages)
      )
    }
  }

  # Add news section
  if (length(other_news_ids) > 0) {
    articles_list[[length(articles_list) + 1]] <- list(
      heading = "News and Notes"
    )

    for (news_id in other_news_ids) {
      news_meta <- read_news_meta(news_id)
      if (!is.null(news_meta)) {
        articles_list[[length(articles_list) + 1]] <- list(
          title = news_meta$title %||%
            tools::toTitleCase(gsub("-", " ", gsub("^RJ-\\d+-\\d+-", "", news_id))),
          author = news_meta$author,
          slug = news_id,
          pages = news_meta$pages
        )
      } else {
        nice_title <- tools::toTitleCase(gsub("-", " ", gsub("^RJ-\\d+-\\d+-", "", news_id)))
        articles_list[[length(articles_list) + 1]] <- list(
          title = nice_title,
          slug = news_id
        )
      }
    }
  }

  # Determine month
  month_name <- if (issue_num <= 4) ISSUE_MONTHS[issue_num] else "December"
  if (!is.null(rmd_date)) {
    date_parts <- strsplit(as.character(rmd_date), "-")[[1]]
    if (length(date_parts) >= 2) {
      month_num <- as.integer(date_parts[2])
      if (month_num >= 1 && month_num <= 12) {
        month_name <- MONTH_NAMES[month_num]
      }
    }
  }

  list(
    issue = issue_id,
    year = year,
    volume = vol,
    num = issue_num,
    month = month_name,
    articles = articles_list
  )
}

# ============================================================================
# Main
# ============================================================================

# Process a single issue with the given generator function
process_issue <- function(issue_id, generator) {
  yml_file <- file.path("issues", issue_id, paste0(issue_id, ".yml"))

  if (!dir.exists(file.path("issues", issue_id))) {
    message("  Skipping ", issue_id, " (no issue directory)")
    return(invisible(NULL))
  }

  parts <- strsplit(issue_id, "-")[[1]]
  year <- as.integer(parts[1])
  issue_num <- as.integer(parts[2])

  if (file.exists(yml_file) && !needs_news_slug_fix(yml_file, year, issue_num)) {
    message("  Skipping ", issue_id, " (yml exists)")
    return(invisible(NULL))
  }

  yml_data <- generator(year, issue_num)
  write_yml(yml_data, yml_file)
  message("  Generated: ", yml_file, " (", length(yml_data$articles), " entries)")
}

# Parse bib file once for R Journal era
message("Parsing RJournal.bib...")
bib_raw <- read.bib("RJournal.bib")
bib_articles <- lapply(bib_raw, bib_entry_to_article)
message("Found ", length(bib_articles), " entries\n")

# Define all issues
rnews_issues <- c(
  "2001-1", "2001-2", "2001-3",
  "2002-1", "2002-2", "2002-3",
  "2003-1", "2003-2", "2003-3",
  "2004-1", "2004-2",
  "2005-1", "2005-2", "2005-3",
  "2006-1", "2006-2", "2006-3", "2006-4", "2006-5",
  "2007-1", "2007-2", "2007-3",
  "2008-1", "2008-2"
)

rjournal_issues <- c(
  "2009-1", "2009-2",
  "2010-1", "2010-2",
  "2011-1", "2011-2",
  "2012-1", "2012-2",
  "2013-1", "2013-2",
  "2014-1", "2014-2",
  "2015-1", "2015-2",
  "2016-1", "2016-2",
  "2017-1", "2017-2",
  "2018-1", "2018-2",
  "2019-1", "2019-2",
  "2020-1", "2020-2",
  "2021-1", "2021-2",
  "2022-1", "2022-2", "2022-3", "2022-4",
  "2023-1", "2023-2", "2023-3", "2023-4",
  "2024-1", "2024-2", "2024-3", "2024-4",
  "2025-1", "2025-2", "2025-3"
)

# Process all issues
message("Processing R News issues (2001-2008)...")
for (issue_id in rnews_issues) {
  process_issue(issue_id, generate_rnews_yml)
}

message("\nProcessing R Journal issues (2009+)...")
for (issue_id in rjournal_issues) {
  process_issue(issue_id, function(y, n) generate_rjournal_yml(y, n, bib_articles))
}

message("\nDone!")
