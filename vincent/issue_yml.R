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

should_regen_yml <- function(yml_file) {
  if (!file.exists(yml_file)) {
    return(TRUE)
  }

  lines <- readLines(yml_file, warn = FALSE)
  articles_idx <- which(trimws(lines) == "articles:")
  if (length(articles_idx) == 0) {
    return(TRUE)
  }

  after <- lines[(articles_idx[1] + 1):length(lines)]
  after <- after[trimws(after) != ""]
  if (length(after) == 0) {
    return(TRUE)
  }

  any(grepl("^\\s*-\\s+", after))
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
    # Read metadata from each article's .Rmd file
    article_entries <- list()

    for (art_id in matching_articles) {
      rmd_file <- file.path("articles", art_id, paste0(art_id, ".Rmd"))
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

          meta_volume <- meta$volume %||% meta$journal$volume
          meta_issue <- meta$issue %||% meta$journal$issue
          if (is.null(meta_volume) || is.null(meta_issue)) {
            next
          }
          if (
            as.integer(meta_volume) != vol ||
              as.integer(meta_issue) != issue_num
          ) {
            next
          }

          # Extract author
          author <- NULL
          if (!is.null(meta$author)) {
            if (is.list(meta$author)) {
              author <- sapply(meta$author, function(a) a$name %||% a)
            } else {
              author <- meta$author
            }
          }

          # Extract pages
          pages <- NULL
          if (!is.null(meta$journal$firstpage)) {
            if (
              !is.null(meta$journal$lastpage) &&
                meta$journal$lastpage != meta$journal$firstpage
            ) {
              pages <- list(meta$journal$firstpage, meta$journal$lastpage)
            } else {
              pages <- meta$journal$firstpage
            }
          }

          # Get first page for sorting
          first_page <- if (!is.null(meta$journal$firstpage)) {
            as.integer(meta$journal$firstpage)
          } else {
            9999
          }

          article_entries[[length(article_entries) + 1]] <- list(
            title = meta$title,
            author = author,
            slug = art_id,
            pages = pages,
            first_page = first_page
          )
        },
        error = function(e) NULL
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

# Parse RJournal.bib file
parse_bib <- function(bib_file) {
  lines <- readLines(bib_file, warn = FALSE)
  articles <- list()
  i <- 1

  while (i <= length(lines)) {
    line <- lines[i]
    if (grepl("^@article\\{", line, ignore.case = TRUE)) {
      key <- sub("^@article\\{([^,]+),.*", "\\1", line, ignore.case = TRUE)
      entry <- list(key = key)
      i <- i + 1

      while (i <= length(lines) && !grepl("^\\}$", trimws(lines[i]))) {
        field_line <- lines[i]
        if (grepl("^\\s*\\w+\\s*=", field_line)) {
          field_match <- regmatches(
            field_line,
            regexec("^\\s*(\\w+)\\s*=\\s*(.+)$", field_line)
          )[[1]]
          if (length(field_match) >= 3) {
            field_name <- tolower(field_match[2])
            field_value <- field_match[3]
            field_value <- sub(",\\s*$", "", field_value)
            field_value <- gsub("^\\{+|\\}+$", "", field_value)
            field_value <- gsub("^\\{|\\}$", "", field_value)
            field_value <- gsub('^\\"|\\\"$', "", field_value)
            field_value <- trimws(field_value)
            entry[[field_name]] <- field_value
          }
        }
        i <- i + 1
      }

      if (!is.null(entry$volume)) {
        entry$volume <- as.integer(gsub("[^0-9]", "", entry$volume))
      }
      if (!is.null(entry$number)) {
        entry$number <- as.integer(gsub("[^0-9]", "", entry$number))
      }
      if (!is.null(entry$year)) {
        entry$year <- as.integer(gsub("[^0-9]", "", entry$year))
      }

      articles[[length(articles) + 1]] <- entry
    }
    i <- i + 1
  }
  articles
}

# Read news item metadata from .Rmd
read_news_meta <- function(news_id) {
  news_rmd <- file.path("news", news_id, paste0(news_id, ".Rmd"))
  if (!file.exists(news_rmd)) {
    return(NULL)
  }

  tryCatch(
    {
      lines <- readLines(news_rmd, warn = FALSE)
      yaml_end <- which(lines == "---")[2]
      if (is.na(yaml_end) || yaml_end <= 2) {
        return(NULL)
      }

      meta <- yaml.load(paste(lines[2:(yaml_end - 1)], collapse = "\n"))

      # Extract author
      author <- NULL
      if (!is.null(meta$author)) {
        if (is.list(meta$author)) {
          author <- sapply(meta$author, function(a) a$name %||% a)
        } else {
          author <- meta$author
        }
      }

      # Extract pages
      pages <- NULL
      if (!is.null(meta$journal$firstpage)) {
        if (
          !is.null(meta$journal$lastpage) &&
            meta$journal$lastpage != meta$journal$firstpage
        ) {
          pages <- list(meta$journal$firstpage, meta$journal$lastpage)
        } else {
          pages <- meta$journal$firstpage
        }
      }

      list(
        title = meta$title,
        author = author,
        slug = news_id,
        pages = pages
      )
    },
    error = function(e) NULL
  )
}

generate_rjournal_yml <- function(year, issue_num, bib_articles) {
  vol <- get_volume(year)
  issue_id <- sprintf("%d-%d", year, issue_num)

  # Filter articles for this issue
  issue_arts <- Filter(
    function(a) {
      !is.null(a$volume) &&
        !is.null(a$number) &&
        !is.null(a$year) &&
        a$volume == vol &&
        a$number == issue_num &&
        a$year == year
    },
    bib_articles
  )

  issue_arts <- Filter(
    function(a) {
      !is.null(a$key) && grepl("^RJ-\\d{4}-\\d{3}$", a$key)
    },
    issue_arts
  )

  # Sort by pages
  if (length(issue_arts) > 0) {
    pages_num <- sapply(issue_arts, function(a) {
      if (is.null(a$pages)) {
        return(9999)
      }
      as.numeric(sub("-.*", "", a$pages))
    })
    issue_arts <- issue_arts[order(pages_num)]
  }

  # Find news items
  news_pattern <- sprintf("^RJ-%d-%d-", year, issue_num)
  all_news_dirs <- list.dirs("news", recursive = FALSE, full.names = FALSE)
  matching_news <- sort(all_news_dirs[grepl(news_pattern, all_news_dirs)])

  # Separate editorial from other news
  editorial_ids <- matching_news[grepl(
    "editorial",
    matching_news,
    ignore.case = TRUE
  )]
  other_news_ids <- matching_news[
    !grepl("editorial", matching_news, ignore.case = TRUE)
  ]

  # Read .Rmd for date info
  rmd_file <- file.path("issues", issue_id, paste0(issue_id, ".Rmd"))
  rmd_date <- NULL
  if (file.exists(rmd_file)) {
    tryCatch(
      {
        lines <- readLines(rmd_file, warn = FALSE)
        yaml_end <- which(lines == "---")[2]
        if (!is.na(yaml_end) && yaml_end > 2) {
          rmd_meta <- yaml.load(paste(lines[2:(yaml_end - 1)], collapse = "\n"))
          rmd_date <- rmd_meta$date
        }
      },
      error = function(e) NULL
    )
  }

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

    # Add articles
    for (art in issue_arts) {
      # Parse author string to list
      author_str <- art$author %||% ""
      authors <- trimws(strsplit(author_str, " and ")[[1]])

      articles_list[[length(articles_list) + 1]] <- list(
        slug = art$key,
        title = gsub("\\{|\\}", "", art$title %||% art$key),
        author = as.list(authors),
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
            tools::toTitleCase(gsub(
              "-",
              " ",
              gsub("^RJ-\\d+-\\d+-", "", news_id)
            )),
          author = news_meta$author,
          slug = news_id,
          pages = news_meta$pages
        )
      } else {
        # Fallback
        nice_title <- gsub("^RJ-\\d+-\\d+-", "", news_id)
        nice_title <- tools::toTitleCase(gsub("-", " ", nice_title))
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

# Parse bib file once for R Journal era
message("Parsing RJournal.bib...")
bib_articles <- parse_bib("RJournal.bib")
message("Found ", length(bib_articles), " entries\n")

# Define all issues
# R News era: 2001-2008
rnews_issues <- c(
  "2001-1",
  "2001-2",
  "2001-3",
  "2002-1",
  "2002-2",
  "2002-3",
  "2003-1",
  "2003-2",
  "2003-3",
  "2004-1",
  "2004-2",
  "2005-1",
  "2005-2",
  "2005-3",
  "2006-1",
  "2006-2",
  "2006-3",
  "2006-4",
  "2006-5",
  "2007-1",
  "2007-2",
  "2007-3",
  "2008-1",
  "2008-2"
)

# R Journal era: 2009-2025
rjournal_issues <- c(
  "2009-1",
  "2009-2",
  "2010-1",
  "2010-2",
  "2011-1",
  "2011-2",
  "2012-1",
  "2012-2",
  "2013-1",
  "2013-2",
  "2014-1",
  "2014-2",
  "2015-1",
  "2015-2",
  "2016-1",
  "2016-2",
  "2017-1",
  "2017-2",
  "2018-1",
  "2018-2",
  "2019-1",
  "2019-2",
  "2020-1",
  "2020-2",
  "2021-1",
  "2021-2",
  "2022-1",
  "2022-2",
  "2022-3",
  "2022-4",
  "2023-1",
  "2023-2",
  "2023-3",
  "2023-4",
  "2024-1",
  "2024-2",
  "2024-3",
  "2024-4",
  "2025-1",
  "2025-2",
  "2025-3"
)

# Process R News issues
message("Processing R News issues (2001-2008)...")
for (issue_id in rnews_issues) {
  yml_file <- file.path("issues", issue_id, paste0(issue_id, ".yml"))

  # Check if issue directory exists
  if (!dir.exists(file.path("issues", issue_id))) {
    message("  Skipping ", issue_id, " (no issue directory)")
    next
  }

  parts <- strsplit(issue_id, "-")[[1]]
  year <- as.integer(parts[1])
  issue_num <- as.integer(parts[2])

  # Skip if yml already exists unless it needs regeneration
  if (
    file.exists(yml_file) && !needs_news_slug_fix(yml_file, year, issue_num)
  ) {
    message("  Skipping ", issue_id, " (yml exists)")
    next
  }

  yml_data <- generate_rnews_yml(year, issue_num)
  write_yml(yml_data, yml_file)
  message(
    "  Generated: ",
    yml_file,
    " (",
    length(yml_data$articles),
    " entries)"
  )
}

# Process R Journal issues
message("\nProcessing R Journal issues (2009+)...")
for (issue_id in rjournal_issues) {
  yml_file <- file.path("issues", issue_id, paste0(issue_id, ".yml"))

  # Check if issue directory exists
  if (!dir.exists(file.path("issues", issue_id))) {
    message("  Skipping ", issue_id, " (no issue directory)")
    next
  }

  parts <- strsplit(issue_id, "-")[[1]]
  year <- as.integer(parts[1])
  issue_num <- as.integer(parts[2])

  # Skip if yml already exists unless it needs regeneration
  if (
    file.exists(yml_file) && !needs_news_slug_fix(yml_file, year, issue_num)
  ) {
    message("  Skipping ", issue_id, " (yml exists)")
    next
  }

  yml_data <- generate_rjournal_yml(year, issue_num, bib_articles)
  write_yml(yml_data, yml_file)
  message(
    "  Generated: ",
    yml_file,
    " (",
    length(yml_data$articles),
    " entries)"
  )
}

message("\nDone!")
