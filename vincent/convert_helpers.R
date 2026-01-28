# Shared helpers for article/news conversion scripts

# ============================================================================
# Constants
# ============================================================================

ISSUE_MONTHS <- c("March", "June", "September", "December")
MONTH_NAMES <- c(
  "January",
  "February",
  "March",
  "April",
  "May",
  "June",
  "July",
  "August",
  "September",
  "October",
  "November",
  "December"
)

# ============================================================================
# Core utilities
# ============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

get_volume <- function(year) {
  if (year < 2009) year - 2000 else year - 2008
}

normalize_yaml_booleans <- function(lines) {
  start_idx <- which(trimws(lines) == "---")[1]
  if (is.na(start_idx)) {
    return(lines)
  }
  end_rel <- which(
    trimws(lines[(start_idx + 1):length(lines)]) %in% c("---", "...")
  )[1]
  if (is.na(end_rel)) {
    return(lines)
  }
  end_idx <- start_idx + end_rel

  yaml_lines <- lines[(start_idx + 1):(end_idx - 1)]
  # Convert yes/no to true/false (case-insensitive)
  yaml_lines <- gsub(
    "^(\\s*[^:#]+:\\s*)(yes|YES|Yes)\\s*$",
    "\\1true",
    yaml_lines
  )
  yaml_lines <- gsub(
    "^(\\s*[^:#]+:\\s*)(no|NO|No)\\s*$",
    "\\1false",
    yaml_lines
  )
  yaml_lines <- gsub("^(\\s*-\\s*)(yes|YES|Yes)\\s*$", "\\1true", yaml_lines)
  yaml_lines <- gsub("^(\\s*-\\s*)(no|NO|No)\\s*$", "\\1false", yaml_lines)

  c(lines[1:start_idx], yaml_lines, lines[(end_idx):length(lines)])
}

parse_front_matter <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  lines <- readLines(path, warn = FALSE)
  start_idx <- which(trimws(lines) == "---")[1]
  if (is.na(start_idx)) {
    return(NULL)
  }
  end_rel <- which(
    trimws(lines[(start_idx + 1):length(lines)]) %in% c("---", "...")
  )[1]
  if (is.na(end_rel)) {
    return(NULL)
  }
  end_idx <- start_idx + end_rel
  if (end_idx <= start_idx + 1) {
    return(NULL)
  }
  yaml_text <- paste(lines[(start_idx + 1):(end_idx - 1)], collapse = "\n")
  tryCatch(yaml::yaml.load(yaml_text), error = function(e) NULL)
}

# Check if .Rmd has content beyond YAML front matter
has_body_content <- function(rmd_file) {
  lines <- readLines(rmd_file, warn = FALSE)
  if (length(lines) == 0) {
    return(FALSE)
  }

  trimmed <- trimws(lines)
  non_empty <- trimmed != ""
  if (!any(non_empty)) {
    return(FALSE)
  }

  first_non_empty <- which(non_empty)[1]
  if (trimmed[first_non_empty] != "---") {
    return(TRUE)
  }

  end_rel <- which(
    trimmed[(first_non_empty + 1):length(trimmed)] %in% c("---", "...")
  )[1]
  if (is.na(end_rel)) {
    return(TRUE)
  }
  end_idx <- first_non_empty + end_rel

  if (end_idx >= length(trimmed)) {
    return(FALSE)
  }

  any(trimmed[(end_idx + 1):length(trimmed)] != "")
}

clean_text <- function(text) {
  if (is.null(text)) {
    return("")
  }
  text <- as.character(text)
  text <- gsub("^\\s+|\\s+$", "", text)
  text
}

yaml_quote <- function(text) {
  text <- clean_text(text)
  text <- gsub('"', '\\"', text, fixed = TRUE)
  sprintf('"%s"', text)
}

normalize_authors <- function(author) {
  if (is.null(author)) {
    return(list(display = "", bibtex = ""))
  }

  if (is.list(author) && !is.data.frame(author)) {
    entries <- lapply(author, function(a) {
      if (is.list(a)) {
        first <- clean_text(a$first_name %||% a$firstname %||% a$given %||% "")
        last <- clean_text(
          a$last_name %||%
            a$lastname %||%
            a$family %||%
            a$surname %||%
            a$name %||%
            ""
        )
        name <- trimws(paste(first, last))
        bib <- trimws(paste(last, first))
      } else {
        name <- clean_text(a)
        bib <- name
      }
      name <- sub("^by\\s+", "", name, ignore.case = TRUE)
      bib <- sub("^by\\s+", "", bib, ignore.case = TRUE)
      list(name = name, bib = bib)
    })
    display <- paste(
      vapply(entries, function(e) e$name, character(1)),
      collapse = ", "
    )
    bibtex <- paste(
      vapply(entries, function(e) e$bib, character(1)),
      collapse = " and "
    )
    return(list(display = display, bibtex = bibtex))
  }

  # Handle character vectors (e.g., c("Author One", "Author Two"))
  if (length(author) > 1) {
    author_vec <- vapply(author, clean_text, character(1))
    author_vec <- sub("^by\\s+", "", author_vec, ignore.case = TRUE)
    return(list(
      display = paste(author_vec, collapse = ", "),
      bibtex = paste(author_vec, collapse = " and ")
    ))
  }

  author_str <- clean_text(author)
  author_str <- sub("^by\\s+", "", author_str, ignore.case = TRUE)
  list(display = author_str, bibtex = author_str)
}

extract_article_meta <- function(dir_path, slug) {
  rmd_file <- file.path(dir_path, paste0(slug, ".Rmd"))
  wrapper_file <- file.path(dir_path, "RJwrapper.md")

  meta <- parse_front_matter(rmd_file)
  if (is.null(meta)) {
    meta <- parse_front_matter(wrapper_file)
  }

  meta
}

build_citation <- function(
  authors,
  title,
  journal,
  year,
  volume,
  issue,
  pages,
  doi = ""
) {
  parts <- c()
  if (nchar(authors) > 0) {
    parts <- c(parts, authors)
  }
  if (nchar(title) > 0) {
    parts <- c(parts, sprintf('"%s"', title))
  }
  if (nchar(journal) > 0) {
    parts <- c(parts, journal)
  }
  if (nchar(year) > 0) {
    parts <- c(parts, year)
  }

  vol_issue <- ""
  if (nchar(volume) > 0 && nchar(issue) > 0) {
    vol_issue <- paste0(volume, "(", issue, ")")
  } else if (nchar(volume) > 0) {
    vol_issue <- volume
  } else if (nchar(issue) > 0) {
    vol_issue <- issue
  }
  if (nchar(vol_issue) > 0) {
    parts <- c(parts, vol_issue)
  }

  if (nchar(pages) > 0) {
    parts <- c(parts, pages)
  }

  if (nchar(doi) > 0) {
    doi_link <- sprintf("https://doi.org/%s", doi)
    parts <- c(parts, doi_link)
  }

  paste(parts, collapse = ", ")
}

build_bibtex <- function(
  slug,
  title,
  authors_bib,
  journal,
  year,
  volume,
  issue,
  issn,
  pages,
  url
) {
  if (nchar(title) == 0 || nchar(authors_bib) == 0) {
    return("")
  }

  lines <- c(
    sprintf("@article{%s,", slug),
    sprintf("  author = {%s},", authors_bib),
    sprintf("  title = {%s},", title),
    sprintf("  journal = {%s},", journal)
  )

  if (nchar(year) > 0) {
    lines <- c(lines, sprintf("  year = {%s},", year))
  }
  if (nchar(volume) > 0) {
    lines <- c(lines, sprintf("  volume = {%s},", volume))
  }
  if (nchar(issue) > 0) {
    lines <- c(lines, sprintf("  issue = {%s},", issue))
  }
  if (nchar(issn) > 0) {
    lines <- c(lines, sprintf("  issn = {%s},", issn))
  }
  if (nchar(pages) > 0) {
    lines <- c(lines, sprintf("  pages = {%s},", pages))
  }
  if (nchar(url) > 0) {
    lines <- c(lines, sprintf("  note = {%s},", url))
  }

  lines[length(lines)] <- sub(",", "", lines[length(lines)])
  lines <- c(lines, "}")
  paste(lines, collapse = "\n")
}

find_pdf_name <- function(root_dir, slug) {
  pdf_file <- file.path(root_dir, paste0(slug, ".pdf"))
  if (file.exists(pdf_file)) {
    return(paste0(slug, ".pdf"))
  }

  pdf_candidates <- list.files(
    root_dir,
    pattern = "\\.pdf$",
    full.names = FALSE
  )
  if (length(pdf_candidates) == 0) {
    return("")
  }
  pdf_candidates[1]
}

find_issue_pdf <- function(issue_dir, issue_id) {
  pdf_files <- list.files(issue_dir, pattern = "\\.pdf$", full.names = FALSE)
  rj_pdf <- paste0("RJ-", issue_id, ".pdf")
  plain_pdf <- paste0(issue_id, ".pdf")
  if (rj_pdf %in% pdf_files) {
    return(rj_pdf)
  }
  if (plain_pdf %in% pdf_files) {
    return(plain_pdf)
  }
  if (length(pdf_files) > 0) {
    return(pdf_files[1])
  }
  ""
}

extract_pages <- function(meta) {
  firstpage <- meta$journal$firstpage %||% meta$firstpage
  lastpage <- meta$journal$lastpage %||% meta$lastpage
  if (is.null(firstpage)) {
    return(NULL)
  }
  if (!is.null(lastpage) && lastpage != firstpage) {
    list(as.integer(firstpage), as.integer(lastpage))
  } else {
    as.integer(firstpage)
  }
}

pages_to_string <- function(pages) {
  if (is.null(pages)) {
    return("")
  }
  p <- unlist(pages)
  if (length(p) == 2 && p[1] != p[2]) {
    paste0(p[1], "-", p[2])
  } else if (length(p) >= 1) {
    as.character(p[1])
  } else {
    ""
  }
}

build_article_reference <- function(dir_path, slug) {
  meta <- extract_article_meta(dir_path, slug)

  title <- clean_text(meta$title %||% slug)

  author_info <- normalize_authors(meta$author)
  authors <- author_info$display
  authors_bib <- author_info$bibtex

  date <- clean_text(meta$date %||% "")
  year <- if (nchar(date) >= 4) substr(date, 1, 4) else ""

  journal_title <- clean_text(meta$journal$title %||% "")
  if (nchar(journal_title) == 0) {
    journal_title <- if (grepl("^RN-", slug)) "R News" else "R Journal"
  }
  volume <- clean_text(meta$journal$volume %||% meta$volume %||% "")
  issue <- clean_text(meta$journal$issue %||% meta$issue %||% "")
  issn <- clean_text(meta$journal$issn %||% "")

  firstpage <- clean_text(meta$journal$firstpage %||% "")
  lastpage <- clean_text(meta$journal$lastpage %||% "")
  pages <- ""
  if (nchar(firstpage) > 0 && nchar(lastpage) > 0 && firstpage != lastpage) {
    pages <- paste0(firstpage, "-", lastpage)
  } else if (nchar(firstpage) > 0) {
    pages <- firstpage
  }

  canonical_url <- paste0("https://journal.r-project.org/articles/", slug, "/")

  doi <- ""
  if (grepl("^RJ-\\d{4}-\\d{3}$", slug)) {
    doi <- paste0("10.32614/", slug)
  }

  issue_link <- ""
  issue_label <- ""
  issue_year <- year
  if (nchar(issue_year) == 0 && grepl("^RJ-\\d{4}-", slug)) {
    issue_year <- substr(slug, 4, 7)
  }
  if (nchar(issue_year) > 0 && nchar(issue) > 0) {
    issue_label <- sprintf("Volume %s, Issue %s", volume, issue)
    issue_link <- sprintf("../../issues/%s-%s/", issue_year, issue)
  }

  citation <- build_citation(
    authors,
    title,
    journal_title,
    year,
    volume,
    issue,
    pages,
    doi
  )
  bibtex <- build_bibtex(
    slug,
    title,
    authors_bib,
    journal_title,
    year,
    volume,
    issue,
    issn,
    pages,
    canonical_url
  )

  list(
    title = title,
    authors = authors,
    citation = citation,
    bibtex = bibtex,
    pdf_name = find_pdf_name(dir_path, slug),
    issue_label = issue_label,
    issue_link = issue_link
  )
}

build_citation_block <- function(citation, bibtex) {
  lines <- character(0)
  if (nchar(citation) > 0) {
    lines <- c(
      lines,
      sprintf("<div class=\"paper-citation\">%s</div>", citation)
    )
  }
  if (nchar(bibtex) > 0) {
    lines <- c(
      lines,
      "<details class=\"paper-bibtex\">",
      "  <summary>BibTeX</summary>",
      "  <pre><code class=\"language-bibtex\">",
      bibtex,
      "  </code></pre>",
      "</details>"
    )
  }
  lines
}

render_embed_html <- function(root_dir, slug, rmd_file) {
  html_file <- file.path(root_dir, paste0(slug, ".html"))
  temp_rmd <- file.path(root_dir, paste0(slug, "-render.Rmd"))

  lines <- readLines(rmd_file, warn = FALSE)
  lines <- normalize_yaml_booleans(lines)
  writeLines(lines, temp_rmd)

  output_format <- rmarkdown::html_document(self_contained = TRUE)
  rmarkdown::render(
    input = temp_rmd,
    output_format = output_format,
    output_file = paste0(slug, ".html"),
    output_dir = root_dir,
    quiet = TRUE
  )

  if (!file.exists(html_file)) {
    stop("rmarkdown render failed")
  }

  unlink(temp_rmd)
  TRUE
}

write_iframe_index <- function(root_dir, slug) {
  index_file <- file.path(root_dir, "index.qmd")
  lines <- c(
    "---",
    "page-layout: full",
    "format:",
    "  html:",
    "    toc: false",
    "---",
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

create_pdf_index <- function(root_dir, slug) {
  qmd_file <- file.path(root_dir, "index.qmd")
  if (file.exists(qmd_file)) {
    return(FALSE)
  }

  pdf_file <- file.path(root_dir, paste0(slug, ".pdf"))
  if (!file.exists(pdf_file)) {
    pdf_candidates <- list.files(
      root_dir,
      pattern = "\\.pdf$",
      full.names = FALSE
    )
    if (length(pdf_candidates) == 0) {
      message("  Skipping ", slug, " (no PDF found)")
      return(FALSE)
    }
    pdf_name <- pdf_candidates[1]
  } else {
    pdf_name <- paste0(slug, ".pdf")
  }

  qmd_lines <- c(
    "---",
    "page-layout: full",
    "format:",
    "  html:",
    "    toc: false",
    "---",
    "",
    "<div class=\"paper-reader full-bleed\">",
    sprintf(
      "  <embed src=\"%s\" type=\"application/pdf\" height=\"955px\" width=\"100%%\">",
      pdf_name
    ),
    "</div>"
  )

  writeLines(qmd_lines, qmd_file)
  message("  Created ", qmd_file)
  TRUE
}
