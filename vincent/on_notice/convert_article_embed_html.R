# Convert RJ-2025-001.Rmd to embedded HTML and index.qmd

library(knitr)
library(yaml)
library(quarto)

args <- commandArgs(trailingOnly = TRUE)
slug <- if (length(args) >= 1) args[1] else "RJ-2025-002"

article_dir <- file.path("articles", slug)
rmd_file <- file.path(article_dir, paste0(slug, ".Rmd"))
qmd_file <- file.path(article_dir, paste0(slug, ".qmd"))
html_file <- file.path(article_dir, paste0(slug, ".html"))
index_file <- file.path(article_dir, "index.qmd")

if (!dir.exists(article_dir)) {
  stop("Missing article directory: ", article_dir)
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
  yaml_text <- paste(lines[(start_idx + 1):(end_idx - 1)], collapse = "\n")
  tryCatch(yaml.load(yaml_text), error = function(e) NULL)
}

yaml_quote <- function(text) {
  text <- gsub('"', '\\"', text, fixed = TRUE)
  sprintf('"%s"', text)
}

author_to_string <- function(author) {
  if (is.null(author)) {
    return("")
  }
  if (is.list(author) && !is.data.frame(author)) {
    names <- vapply(
      author,
      function(a) {
        if (is.list(a)) {
          if (!is.null(a$name)) {
            return(a$name)
          }
          first <- a$first_name %||% a$firstname %||% ""
          last <- a$last_name %||%
            a$lastname %||%
            a$family %||%
            a$surname %||%
            ""
          return(trimws(paste(first, last)))
        }
        a
      },
      character(1)
    )
    return(paste(names, collapse = ", "))
  }
  as.character(author)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

if (!file.exists(rmd_file)) {
  stop("Missing source file: ", rmd_file)
}

message("Converting Rmd to Qmd...")
knitr::convert_chunk_header(rmd_file, output = qmd_file, type = "yaml")

message("Updating YAML preamble format...")
lines <- readLines(qmd_file, warn = FALSE)
start_idx <- which(trimws(lines) == "---")[1]
if (is.na(start_idx)) {
  stop("Missing YAML front matter in ", qmd_file)
}
end_rel <- which(
  trimws(lines[(start_idx + 1):length(lines)]) %in% c("---", "...")
)[1]
if (is.na(end_rel)) {
  stop("Unterminated YAML front matter in ", qmd_file)
}
end_idx <- start_idx + end_rel

yaml_lines <- lines[(start_idx + 1):(end_idx - 1)]
body_lines <- lines[(end_idx + 1):length(lines)]

# Remove existing format block (format: ...)
is_format <- grepl("^\\s*format\\s*:", yaml_lines)
if (any(is_format)) {
  fmt_start <- which(is_format)[1]
  fmt_indent <- regexpr("^\\s*", yaml_lines[fmt_start])
  fmt_indent_len <- attr(fmt_indent, "match.length")
  fmt_end <- fmt_start
  for (i in (fmt_start + 1):length(yaml_lines)) {
    if (i > length(yaml_lines)) {
      break
    }
    line <- yaml_lines[i]
    if (trimws(line) == "") {
      fmt_end <- i
      next
    }
    indent <- attr(regexpr("^\\s*", line), "match.length")
    if (indent <= fmt_indent_len && grepl("^[^#]", trimws(line))) {
      break
    }
    fmt_end <- i
  }
  yaml_lines <- yaml_lines[-(fmt_start:fmt_end)]
}

# Insert embed-resources format at the end of YAML
format_lines <- c(
  "format:",
  "  html:",
  "    embed-resources: true"
)
yaml_lines <- c(yaml_lines, format_lines)

# Normalize overlong fenced div markers (e.g., "::::::::")
body_lines <- gsub("^(\\s*):{4,}\\s*$", "\\1:::", body_lines)
body_lines <- gsub("^(\\s*):{4,}\\s*(\\{.*\\})$", "\\1:::\\2", body_lines)

front_matter <- c("---", yaml_lines, "---", "")
writeLines(c(front_matter, body_lines), qmd_file)

message("Rendering HTML with Quarto...")
quarto_bin <- Sys.which("quarto")
if (nchar(quarto_bin) == 0) {
  stop("Quarto CLI not found in PATH")
}
render_args <- c(
  "render",
  "--no-project",
  "--output",
  paste0(slug, ".html"),
  basename(qmd_file)
)
old_wd <- getwd()
setwd(article_dir)
on.exit(setwd(old_wd), add = TRUE)
result <- system2(quarto_bin, render_args, stdout = TRUE, stderr = TRUE)
status <- attr(result, "status")
if (!is.null(status) && status != 0) {
  stop("Quarto render failed")
}

message("Creating index.qmd with embedded HTML...")
meta <- parse_front_matter(rmd_file)
title <- as.character(meta$title %||% slug)
author <- author_to_string(meta$author)

index_lines <- c(
  "---",
  sprintf("title: %s", yaml_quote(title)),
  if (nchar(author) > 0) sprintf("author: %s", yaml_quote(author)) else NULL,
  "---",
  "",
  "```{=html}",
  sprintf(
    "<iframe width=\"780\" height=\"500\" src=\"%s.html\" title=\"%s\"></iframe>",
    slug,
    gsub('"', "&quot;", title, fixed = TRUE)
  ),
  "```"
)

index_lines <- index_lines[!is.na(index_lines)]
writeLines(index_lines, index_file)

message("Done!")
