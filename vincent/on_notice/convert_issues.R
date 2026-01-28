# Convert all _issues .Rmd files to .qmd format

convert_rmd_to_qmd <- function(rmd_path) {
  lines <- readLines(rmd_path, warn = FALSE)

  # Process each line
  i <- 1
  new_lines <- character()

  while (i <= length(lines)) {
    line <- lines[i]

    # Check for code chunk start
    if (grepl("^```\\{", line)) {
      # Parse chunk header
      # Extract language and options
      chunk_match <- regmatches(line, regexec("^```\\{(\\w+)(.*)\\}$", line))[[1]]

      if (length(chunk_match) >= 2) {
        lang <- chunk_match[2]
        opts_str <- chunk_match[3]

        # Parse options
        opts <- list()
        if (nzchar(trimws(opts_str))) {
          # Remove leading comma and whitespace
          opts_str <- sub("^\\s*,\\s*", "", opts_str)
          # Split by comma (but not inside quotes)
          opt_pairs <- strsplit(opts_str, ",\\s*(?=\\w+\\s*=)", perl = TRUE)[[1]]
          for (opt in opt_pairs) {
            if (grepl("=", opt)) {
              parts <- strsplit(opt, "\\s*=\\s*", perl = TRUE)[[1]]
              key <- trimws(parts[1])
              value <- trimws(parts[2])
              # Convert R-style to YAML-style
              if (value == "FALSE") value <- "false"
              if (value == "TRUE") value <- "true"
              # Remove quotes for simple strings
              value <- gsub('^"(.*)"$', "\\1", value)
              opts[[key]] <- value
            }
          }
        }

        # Write new chunk header
        new_lines <- c(new_lines, paste0("```{", lang, "}"))

        # Write options as #| comments
        for (key in names(opts)) {
          new_lines <- c(new_lines, paste0("#| ", key, ": ", opts[[key]]))
        }
      } else {
        new_lines <- c(new_lines, line)
      }
    } else {
      new_lines <- c(new_lines, line)
    }

    i <- i + 1
  }

  # Write .qmd file
  qmd_path <- sub("\\.Rmd$", ".qmd", rmd_path)
  writeLines(new_lines, qmd_path)

  # Remove old .Rmd file
  file.remove(rmd_path)

  message("Converted: ", basename(rmd_path), " -> ", basename(qmd_path))
}

# Find all .Rmd files in _issues
rmd_files <- list.files("_issues", pattern = "\\.Rmd$", recursive = TRUE, full.names = TRUE)

message("Found ", length(rmd_files), " .Rmd files to convert\n")

for (f in rmd_files) {
  convert_rmd_to_qmd(f)
}

message("\nDone! Converted ", length(rmd_files), " files.")
