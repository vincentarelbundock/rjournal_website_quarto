# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the R Journal article repository website built with Quarto. It contains 785+ academic articles (2009-2025) and 68+ journal issues (2001-2025) in a structured archive format.

## Build Commands

```bash
# Render the entire site
quarto render

# Preview with live reload
quarto preview

# Render a single file
quarto render path/to/file.qmd
```

Output goes to `_site/` directory by default.

## Architecture

### Directory Structure

- `_articles/` - Published articles using `RJ-YYYY-NNN` naming convention
- `_issues/` - Journal issues using `YYYY-N` naming convention
- `posts/` - Blog-style posts with RSS feed support
- `_quarto.yml` - Main site configuration

### Article Structure

Each article directory (e.g., `_articles/RJ-2025-008/`) contains:
- Source file (`.Rmd` or `.qmd`)
- `RJwrapper.md` - Markdown wrapper with metadata
- `.bib` bibliography file
- Generated outputs (`.html`, `.pdf`)
- `figures/` directory
- Supplementary R code and materials

### Configuration

- `_quarto.yml` - Site-wide settings (theme: cosmo, navbar, RSS)
- `posts/_metadata.yml` - Post-specific settings (`freeze: true` disables re-execution)

## Content Format

Articles are written in R Markdown (`.Rmd`) or Quarto Markdown (`.qmd`) with:
- YAML frontmatter for metadata
- R code chunks for reproducible analysis
- LaTeX-style math notation
- BibTeX citations
