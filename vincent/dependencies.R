# Extract package dependencies from article front matter.

get_article_packages <- function(articles_dir = "articles") {
  rmd_files <- list.files(
    articles_dir,
    pattern = "\\.Rmd$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(rmd_files) == 0) {
    return(character())
  }

  packages <- unlist(lapply(rmd_files, function(path) {
    front_matter <- rmarkdown::yaml_front_matter(path)
    pkg <- front_matter$packages
    if (is.null(pkg)) {
      return(character())
    }
    if (is.list(pkg)) {
      pkg <- unlist(pkg, use.names = FALSE)
    }
    as.character(pkg)
  }), use.names = FALSE)

  unique(trimws(packages[packages != ""]))
}

get_article_dependencies <- function(articles_dir = "articles") {
  packages <- get_article_packages(articles_dir)
  if (length(packages) == 0) {
    return(list(packages = character(), dependencies = character(), missing = character()))
  }

  dep_map <- tools::package_dependencies(
    packages,
    recursive = TRUE,
    which = c("Depends", "Imports", "LinkingTo")
  )

  missing <- names(dep_map)[vapply(dep_map, function(x) all(is.na(x)), logical(1))]
  dependencies <- unique(unlist(dep_map, use.names = FALSE))
  dependencies <- dependencies[!is.na(dependencies)]

  list(
    packages = packages,
    dependencies = sort(unique(c(packages, dependencies))),
    missing = missing
  )
}

install_dependencies <- function(articles_dir = "articles") {
  results <- get_article_dependencies(articles_dir)
  pkgs <- unique(results$dependencies)
  if (length(pkgs) == 0) {
    return(invisible(character()))
  }
  install.packages(pkgs, type = "binary")
  invisible(pkgs)
}

if (sys.nframe() == 0) {
  install_dependencies()
}
