.PHONY: all toc yml issue article-html article-pdf article news-html news-pdf news render preview clean clean-docs-rmd nuke-index nuke-yml nuke subset help fresh
.DEFAULT_GOAL := help

help: ## Display this help screen
	@echo "Available commands:"
	@grep -E '^[a-z.A-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-18s %s\n", $$1, $$2}' | sort

all: issue render ## Build issues, render site

fresh: subset news article issue render ## Refresh subset, pages, and render

yml: ## Generate missing yml files for all eras
	Rscript vincent/generate_missing_yml.R

issue: yml ## Generate issue TOC pages
	Rscript vincent/generate_issue_pages.R

article-html: ## Render embedded HTML for 2024 articles
	Rscript vincent/convert_all_embed_html.R

article-pdf: article-html ## Create PDF index.qmd where missing
	Rscript vincent/generate_pdf_only_pages.R

article: article-html article-pdf ## Generate article HTML and PDF pages

news-html: ## Render embedded HTML for news items
	Rscript vincent/generate_news_html.R

news-pdf: ## Create PDF index.qmd for news where missing
	Rscript vincent/generate_news_pdf.R

news: news-html news-pdf ## Generate news HTML and PDF pages

render: ## Render the entire site
	quarto render .
	touch docs/.nojekyll
	find docs -name "*.Rmd" -type f -delete

preview: ## Preview site with live reload
	quarto preview .

clean: ## Clean site output and generated indices
	rm -rf _site
	rm -rf _freeze
	find issues -name "index.qmd" -type f -delete

clean-docs-rmd: ## Remove all .Rmd files from docs/
	find docs -name "*.Rmd" -type f -delete

nuke-index: ## Delete converted article index.qmd files
	find articles -name "index.qmd" -type f -delete

nuke-yml: ## Delete generated issue yml files
	find issues -name "*.yml" -type f -delete

nuke: ## Replace articles/, issues/, and news/ from rjournal.github.io
	rm -rf articles issues news _site
	cp -R "$(HOME)/Downloads/rjournal.github.io/_articles" articles
	cp -R "$(HOME)/Downloads/rjournal.github.io/_issues" issues
	cp -R "$(HOME)/Downloads/rjournal.github.io/_news" news

subset: nuke ## Keep only 2001, 2008, 2014, 2025 in articles/, issues/, news/
	find articles -mindepth 1 -maxdepth 1 -type d -not -name "RJ-2001-*" -not -name "RJ-2008-*" -not -name "RJ-2014-*" -not -name "RJ-2025-*" -not -name "RN-2001-*" -not -name "RN-2008-*" -not -name "RN-2014-*" -not -name "RN-2025-*" -exec rm -rf {} +
	find issues -mindepth 1 -maxdepth 1 -type d -not -name "2001-*" -not -name "2008-*" -not -name "2014-*" -not -name "2025-*" -exec rm -rf {} +
	find news -mindepth 1 -maxdepth 1 -type d -not -name "RJ-2001-*" -not -name "RJ-2008-*" -not -name "RJ-2014-*" -not -name "RJ-2025-*" -not -name "RN-2001-*" -not -name "RN-2008-*" -not -name "RN-2014-*" -not -name "RN-2025-*" -exec rm -rf {} +
