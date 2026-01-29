.PHONY: all yml issue article-html article-pdf article news-html news-pdf news deploy render preview nuke subset help
.DEFAULT_GOAL := help

help: ## Display this help screen
	@echo "Available commands:"
	@grep -E '^[a-z.A-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-18s %s\n", $$1, $$2}' | sort

all: subset news article issue render ## Refresh subset, pages, render, preview

deploy: ## deploy _site/ to docs/
	find _site/articles -type f ! -name "*.html" -delete
	rm -rf docs/
	mv _site docs

yml: ## Generate missing yml files for all eras
	Rscript vincent/issue_yml.R

issue: yml ## Generate issue TOC pages
	Rscript vincent/issue_html.R

news-html:
	Rscript vincent/news_html.R

news-pdf:
	Rscript vincent/news_pdf.R

news: news-html news-pdf ## Generate news HTML and PDF pages

article-html:
	Rscript vincent/article_html.R

article-pdf:
	Rscript vincent/article_pdf.R

article: article-html article-pdf ## Generate article HTML and PDF landing pages

render: ## Render the entire site
	quarto render .
	find _site -name "*.Rmd" -type f -delete

preview: ## Preview site with live reload
	quarto preview .

nuke: ## Replace articles/, issues/, and news/ from rjournal.github.io
	rm -rf articles issues news _site docs
	cp -R "$(HOME)/Downloads/rjournal.github.io/_articles" articles
	cp -R "$(HOME)/Downloads/rjournal.github.io/_issues" issues
	cp -R "$(HOME)/Downloads/rjournal.github.io/_news" news

subset: nuke ## Keep only 2001, 2008, 2014, 2025 in articles/, issues/, news/
	# find articles -mindepth 1 -maxdepth 1 -type d -not -name "RJ-2001-*" -not -name "RJ-2008-*" -not -name "RJ-2014-*" -not -name "RJ-2025-*" -not -name "RN-2001-*" -not -name "RN-2008-*" -not -name "RN-2014-*" -not -name "RN-2025-*" -exec rm -rf {} +
	# find issues -mindepth 1 -maxdepth 1 -type d -not -name "2001-*" -not -name "2008-*" -not -name "2014-*" -not -name "2025-*" -exec rm -rf {} +
	# find news -mindepth 1 -maxdepth 1 -type d -not -name "RJ-2001-*" -not -name "RJ-2008-*" -not -name "RJ-2014-*" -not -name "RJ-2025-*" -not -name "RN-2001-*" -not -name "RN-2008-*" -not -name "RN-2014-*" -not -name "RN-2025-*" -exec rm -rf {} +
