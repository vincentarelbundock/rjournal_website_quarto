# Problem: `distill`

The *R Journal* website uses the `distill` package to create and manage its content. This package has several problems:

1. This package is no longer actively developed: there has been no commit on Github for 2.5 years, despite 145 open issues on the ticket tracker.
2. AFAICT, it does not include useful features like search. 

### Proposed solution 

Migrate the website to `Quarto`.

# Problem: Missing meta-data

For many years, Editors created useful meta-data files in YAML format to specify the structure of an issue, including article titles, authors, page numbers, and abstracts. These files were used to automatically generate the Table of Contents for each issue on the website.

Unfortunately, not all issues have these meta-data files. To build issue pages, we must sometimes parse the articles themselves.

### Proposed solution

1. Complete the coverage of meta-data files for all past issues.
2. For future issues, require Editors to create the `.yml` for each issue.
3. Use the issue meta-data `.yml` files to generate landing pages (ToC) for each issue.

# Problem: Building articles from source

As far as I can tell, the current website setup re-builds all articles from source every time the website is deployed. This is problematic:

1. *Insecure.* Who wants to run arbitrary R code on their machines?
2. *Unreliable.* Old notebooks may no longer run when packages change, and the *R Journal* Editors should not be in charge of maintaining every authors' code.
3. *Inefficient.* It takes a long time to re-build all articles.

### Proposed solution

When an article is formally accepted assigned to an issue, the Editor saves the raw files, and also two *rendered* copies in the `rjournal.github.io` repository:

1. PDF
2. HTML with embedded resources (i.e., inline images, CSS, etc.)

We no longer re-build the full articles (and execute all code) every time we deploy the website. Instead, pre-rendered PDF and HTML files are embeded into the landing page of the corresponding article. For PDF files, we show a preview using `pdf.js`. For HTML files, we embed the full HTML content inside an `<iframe>`.

One downside is that the styling of articles rendered a long time ago may not match the latest website theme. In my view, this is a small price to pay for stability and long term maintainability.

# Problem: No search functionality

The current website has no search functionality, making it difficult to find articles on specific topics.

### Proposed solution

Quarto has built-in support.

# Problem: Acceptable submission formats

Authors want to submit articles in a variety of formats.

### Proposed solution

*R Journal* supplies templates for 3 formats but encourages Quarto.

* Rmarkdown submissions `.Rmd`
    - Compile to PDF and standalone HTML 
    - Embed HTML on website (with PDF download link)
* Quarto
    - Compile to PDF and standalone HTML
    - Embed HTML on website (with PDF download link)
* LaTeX
    - Compile to PDF only
    - Embed PDF on website  

# TODO

+ [ ] Quarto template to match Rmarkdown and LaTeX templates.
+ [ ] Design a clean minimalist theme for the website.
+ [ ] Hire someone to manually inspect 
    + [ ] All issue pages
    + [ ] Large random sample of articles
+ [ ] BibTeX references to article landing pages
+ [ ] BibTeX references to news landing pages
+ [ ] DOI displayed prominently where applicable
+ [ ] Re-use policy where applicable


