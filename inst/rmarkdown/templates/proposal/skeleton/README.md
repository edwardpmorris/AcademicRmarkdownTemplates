# Proposal template

## Directory structure

+ `/data` All files with data such as tables, spreadsheets
+ `/figures` Any images or graphics; including code definitions of diagrams (.mmd, .DOT)
+ `/config` Proposal metadata (`metadata.yaml`), bibliography (`bibliography.bib`), list of figures (`figure_list.yaml`), document change list (`doc_change_list.yaml`), css and style tweaks.

## Instructions

Fill in: 

+ Rmarkdown document (`*.Rmd`) yaml front matter, `title`, `author` and 
optionally `params/major_version`, `params/author`, `params/modification` and
`params/status`. The output format...
+ `/config/metadata.yaml`; the first 4 entries are required.
+ Add BibTeX citations to `/config/bibliography.bib`.
+ `/config/figure_list.yaml`; note `label` should be a short unique name by which to index the figure, path is only the filename, caption is optional.
+ Add figures and graphics to `/figures`.
+ Add data to `/data`.

## Usage

Knitting the Rmarkdown document automatically checks for a change in `params/major_version` compared to the last entry in the `doc_change_list` if so it adds a new entry using the fields `params/author`, `params/modification` and
`params/status`.

Helper functions are available for inserting a figure with kfigr html tags using the `label` fied in `/config/figure_list.yaml`. For example, to insert a 'test_figure' and refer to it in the text, you would add:

```{r}

`r insertFig("test_figure", "config/figure_list.yaml")`

Something backed up by a plot (`r figr("test_figure")`)
```