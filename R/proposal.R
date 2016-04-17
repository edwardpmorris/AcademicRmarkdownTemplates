#' A generic template for an academic proposal
#'
#' @return MS Word (DOCX) proposal template
#' @export
proposal <-
  function(toc = FALSE,
           toc_depth = 3,
           fig_width = 5,
           fig_height = 4,
           fig_caption = FALSE,
           highlight = "default",
           reference_docx = "default",
           keep_md = FALSE,
           md_extensions = NULL,
           pandoc_args = NULL)
  {
    knitr <- rmarkdown::knitr_options(opts_chunk = list(
      dev = "png",
      dpi = 96, # change to 150?
      fig.width = fig_width,
      fig.height = fig_height
    ))
    args <- c()
    if (rmarkdown::pandoc_available("1.14"))
      args <- c(args, rmarkdown::pandoc_toc_args(toc, toc_depth))
    else
      warning("table of contents for word_document requires pandoc >= 1.14")
    if (!is.null(highlight))
      highlight <- match.arg(highlight, highlighters())
    args <- c(args, rmarkdown::pandoc_highlight_args(highlight))
    if (!is.null(reference_docx) && !identical(reference_docx,
                                               "default")) {
      args <- c(args,
                "--reference-docx",
                rmarkdown::pandoc_path_arg(reference_docx))
    }
    args <- c(args, pandoc_args)
    output_format(
      knitr = knitr,
      pandoc = pandoc_options(
        to = "docx",
        from = from_rmarkdown(fig_caption, md_extensions),
        args = args
      ),
      keep_md = keep_md
    )
  }




function(number_sections = FALSE,
         fig_width = 7,
         fig_height = 5,
         fig_retina = if (!fig_caption)
           2,
         fig_caption = FALSE,
         dev = 'png',
         smart = TRUE,
         self_contained = TRUE,
         highlight = "default",
         mathjax = "default",
         extra_dependencies = NULL,
         css = NULL,
         includes = NULL,
         keep_md = FALSE,
         lib_dir = NULL,
         md_extensions = NULL,
         pandoc_args = NULL,
         ...) {
  toc <- FALSE
  toc_depth <- 3
  theme <- NULL
  template <- "default"
  code_folding <- "none"

  dep <- htmltools::htmlDependency(
    "minimal",
    "0.0.1",
    system.file("rmarkdown", "templates", "proposal", "resources", package = "academicRmarkdownTemplates"),
    stylesheet = "CUSTOMIZE_ME.css"
  )

  extra_dependencies <- append(extra_dependencies, list(dep))


  args <- c("--standalone")
  args <- c(args, "--section-divs")
  args <- c(args, rmarkdown::pandoc_toc_args(toc, toc_depth))

  args <- c(args,
            "--template",
            rmarkdown::pandoc_path_arg(
              system.file("rmarkdown", "templates", "base.html",
                          package = "academicRmarkdownTemplates")
            ))

  if (number_sections)
    args <- c(args, "--number-sections")

  for (css_file in css)
    args <- c(args, "--css", rmarkdown::pandoc_path_arg(css_file))

  pre_processor <- function(metadata,
                            input_file,
                            runtime,
                            knit_meta,
                            files_dir,
                            output_dir) {
    if (is.null(lib_dir))
      lib_dir <- files_dir

    args <- c()
    args <- c(
      args,
      pandoc_html_highlight_args(highlight,
                                 template, self_contained, lib_dir,
                                 output_dir)
    )
    args <- c(
      args,
      includes_to_pandoc_args(
        includes = includes,
        filter = if (identical(runtime, "shiny"))
          normalize_path
        else
          identity
      )
    )
    args

  }

  output_format(
    knitr = rmarkdown::knitr_options_html(fig_width, fig_height,
                                          fig_retina, keep_md, dev),
    pandoc = rmarkdown::pandoc_options(
      to = "html",
      from = from_rmarkdown(fig_caption,
                            md_extensions),
      args = args
    ),
    keep_md = keep_md,
    clean_supporting = self_contained,
    pre_processor = pre_processor,
    base_format = rmarkdown::html_document_base(
      smart = smart,
      theme = theme,
      self_contained = self_contained,
      lib_dir = lib_dir,
      mathjax = mathjax,
      template = template,
      pandoc_args = pandoc_args,
      extra_dependencies = extra_dependencies,
      ...
    )
  )
}
