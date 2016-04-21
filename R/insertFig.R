#' Create a Rmarkdown figure entry
#'
#' @param label The label of the figure  
#' @param figure_list  A YAML list of figures
#'
#' @return Rmarkdown defining a kfigr chunk and a figure with details selected 
#' the YAML figure_list
#' @export
#'
#' @examples
#' #figure_list <- "config/figure_list.yaml"
#' #label <- "test_figure"
#' #insertFig(label, figure_list)
insertFig <- function(label, figure_list){
  fl <- yaml::yaml.load_file(figure_list)
  names(fl) <- sapply(fl, FUN=function(x) x$label)
  caption <- fl[[label]][['caption']]
  path <- fl[[label]][['path']]
  return(
    paste0(
      "```{r ", label, ", anchor='Figure', include=FALSE}\n```",
      "![`r figr('", label, "', T)`. ",
      caption, "](figures/", path, ")"
    )
  )
}