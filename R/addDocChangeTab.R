yaml_path="document_change_record.yaml"
#' Add a document change record table
#'
#' Convert a YAML document listing 'document changes' into a markdown table
#'ready for inserting in a document.
#'
#' @param yaml_path Path to YAML file 
#' @param table_out If option 'kable' given output is a markdown table, otherwise a dataframe
#'
#' @return If option 'kable' given output is a markdown table, otherwise a dataframe
#' @export
#'
#' @examples
#' #yaml_path="document_change_record.yaml"
#' #addDocChangeTab(yaml_path, table_out="kable")
addDocChangeTab <- function(yaml_path, table_out="kable"){
  l <- yaml::yaml.load_file(yaml_path)
  nms <- names(l[[1]])
  df <- data.frame(t(matrix(unlist(l), nrow=5, byrow=F)),stringsAsFactors=FALSE)
  names(df) <- nms
  if (table_out=="kable"){
    df <- knitr::kable(df)
  } 
  return(df)
}