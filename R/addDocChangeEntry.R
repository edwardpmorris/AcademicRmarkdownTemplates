#' Add entry to a YAML document change list
#'
#' Versioning follows this specification http://programmers.stackexchange.com/questions/141973/how-do-you-achieve-a-numeric-versioning-scheme-with-git
# x-y-<number of commits>-r<git-hash>
# x and y are manually set
# number of commits and r git hash are auto derived via git

#'
#' @param x-y The major version
#' @param authors The authors that made changes
#' @param modifications Comment on version/modifications
#' @param status version statius, i.e., Draft, Private, Public
#' @param doc_change_yaml The yaml file with document change metadata
#' @param simple Use simple version number 
#'
#' @return If the last entry of the document change metadata is different than 
#' the present version the YAML metadat is updated with a new entry.
#' @export
#'
addDocChangeEntry <- function(x_y="0-0", authors="", modification="",
                              status="", doc_change_yaml="config/document_change_record.yaml",
                              simple=T){
  ver <- addVersion(x_y=x_y, simple=T)
  l <- yaml::yaml.load_file(doc_change_yaml)
  df <- as.data.frame(matrix(unlist(l),nrow = 5), stringsAsFactors = F)
  vers <- df[3, ncol(df)]
  if (ver != vers){
    l[[length(l)+1]] <- list(Authors = authors,
                             Modification = modification,
                             Version= ver,
                             Date= format(Sys.Date(), "%Y-%m-%d"),
                             Status= status)
    writeLines(yaml::as.yaml(l), con = doc_change_yaml)
    print("Document change metadata updated")
  } else {
    print("No changes in version, document change metadata not updated")
  }
}