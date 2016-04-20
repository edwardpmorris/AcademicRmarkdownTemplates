#' Create version string from Git
#'
#' Versioning follows this specification http://programmers.stackexchange.com/questions/141973/how-do-you-achieve-a-numeric-versioning-scheme-with-git
# x.y.<number of commits>.r<git-hash>
# x and y are manually set
# number of commits and r git hash are auto derived via git

#'
#' @param x.y The major version
#'
#' @return A string representing the unique version string
#' @export
#'
#' @examples
#' In a directory with Git version control
#' addVersion()
addVersion <- function(x.y="0.0", simple=T){
  nc <- system("git rev-list HEAD", intern=T)
  nc <- length(nc)
  gh <- system("git rev-parse HEAD", intern=T)
  if (simple==F){
    return(paste0(x.y,".",nc))
  }else{
    return(paste0(x.y,".",nc,".r",gh))  
  }
}