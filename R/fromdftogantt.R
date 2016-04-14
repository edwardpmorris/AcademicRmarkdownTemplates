#' Make a Gantt diagram from a dataframe
#'
#' @param df Dataframe
#' @param width Width of image in pixels
#' @param Title Title of  diagram
#' @param filename Optionally write out to a html file. Default is not.
#'
#' @return A htmlwigets object representing a Gantt diagram
#' @export
#'
#' @examples
#' \dontrun{
#' # file_path <- "asana_export1.csv"
#' # project_start <- "2016-06-01"
#' # df <- parseAsana(file_path,project_start)
#' # df <- df[df$type %in% c(" Milestone", "Task"),]
#' # df[grep("Milestone", df$type),'type'] <- "crit"
#' # df[grep("Subtask", df$type),'type'] <- "done"
#' # df[grep("Task", df$type),'type'] <- "active"
#' # fromdftogantt(df)
#' }
fromdftogantt<-function(df, width = 600, Title=" ", filename = NULL){
  txt<-paste("gantt","dateFormat  YYYY-MM-DD",paste("title",Title),"",sep="\n")
  for(i in unique(df$section)){
    txt<-paste(txt,paste("section",i),sep="\n")
    for(j in which(df$section==i)){

      txt<-paste(txt,paste0(df$task[j],":",df$type[j],",",
                            df$start[j],",",
                            df$end[j]),sep="\n")
    }
    txt<-paste0(txt,"\n")
  }
  m<-DiagrammeR::mermaid(txt, width = width)
  m$x$config = list(ganttConfig = list(
    axisFormatter = list(list(
      "%m-%Y"
      ,htmlwidgets::JS(
        'function(d){ return d.getDate() == 1 }'
      )
    ))))
  m$x$config = list(ganttConfig = list(
    titleTopMargin=25,
    barHeight=20,
    barGap=5,
    topPadding=50,
    sidePadding=100,
    gridLineStartPadding=35,
    fontSize=11,
    numberSectionStyles=2
    ))
  if(!is.null(filename)){
    htmltools::save_html(htmltools::as.tags(m),file=filename)
  } else{
    return(m)
  }
}
