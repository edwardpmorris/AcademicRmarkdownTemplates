#' Parse an Asana task list
#'
#' Export an Asana task list and convert it to a dataframe ready for making a
#' Gantt diagram using DiagrammeR. As Asana does not have a 'Start Date' field
#' Tags are used to indicate periods of time. For example 'start: 0' 'end: 6'
#' represents a task that starts at month 0 and ends month 6. Tags are also
#' used to indicate 'Milestone's (important events) and 'Deliverable's (outputs
#' to be delivered by project). Sections are also selected...
#'
#' @param file_path File exported as CSV or JSON (not implemented) from Asana.
#' @param project_start ISO 8601 date of project start.
#'
#' @return A dataframe suitable for producing a Gantt diagram.
#' @export
#'
#' @examples
#' # file_path <- "asana_export1.csv"
#' # project_start <- "2016-06-01"
#' # df <- parseAsana(file_path,project_start)
parseAsana <- function(file_path, project_start, format = "csv") {
  switch (format,
          csv = out <- read.csv(file_path, as.is = T),
          JSON = return(print("Not implemented yet")))
  # extract names, tags and due dates
  tags <- out$Tags
  dd <- out$Due.Date
  tags <- lapply(tags, function(x) {
    yaml::yaml.load(gsub(",", "\n", x))
  })
  nms <- out$Name
  wp <- out$Projects
  # convert tags
  convertTags <- function(tag) {
    df <- data.frame(start = "",
                     end = "",
                     type = "")
    nms <- names(tag)
    if (any(grepl("T", nms))) {
      df$type <- "Task"
    }
    if (any(grepl("M", nms))) {
      df$type <- paste(df$type, "Milestone")
    }
    if (any(grepl("D", nms))) {
      df$type <- paste(df$type, "Deliverable")
    }
    if (any(grep("Start", nms))) {
      df$start <- as.character(tag[['Start']])
    }
    if (any(grepl("End", nms))) {
      df$end <- as.character(tag[['End']])
    }
    df$type <- sub(" +$", "", df$type)
    if (df$type == "") {
      df$type <- "Subtask"
    }
    return(df)
  }
  df <- do.call(rbind, lapply(tags, convertTags))
  # convert to real dates
  calcDate <- function(month, project_start) {
    if (is.na(month)) {
      out <- ""
    } else{
      out <-
        as.character(format(as.POSIXct(project_start) + (month * 60 * 60 * 24 * 30), "%Y-%m-%d"))
    }
    return(out)
  }
  df$start <-
    mapply(calcDate,
           as.numeric(df$start),
           MoreArgs = list(project_start = project_start))
  df$end <-
    mapply(calcDate,
           as.numeric(df$end),
           MoreArgs = list(project_start = project_start))
  # add date to Milestones
  df[grep("Milestone", df$type), 'end'] <- dd[grep("Milestone", df$type)]
  df[grep("Milestone", df$type), 'start'] <- as.character(as.POSIXct(dd[grep("Milestone", df$type)]) - (1*60*60*24))
  # prepare output df
  df$pos <- paste0("P", rownames(df))
  df$task <- gsub(":", "", out$Name)
  df$section <- wp
  return(df)
}
