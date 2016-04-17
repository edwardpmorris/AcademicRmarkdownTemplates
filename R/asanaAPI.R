#' Interact with Asana API
#'
#' @param path Pathof API query
#' @name asanaAPI
NULL
#> NULL

#' @rdname asanaAPI
#' @export
asana_GET <- function(path, ..., pat = asana_pat()) {
  auth <- asana_auth(pat)
  req <- httr::GET("https://app.asana.com", path = paste0("api/1.0", path), auth, ...)
  asana_check(req)
  return(req)
}

#' @rdname asanaAPI
#' @export
asana_auth <- function(pat = asana_pat()) {
  httr::add_headers(Authorization = paste("Bearer", pat))
  #httr::authenticate(pat, "x-oauth-basic", "basic")
}

#' @rdname asanaAPI
#' @export
asana_check <- function(req) {
  if (req$status_code < 400) return(invisible())
  message <- asana_parse(req)$message
  stop("HTTP failure: ", req$status_code, "\n", message, call. = FALSE)
}

#' @rdname asanaAPI
#' @export
asana_parse <- function(req, simplifyVector = FALSE) {
  text <- httr::content(req, as = "text")
  if (identical(text, "")) stop("No output to parse", call. = FALSE)
  jsonlite::fromJSON(text, simplifyVector = simplifyVector)
}

#' @rdname asanaAPI
#' @export
asana_pat <- function() {
  Sys.getenv('ASANA_PAT')
}

#' @rdname asanaAPI
#' @export
has_pat <- function() !identical(asana_pat(), "")

#' @rdname asanaAPI
#' @export
asana_get_workspaces <- function(){
  path <- "/users/me"
  ap <- asana_GET(path)
  ap <- asana_parse(ap)
  ap <- as.data.frame(t(matrix(unlist(ap$data$workspaces),2,byrow = F)), stringsAsFactors=F)
  names(ap) <- c("id", "name")
  return(ap)
}

#' @rdname asanaAPI
#' @export
asana_get_projects <- function(workspace_name) {
  ws <- asana_get_workspaces()
  path <- paste0(
    "/workspaces/"
    ,
    ws[which(ws$name == workspace_name), 'id']
    ,
    "/projects?opt_expand=workspace&opt_fields=color,notes,name&limit=50"
  )
  ap <- asana_GET(path)
  ap <- asana_parse(ap)
  nms <- names(ap$data[[1]])
  ap <- as.data.frame(t(matrix(unlist(ap$data), length(nms), byrow = F)), stringsAsFactors =
                        F)
  names(ap) <- nms
  return(ap)
}

#' @rdname asanaAPI
#' @export
asana_get_sections <- function(project_name, workspace_name){
  ws <- get_projects(workspace_name)
  path <- paste0("/projects/",
  ws[which(ws$name == project_name), 'id'],
  "/sections?opt_fields=name,id,current_status,due_date,archived,color,notes&limit=50"
  )
  ap <- asana_GET(path)
  ap <- asana_parse(ap)
  nms <- names(ap$data[[1]])
  ap <- as.data.frame(t(matrix(unlist(ap$data),length(nms),byrow = F)), stringsAsFactors=F)
  names(ap) <- nms
  return(ap)
}

#' @rdname asanaAPI
#' @export
asana_get_task_info <- function(task_id){
  path <- paste0("/tasks/", task_id, "?opt_fields=assignee.name,name,tags.name,created_at,completed_at,modified_at,due_on,notes,projects.name,parent.name")
  #Task ID,Created At,Completed At,Last Modified,Name,Assignee,Due Date,Tags,Notes,Projects,Parent Task
  ap <- asana_GET(path)
  ap <- asana_parse(ap, simplifyVector=T)
  chkNull <- function(x){ifelse(is.null(x),"",x)}
  df <- data.frame(Task.ID=chkNull(ap$data$id)
                   ,Created.At=chkNull(ap$data$created_at)
                   ,Completed.At=chkNull(ap$data$completed_at)
                   ,Last.Modified=chkNull(ap$data$modified_at)
                   ,Name=chkNull(ap$data$name)
                   ,Assignee=chkNull(ap$data$assignee$name)
                   ,Due.Date=chkNull(ap$data$due_on)
                   ,Tags=chkNull(paste(ap$data$tags$name,collapse = ","))
                   ,Notes=chkNull(ap$data$notes)
                   ,Projects=chkNull(ap$data$projects$name)
                   ,Parent.Task=chkNull(ap$data$parent)
                   ,stringsAsFactors = F)
  return(df)
}

#' @rdname asanaAPI
#' @export
asana_get_tags <- function(task_id){
  path <- paste0("/tasks/", task_id, "/tags?limit=50")
  ap <- asana_GET(path)
  ap <- asana_parse(ap)
  nms <- names(ap$data[[1]])
  ap <- as.data.frame(t(matrix(unlist(ap$data),length(nms),byrow = F)), stringsAsFactors=F)
  names(ap) <- nms
  ap <- paste(ap$name,collapse = ",")
  return(ap)
}

#' @rdname asanaAPI
asana_get_tasks <- function(project_id){
  path <- paste0("/projects/", project_id,"/tasks?opt_fields=assignee.name,name,tags.name,created_at,completed_at,modified_at,due_on,notes,projects.name,parent.name")
  ap <- asana_GET(path)
  ap <- asana_parse(ap, simplifyVector=T)
  chkNull <- function(x){ifelse(is.null(x),"",x)}
  df <- data.frame(Task.ID=ap$data$id
                   ,Created.At=ap$data$created_at
                   ,Completed.At=ap$data$completed_at
                   ,Last.Modified=ap$data$modified_at
                   ,Name=ap$data$name
                   ,Assignee=ap$data$assignee$name
                   ,Due.Date=ap$data$due_on
                   ,Tags=sapply(ap$data$tags,
                                function(x) paste(x$name,collapse = ","))
                   ,Notes=ap$data$notes
                   ,Projects=sapply(ap$data$projects,
                                    function(x) paste(x$name,collapse = ","))
                   ,Parent.Task=ap$data$parent
                   ,stringsAsFactors = F)
  return(df)
}

#' @rdname asanaAPI
#' @export
asana_get_tasks_per_project <- function(workspace_name, file_path=NULL){
  ws <- get_projects(workspace_name)
  out <- lapply(ws$id, get_tasks)
  out <- do.call(rbind,out)
  if(is.null(file_path)){ return(out)
    }else{write.csv(out,file = file_path)}
}
