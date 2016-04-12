df <- data.frame(task = c("task1", "task2", "task3"),
                 status = c("done", "active", "crit"),
                 pos = c("first_1", "first_2", "first_3"),
                 start = c("2016-06-01", "2016-12-01", "2017-12-01"),
                 end = c("2016-08-01", "2017-06-01", "2018-06-01"))

df <- read.csv("test_tasks.csv")

library(DiagrammeR)  
library(tidyr)
library(dplyr)

m <- mermaid(
  paste0(
    # mermaid "header", each component separated with "\n" (line break)
    "gantt", "\n", 
    "dateFormat  YYYY-MM-DD", "\n", 
    "title A Very Nice Gantt Diagram", "\n",
    # unite the first two columns (task & status) and separate them with ":"
    # then, unite the other columns and separate them with ","
    # this will create the required mermaid "body"
    paste(df %>%
            unite(i, task, status, sep = ":") %>%
            unite(j, i, pos, start, end, sep = ",") %>%
            .$j, 
          collapse = "\n"
    ), "\n"
  )
)

m$x$config = list(ganttConfig = list(
    axisFormatter = list(list(
      "%b, %Y" 
      ,htmlwidgets::JS(
        'function(d){ return d.getDay() == 1 }' 
      )
    ))
  ))

m
