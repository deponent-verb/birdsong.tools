#' compute_oscores_dep2 function
#' 
#' Dependency function for compute_oscores. Compares the overlap scores for table1
#' and table2, using table as a reference. 
#' 
#'
#' @param table1 
#' @param table2 
#'
#' @return
#' @export
#'
#' @examples
compute_oscores_dep2 <- function(table1, table2){
  notes = split(table2, row(table2[,1]))
  notelist = lapply(notes, function(table_row){
    note = c(table_row$start, table_row$end)
  })
  scores = lapply(notelist, function(note){compute_oscores_dep(note,table = table1)})
}


# function(table1, table2){
#   notes = split(table2, row(table2[,1]))
#   notelist = lapply(notes, function(table_row){
#     note = c(table_row$start, table_row$end)
#   })
#   scores = lapply(notelist, function(note){compute_oscores_dep(note,table = table1)})
# }