#' compute_oscores_dep2 function
#' 
#' Dependency function for compute_oscores. Compares the overlap scores for table1
#' and table2, using table1 as a reference. 
#' 
#'
#' @param table1: A unit table tibble containing the original sound file, 
#' start/end times of the note,the note position and note_label. 
#' @param table2: A unit table tibble containing the original sound file, 
#' start/end times of the note,the note position and note_label.  
#'
#' @return A numeric vector with the sum of each of the 3 overlaps scores.
#' @export
#'
#' @examples table1 = tibble::tibble(start = c(0.30, 0.55, 1.5, 2.5), end = c(0.51, 0.7, 2.2, 3), 
#' sound.files = "JS001.wav", pos =c(1,2,3,4), note_label = "Curve")
#' table2 = tibble::tibble(start = c(0.4, 0.6, 1.5, 2.7), end = c(0.51, 0.7, 2.2, 3.2), 
#' sound.files = "JS001.wav", pos =c(1,2,3,4), note_label = "Curve")
#' compute_oscores_dep2(table1,table2)
compute_oscores_dep2 <- function(table1, table2){
  notes = split(table2, row(table2[,1]))
  notelist = lapply(notes, function(table_row){
    note = c(table_row$start, table_row$end)
  })
  scores = lapply(notelist, function(note){compute_oscores_dep(note,table = table1)})
  #bind everything in a dataframe
  score_table = t(as.data.frame(scores)) %>%
    as.data.frame()
  #rename columns labels for ease of debugging
  colnames(score_table) = c("sc1","sc2","sc3")
  #add scores for all the notes
  score_table = score_table %>%
   dplyr::mutate(stats = sc1 + sc2 + sc3)
  
  return(score_table$stats)
}
