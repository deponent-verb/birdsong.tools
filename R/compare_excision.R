#' compare_excision function
#' 
#' Takes two unit table of notes and compares the note excisions. 
#' Table1 corresponds to manually excised notes, Table2 corresponds
#' to computationally excised notes. The function goes through each 
#' note in Table1, tries to match it to a note in Table2 and 
#' quantifies the difference. The process is repeated to compare
#' notes in Table2 to those in Table1. 
#'
#' @param table1: A unit table tibble containing the original sound file, 
#' start/end times of the note,the note position and note_label.  
#' @param table2: A unit table tibble containing the original sound file, 
#' start/end times of the note and the note position and note_label.
#'
#' @return Tibble containing the sound file, note positions in tables 1 and 2,
#' the square difference in the note intervals, a logical (matched) indicating
#' whether a matching note was found and the class of the original note. 
#' @export 
#' 
#' @importFrom plyr rbind.fill
#'
#' @examples table1 = tibble(start = c(0.35, 0.55), end = c(0.49, 0.7), sound.files = "JS001.wav")
#' table2 = tibble(start = c(0.37, 0.6), end = c(0.45, 0.7), sound.files = "JS001.wav")
#' compare_excision(table1,table2)
compare_excision <- function(table1,table2){
  
  #match notes from table1 to table2 and compute difference ----
  
  #get a list of all notes in table1
  note_list1 = split(table1, row(table1[,1]))
  
  #attempt match each note to something in table2 and compute difference
  diffs1 = lapply(note_list1, note_compare, unit_table = table2) 
  
  #match notes from table2 to table1 and compute difference ----
  
  note_list2 = split(table2, row(table2[,2]))
  
  #attempt match each note to something in table2 and compute difference
  diffs2 = lapply(note_list2, note_compare, unit_table = table1) 
  
  #bind everything together into one data frame
  result = plyr::rbind.fill( c(diffs1,diffs2) )
  
  return(result)
}

