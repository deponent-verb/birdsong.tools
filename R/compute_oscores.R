#' compute_oscores function
#' 
#' Compares a computational unit table against a manual unit table using the 
#' overlap scores defined in overlap_score. Function attempts to match 
#' each computational note to a note in the manual table with the most 
#' overlap.
#' 
#'
#' @param manual_table: A unit table tibble containing the original sound file, 
#' start/end times of the note,the note position and note_label.
#' @param comp_table: A unit table tibble containing the original sound file, 
#' start/end times of the note,the note position and note_label. 
#'
#' @return A numeric vector containing the totals of the 3 overlaps scores. 
#' @export
#'
#' @examples table1 = tibble::tibble(start = c(0.30, 0.55, 1.5, 2.5), end = c(0.51, 0.7, 2.2, 3), 
#' sound.files = c(rep("JS001.wav",2),rep("JS002.wav",2)) , pos =c(1,2,3,4), note_label = "Curve")
#' table2 = tibble::tibble(start = c(0.25, 0.65, 1.6, 2.4), end = c(0.45, 0.75, 2.1, 2.9), 
#' sound.files = c(rep("JS001.wav",2),rep("JS002.wav",2)), pos =c(1,2,3,4), note_label = "Unclassifed")
#' compute_oscores(table1,table2)
compute_oscores = function(manual_table, comp_table){
  
  #split tables by recording
  truth_tables = partition_unit_table(manual_table)
  comp_tables = partition_unit_table(comp_table)
  
  #compute scores for each table pair
  #lapply(table1 = truth_tables, table2 = comp_tables, FUN= compute_oscores_dep2)
  
  table_scores = purrr::map2(.x = truth_tables, .y = comp_tables, .f = compute_oscores_dep2)
                                      
  
  
  
  #get a list of all notes comp_table
  #comp_notelist = split(comp_table, row(comp_table[,1]))
  
  # function(note){
  #   recording = note$sound.files
  #   #get smaller table with notes from the same recording
  #   song_table = comp_table %>%
  #     dplyr::filter(sound.files == recording)
  #   #compute overlap between computational note and notes in the manual table
  #   song_table = song_table %>% 
  #     dplyr::mutate(olap = check_overlap( c(note$start,note$end) , c(start,end)))
  # }
  
}

# note1 = c(0.1,0.7)
# note2 = c(0.5,0.9)

#case of 1 overlap

#case of multiple overlaps, pick biggest one

# rng = cbind(pmin(ranges[,1], ranges[,2]), pmax(ranges[,1], ranges[,2]),
#             pmin(ranges[,3], ranges[,4]), pmax(ranges[,3], ranges[,4]))
# 
# olap = (rng[,1] <= rng[,4]) & (rng[,2] >= rng[,3])
# 
# (pmin(ranges[,1], ranges[,2]) <= pmax(ranges[,3], ranges[,4])) &
#   (pmax(ranges[,1], ranges[,2]) >= pmin(ranges[,3], ranges[,4]))