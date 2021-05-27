#' add_transitions function
#' 
#' Takes in a unit table of notes and adds a new column
#' indicating the next adjacent note class transition. 
#' For example, if note A is immediately followed by note B, 
#' note A is recorded as having a A-B transition. The notes
#' from each recording are treated separately. 
#'
#' @param unit_table: A unit table tibble containing the original sound file, 
#' start/end times of the note,the note position and note_label. 
#'
#' @return
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' 
#' @export
#'
#' @examples unit_table = tibble(start = c(0.37, 0.6, 0.2, 1.8), end = c(0.45, 0.7, 0.4, 2), pos = c(1,2,1,2),sound.files = c("JS001.wav","JS001.wav","JS002.wav","JS002.wav") , note_label = "Curve")
#' add_transitions(unit_table)
add_transitions <- function(unit_table){
  
  #check inputs
  
  #check all the correct columns are present
  correct_cols = c("start", "end", "sound.files","note_label", "pos")
  if(sum(colnames(unit_table) %in% correct_cols ) != length(correct_cols) ){
    stop("note argument does not have the correct columns. See documentation.")
  }
  
  #function starts here
  
  #obtain list of songs
  song_list = unit_table$sound.files %>% unique()
  
  #make a tibble for each song
  song_tables = lapply(song_list, function(song){
    unit_table %>%
      dplyr::filter(sound.files == song) %>%
      #ensure rows are in ascending order by pos
      dplyr::arrange(pos)
  })
  
  test %>% 
    dplyr::mutate(transition = 1)
  
  transitions = rep(NA, nrow(test))
  
  for(i in 1:nrow(test)){
    if(i == nrow(test)){
      transitions[i] = "end"
    } else {
      transitions[i] = paste(test$note_label[i], test$note_label[i + 1])
    }
  }
  
  
  
  return(0)
}