#' identify_intro_dep
#' 
#' Dependency for identify_intro. Takes a unit table and assigns notes
#' to the introduction. The notes from the input are all from the same
#' recording. 
#'
#' @param unit_table: A unit table tibble containing the original sound file, 
#' start/end times of the note,the note position and note_label. Notes are 
#' all from the same recording.  
#'
#' @return A logical vector indicating
#' whether each note has been assigned to the introduction. 
#' @export
#'
#' @examples unit_table = tibble::tibble(start = c(0.2, 0.6, 0.75, 1.1, 1.8, 2.5, 4),
#'end = c(0.25, 0.7, 0.9, 1.6, 1.9, 3, 4.2),pos = seq(7),sound.files = "JS001.wav")
#'identify_intro_dep(unit_table)
#'
identify_intro_dep <- function(unit_table){
  
  #compute the gaps between all the notes
  gaps = get_gaps(unit_table)
  #identify notes which are <0.25s close
  close_notes = gaps$gap_dur < 0.25
  #find the first note of intro
  start_index = which(close_notes)[1]
  
  #find run length of intro
  x = rle(close_notes)
  #find the run length of the first T
  run_len = x$lengths[which(x$values)[1]]
  
  if(run_len > 5){
    intro_len = 5
  } else {
    intro_len = run_len
  }
  
  end_index = start_index + intro_len - 1
  
  #initialise intro vec
  intro = rep(F,nrow(unit_table))
  intro[start_index:end_index] = T
  
  return(intro)
}