#' note_compare function
#' 
#' Takes a note from one unit table and compares it with the corresponding note
#' found in another unit table. 
#'
#' @param note : A unit table tibble containing the original sound file, 
#' start/end times of the note and the note position. 
#' @param unit_table : A tibble of notes ontaining the original sound file, 
#' start/end times of the note and the note position. 
#'
#' @return A tibble with the sound file, the positions of the notes in the 
#' 2 original unit tables, the square difference in the intervals and a 
#' logical indicating whether a note match was found. The difference is 
#' NA if no matching note was found. 
#' 
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate
#' @importFrom data.table between
#' @export
#'
#' @examples note = tibble(start = 0.35, end = 0.49, sound.files = "JS001.wav", pos = 2)
#' unit_table = tibble(start = c(0.35, 0.55), end = c(0.49, 0.7), sound.files = "JS001.wav", pos =c(2,3))
#' note_compare(note, unit_table)
note_compare <- function(note, unit_table){
  
  recording = note$sound.files
  
  #take all the notes from the same recording
  candidate_notes = unit_table %>%
    dplyr::filter(sound.files == recording)
  
  #find the corresponding note
  candidate_notes = candidate_notes %>%
    dplyr::mutate(match = data.table::between(note$midpoint,
                                       candidate_notes$start,
                                       candidate_notes$end)
    )
  
  #compute difference between becky's note and corresponding matched note
  if(any(candidate_notes$match)){
    match = T
    matched_note = candidate_notes[which(candidate_notes$match),]
    diff = (becky_note$start - matched_note$start)^2 +
      (becky_note$end - matched_note$end)^2
  } else {
    diff = NA
    match = F
  }
  
  result = tibble::tibble(
    sound_file = recording, 
    becky_note_pos = becky_note$selec,
    anthony_note_pos = 1 , #change later
    difference = diff,
    matched = match
  )
  
  return(result)
}