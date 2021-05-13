test_that("compare_excision works",{
  expect_equal(1,1)
  
})

becky = read_csv("~/work/PhD/Data/Files for Anthony/Files for Anthony/JavaSparrow_UnitTable.csv")

anthony = read_csv("~/work/PhD/Data/note_excision/datasets/measurements.csv")

#data cleaning

anthony = anthony %>%
  dplyr::rename(sound.files = filename,
                start = interval_start, 
                end = interval_end)

library(tidyverse)

#data manipulation
becky = becky %>%
  mutate(midpoint = (start + end)*0.5 )

#loop through individual notes
becky_note = becky[1,]

recording = becky_note$sound.files

#take all the notes from the same recording
candidate_notes = anthony %>%
  dplyr::filter(sound.files == recording)

#find the corresponding note
candidate_notes = candidate_notes %>%
  mutate(match = data.table::between(becky_note$midpoint,
                                     candidate_notes$interval_start,
                                     candidate_notes$interval_end)
         )

#compute difference between becky's note and corresponding matched note
if(any(candidate_notes$match)){
  match = T
  matched_note = candidate_notes[which(candidate_notes$match),]
  diff = (becky_note$start - matched_note$interval_start)^2 +
    (becky_note$end - matched_note$interval_end)^2
} else {
  diff = NA
  match = F
}

tibble::tibble(
  sound_file = recording, 
  becky_note_pos = becky_note$selec,
  anthony_note_pos = 1 , #change later
  difference = diff,
  matched = match
)
