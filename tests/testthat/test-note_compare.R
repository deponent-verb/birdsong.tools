test_that("note_compare works",{
  
  note = tibble::tibble(start = 0.35, end = 0.49, sound.files = "JS001.wav", pos = 2)
  
  unit_table = tibble::tibble(start = c(0.30, 0.55, 1.5), end = c(0.51, 0.7, 2.2), 
                      sound.files = "JS001.wav", pos =c(2,3,4))
  
  note_compare(note, unit_table)
  
})