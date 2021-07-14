test_that("compute_oscores_dep2 works",{
  table1 = tibble::tibble(start = c(0.30, 0.55, 1.5, 2.5), end = c(0.51, 0.7, 2.2, 3), 
                          sound.files = "JS001.wav" , pos =c(1,2,3,4), note_label = "Curve")
  table2 = tibble::tibble(start = c(0.25, 0.65, 1.6, 2.4), end = c(0.45, 0.75, 2.1, 2.9), 
                          sound.files = "JS001.wav", pos =c(1,2,3,4), note_label = "Unclassifed")
  compute_oscores_dep2(table1 = table1, table2 = table2)
})