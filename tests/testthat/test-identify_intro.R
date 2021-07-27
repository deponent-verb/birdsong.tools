test_that("identify_intro works",{
  unit_table1 = tibble::tibble(start = c(0.2, 0.6, 0.75, 1.1, 1.8, 2.5, 4),
                              end = c(0.25, 0.7, 0.9, 1.6, 1.9, 3, 4.2), 
                              pos = seq(7),
                              sound.files = "JS001.wav"
                              )
  
  unit_table2 = tibble::tibble(start = c(0.21, 0.6, 0.75, 1.1, 1.8, 2, 3.1,4.3),
                              end = c(0.25, 0.7, 0.9, 1.7, 1.9, 3, 4.2,4.8),
                              pos = seq(8),sound.files = "JS002.wav"
                              )
  
  unit_table = rbind(unit_table1,unit_table2)
  output = identify_intro(unit_table)
  
  table_list = list(unit_table1, unit_table2)
  intro = lapply(table_list, identify_intro_dep) %>% unlist()
  ans = unit_table %>% dplyr::mutate(intro = intro)
  
  expect_equal(output,as.data.frame(ans))
})