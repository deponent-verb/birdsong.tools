#' overlap_scores function
#' 
#' Compares two unit tables by computing the overlap scores for each pair
#' of matched notes. Notes are considered matching if their intervals overlap. 
#' 3 scores are computed for each note pair. For any matched pair, denote
#' the intervals as the sets y and z. The scores are defined as follows.
#' \eqn{score1 = (y \cap z) / y},
#' \eqn{score2 = (y \cap z) / (y \cup z)},
#' \eqn{score3 = |(y \cap z)|^2 / |y| \times |z|}
#'
#' @param table1 
#' @param table2 
#'
#' @return
#' @export
#'
#' @examples asdad
overlap_scores = function(table1, table2){
  
}

note1 = c(0.1,0.7)
note2 = c(0.5,0.9)

#case of 1 overlap

#case of multiple overlaps