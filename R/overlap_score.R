#' overlap_score function
#' 
#' Computes the overlap scores between 2 notes. The scores are defined as follows.
#' \eqn{score1 = (y \cap z) / y},
#' \eqn{score2 = (y \cap z) / (y \cup z)},
#' \eqn{score3 = |(y \cap z)|^2 / |y| \times |z|}
#'
#' @param note1 : A 2-dim numeric vector. 
#' @param note2 : A 2-dim numeric vector. 
#'
#' @return A numeric vector for scores1,2,3. 
#' @export
#'
#' @examples note1 = c(1,5), note2 = c(3,4)
#' overlap_score(note1,note2)
overlap_score <- function(note1,note2){
  check_overlap()
}