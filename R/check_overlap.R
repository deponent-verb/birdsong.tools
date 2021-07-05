#' check_overlap function
#' 
#' Takes 2 intervals and computes how much they overlap. 
#'
#' @param note1 : A 2-dim numeric vector. 
#' @param note2 : A 2-dim numeric vector. 
#'
#' @return A numeric scalar value indicating the amount of overlap. If
#' the intervals do not overlap, the value is 0. 
#' @export
#'
#' @examples
check_overlap = function(note1, note2){
  
  #check inputs
  if(dim(note1) != 2 || is.numeric(note1) != F ){
    stop("note1 must be a 2-dim numeric vector")
  }
  
  if(dim(note2) != 2 || is.numeric(note2) != F ){
    stop("note2 must be a 2-dim numeric vector")
  }
  
  #check interval vector is sorted
  
  #function starts here
  left_olap = (note1[1] <= note2[2]) & (note1[2] >= note2[1])
  right_olap = (note2[1] <= note1[2]) & (note2[2] >= note1[1])
  
  print(left_olap)
  print(right_olap)
  
  if(left_olap){
    res = note2[1] - note1[2]
  } else if (right_olap) {
    res = note2[2] - note1[1] 
  } else {
    res = 1
  }
  
  return(res)
}

# 
# rng = cbind(pmin(ranges[,1], ranges[,2]), pmax(ranges[,1], ranges[,2]),
#             pmin(ranges[,3], ranges[,4]), pmax(ranges[,3], ranges[,4]))
# 
# olap = (rng[,1] <= rng[,4]) & (rng[,2] >= rng[,3])
# 
# (pmin(ranges[,1], ranges[,2]) <= pmax(ranges[,3], ranges[,4])) &
#   (pmax(ranges[,1], ranges[,2]) >= pmin(ranges[,3], ranges[,4]))