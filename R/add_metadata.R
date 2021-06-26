#' add_metadata function
#' 
#' Add the metadata to a dataframe based on a common ID column. 
#'
#' @param data : A dataframe with ID and some other columns 
#' @param metadata: A metadata table with ID
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows
#'
#' @return The original dataframe with the additional columns from the 
#' metadata.
#' @export
#'
#' @examples data = tibble::tibble(x = c(1,2), ID = c("JS001", "JS002"))
#' metadata = tibble::tibble(y = c(3,4), ID = c("JS001", "JS002"))
add_metadata <- function(data, metadata){
  
  #check ID columns match
  check =  all(is.element(data$ID, metadata$ID))
  if(check == FALSE){
    stop("Mismatching IDs.")
  }
  
  #take variables from the metadata table based on ID
  meta_rows = lapply(as.list(data$ID), function(ID){
    row_index = which(metadata$ID == ID)
    meta_info = metadata[row_index,]
    meta_info$ID = NULL
    return(meta_info)
    #return(metadata)
  })
  
  #bind the new metadata columns to the main dataframe
  res = cbind(data, dplyr::bind_rows(meta_rows))
  
  return(res)
}
