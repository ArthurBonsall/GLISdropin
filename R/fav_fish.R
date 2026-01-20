#' Print Name of Favourite Fish
#'
#' This function takes an MNR species code and outputs a test string identifying your favourite fish.
#'
#'
#' @export
fav_fish <- function(SPC_Code){

  #Check to ensure only one fish is specified
  if(length(SPC_Code) > 1){stop("TESTING MERGE CONFLICTS")}

  #Add a leading zero if required
  if(nchar(SPC_Code) == 2){SPC_Code = paste0("0", SPC_Code)}

  #Return the species record from GLIS
  SPC_Record <- suppressMessages(glfishr::get_species(list(spc = SPC_Code)))

  #Check to see if a species record was returned
  if(is.null(nrow(SPC_Record)) == TRUE){stop("The species code entered does not exist.")}else{
    paste0("My favourite fish is the ", SPC_Record$SPC_NMCO, "!")
  }
}
