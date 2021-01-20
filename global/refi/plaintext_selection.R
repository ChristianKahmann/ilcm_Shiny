#' new plaintext selection
#' @param  guid
#' @param start_position
#' @param end_position
#' @param name
#' @param creating_user
#' @param creation_datetime
#' @param description
#' @param code_ref
#'  
#' @return datframe with parameters (like above)
T_new_plaintext_selection <- function(
  guid = guid
  , start_position = start_position
  , end_position = end_position
  , name = NULL
  , creating_user = NULL
  , creation_datetime = NULL
  , description = NULL
  , code_ref = NULL
){
  df <- data.frame(
    guid = character(0)
    , start_position = character(0)
    , end_position = character(0)
    , name = character(0)
    , creating_user = character(0)
    , creation_datetime = character(0)
    , description = character(0)
    , code_ref = character(0)
  )
  return(df)
}