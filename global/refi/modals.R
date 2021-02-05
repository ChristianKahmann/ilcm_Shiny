#' dialog annotation scheme export
#' @param failed
#' 
#' @return 
#' @export
#' @example 
dialog_annotation_scheme_export <- function(failed= FALSE){
  modalDialog(
    textInput(
      inputId = "modal_export_annotation_scheme_project_name"
      , label = "Projectname"
      , value = "REFI-QDA-Codebook-Exchange"
    )
    , footer = tagList(
      modalButton(label = "Cancel")
      , withBusyIndicatorUI(
        actionButton(
          inputId = "export_annotation_scheme_modal_ok"
          , label = "Prepare Export"
        )
      )
    )
    , size = "m"
  )
}

#' dialog collection export
#' @param failed
#' 
#' @return 
#' @export
#' @example 
dialog_collection_export <- function(failed= FALSE){
  modalDialog(
    textInput(
      inputId = "modal_export_collection_project_name"
      , label = "Projectname"
      , value = "REFI-QDA-Project-Exchange"
    )
    , footer = tagList(
      modalButton(label = "Cancel")
      ,  withBusyIndicatorUI(
        actionButton(
          inputId = "export_collection_modal_ok"
          , label = "Prepare Export"
        )
      )
    )
    , size = "m"
  )
}

#' dialog_collection with annotation scheme export
#' @param failed
#' 
#' @return 
#' @example 
#' @export
dialog_collection_with_annotation_scheme_export <- function(failed= FALSE){
  modalDialog(
    textInput(
      inputId = "modal_export_collection_with_annotation_scheme_project_name"
      , label = "Projectname"
      , value = "REFI-QDA-Project-Exchange"
    )
    , footer = tagList(
      modalButton(label = "Cancel")
      ,withBusyIndicatorUI( 
        actionButton(
          inputId = "export_collection_with_annotation_scheme_modal_ok"
          , label = "Prepare Export"
        )
      )
    )
    , size = "m"
  )
}

#' dialog topic model export
#' @param failed
#' 
#' @return 
#' @export
#' @example 
dialog_topic_model_export <- function(failed= FALSE){
  modalDialog(
    textInput(
      inputId = "modal_export_topic_model_project_name"
      , label = "Projectname"
      , value = "REFI-QDA-Project-Exchange"
    )
    , footer = tagList(
      modalButton(label = "Cancel")
      , withBusyIndicatorUI(
        actionButton(
          inputId = "export_topic_model_modal_ok"
          , label = "Prepare Export"
        )
      )
    )
    , size = "m"
  )
}

#' dialof classification export
#' @param failed 
#' 
#' @return 
#' @export
#' @example 
dialog_classification_export <- function(failed= FALSE){
  modalDialog(
    textInput(
      inputId = "modal_export_classification_project_name"
      , label = "Projectname"
      , value = "REFI-QDA-Project-Exchange"
    )
    , footer = tagList(
      modalButton(label = "Cancel")
      , withBusyIndicatorUI(
        actionButton(
          inputId = "export_classification_modal_ok"
          , label = "Prepare Export"
        )
      )
    )
    , size = "m"
  )
}
#paste0("REFI-QDA-Project-Exchange_", Sys.Date(), "_", format(Sys.time(), "%H%M%S"))