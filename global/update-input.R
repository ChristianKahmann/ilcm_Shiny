#' Create a function that wraps a Shiny input function in code that adds information
#' about the tag type
#' @param inputType
#' 
#' @return 
#' @export
#' @example 
updateableInput <- function(inputType) {
  function(...) {
    shinyFuncName <- as.character(as.list(match.call()[1]))
    if(shinyFuncName == "prettyRadioButtons" || shinyFuncName == "materialSwitch"){
      shinyFunc <- get(shinyFuncName, envir = as.environment("package:shinyWidgets"))
    }
    else{
      shinyFunc <- get(shinyFuncName, envir = as.environment("package:shiny"))
    }
    shiny::tagAppendAttributes(
      shinyFunc(...),
      `data-input-type` = inputType
    )
  }
}

#' define what Shiny inputs you want to support
#' (the following three common input types are tested; the code here probably will
#' not work as-is for ALL inputs but you should be able to modify it slightly for
#' other inputs)
textInput <- updateableInput("Text")
numericInput <- updateableInput("Numeric")
selectInput <- updateableInput("Select")
selectizeInput <- updateableInput("Select")
checkboxInput <- updateableInput("Checkbox")
sliderInput <- updateableInput("Slider")
radioButtons <- updateableInput("RadioButtons")
prettyRadioButtons <- updateableInput("RadioButtons")
materialSwitch <- updateableInput("MaterialSwitch")

#' Update a single Shiny input without specifying its type
#' @param session
#' @param id
#' @param value
#' 
#' @return 
#' @export
#' @example 
updateShinyInput <- function(session, id, value) {
  shinyUpdateInputId <- paste0("shiny-update-input-", id)
  js$getInputType(id, shinyUpdateInputId)
  shiny::observeEvent(session$input[[shinyUpdateInputId]], {
    inputType <- session$input[[shinyUpdateInputId]]
    if(inputType == "RadioButtons" || inputType == "MaterialSwitch"){
      updateFunc <- sprintf("update%s", inputType)
    }
    else{
      updateFunc <- sprintf("update%sInput", inputType)
    }
    funcParams <- list(session = session, inputId = id)
    
    if (inputType == "Select" || inputType == "RadioButtons") {
      funcParams[['selected']] <- value
    } else {
      funcParams[['value']] <- value
    }
    do.call(updateFunc, funcParams)
  })
}

#' Update multiple Shiny inputs simultaneously
#' @param session
#' @param updates
#' 
#' @return 
#' @export
#' @example 
updateShinyInputs <- function(session, updates) {
  lapply(names(updates), function(id) {
    updateShinyInput(session, id, updates[[id]])
  })
}