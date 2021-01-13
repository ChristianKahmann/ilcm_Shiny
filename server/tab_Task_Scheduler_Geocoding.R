source("server/tab_Task_Schedular_StandardParameters.R")

#' render analysis parameter for geocoding
output$Analysis_Parameter_Geocoding<-renderUI({
  
    c(
      
      standardparameters <- getTagListStanddardParameters(),
      #posAndNerParams <- getTagListParametersPOSandNER(),
    
      #######################
      #specific parameters
      #######################
      
      
      ###################
      # submit button
      ###################
      tagList(
        bsButton(inputId = "Geocoding_Submit_Script",label = "Submit Request",icon = icon("play-circle"),type = "primary")
      )
    )
    
  
  
})

output$blacklist_UI <- standard_blacklist_UI
output$whitelist_UI <- standard_whitelist_UI

#' validate submitted script
#' depends on:
#'   input$Geocoding_Submit_Script: submitted script from user
observeEvent(input$Geocoding_Submit_Script,{

  functionToSetParametersIndependentOnUIinput <- function(parameters){
    parameters$reduce_POS <- c("all") 
    parameters$reduce_NER <- c("LOC", "GPE") 
    return (parameters)
  }
  functionToAppendAdditionalParameters = NULL
  filepathForScript = 'Rscript collections/scripts/Geocoding_Script.R'
  folderpathForCustomScripts = 'Rscript collections/scripts/Geocoding/'
  standardProcessingAfterSubmittingTask(
    input = input, 
    functionToSetParametersIndependentOnUIinput = functionToSetParametersIndependentOnUIinput, 
    functionToAppendAdditionalParameters = functionToAppendAdditionalParameters, 
    filepathForScript = filepathForScript, 
    folderpathForCustomScripts = folderpathForCustomScripts)

  }
)