#select Input to choose a collection
output$Task_Scheduler_Collection<-renderUI({
  values$coll_saved
  selectizeInput(inputId = "collection_selected",label = "Collection:",choices = stringr::str_replace_all(string = list.files(path ="collections/collections/"),pattern = ".RData",replacement = ""))
})

output$custom_script_options_UI<-renderUI({
  values$new_script_saved
  if(input$analysis_selected==""){
    shinyWidgets::sendSweetAlert(session=session,title = "Please select an analysis",text = "Please choose a type of analysis",type = "warning")
    return(NULL)
  }
  else{
    choices<-list.files(paste0("collections/scripts/",input$analysis_selected,"/"))
    
    if(length(choices)==0){
      shinyWidgets::sendSweetAlert(session=session,title = "no custom script found!",text = "You can edit and save scipts in the 'Scripts-Tab' in the sidebar. Otherwise the standard script will be used.",type = "warning")
      return(NULL)
    }
    else{
      return(
        selectInput(inputId = "custom_script_options",label = "available custom scripts",choices = choices)
      )
    }
  }
})


source(file.path("server","tab_Task_Scheduler_Coocs.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Frequency_Extraction.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Dictionary_Extraction.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Sentiment_Analysis.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Volatility_Analysis.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Topic_Model.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Classification.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Rest.R"),local = T)$value
#source(file.path("server","tab_Task_Scheduler_Factorial_Analysis.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Vector_Space_Representation.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Document_Deduplication.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Keyword_Extraction.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Syntactic_Parsing.R"),local = T)$value

#check if analysis help button was clicked, if yes,open modal with rmd.files
observeEvent(input$analysis_help,ignoreInit = T,{
  path_to_tutorial<-paste0("Tutorials/",input$analysis_selected,".md")
  if(file.exists(path_to_tutorial)){
    showModal(
      modalDialog(title = paste0("Tutorial for ",input$analysis_selected),size = "l",fade = T,
                  includeMarkdown(path_to_tutorial)
                  
      )
    )
  }
  else{
    shinyWidgets::sendSweetAlert(session=session,title = "No Tutorial found",text = "For your chosen analysis, there is no tutorial .md file located in the tutorials directory Sorry!",type = "warning")
  }
  
})

