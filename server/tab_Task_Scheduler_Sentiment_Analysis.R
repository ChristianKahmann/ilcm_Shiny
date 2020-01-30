
#parameters for sentiment analysis
output$Analysis_Parameter_SA<-renderUI({
  tagList(
    tags$hr(),
    #standard parameters
    fluidRow(
      column(1,
             selectInput(inputId = "SA_baseform",label = "Baseform Reduction",choices = c("stemming","none"),selected = "none")%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Specify whether and how to do baseform reduction. The options are stemming, lemmatization or no baseorm reduction.", placement = "right"
                   )
               )
             ),
      column(1,
             checkboxInput(inputId = "SA_lowercase",label = "Transform tokens to lowercase?",value = T)%>%
               shinyInput_label_embed(
               shiny_iconlink() %>%
                 bs_embed_popover(
                   title = "Should lowercasing (set all characters to lower case) be applied?", placement = "left"
                 )
             )),
      column(2,
             selectInput(inputId = "SA_ngram",label = "ngrams?",choices = c(1,2,3),selected = 1,multiple = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Should n-grams be included in the analysis? They will be connected with '_'.", placement = "right"
                   )
               )
             ),
      column(2,
             textInput(inputId = "SA_remove_custom",label = HTML("remove custom words"),placeholder ="Add words (Seperated by ,)")%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Delete specific words from the analysis. Seperate them with ','.", placement = "right"
                   )
               )
      )
    ),
    #specific parameters
    tags$hr(),
    tags$h4("Sentiment parameters"),
    fluidRow(
      column(2,
             selectInput(inputId = "SA_sentiment_dictionary",label = "Sentiment Dictionary",choices = list.files("collections/sentiments/"))%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Specify the sentiment dictionary. You can put a csv file containing the words with their corresponding weights as csv in the directory: 'collections/sentiments/'. ", placement = "right"
                   )
               )
      ),
      column(2,
             selectInput(inputId = "SA_avg",label = "Build Document Score by:",choices = c("mean","sum"))%>%
                           shinyInput_label_embed(
                             shiny_iconlink() %>%
                               bs_embed_popover(
                                 title = "How to aggreagte a score for every document. Mean: average of all scores in a document (document length has small influence).
                                 Sum: sum of all scores in a document (document length can have large influence).", placement = "right"
                               )
                           )
      )
    ),
    bsButton(inputId = "SA_Submit_Script",label = "Submit Request",icon = icon("play-circle"),type = "primary")
  )
})




#start Sentiment analysis script, if submit button is clicked
observeEvent(input$SA_Submit_Script,{
  #save needed parameters
  parameters<-list(collection=input$collection_selected,
                   baseform_reduction=input$SA_baseform,
                   ngrams=input$SA_ngram,
                   lowercase=input$SA_lowercase,
                   Sentiment_Dictionary=input$SA_sentiment_dictionary,
                   Document_Score_Aggregation=input$SA_avg,
                   remove_custom=input$SA_remove_custom
  )
  #create process ID
  ID<-get_task_id_counter()+1
  set_task_id_counter(ID)
  #save metadata for process
  process_info<-list(ID,isolate(input$collection_selected),isolate(input$analysis_selected),as.character(Sys.time()))
  #save logfile path
  logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
  #create logfile
  write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
  #save data needed in script execution 
  save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
  #start script
  if(input$use_custom_script==TRUE && !is.null(input$custom_script_options)){
    shinyWidgets::sendSweetAlert(session=session,title = "Starting a custom script",text = "You are about to start a custom script. Caution with the calculation and results!",type = "info")
    system(paste0('Rscript collections/scripts/Sentiment_Analysis/',input$custom_script_options,' &'))
  }
  else{
    system(paste('Rscript collections/scripts/Sentiment_Analysis_Script.R','&'))
    #show modal when process is started
    showModal(modalDialog(
      title = "Process started",
      "The process was succesfully started. Check details in 'My Tasks'."
    ))
  }
})
