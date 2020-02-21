
#parameters for sentiment analysis
output$Analysis_Parameter_DD<-renderUI({
  tagList(
    tags$hr(),
    #standard parameters
    fluidRow(
      # column(2,
      #        materialSwitch(inputId = "DD_interactive",label = "Interactive mode?",value = TRUE,status = "primary",right = T)%>%
      #          shinyInput_label_embed(
      #            shiny_iconlink() %>%
      #              bs_embed_popover(
      #                title = "The interactive mode allows user interaction in choosing the strategy and the documents to keep by hand inside the Results-Tab.
      #                The non nteractive mode will create a new collection with the suffix '_deduplicated' based on the parameters chosen in the task scheduler.", placement = "right"
      #              )
      #          ) 
      # ),
      column(2,
             #"ratio of matches" entfernt, da kein symetrisches Maß, die graphbasierte Entfernung dies aber vorsieht  
             selectInput(inputId = "DD_similarity_measure",label = "Similarity measure",choices = c("jaccard similarity","jaccard bag similarity"),multiple = F)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Choose the similarity measure for the pairwise document comparison.
                                      The Jaccard similarity measures similarity between finite sample sets, and is defined as the size of the intersection divided by the size of the union of the sample sets.
                                      The function jaccard bag similarity treats a and b as bags rather than sets,
                                      so that the result is a fraction where the numerator is the sum of each matching element counted the minimum number of times it appears in 
                                      each bag, and the denominator is the sum of the lengths of both bags.
                                      The function ratio of matches finds the ratio between the number of items in b that are also in a and the total number of items in b.
                                      Note that this similarity measure is directional: it measures how much b borrows from a, but says nothing about how much of a borrows from b",
                     placement = "right"
                   )
               ) 
      )
    ),
    # fluidRow(
    #   conditionalPanel(condition="input.DD_interactive==false",
    #                     column(1,
    #                           conditionalPanel(condition = 'input.DD_similarity_measure=="jaccard bag similarity"',
    #                                            numericInput(inputId = "DD_threshold",label="Threshold",value = 0.2,min=0,max=0.5,step = 0.05)%>%
    #                                              shinyInput_label_embed(
    #                                                shiny_iconlink() %>%
    #                                                  bs_embed_popover(
    #                                                    title = "If a documents pairs similarity is bigger than the treshold, one of them will be removed according to the chosen strategy.", placement = "right"
    #                                                  )
    #                                              ) 
    #                           ),
    #                           conditionalPanel(condition = 'input.DD_similarity_measure!="jaccard bag similarity"',
    #                                            numericInput(inputId = "DD_threshold",label="Threshold",value = 0.4,min=0,max=1,step = 0.05)%>%
    #                                              shinyInput_label_embed(
    #                                                shiny_iconlink() %>%
    #                                                  bs_embed_popover(
    #                                                    title = "If a documents pairs similarity is bigger than the treshold, one of them will be removed according to the chosen strategy.", placement = "right"
    #                                                  )
    #                                              ) 
    #                           )
    #                    ),
    #                    column(2,
    #                           selectInput(inputId = "DD_strategy",label = "Document choice strategy",choices = c("longest","shortest","latest","earliest","random","maximum node degree"))%>%
    #                             shinyInput_label_embed(
    #                               shiny_iconlink() %>%
    #                                 bs_embed_popover(
    #                                   title = "Which strategy shall be applied, while choosing the documents to remove from the collection. 'longest' and 'shortest' consider the amount of tokens per document. 
    #                                               Whereas 'latest' and 'earliest' use the meta data 'date' to determine which document to keep and which to remove. The random option selects one of the documents randomly." , placement = "right"
    #                                 )
    #                             ) 
    #                    )
    #   )
    # ),
    bsButton(inputId = "DD_Submit_Script",label = "Submit Request",icon = icon("play-circle"),type = "primary")
  )
})




#start Sentiment analysis script, if submit button is clicked
observeEvent(input$DD_Submit_Script,{
  #save needed parameters
  parameters<-list(collection=input$collection_selected,
                   #DD_interactive=input$DD_interactive,
                   DD_interactive=TRUE,
                   DD_similarity_measure=input$DD_similarity_measure,
                   DD_treshold=input$DD_threshold,
                   DD_strategy=input$DD_strategy
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
    system(paste0('Rscript collections/scripts/Document_Deduplication/',input$custom_script_options,' &'))
  }
  else{
    system(paste('Rscript collections/scripts/Document_Deduplication_Script.R','&'))
    #show modal when process is started
    showModal(modalDialog(
      title = "Process started",
      "The process was succesfully started. Check details in 'My Tasks'."
    ))
  }
})
