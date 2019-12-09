
#parameters for sentiment analysis
output$Analysis_Parameter_KE<-renderUI({
  tagList(
    tags$hr(),
    #standard parameters
    fluidRow(
      column(1,
             selectInput(inputId = "KE_baseform",label = "Baseform Reduction",choices = c("lemma","none"),selected = "none")%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Specify whether and how to do baseform reduction. The options are stemming, lemmatization or no baseform reduction.", placement = "right"
                   )
               )
      ),
      column(1,
             checkboxInput(inputId = "KE_lowercase",label = "Transform tokens to lowercase?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Should lowercasing (set all characters to lower case) be applied?", placement = "left"
                   )
               )
      )
    ),
    tags$hr(),
    #specific parameters
    tags$h4("Keyword extraction parameters"),
    fluidRow(
      column(2,
             radioButtons(inputId = "KE_Mode",label = "Mode",choices = c("without reference","with reference"),selected = "without reference")%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Keywords can be extracted using a reference corpus and comparing which words appear significantly more frequent.
                     Keywords can also be extracted with a reference corpus using only information available for the given documents.", placement = "right"
                   )
               )    
      )
    ),
    fluidRow(
      conditionalPanel(condition = "input.KE_Mode=='without reference'" ,
                       column(2,
                              selectInput(inputId = "KE_no_ref_method",label = "Method",choices = c("RAKE","PMI Collocation","Phrase Sequence","Textrank"),multiple = F)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "4 methods of identifying keywords in text without a reference corpus are implemented: RAKE (Rapid Automatic Keyword Extraction),
                     Collocation ordering using Pointwise Mutual Information, Parts of Speech phrase sequence detection and Textrank.", placement = "right"
                                    )
                                )
                       ),
                       column(2,
                              numericInput(inputId = "KE_no_ref_n_min",label = "Minimal number of occurrences per keyword",value = 1,min = 1,max = 10000000,step = 1)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Integer indicating the frequency of how many times a keywords should at least occur in the data in order to be returned.", placement = "right"
                                    )
                                )
                       ),
                       column(2,
                              numericInput(inputId = "KE_no_ref_ngram_max",label = "Maximal number of words per keyword",value = 8,min = 1,max = 100,step = 1)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Integer indicating the maximum number of words that there should be in each keyword", placement = "right"
                                    )
                                )
                       ),
                       column(2,
                              textInput(inputId = "KE_seperator",label = "Seperator",value = "_")%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Which sign should be used to bind ngrams together. Defaults to '_'", placement = "right"
                                    )
                                )
                              
                              ),
                       conditionalPanel(condition = "input.KE_no_ref_method=='RAKE' || input.KE_no_ref_method=='Textrank'",
                                        column(2,
                                               selectInput(inputId = "KE_filter",label = "Relevant POS-Tags",choices = c("NOUN","ADJ","VERB","PROPN","ADP","AUX","CCONJ","DET","INTJ","NUM","PART",
                                                                                                                                "PRON","PUNCT","SCONJ","SYM","X"),
                                                           selected = c("NOUN","ADJ"),multiple = T)%>%
                                                 shinyInput_label_embed(
                                                   shiny_iconlink() %>%
                                                     bs_embed_popover(
                                                       title = "Specify which words (part of speech) are relevant for the current keyword extraction. E.g. nouns + adjectives or verbs + nouns...", placement = "right"
                                                     )
                                                 )
                                        )
                       ),
                       conditionalPanel(condition = "input.KE_no_ref_method=='Phrase Sequence'",
                                        column(2,
                                               textInput(inputId = "KE_phrase",label = "Phrase pattern",value = "(A|N)*N(P+D*(A|N)*N)*")%>%
                                                 shinyInput_label_embed(
                                                   shiny_iconlink() %>%
                                                     bs_embed_popover(
                                                       title = 'Regular expression to identify potential keywords. E.g. simple noun phrase: "(A|N)*N(P+D*(A|N)*N)*.
                                                       Abbreviations:A: adjective 
C: coordinating conjuction

D: determiner

M: modifier of verb

N: noun or proper noun

P: preposition

O: other elements  ', placement = "right"
                                                     )
                                                 )
                                        )
                       )
      )
      
    ),
    bsButton(inputId = "KE_Submit_Script",label = "Submit Request",icon = icon("play-circle"),type = "primary")
  )
})




#start Sentiment analysis script, if submit button is clicked
observeEvent(input$KE_Submit_Script,{
  #save needed parameters
  parameters<-list(collection=input$collection_selected,
                   baseform_reduction=input$KE_baseform,
                   lowercase=input$KE_lowercase,
                   KE_Mode=input$KE_Mode,
                   KE_no_ref_method=input$KE_no_ref_method,
                   KE_no_ref_n_min=input$KE_no_ref_n_min,
                   KE_no_ref_ngram_max=input$KE_no_ref_ngram_max,
                   KE_filter=input$KE_filter,
                   KE_phrase=input$KE_phrase,
                   KE_seperator=input$KE_seperator
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
    system(paste0('Rscript collections/scripts/Keyword_Extraction/',input$custom_script_options,' &'))
  }
  else{
    system(paste('Rscript collections/scripts/Keyword_Extraction_Script.R','&'))
    #show modal when process is started
    showModal(modalDialog(
      title = "Process started",
      "The process was succesfully started. Check details in 'My Tasks'."
    ))
  }
})
