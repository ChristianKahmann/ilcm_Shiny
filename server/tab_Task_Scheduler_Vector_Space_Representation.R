
#parameters for sentiment analysis
output$Analysis_Parameter_VS<-renderUI({
  tagList(
    tags$hr(),
    #standard parameters
    fluidRow(
      column(2,
             checkboxInput(inputId = "VS_use_model",label = "Use an existing model?",value = FALSE)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Caution! When using an existing model, the chosen collection will be of no further use.", placement = "right"
                   )
               )
      ),
      column(2,
             conditionalPanel(condition = 'input.VS_use_model==true',
                              selectInput(inputId = "VS_model",label = "Choose a pre-calculated model.",choices = list.files(path = "collections/word_vector_models/"))%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "The models can be placed inside 'collections/word_vector_models/'. .vec and .bin can be imported", placement = "right"
                                    )
                                )
             )
      )
    ),
    conditionalPanel(condition = 'input.VS_use_model==false',
                     fluidRow(
                       column(1,
                              checkboxInput(inputId = "VS_lowercase",label = "Transform tokens to lowercase?",value = F)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Should lowercasing (set all characters to lower case) be applied?", placement = "left"
                                    )
                                )),
                       column(2,
                              selectInput(inputId = "VS_ngram",label = "ngrams?",choices = c(1,2,3),selected = 1,multiple = T)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Should n-grams be included in the analysis? They will be connected with '_'.", placement = "right"
                                    )
                                )),
                       column(1,
                              checkboxInput(inputId = "VS_stopwords",label = "Remove stopwords?",value = F)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Should stopwords be removed from the analysis? The stopwords of the language specified during dataimport for this dataset will be used.", placement = "top"
                                    )
                                )
                       ),
                       column(1,
                              checkboxInput(inputId = "VS_punctuation",label = "Remove punctuation?",value = T)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Remove words, which reflect punctuation from the analysis.", placement = "right"
                                    )
                                )
                       )
                      ),
                     tags$hr(),
                     #specific parameters
                     tags$hr(),
                     tags$h4("Word2Vec parameters"),
                     fluidRow(
                       column(2,
                              numericInput(inputId = "VS_min_occ",label = "Minimum number of occurrences",value = 1,min = 1,step = 1)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Minimum times a word must appear to be included in the samples. High values help reduce model size.", placement = "right"
                                    )
                                )
                       ),
                       column(2,
                              numericInput(inputId = "VS_vectors",label = "Number of vectors",value = 200)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "The number of vectors to output. Defaults to 100. More vectors usually means more precision, but also more random error, higher memory usage, and slower operations. Sensible choices are probably in the range 100-500.", placement = "right"
                                    )
                                )
                       ),
                       column(2,
                              numericInput(inputId = "VS_threads",label = "Number of threads",value = 2)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Number of threads to run training process on. Defaults to 1; up to the number of (virtual) cores on your machine may speed things up.", placement = "right"
                                    )
                                )
                       ),
                       column(2,
                              numericInput(inputId = "VS_window",label = "Windowsize",value = 12)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "The size of the window (in words) to use in training.", placement = "right"
                                    )
                                )
                       ),
                       column(2,
                              numericInput(inputId = "VS_iter",label = "Number of iterations",value = 5)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Number of passes to make over the corpus in training.", placement = "right"
                                    )
                                )
                       ),
                       column(2,
                              numericInput(inputId = "VS_neg_samples",label = "Number of negative samples",value = 0)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Number of negative samples to take in skip-gram training. 0 means full sampling, while lower numbers give faster training. For large corpora 2-5 may work; for smaller corpora, 5-15 is reasonable.", placement = "right"
                                    )
                                )
                       )
                     )
    ),
    fluidRow(
      column(2,
             checkboxInput(inputId = "VS_train_dim_reduction",label = "Pre-train Dimensionality Reduction on whole dataset?",value = F)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Caution! Hardware intensive", placement = "right"
                   )
               )
      )
    ),
    tags$hr(),
    bsButton(inputId = "VS_Submit_Script",label = "Submit Request",icon = icon("play-circle"),type = "primary")
  )
})




#start Sentiment analysis script, if submit button is clicked
observeEvent(input$VS_Submit_Script,{
  #VSve needed parameters
  parameters<-list(input$collection_selected,input$VS_lowercase,input$VS_ngram,input$VS_stopwords,input$VS_punctuation,input$VS_min_occ,
                   input$VS_vectors,input$VS_threads,input$VS_window,input$VS_iter,input$VS_neg_samples,input$VS_use_model,input$VS_model,
                   input$VS_train_dim_reduction)
  #save needed parameters
  parameters<-list(collection=input$collection_selected,
                   ngrams=input$VS_ngram,
                   remove_stopwords=input$VS_stopwords,
                   lowercase=input$VS_lowercase,
                   remove_punctuation=input$VS_punctuation,
                   w2v_min_occ=input$VS_min_occ,
                   w2v_vectors=input$VS_vectors,
                   w2v_threads=input$VS_threads,
                   w2v_window=input$VS_window,
                   w2v_iterations=input$VS_iter,
                   w2v_neg_samples=input$VS_neg_samples,
                   class_use_model=input$VS_use_model,
                   class_model=input$VS_model,
                   class_dim_reduction=input$VS_train_dim_reduction
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
    system(paste0('Rscript collections/scripts/Vector_Space_Representation/',input$custom_script_options,' &'))
  }
  else{
    system(paste('Rscript collections/scripts/Vector_Space_Representation_Script.R','&'))
    #show modal when process is started
    showModal(modalDialog(
      title = "Process started",
      "The process was succesfully started. Check details in 'My Tasks'."
    ))
  }
})
