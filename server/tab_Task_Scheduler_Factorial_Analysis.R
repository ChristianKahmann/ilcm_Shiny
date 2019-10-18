
#render the parameter set for cooccurrence analysis
output$Analysis_Parameter_FA<-renderUI({
  tagList(
    tags$hr(),
    #standard parameters
    fluidRow(
      column(1,
             selectInput(inputId = "FA_baseform",label = "Baseform Reduction",choices = c("lemma","stemming","none"),selected = "none")),
      column(2,
             sliderInput(inputId = "FA_min_char",label = "Min #chars for words",value = c(2,50),min = 1,step = 1,max = 100)),
      column(2,
             selectInput(inputId = "FA_ngram",label = "ngrams?",choices = c(1,2,3),selected = 1,multiple = T)),
      column(2,
             textInput(inputId = "FA_remove_custom",label = HTML("remove custom words"),placeholder ="Add words (Seperated by ,)")
      )
    ),
    fluidRow(
      column(1,
             checkboxInput(inputId = "FA_remove_stopwords",label = "Remove Stopwords",value = T)),
      column(1,
             checkboxInput(inputId = "FA_lowerKAse",label = "Transform tokens to lowercase?",value = T)),
      column(1,
             checkboxInput(inputId = "FA_remove_numbers",label = "Remove Numbers?",value = T)),
      column(1,
             checkboxInput(inputId = "FA_remove_punctuation",label = "Remove Punctuation?",value = T)),
      column(1,
             checkboxInput(inputId = "FA_remove_hyphenation",label = "Remove Hyphenation?",value = T)),
      column(1,
             checkboxInput(inputId = "FA_consolidate_entities",label = "Consolidate Entities?",value = T)),
      column(1,
             checkboxInput(inputId = "FA_use_custom_blacklist",label = "use custom blacklist?",value = F)
      ),
      column(2,
             conditionalPanel(condition = "input.FA_use_custom_blacklist==true",
                              shinyWidgets::prettyRadioButtons(inputId = "FA_blacklist",label = "Blacklists",
                                                               choices = stringr::str_replace_all(string = list.files("collections/blacklists/"),pattern = ".txt",replacement = ""),
                                                               fill=T,animation = "tada",selected = NULL)
             )
      )
    ),
    tags$hr(),
    tags$h4("Pruning"),
    fluidRow(
      column(1,
             tags$div(selectInput(inputId = "FA_termfreq_type",label = "term frequency type",choices = c("count","prop","rank","quantile"),multiple = F,selected = "count"))),
      column(2,
             tags$br(),
             conditionalPanel(condition = 'input.FA_termfreq_type=="count"',
                              numericInput(inputId = "FA_min_termfreq_c",label = "min. term frequency",min = 0,step = 1,value=NULL),
                              numericInput(inputId = "FA_max_termfreq_c",label = "max. term frequency",min = 1,step = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.FA_termfreq_type=="prop"',
                              numericInput(inputId = "FA_min_termfreq_p",label = "min. term probability",min = 0,step = 0.01,max = 1,value=NULL),
                              numericInput(inputId = "FA_max_termfreq_p",label = "max. term probability",min = 0.001,step = 0.01,max = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.FA_termfreq_type=="rank"',
                              numericInput(inputId = "FA_min_termfreq_r",label = "min. term rank",min = 1,step = 1,value=NULL),
                              numericInput(inputId = "FA_max_termfreq_r",label = "max. term rank",min = 1,step = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.FA_termfreq_type=="quantile"',
                              numericInput(inputId = "FA_min_termfreq_q",label = "min. term quantile",min = 0,max = 1,step = 0.25,value=NULL),
                              numericInput(inputId = "FA_max_termfreq_q",label = "max. term quantile",min = 0,max = 1,step = 0.25,value=NULL)
             )
      ),
      column(1,
             tags$div(selectInput(inputId = "FA_docfreq_type",label = "doc frequency type",choices = c("count","prop","rank","quantile"),multiple = F,selected = "prop"))),
      column(2,
             tags$br(),
             conditionalPanel(condition = 'input.FA_docfreq_type=="count"',
                              numericInput(inputId = "FA_min_docfreq_c",label = "min. doc frequency",min = 0,step = 1,value=NULL),
                              numericInput(inputId = "FA_max_docfreq_c",label = "max. doc frequency",min = 1,step = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.FA_docfreq_type=="prop"',
                              numericInput(inputId = "FA_min_docfreq_p",label = "min. doc probability",min = 0,step = 0.01,max = 1,value=NULL),
                              numericInput(inputId = "FA_max_docfreq_p",label = "max. doc probability",min = 0.001,step = 0.01,max = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.FA_docfreq_type=="rank"',
                              numericInput(inputId = "FA_min_docfreq_r",label = "min. doc rank",min = 1,step = 1,value=NULL),
                              numericInput(inputId = "FA_max_docfreq_r",label = "max. doc rank",min = 1,step = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.FA_docfreq_type=="quantile"',
                              numericInput(inputId = "FA_min_docfreq_q",label = "min. doc quantile",min = 0,max = 1,step = 0.25,value=NULL),
                              numericInput(inputId = "FA_max_docfreq_q",label = "max. doc quantile",min = 0,max = 1,step = 0.25,value=NULL)
             )
      )
      
    ),
    tags$hr(),
    #specific parameters
    fluidRow(
      column(2,
             selectInput(inputId = "FA_POS_TYPES",label = "POS-Types",
                         choices =c("all","NOUN","VERB","ADJ","PUNCT","SYM","ADP","PART","ADV","INTJ","X") ,selected = "all",multiple = T)
      ),
      column(2,
             selectInput(inputId = "FA_ENTITY_TYPES",label = "NER-Tags",
                         choices =c("all","PERSON","ORG","GPE","PRODUCT","NORP","FACILITY","LOC","EVENT","WORK_OF_ART","LAW",
                                    "LANGUAGE","DATE","TIME","PERCENT","MONEY","QUANTITY","ORDINAL","KARDINAL") ,selected = "all",multiple=T)
      )
    ),
    actionButton(inputId = "FA_Submit_Script",label = "Submit Request")
  )
})




#start cooccurrence analysis script, if submit button is clicked
observeEvent(input$FA_Submit_Script,{
  #get pruning parameters
  if(input$FA_termfreq_type=="count"){
    min_t<-input$FA_min_termfreq_c
    max_t<-input$FA_max_termfreq_c
  }
  if(input$FA_docfreq_type=="count"){
    min_d<-input$FA_min_docfreq_c
    max_d<-input$FA_max_docfreq_c
  }
  if(input$FA_termfreq_type=="rank"){
    min_t<-input$FA_min_termfreq_r
    max_t<-input$FA_max_termfreq_r
  }
  if(input$FA_docfreq_type=="rank"){
    min_d<-input$FA_min_docfreq_r
    max_d<-input$FA_max_docfreq_r
  }
  if(input$FA_termfreq_type=="prop"){
    min_t<-input$FA_min_termfreq_p
    max_t<-input$FA_max_termfreq_p
  }
  if(input$FA_docfreq_type=="prop"){
    min_d<-input$FA_min_docfreq_p
    max_d<-input$FA_max_docfreq_p
  }
  if(input$FA_termfreq_type=="quantile"){
    min_t<-input$FA_min_termfreq_q
    max_t<-input$FA_max_termfreq_q
  }
  if(input$FA_docfreq_type=="quantile"){
    min_d<-input$FA_min_docfreq_q
    max_d<-input$FA_max_docfreq_q
  }
  if(is.null(min_t))min_t<-NA
  if(is.null(max_t))max_t<-NA
  if(is.null(min_d))min_d<-NA
  if(is.null(max_d))max_d<-NA
  #save needed parameters
  parameters<-list(input$collection_selected,input$FA_baseform,input$FA_min_char,input$FA_ngram,input$FA_remove_stopwords,input$FA_lowerKAse,input$FA_remove_numbers,
                   input$FA_remove_punctuation,input$FA_remove_hyphenation,min_t,max_t,min_d,max_d,
                   input$FA_remove_custom,input$FA_consolidate_entities,input$FA_blacklist,input$FA_POS_TYPES,input$FA_ENTITY_TYPES,
                   input$FA_termfreq_type,input$FA_docfreq_type)
  #save metadata for process
  process_info<-list(round(runif(1)*1000),isolate(input$collection_selected),isolate(input$analysis_selected),as.character(Sys.time()))
  #save logfile path
  logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
  #create logfile
  write(unlist(process_info),file = paste("collections/logs/running/",process_info[[1]],".txt",sep=""))
  #save data needed in script execution
  save(parameters,logfile,process_info,file="collections/tmp/tmp.RData")
  #start script
  if(input$use_custom_script==TRUE && !is.null(input$custom_script_options)){
    shinyWidgets::sendSweetAlert(session=session,title = "Starting a custom script",text = "You are about to start a custom script. Caution with the calculation and results!",type = "info")
    system(paste0('Rscript collections/scripts/Factorial_Analysis/',input$custom_script_options,' &'))
  }
  else{
    system(paste('Rscript collections/scripts/Factorial_Analysis_Script.R','&'))
    #show modal when process is started
    showModal(modalDialog(
      title = "Process started",
      "The process was succesfully started. Check details in 'My Tasks'."
    ))
  }
})
