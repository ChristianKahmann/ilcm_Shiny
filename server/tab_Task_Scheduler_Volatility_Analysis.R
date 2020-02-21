
#render the parameter set for volatility analysis
output$Analysis_Parameter_VA<-renderUI({
  tagList(
    tags$hr(),
    #standard parameters
    fluidRow(
      column(1,
             selectInput(inputId = "VA_baseform",label = "Baseform Reduction",choices = c("lemma","stemming","none"),selected = "none")#,
             #shinyBS::bsPopover(id = "VA_baseform", title = "Specify whether and how to do baseform reduction. The options are stemming, lemmatization or no baseorm reduction.",
            #                    content = '<a href="https://www.google.com/">Google Homepage</a>',placement = "right",trigger = "hover",list(container = "body"))
             %>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Specify whether and how to do baseform reduction. The options are stemming, lemmatization or no baseorm reduction.", placement = "right"
                   )
               )
      ),
      column(2,
             sliderInput(inputId = "VA_min_char",label = "Min #chars for words",value = c(2,50),min = 1,step = 1,max = 100)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Set the minimum and maximum number of characters of a word to be included in the analysis.", placement = "right"
                   ,html=T)
               )),
      column(1,
             selectInput(inputId = "VA_ngram",label = "N-grams",choices = c(1,2,3),selected = 1,multiple = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Should n-grams be included in the analysis? They will be connected with '_'.", placement = "right"
                   )
               )),
      column(2,
             textInput(inputId = "VA_remove_custom",label = HTML("Remove custom words"),placeholder ="Add words (Seperated by ,)")%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Delete specific words from the analysis. Seperate them with ','.", placement = "right"
                   )
               )
      ),
      column(2,
             textInput(inputId = "VA_keep_custom",label = HTML("Keep custom words"),placeholder ="Add words (Seperated by ,)")%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Keep specific words in the analysis. Seperate them with ','. If both custom words and whitelist are specified, they will be merged together.", placement = "right"
                   )
               )
      )
    ),
    fluidRow(
      column(1,
             checkboxInput(inputId = "VA_remove_stopwords",label = "Remove Stopwords",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Should stopwords be removed from the analysis? The stopwords of the language specified during dataimport for this dataset will be used.", placement = "top"
                   )
               )),
      column(1,
             checkboxInput(inputId = "VA_lowercase",label = "Transform tokens to lowercase?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Should lowercasing (set all characters to lower case) be applied?", placement = "left"
                   )
               )),
      column(1,
             checkboxInput(inputId = "VA_remove_numbers",label = "Remove Numbers?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Remove types which consist of numbers only from the analysis.", placement = "bottom"
                   )
               )),
      column(1,
             checkboxInput(inputId = "VA_remove_numbers_all",label = "Remove everything containing a number number?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Remove words which are composed of at least one number.", placement = "right"
                   )
               )),
      column(1,
             checkboxInput(inputId = "VA_remove_punctuation",label = "Remove Punctuation?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Remove words, which reflect punctuation from the analysis.", placement = "right"
                   )
               )),
      column(1,
             checkboxInput(inputId = "VA_remove_hyphenation",label = "Remove Hyphenation?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Remove words, which reflect hyphens from the analysis.", placement = "right"
                   )
               )),
      column(1,
             checkboxInput(inputId = "VA_consolidate_entities",label = "Consolidate Entities?",value = F)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Consolidate found entities by binding them together with '_' and treat them as a single word afterwards.", placement = "right"
                   )
               ))
    ),
    fluidRow(
      column(1,
             checkboxInput(inputId = "VA_use_custom_blacklist",label = "use custom blacklist?",value = F)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Use one of the specified blacklists to remove a set of words from the analysis.", placement = "right"
                   )
               )
      ),
      column(2,
             conditionalPanel(condition = "input.VA_use_custom_blacklist==true",
                              uiOutput(outputId = "VA_blacklist_UI")
             )
      ),
      column(1,
             checkboxInput(inputId = "VA_use_custom_whitelist",label = "use custom whitelist?",value = F)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Use of the specified whitelists to protect a set of words from the removal during the pre-processing or to calculate the analysis for these words exclusivly.", placement = "right"
                   )
               )
      ),
      column(2,
             conditionalPanel(condition = "input.VA_use_custom_whitelist==true",
                              uiOutput(outputId = "VA_whitelist_UI")
             )
      )
    ),
    fluidRow(
      conditionalPanel(condition = "input.VA_use_custom_whitelist==true || input.VA_keep_custom.length>=1",
                       tags$br(),
                       tags$h5("Whitelist Options:"),
                       column(1,
                              checkboxInput(inputId = "VA_whitelist_only",label = "Exclude all words apart from the whitelist entries",value = FALSE)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Should all words apart from the entries in the whitelist be excluded from the analysis?", placement = "right"
                                    )
                                )
                       ),
                       column(2,
                              checkboxInput(inputId = "VA_whitelist_only_results",label = "Volatility calculation for whitelist terms only",value = FALSE)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Use regular pre-processing but calculate the context volatility only for those words in the whitelist.", placement = "right"
                                    )
                                )
                       ),
                       column(1,
                              conditionalPanel(condition = "(input.VA_use_custom_whitelist==true || input.VA_keep_custom.length>=1) && (input.VA_ngram.includes('2') || input.VA_ngram.includes('3'))",
                                               checkboxInput(inputId = "VA_whitelist_expand",label = "Expand whitelist?",value = FALSE)%>%
                                                 shinyInput_label_embed(
                                                   shiny_iconlink() %>%
                                                     bs_embed_popover(
                                                       title = "Should the words whitelist be expaned using n-grams?", placement = "right"
                                                     )
                                                 )
                              )  
                              
                       )
      )
    ),
    tags$hr(),
    tags$h4("Pruning"),
    fluidRow(
      column(1,
             tags$div(
               selectInput(inputId = "VA_termfreq_type",label = "term frequency type",choices = c("count","prop","rank","quantile"),multiple = F,selected = "count")%>%
                 shinyInput_label_embed(
                   shiny_iconlink() %>%
                     bs_embed_popover(
                       title = "Remove words from the analysis that exhibit a specified lower and/or upper bound of occurrences.
                                      These bound can be specified by a explicit number of occurrences (count based), by setting a probability (prob),
                                      by using a rank to keep just words within a certain range of occurrence ranks or by specifying a quantile to cut of.",
                       placement = "right",
                       html="true"
                     )
                 )
             )
      ),
      column(2,
             tags$br(),
             conditionalPanel(condition = 'input.VA_termfreq_type=="count"',
                              numericInput(inputId = "VA_min_termfreq_c",label = "min. term frequency",min = 0,step = 1,value=NULL),
                              numericInput(inputId = "VA_max_termfreq_c",label = "max. term frequency",min = 1,step = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.VA_termfreq_type=="prop"',
                              numericInput(inputId = "VA_min_termfreq_p",label = "min. term probability",min = 0,step = 0.01,max = 1,value=NULL),
                              numericInput(inputId = "VA_max_termfreq_p",label = "max. term probability",min = 0.001,step = 0.01,max = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.VA_termfreq_type=="rank"',
                              numericInput(inputId = "VA_min_termfreq_r",label = "min. term rank",min = 1,step = 1,value=NULL),
                              numericInput(inputId = "VA_max_termfreq_r",label = "max. term rank",min = 1,step = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.VA_termfreq_type=="quantile"',
                              numericInput(inputId = "VA_min_termfreq_q",label = "min. term quantile",min = 0,max = 1,step = 0.25,value=NULL),
                              numericInput(inputId = "VA_max_termfreq_q",label = "max. term quantile",min = 0,max = 1,step = 0.25,value=NULL)
             )
      ),
      column(1,
             tags$div(
               selectInput(inputId = "VA_docfreq_type",label = "doc frequency type",choices = c("count","prop","rank","quantile"),multiple = F,selected = "prop")%>%
                 shinyInput_label_embed(
                   shiny_iconlink() %>%
                     bs_embed_popover(
                       title = "Remove words from the analysis that exhibit a specified lower and/or upper bound of document occurrences.
                                      These bound can be specified by a explicit number of document occurrences (count based), by setting a probability (prob),
                                      by using a rank to keep just words within a certain range of document occurrence ranks or by specifying a quantile to cut of.",
                       placement = "right",
                       html="true"
                     )
                 )
             )
      ),
      column(2,
             tags$br(),
             conditionalPanel(condition = 'input.VA_docfreq_type=="count"',
                              numericInput(inputId = "VA_min_docfreq_c",label = "min. doc frequency",min = 0,step = 1,value=NULL),
                              numericInput(inputId = "VA_max_docfreq_c",label = "max. doc frequency",min = 1,step = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.VA_docfreq_type=="prop"',
                              numericInput(inputId = "VA_min_docfreq_p",label = "min. doc probability",min = 0,step = 0.01,max = 1,value=NULL),
                              numericInput(inputId = "VA_max_docfreq_p",label = "max. doc probability",min = 0.001,step = 0.01,max = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.VA_docfreq_type=="rank"',
                              numericInput(inputId = "VA_min_docfreq_r",label = "min. doc rank",min = 1,step = 1,value=NULL),
                              numericInput(inputId = "VA_max_docfreq_r",label = "max. doc rank",min = 1,step = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.VA_docfreq_type=="quantile"',
                              numericInput(inputId = "VA_min_docfreq_q",label = "min. doc quantile",min = 0,max = 1,step = 0.25,value=NULL),
                              numericInput(inputId = "VA_max_docfreq_q",label = "max. doc quantile",min = 0,max = 1,step = 0.25,value=NULL)
             )
      )
      
    ),
    #specific parameters
    tags$hr(),
    tags$h4("Volatility Analysis parameters"),
    fluidRow(
      column(1,
             numericInput(inputId = "VA_min_Cooc_Freq",label = "Minimum Cooccurrence frequency:",value = 2,min = 1,step = 1)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Minimum number of shared occurrences of 2 words to be included in the analysis as a co-occurrence",
                     placement = "right"
                   )
               )
      ),
      column(1,
             selectInput(inputId = "VA_Cooc_Measure",label = "Co-occurrence significance measure",choices = setNames(nm = c("Dice","Mutual Information","Log-likelihood"),c("DICE","MI","LOGLIK")))%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Significance measures for word associations are the basis for measuring context change. 3 different measures are implemented so far: Dice, Mutual Information and Log-Likelihood. ",
                     placement = "right"
                   )
               )
      ),
      column(1,
             selectInput(inputId = "VA_timeintervall",label = HTML("Time intervall <br/> <br/>"),choices = c("day","week","month","year"),selected = "day")%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "The time intervall in which context chagne should be measured.",
                     placement = "right"
                   )
               )),
      column(1,
             numericInput(inputId = "VA_history",label = HTML("Memory <br/> <br/>"),value = 3,min=1,step=1)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "The number of units according to chosen timeintervall, which is used to calculate context change.",
                     placement = "right"
                   )
               )),
      column(1,
             #"sig_vc_window",
             #"sig_sd_window","rank_minmax","rank_no_zero",
             #"rank_max_rank","rank_recommender"
             selectInput(inputId = "VA_method",label = HTML("Method <br/> <br/>"),choices = c("sig_simple","sig_cosine"),selected = "sig_simple")%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Context Volatility can be calculated with different approaches. Here you can choose this method.",
                     placement = "right"
                   )
               )),
      column(1,
             selectInput(inputId = "VA_cooc_type",label = HTML("Co-occurrence window <br/>"),choices = c("Document","Sentence"),selected = "Sentence")%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Specify the co-occurrence window! So far Document and Sentence co-occurrence calculation is implemented",
                     placement = "right"
                   )
               ))
      ),
    fluidRow(
      column(2,
             conditionalPanel(condition='input.VA_method=="sig_simple" || input.VA_method=="sig_cosine"',
                              selectInput(inputId = "VA_weightfactor",label = "Weightfactor",choices = c("linear","exponential"),selected = "linear")%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "This parameter specifies the influence of the individual points in time in the past on the calculation of an expected value with the respective present.
                                      The shorter a point in time lies in the past, the higher its weight for the calculation of the expected value.
                                      If a linear weightfactor is taken, the weights for a set memorysize of 3 could look as follows: (0.5, 0.333, 0.1667).
                                      Using an exponential weighting factor, the weights would be as follows: (0.665, 0.2447, 0.09).",
                                      placement = "right"
                                    )
                                )
                              )
             ),
      column(2,
             selectInput(inputId = "VA_POS_TYPES",label = "POS-Types",
                         choices =c("all","NOUN","VERB","ADJ","PUNCT","SYM","ADP","PART","ADV","INTJ","X") ,selected = "all",multiple = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Should the analysis be limited to words from a certain range of POS-Types. If this is the case make sure to exclude 'all' from the selection.",
                     placement = "right"
                   )
               )
      ),
      column(2,
             selectInput(inputId = "VA_ENTITY_TYPES",label = "NER-Tags",
                         choices =c("all","PERSON","ORG","GPE","PRODUCT","NORP","FACILITY","LOC","EVENT","WORK_OF_ART","LAW",
                                    "LANGUAGE","DATE","TIME","PERCENT","MONEY","QUANTITY","ORDINAL","CARDINAL") ,selected = "all",multiple=T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Should the analysis be limited to words from a certain range of NER-Tags. If this is the case make sure to exclude 'all' from the selection. Using the NER-tag option causes the consolidation of entities.",
                     placement = "right"
                   )
               )
      )
    ),
    tags$hr(),
    bsButton(inputId = "VA_Submit_Script",label = "Submit Request",icon = icon("play-circle"),type = "primary")
  )
})


output$VA_whitelist_UI<-renderUI({
  values$invalidate_whitelists
  if(length(list.files("collections/whitelists/"))==0){
    return(HTML("No whitelists available. You can create whitelist in the Scripts-Whitelist Tab"))
  }
  else{
    return(
      shinyWidgets::prettyRadioButtons(inputId = "VA_whitelist",label = "Whitelists",
                                       choices = stringr::str_replace_all(string = list.files("collections/whitelists/"),pattern = ".txt",replacement = ""),
                                       fill=T,animation = "tada",selected = NULL)
    )
  }
})

output$VA_blacklist_UI<-renderUI({
  values$invalidate_blacklists
  if(length(list.files("collections/blacklists/"))==0){
    return(HTML("No blacklists available. You can create blacklists in the Scripts-Blacklist Tab"))
  }
  else{
    return(
      shinyWidgets::prettyRadioButtons(inputId = "VA_blacklist",label = "Blacklists",
                                       choices = stringr::str_replace_all(string = list.files("collections/blacklists/"),pattern = ".txt",replacement = ""),
                                       fill=T,animation = "tada",selected = NULL)
    )
  }
})


#start volatility analysis script, if submit button is clicked
observeEvent(input$VA_Submit_Script,{
  valid<-check_pruning_parameters(min_t_c = input$VA_min_termfreq_c,max_t_c = input$VA_max_termfreq_c,min_t_p =input$VA_min_termfreq_p,max_t_p =  input$VA_max_termfreq_p
                                  ,min_t_r =input$VA_min_termfreq_r,max_t_r = input$VA_max_termfreq_r,min_t_q = input$VA_min_termfreq_q, max_t_q = input$VA_max_termfreq_q
                                  ,min_d_c = input$VA_min_docfreq_c,max_d_c = input$VA_max_docfreq_c,min_d_p = input$VA_min_docfreq_p,max_d_p = input$VA_max_docfreq_p
                                  ,min_d_r = input$VA_min_docfreq_r,max_d_r = input$VA_max_docfreq_r,min_d_q = input$VA_min_docfreq_q,max_d_q = input$VA_max_docfreq_q)
  if(isFALSE(valid)){
    shinyWidgets::confirmSweetAlert(session = session,title = "Check pruning settings!",text = HTML("It seems your current pruning input parameters don't make sense. It's very likely, that the whole vocabulary will be removed.
                           Check <a href='https://quanteda.io/reference/dfm_trim.html' title='quanteda pruning'> Quanteda Pruning Settings </a>"),html=T,inputId="VA_pruning_continue",
                                    type="warning",closeOnClickOutside = T,btn_labels = c("Change Settings","Continue anyway!"))
  }
  else{
    #get pruning parameters
    if(input$VA_termfreq_type=="count"){
      min_t<-input$VA_min_termfreq_c
      max_t<-input$VA_max_termfreq_c
    }
    if(input$VA_docfreq_type=="count"){
      min_d<-input$VA_min_docfreq_c
      max_d<-input$VA_max_docfreq_c
    }
    if(input$VA_termfreq_type=="rank"){
      min_t<-input$VA_min_termfreq_r
      max_t<-input$VA_max_termfreq_r
    }
    if(input$VA_docfreq_type=="rank"){
      min_d<-input$VA_min_docfreq_r
      max_d<-input$VA_max_docfreq_r
    }
    if(input$VA_termfreq_type=="prop"){
      min_t<-input$VA_min_termfreq_p
      max_t<-input$VA_max_termfreq_p
    }
    if(input$VA_docfreq_type=="prop"){
      min_d<-input$VA_min_docfreq_p
      max_d<-input$VA_max_docfreq_p
    }
    if(input$VA_termfreq_type=="quantile"){
      min_t<-input$VA_min_termfreq_q
      max_t<-input$VA_max_termfreq_q
    }
    if(input$VA_docfreq_type=="quantile"){
      min_d<-input$VA_min_docfreq_q
      max_d<-input$VA_max_docfreq_q
    }
    if(is.null(min_t))min_t<-NA
    if(is.null(max_t))max_t<-NA
    if(is.null(min_d))min_d<-NA
    if(is.null(max_d))max_d<-NA
    #save needed parameters
    parameters<-list(collection=input$collection_selected,
                     baseform_reduction=input$VA_baseform,
                     min_char=input$VA_min_char,
                     ngrams=input$VA_ngram,
                     remove_stopwords=input$VA_remove_stopwords,
                     lowercase=input$VA_lowercase,
                     remove_numbers=input$VA_remove_numbers,
                     remove_numbers_all=input$VA_remove_numbers_all,
                     remove_punctuation=input$VA_remove_punctuation,
                     remove_hyphenation=input$VA_remove_hyphenation,
                     min_term=min_t,
                     max_term=max_t,
                     min_document=min_d,
                     max_document=max_d,
                     min_cooc_freq=input$VA_min_Cooc_Freq,
                     va_timeintervall=input$VA_timeintervall,
                     va_history=input$VA_history,
                     va_method=input$VA_method,
                     cooc_significance_measure=input$VA_Cooc_Measure,
                     cooc_window=input$VA_cooc_type,
                     remove_custom=input$VA_remove_custom,
                     consolidate_entities=input$VA_consolidate_entities,
                     blacklist=input$VA_blacklist,
                     reduce_POS=input$VA_POS_TYPES,
                     reduce_NER=input$VA_ENTITY_TYPES,
                     termfreq_type=input$VA_termfreq_type,
                     docfreq_type=input$VA_docfreq_type,
                     va_weightfactor=input$VA_weightfactor,
                     va_method=input$VA_method,
                     keep_custom=input$VA_keep_custom,
                     use_blacklist=input$VA_use_custom_blacklist,
                     use_whitelist=input$VA_use_custom_whitelist,
                     whitelist=input$VA_whitelist,
                     whitelist_only=input$VA_whitelist_only,
                     whitelist_expand=input$VA_whitelist_expand,
                     whitelist_only_results=input$VA_whitelist_only_results
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
      system(paste0('Rscript collections/scripts/Volatility_Analysis/',input$custom_script_options,' &'))
    }
    else{
      system(paste('Rscript collections/scripts/Volatility_Analysis_Script.R','&'))
      #show modal when process is started
      showModal(modalDialog(
        title = "Process started",
        "The process was succesfully started. Check details in 'My Tasks'."
      ))
    }
  }
})


#start script after continue anyway is clicked even though pruning settings seem to be wrong
observeEvent(input$VA_pruning_continue,ignoreInit = T,{
  validate(
    need(isTRUE(input$VA_pruning_continue),message=F)
  )
  #get pruning parameters
  if(input$VA_termfreq_type=="count"){
    min_t<-input$VA_min_termfreq_c
    max_t<-input$VA_max_termfreq_c
  }
  if(input$VA_docfreq_type=="count"){
    min_d<-input$VA_min_docfreq_c
    max_d<-input$VA_max_docfreq_c
  }
  if(input$VA_termfreq_type=="rank"){
    min_t<-input$VA_min_termfreq_r
    max_t<-input$VA_max_termfreq_r
  }
  if(input$VA_docfreq_type=="rank"){
    min_d<-input$VA_min_docfreq_r
    max_d<-input$VA_max_docfreq_r
  }
  if(input$VA_termfreq_type=="prop"){
    min_t<-input$VA_min_termfreq_p
    max_t<-input$VA_max_termfreq_p
  }
  if(input$VA_docfreq_type=="prop"){
    min_d<-input$VA_min_docfreq_p
    max_d<-input$VA_max_docfreq_p
  }
  if(input$VA_termfreq_type=="quantile"){
    min_t<-input$VA_min_termfreq_q
    max_t<-input$VA_max_termfreq_q
  }
  if(input$VA_docfreq_type=="quantile"){
    min_d<-input$VA_min_docfreq_q
    max_d<-input$VA_max_docfreq_q
  }
  if(is.null(min_t))min_t<-NA
  if(is.null(max_t))max_t<-NA
  if(is.null(min_d))min_d<-NA
  if(is.null(max_d))max_d<-NA
  #save needed parameters
  parameters<-list(collection=input$collection_selected,
                   baseform_reduction=input$VA_baseform,
                   min_char=input$VA_min_char,
                   ngrams=input$VA_ngram,
                   remove_stopwords=input$VA_remove_stopwords,
                   lowercase=input$VA_lowercase,
                   remove_numbers=input$VA_remove_numbers,
                   remove_numbers_all=input$VA_remove_numbers_all,
                   remove_punctuation=input$VA_remove_punctuation,
                   remove_hyphenation=input$VA_remove_hyphenation,
                   min_term=min_t,
                   max_term=max_t,
                   min_document=min_d,
                   max_document=max_d,
                   min_cooc_freq=input$VA_min_Cooc_Freq,
                   va_timeintervall=input$VA_timeintervall,
                   va_history=input$VA_history,
                   va_method=input$VA_method,
                   cooc_significance_measure=input$VA_Cooc_Measure,
                   cooc_window=input$VA_cooc_type,
                   remove_custom=input$VA_remove_custom,
                   consolidate_entities=input$VA_consolidate_entities,
                   blacklist=input$VA_blacklist,
                   reduce_POS=input$VA_POS_TYPES,
                   reduce_NER=input$VA_ENTITY_TYPES,
                   termfreq_type=input$VA_termfreq_type,
                   docfreq_type=input$VA_docfreq_type,
                   va_weightfactor=input$VA_weightfactor,
                   va_method=input$VA_method,
                   keep_custom=input$VA_keep_custom,
                   use_blacklist=input$VA_use_custom_blacklist,
                   use_whitelist=input$VA_use_custom_whitelist,
                   whitelist=input$VA_whitelist,
                   whitelist_only=input$VA_whitelist_only,
                   whitelist_expand=input$VA_whitelist_expand,
                   whitelist_only_results=input$VA_whitelist_only_results
  )
  #create process ID
  ID<-get_task_id_counter()+1
  set_task_id_counter(ID)
  #save metadata for process
  process_info<-list(ID,isolate(input$collection_selected),isolate(input$analysis_selected),as.character(Sys.time()))
  #save logfile path
  logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
  #create logfile
  write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0( "Collection: <b>",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
  #save data needed in script execution 
  save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
  #start script
  if(input$use_custom_script==TRUE && !is.null(input$custom_script_options)){
    shinyWidgets::sendSweetAlert(session=session,title = "Starting a custom script",text = "You are about to start a custom script. Caution with the calculation and results!",type = "info")
    system(paste0('Rscript collections/scripts/Volatility_Analysis/',input$custom_script_options,' &'))
  }
  else{
    system(paste('Rscript collections/scripts/Volatility_Analysis_Script.R','&'))
    #show modal when process is started
    showModal(modalDialog(
      title = "Process started",
      "The process was succesfully started. Check details in 'My Tasks'."
    ))
  }
})
