getTagListStanddardParameters <- function(){
  tagListStandardParameters <- tagList(
    tags$hr(),
    ####################
    #standard parameters
    ####################
    fluidRow(
      column(1,
             selectInput(inputId = "param_baseform",label = "Baseform Reduction",choices = c("lemma","stemming","none"),selected = "none")#,
             #shinyBS::bsPopover(id = "param_baseform", title = "Specify whether and how to do baseform reduction. The options are stemming, lemmatization or no baseorm reduction.",
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
             sliderInput(inputId = "param_min_char",label = "Min #chars for words",value = c(2,50),min = 1,step = 1,max = 100)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Set the minimum and maximum number of characters of a word to be included in the analysis.", placement = "right"
                     ,html=T)
               )),
      column(1,
             selectInput(inputId = "param_ngram",label = "N-grams",choices = c(1,2,3),selected = 1,multiple = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Should n-grams be included in the analysis? They will be connected with '_'.", placement = "right"
                   )
               )),
      column(2,
             textInput(inputId = "param_remove_custom",label = HTML("Remove custom words"),placeholder ="Add words (Seperated by ,)")%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Delete specific words from the analysis. Seperate them with ','.", placement = "right"
                   )
               )
      ),
      column(2,
             textInput(inputId = "param_keep_custom",label = HTML("Keep custom words"),placeholder ="Add words (Seperated by ,)")%>%
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
             checkboxInput(inputId = "param_remove_stopwords",label = "Remove Stopwords",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Should stopwords be removed from the analysis? The stopwords of the language specified during dataimport for this dataset will be used.", placement = "top"
                   )
               )),
      column(1,
             checkboxInput(inputId = "param_lowercase",label = "Transform tokens to lowercase?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Should lowercasing (set all characters to lower case) be applied?", placement = "left"
                   )
               )),
      column(1,
             checkboxInput(inputId = "param_remove_numbers",label = "Remove Numbers?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Remove types which consist of numbers only from the analysis.", placement = "bottom"
                   )
               )),
      column(1,
             checkboxInput(inputId = "param_remove_numbers_all",label = "Remove everything containing a number number?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Remove words which are composed of at least one number.", placement = "right"
                   )
               )),
      column(1,
             checkboxInput(inputId = "param_remove_punctuation",label = "Remove Punctuation?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Remove words, which reflect punctuation from the analysis.", placement = "right"
                   )
               )),
      column(1,
             checkboxInput(inputId = "param_remove_hyphenation",label = "Remove Hyphenation?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Remove words, which reflect hyphens from the analysis.", placement = "right"
                   )
               )),
      column(1,
             checkboxInput(inputId = "param_consolidate_entities",label = "Consolidate Entities?",value = F)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Consolidate found entities by binding them together with '_' and treat them as a single word afterwards.", placement = "right"
                   )
               ))
    ),
    fluidRow(
      column(1,
             checkboxInput(inputId = "param_use_custom_blacklist",label = "use custom blacklist?",value = F)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Use one of the specified blacklists to remove a set of words from the analysis.", placement = "right"
                   )
               )
      ),
      column(2,
             conditionalPanel(condition = "input.param_use_custom_blacklist==true",
                              uiOutput(outputId = "blacklist_UI")
             )
      ),
      column(1,
             checkboxInput(inputId = "param_use_custom_whitelist",label = "use custom whitelist?",value = F)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Use of the specified whitelists to protect a set of words from the removal during the pre-processing or to calculate the analysis for these words exclusivly.", placement = "right"
                   )
               )
      ),
      column(2,
             conditionalPanel(condition = "input.param_use_custom_whitelist==true",
                              uiOutput(outputId = "whitelist_UI")
             )
      )
    ),
    fluidRow(
      conditionalPanel(condition = "input.param_use_custom_whitelist==true || input.param_keep_custom.length>=1",
                       tags$br(),
                       tags$h5("Whitelist Options:"),
                       column(1,
                              checkboxInput(inputId = "param_whitelist_only",label = "Exclude all words apart from the whitelist entries",value = FALSE)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Should all words apart from the entries in the whitelist be excluded from the analysis?", placement = "right"
                                    )
                                )
                       ),
                       column(1,
                              conditionalPanel(condition = "(input.param_use_custom_whitelist==true || input.param_keep_custom.length>=1) && (input.param_ngram.includes('2') || input.param_ngram.includes('3'))",
                                               checkboxInput(inputId = "param_whitelist_expand",label = "Expand whitelist?",value = FALSE)%>%
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
               selectInput(inputId = "param_termfreq_type",label = "term frequency type",choices = c("count","prop","rank","quantile"),multiple = F,selected = "count")%>%
                 shinyInput_label_embed(
                   shiny_iconlink() %>%
                     bs_embed_popover(
                       title = "Remove words from the analysis that exhibit a specified lower and/or upper bound of occurrences.
                       These bounds can be specified by an explicit number of occurrences (count based), by setting a probability (prob),
                       by using a rank to keep just words within a certain range of occurrence ranks or by specifying a quantile to cut of.",
                       placement = "right",
                       html="true"
                     )
                 )
             )
             ),
      column(2,
             tags$br(),
             conditionalPanel(condition = 'input.param_termfreq_type=="count"',
                              numericInput(inputId = "param_min_termfreq_c",label = "min. term frequency",min = 0,step = 1,value=NULL),
                              numericInput(inputId = "param_max_termfreq_c",label = "max. term frequency",min = 1,step = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.param_termfreq_type=="prop"',
                              numericInput(inputId = "param_min_termfreq_p",label = "min. term probability",min = 0,step = 0.01,max = 1,value=NULL),
                              numericInput(inputId = "param_max_termfreq_p",label = "max. term probability",min = 0.001,step = 0.01,max = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.param_termfreq_type=="rank"',
                              numericInput(inputId = "param_min_termfreq_r",label = "min. term rank",min = 1,step = 1,value=NULL),
                              numericInput(inputId = "param_max_termfreq_r",label = "max. term rank",min = 1,step = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.param_termfreq_type=="quantile"',
                              numericInput(inputId = "param_min_termfreq_q",label = "min. term quantile",min = 0,max = 1,step = 0.25,value=NULL),
                              numericInput(inputId = "param_max_termfreq_q",label = "max. term quantile",min = 0,max = 1,step = 0.25,value=NULL)
             )
      ),
      column(1,
             tags$div(
               selectInput(inputId = "param_docfreq_type",label = "doc frequency type",choices = c("count","prop","rank","quantile"),multiple = F,selected = "prop")%>%
                 shinyInput_label_embed(
                   shiny_iconlink() %>%
                     bs_embed_popover(
                       title = "Remove words from the analysis that exhibit a specified lower and/or upper bound of document occurrences.
                       These bounds can be specified by an explicit number of document occurrences (count based), by setting a probability (prob),
                       by using a rank to keep just words within a certain range of document occurrence ranks or by specifying a quantile to cut of.",
                       placement = "right",
                       html="true"
                     )
                 )
             )
             ),
      column(2,
             tags$br(),
             conditionalPanel(condition = 'input.param_docfreq_type=="count"',
                              numericInput(inputId = "param_min_docfreq_c",label = "min. doc frequency",min = 0,step = 1,value=NULL),
                              numericInput(inputId = "param_max_docfreq_c",label = "max. doc frequency",min = 1,step = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.param_docfreq_type=="prop"',
                              numericInput(inputId = "param_min_docfreq_p",label = "min. doc probability",min = 0,step = 0.01,max = 1,value=NULL),
                              numericInput(inputId = "param_max_docfreq_p",label = "max. doc probability",min = 0.001,step = 0.01,max = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.param_docfreq_type=="rank"',
                              numericInput(inputId = "param_min_docfreq_r",label = "min. doc rank",min = 1,step = 1,value=NULL),
                              numericInput(inputId = "param_max_docfreq_r",label = "max. doc rank",min = 1,step = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.param_docfreq_type=="quantile"',
                              numericInput(inputId = "param_min_docfreq_q",label = "min. doc quantile",min = 0,max = 1,step = 0.25,value=NULL),
                              numericInput(inputId = "param_max_docfreq_q",label = "max. doc quantile",min = 0,max = 1,step = 0.25,value=NULL)
             )
      )
      
             )  # end Pruning  
    #,
    # tags$hr(),
    # tags$h4("POS & NER"),
    # fluidRow(
    #   column(2,
    #          selectInput(inputId = "param_POS_TYPES",label = "POS-Types",
    #                      choices =c("all","NOUN","VERB","ADJ","PUNCT","SYM","ADP","PART","ADV","INTJ","X") ,selected = "all",multiple = T)%>%
    #            shinyInput_label_embed(
    #              shiny_iconlink() %>%
    #                bs_embed_popover(
    #                  title = "Should the analysis be limited to words from a certain range of POS-Types. If this is the case make sure to exclude 'all' from the selection.",
    #                  placement = "right"
    #                )
    #            )
    #   ),
    #   column(2,
    #          selectInput(inputId = "param_ENTITY_TYPES",label = "NER-Tags",
    #                      choices =c("all","PERSON","ORG","GPE","PRODUCT","NORP","FACILITY","LOC","EVENT","WORK_OF_ART","LAW",
    #                                 "LANGUAGE","DATE","TIME","PERCENT","MONEY","QUANTITY","ORDINAL","CARDINAL") ,selected = "all",multiple=T)%>%
    #            shinyInput_label_embed(
    #              shiny_iconlink() %>%
    #                bs_embed_popover(
    #                  title = "Should the analysis be limited to words from a certain range of NER-Tags. If this is the case make sure to exclude 'all' from the selection. Using the NER-tag option causes the consolidation of entities.",
    #                  placement = "right"
    #                )
    #            )
    #   )
    #   
    # ) # end POS & NER filtering
  )
}

######
# POS & NER
#############

getTagListParametersPOSandNER <- function(){
  tagListPOSandNER <- tagList(
    tags$hr(),
    tags$h4("POS & NER"),
    fluidRow(
      column(2,
             selectInput(inputId = "param_POS_TYPES",label = "POS-Types",
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
             selectInput(inputId = "param_ENTITY_TYPES",label = "NER-Tags",
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
      
    ) # end POS & NER filtering
  )
}

standard_whitelist_UI<-renderUI({
  values$invalidate_whitelists
  if(length(list.files("collections/whitelists/"))==0){
    return(HTML("No whitelists available. You can create whitelist in the Scripts-Whitelist Tab"))
  }
  else{
    return(
      shinyWidgets::prettyRadioButtons(inputId = "whitelist",label = "Whitelists",
                                       choices = stringr::str_replace_all(string = list.files("collections/whitelists/"),pattern = ".txt",replacement = ""),
                                       fill=T,animation = "tada",selected = NULL)
    )
  }
})

standard_blacklist_UI<-renderUI({
  values$invalidate_blacklists
  if(length(list.files("collections/blacklists/"))==0){
    return(HTML("No blacklists available. You can create blacklists in the Scripts-Blacklist Tab"))
  }
  else{
    return(
      shinyWidgets::prettyRadioButtons(inputId = "blacklist",label = "Blacklists",
                                       choices = stringr::str_replace_all(string = list.files("collections/blacklists/"),pattern = ".txt",replacement = ""),
                                       fill=T,animation = "tada",selected = NULL)
    )
  }
})



#' Title
#'
#' @param functionToSetParametersIndependentOnUIinput A function to set e.g. fixed parameters which are not available via input (e.g. for geocoding, parameter NER needs to be set to LOC and GPE)
#' @param functionToAppendAdditionalParameters a function getting the input and retrieves further additional parameters (will be appended to the standard parameters)
#' @param filepathForScript e.g. 'Rscript collections/scripts/Topic_Model/'
#' @param folderpathForCustomScripts e.g. 'Rscript collections/scripts/Topic_Model/'
#'
#' @return
#' @export
#'
#' @examples
standardProcessingAfterSubmittingTask <- function(input, functionToSetParametersIndependentOnUIinput, functionToAppendAdditionalParameters, filepathForScript, folderpathForCustomScripts){
  
  valid<-check_pruning_parameters(min_t_c = input$param_min_termfreq_c,max_t_c = input$param_max_termfreq_c,min_t_p =input$param_min_termfreq_p,max_t_p =  input$param_max_termfreq_p
                                  ,min_t_r =input$param_min_termfreq_r,max_t_r = input$param_max_termfreq_r,min_t_q = input$param_min_termfreq_q, max_t_q = input$param_max_termfreq_q
                                  ,min_d_c = input$param_min_docfreq_c,max_d_c = input$param_max_docfreq_c,min_d_p = input$param_min_docfreq_p,max_d_p = input$param_max_docfreq_p
                                  ,min_d_r = input$param_min_docfreq_r,max_d_r = input$param_max_docfreq_r,min_d_q = input$param_min_docfreq_q,max_d_q = input$param_max_docfreq_q)
  if(isFALSE(valid)){
    shinyWidgets::confirmSweetAlert(session = session,title = "Check pruning settings!",text = HTML("It seems your current pruning input parameters don't make sense. It's very likely, that the whole vocabulary will be removed.
                                                                                                    Check <a href='https://quanteda.io/reference/dfm_trim.html' title='quanteda pruning'> Quanteda Pruning Settings </a>"),html=T,inputId="param_pruning_continue",
                                    type="warning",closeOnClickOutside = T,btn_labels = c("Change Settings","Continue anyway!"))
  }
  else{
    #get pruning parameters
    if(input$param_termfreq_type=="count"){
      min_t<-input$param_min_termfreq_c
      max_t<-input$param_max_termfreq_c
    }
    if(input$param_docfreq_type=="count"){
      min_d<-input$param_min_docfreq_c
      max_d<-input$param_max_docfreq_c
    }
    if(input$param_termfreq_type=="rank"){
      min_t<-input$param_min_termfreq_r
      max_t<-input$param_max_termfreq_r
    }
    if(input$param_docfreq_type=="rank"){
      min_d<-input$param_min_docfreq_r
      max_d<-input$param_max_docfreq_r
    }
    if(input$param_termfreq_type=="prop"){
      min_t<-input$param_min_termfreq_p
      max_t<-input$param_max_termfreq_p
    }
    if(input$param_docfreq_type=="prop"){
      min_d<-input$param_min_docfreq_p
      max_d<-input$param_max_docfreq_p
    }
    if(input$param_termfreq_type=="quantile"){
      min_t<-input$param_min_termfreq_q
      max_t<-input$param_max_termfreq_q
    }
    if(input$param_docfreq_type=="quantile"){
      min_d<-input$param_min_docfreq_q
      max_d<-input$param_max_docfreq_q
    }
    if(is.null(min_t))min_t<-NA
    if(is.null(max_t))max_t<-NA
    if(is.null(min_d))min_d<-NA
    if(is.null(max_d))max_d<-NA
    #save needed parameters
    parameters<-list(collection=input$collection_selected,
                     baseform_reduction=input$param_baseform,
                     min_char=input$param_min_char,
                     ngrams=input$param_ngram,
                     remove_stopwords=input$param_remove_stopwords,
                     lowercase=input$param_lowercase,
                     remove_numbers=input$param_remove_numbers,
                     remove_numbers_all=input$param_remove_numbers_all,
                     remove_punctuation=input$param_remove_punctuation,
                     remove_hyphenation=input$param_remove_hyphenation,
                     min_term=min_t,
                     max_term=max_t,
                     min_document=min_d,
                     max_document=max_d,
                     min_cooc_freq=input$param_min_Cooc_Freq,
                     remove_custom=input$param_remove_custom,
                     consolidate_entities=input$param_consolidate_entities,
                     blacklist=input$param_blacklist,
                     reduce_POS=input$param_POS_TYPES,
                     reduce_NER=input$param_ENTITY_TYPES,
                     termfreq_type=input$param_termfreq_type,
                     docfreq_type=input$param_docfreq_type,
                     keep_custom=input$param_keep_custom,
                     use_blacklist=input$param_use_custom_blacklist,
                     use_whitelist=input$param_use_custom_whitelist,
                     whitelist=input$param_whitelist,
                     whitelist_expand=input$param_whitelist_expand,
                     whitelist_only=input$param_whitelist_only,
                     # param_number_of_topics=input$param_number_of_topics,
                     # param_alpha=input$param_alpha,
                     # param_method=input$param_method,
                     param_detailed_meta=input$param_detailed_meta_dist
    )

    if(!is.null(functionToSetParametersIndependentOnUIinput)){
      # set specific parameters
      parameters <- functionToSetParametersIndependentOnUIinput(parameters)
    }
    
    if(!is.null(functionToAppendAdditionalParameters)){
      #add further parameters
      specific_parameters <- functionToAppendAdditionalParameters(input) # returns list
      parameters <- c(parameters, specific_parameters)
    }
    
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
    #print("save parameters")
    save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
    #start script
    if(input$use_custom_script==TRUE && !is.null(input$custom_script_options)){
      shinyWidgets::sendSweetAlert(session=session,title = "Starting a custom script",text = "You are about to start a custom script. Caution with the calculation and results!",type = "info")
      system(paste0(folderpathForCustomScripts,input$custom_script_options,' &'))
    }
    else{
      system(paste(filepathForScript,'&'))
      #show modal when process is started
      showModal(modalDialog(
        title = "Process started",
        "The process was succesfully started. Check details in 'My Tasks'."
      ))
    }
  
  }
}