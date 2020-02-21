
output$Analysis_Parameter_CL<-renderUI({
  tagList(
    tags$hr(),
    #standard parameters
    fluidRow(
      column(1,
             selectInput(inputId = "CL_baseform",label = "Baseform Reduction",choices = c("lemma","stemming","none"),selected = "none") %>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Specify whether and how to do baseform reduction. The options are stemming, lemmatization or no baseorm reduction.", placement = "right"
                   )
               )
      ),
      column(2,
             sliderInput(inputId = "CL_min_char",label = "Min #chars for words",value = c(2,50),min = 1,step = 1,max = 100)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Set the minimum and maximum number of characters of a word to be included in the analysis.", placement = "right"
                     ,html=T))
      ),
      column(2,
             selectInput(inputId = "CL_ngram",label = "ngrams?",choices = c(1,2,3),selected = 1,multiple = T) %>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Caution! Here you decide if you want to use uni- bi- or trigrams for the classification. They will be connected with '_'.", placement = "right"
                   )
               )
      ),
      column(2,
             textInput(inputId = "CL_remove_custom",label = HTML("remove custom words"),placeholder ="Add words (Seperated by ,)")%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Delete specific words from the analysis. Seperate them with ','.", placement = "right"
                   )
               )
      ),
      column(2,
             textInput(inputId = "CL_keep_custom",label = HTML("Keep custom words"),placeholder ="Add words (Seperated by ,)")%>%
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
             checkboxInput(inputId = "CL_remove_stopwords",label = "Remove Stopwords",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Should stopwords be removed from the analysis? The stopwords of the language specified during dataimport for this dataset will be used.", placement = "top"
                   )
               )),
      column(1,
             checkboxInput(inputId = "CL_lowercase",label = "Transform tokens to lowercase?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Should lowercasing (set all characters to lower case) be applied?", placement = "left"
                   )
               )),
      column(1,
             checkboxInput(inputId = "CL_remove_numbers",label = "Remove Numbers?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Remove types which consist of numbers only from the analysis.", placement = "bottom"
                   )
               )),
      column(1,
             checkboxInput(inputId = "CL_remove_numbers_all",label = "Remove everything containing a number number?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Remove words which are composed of at least one number.", placement = "right"
                   )
               )),
      column(1,
             checkboxInput(inputId = "CL_remove_punctuation",label = "Remove Punctuation?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Remove words, which reflect punctuation from the analysis.", placement = "right"
                   )
               )),
      column(1,
             checkboxInput(inputId = "CL_remove_hyphenation",label = "Remove Hyphenation?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Remove words, which reflect hyphens from the analysis.", placement = "right"
                   )
               ))
    ),
    fluidRow(
      column(1,
             checkboxInput(inputId = "CL_use_custom_blacklist",label = "use custom blacklist?",value = F)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Use one of the specified blacklists to remove a set of words from the analysis.", placement = "right"
                   )
               )
      ),
      column(2,
             conditionalPanel(condition = "input.CL_use_custom_blacklist==true",
                              uiOutput(outputId = "CL_blacklist_UI")
             )
      ),
      column(1,
             checkboxInput(inputId = "CL_use_custom_whitelist",label = "use custom whitelist?",value = F)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Use of the specified whitelists to protect a set of words from the removal during the pre-processing or to calculate the analysis for these words exclusivly.", placement = "right"
                   )
               )
      ),
      column(2,
             conditionalPanel(condition = "input.CL_use_custom_whitelist==true",
                              uiOutput(outputId = "CL_whitelist_UI")
             )
      )
    ),
    fluidRow(
      conditionalPanel(condition = "input.CL_use_custom_whitelist==true || input.CL_keep_custom.length>=1",
                       tags$br(),
                       tags$h5("Whitelist Options:"),
                       column(1,
                              checkboxInput(inputId = "CL_whitelist_only",label = "Exclude all words apart from the whitelist entries",value = FALSE)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Should all words apart from the entries in the whitelist be excluded from the analysis?", placement = "right"
                                    )
                                )
                       ),
                       column(1,
                              conditionalPanel(condition = "(input.CL_use_custom_whitelist==true || input.CL_keep_custom.length>=1) && (input.CL_ngram.includes('2') || input.CL_ngram.includes('3'))",
                                               checkboxInput(inputId = "CL_whitelist_expand",label = "Expand whitelist?",value = FALSE)%>%
                                                 shinyInput_label_embed(
                                                   shiny_iconlink() %>%
                                                     bs_embed_popover(
                                                       title = "Should the words whitelist be expaned using n-grams? ", placement = "right"
                                                     )
                                                 )
                              )  
                              
                       )
      )
    ),
    fluidRow(
      column(2,
             selectInput(inputId = "CL_POS_TYPES",label = "POS-Types",
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
             selectInput(inputId = "CL_ENTITY_TYPES",label = "NER-Tags",
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
    tags$h4("Pruning"),
    fluidRow(
      column(1,
             selectInput(inputId = "CL_termfreq_type",label = "term frequency type",choices = c("count","prop","rank","quantile"),multiple = F,selected = "count")%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Remove words from the analysis that exhibit a specified lower and/or upper bound of occurrences.
                                      These bound can be specified by a explicit number of occurrences (count based), by setting a probability (prob),
                                      by using a rank to keep just words within a certain range of occurrence ranks or by specifying a quantile to cut of.",
                     placement = "right",
                     html="true"
                   )
               )),
      column(2,
             tags$br(),
             conditionalPanel(condition = 'input.CL_termfreq_type=="count"',
                              numericInput(inputId = "CL_min_termfreq_c",label = "min. term frequency",min = 0,step = 1,value=NULL),
                              numericInput(inputId = "CL_max_termfreq_c",label = "max. term frequency",min = 1,step = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.CL_termfreq_type=="prop"',
                              numericInput(inputId = "CL_min_termfreq_p",label = "min. term probability",min = 0,step = 0.01,max = 1,value=NULL),
                              numericInput(inputId = "CL_max_termfreq_p",label = "max. term probability",min = 0.001,step = 0.01,max = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.CL_termfreq_type=="rank"',
                              numericInput(inputId = "CL_min_termfreq_r",label = "min. term rank",min = 1,step = 1,value=NULL),
                              numericInput(inputId = "CL_max_termfreq_r",label = "max. term rank",min = 1,step = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.CL_termfreq_type=="quantile"',
                              numericInput(inputId = "CL_min_termfreq_q",label = "min. term quantile",min = 0,max = 1,step = 0.25,value=NULL),
                              numericInput(inputId = "CL_max_termfreq_q",label = "max. term quantile",min = 0,max = 1,step = 0.25,value=NULL)
             )
      ),
      column(1,
             selectInput(inputId = "CL_docfreq_type",label = "doc frequency type",choices = c("count","prop","rank","quantile"),multiple = F,selected = "prop")%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Remove words from the analysis that exhibit a specified lower and/or upper bound of document occurrences.
                                      These bound can be specified by a explicit number of document occurrences (count based), by setting a probability (prob),
                                      by using a rank to keep just words within a certain range of document occurrence ranks or by specifying a quantile to cut of.",
                     placement = "right",
                     html="true"
                   )
               )),
      column(2,
             tags$br(),
             conditionalPanel(condition = 'input.CL_docfreq_type=="count"',
                              numericInput(inputId = "CL_min_docfreq_c",label = "min. doc frequency",min = 0,step = 1,value=NULL),
                              numericInput(inputId = "CL_max_docfreq_c",label = "max. doc frequency",min = 1,step = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.CL_docfreq_type=="prop"',
                              numericInput(inputId = "CL_min_docfreq_p",label = "min. doc probability",min = 0,step = 0.01,max = 1,value=NULL),
                              numericInput(inputId = "CL_max_docfreq_p",label = "max. doc probability",min = 0.001,step = 0.01,max = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.CL_docfreq_type=="rank"',
                              numericInput(inputId = "CL_min_docfreq_r",label = "min. doc rank",min = 1,step = 1,value=NULL),
                              numericInput(inputId = "CL_max_docfreq_r",label = "max. doc rank",min = 1,step = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.CL_docfreq_type=="quantile"',
                              numericInput(inputId = "CL_min_docfreq_q",label = "min. doc quantile",min = 0,max = 1,step = 0.25,value=NULL),
                              numericInput(inputId = "CL_max_docfreq_q",label = "max. doc quantile",min = 0,max = 1,step = 0.25,value=NULL)
             )
      )
      
    ),
    #specific parameters
    tags$hr(),
    tags$h4("Classification parameters"),
    fluidRow(
      column(2,
             selectizeInput(inputId = "CL_Context_Unit",label="Context Unit",choices=c("Sentence","Document"))%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "What is the context unit for this classification? Documentwise or sentencewise classification are the available options so far.",
                     placement = "right",
                     html="true"
                   )
               )
      ),
      column(2,
             selectizeInput(inputId = "CL_Project",label="Project",choices=stringr::str_replace_all(string = list.files("collections/annotation_schemes/"),pattern = ".RData",replacement = ""),selected=character(0))%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Which classification project should be used? New projects can be created in 'Categories > Create Annotation Set'.",
                     placement = "right",
                     html="true"
                   )
               )
      ),
      column(3,
             selectizeInput(inputId = "CL_Mode",label="Mode",
                            choices=c("Produce 50 new active learning examples",
                                      "Active learning on whole documents",
                                      "Evaluate Training Set",
                                      "Classify on entire collection"))
      ),
      conditionalPanel(condition='input.CL_Mode!="Evaluate Training Set"',
                       column(1,
                              numericInput(inputId = "CL_c",label = "c Parameter",value = 1,step = 0.1)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "The C parameter tells the SVM optimization how much you want to avoid misclassifying each training example.
                     For large values of C, the optimization will choose a smaller-margin hyperplane if that hyperplane does a better job of getting all the training points classified correctly.
                     Conversely, a very small value of C will cause the optimizer to look for a larger-margin separating hyperplane, even if that hyperplane misclassifies more points",
                                      placement = "right",
                                      html="true"
                                    )
                                )
                       )
      ),
      conditionalPanel(condition='input.CL_Mode=="Classify on entire collection"',
                       column(2,
                              numericInput(inputId = "CL_Threshold",label = "Positive score threshold",value = 0.8,min = 0,max=1)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "How high must the certainty be so that a document/sentence belongs to the respective class?",
                                      placement = "right",
                                      html="true"
                                    )
                                )
                       )
      )
    ),
    fluidRow(
      conditionalPanel(condition='input.CL_Mode=="Produce 50 new active learning examples"',
                       column(2,
                              uiOutput(outputId = "CL_UI_Category")
                       ),
                       column(2,
                              selectInput(inputId = "CL_active_learning_strategy",label = "Strategy to pick learning examples",choices = c("Least confidence"="LC","Most confidence"="MC","Least confidence with bias"="LCB"),multiple = F)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Specify the strategy on which the document selection for the active learning examples is based on.",
                                      placement = "right",
                                      html="true"
                                    )
                                )
                       )
      ),
      column(1,
             conditionalPanel(condition='input.CL_Mode=="Produce 50 new active learning examples"',
                              checkboxInput(inputId = "CL_use_dict",label = "Use Dictionary?",value = FALSE)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Do you wish to use a dictioanry to create the active learning examples. The specified dictionaries column names and the categories for that project have to match exactly.",
                                      placement = "right",
                                      html="true"
                                    )
                                )
             )
      ),
      column(7,
             conditionalPanel(condition='input.CL_use_dict==true && input.CL_Mode=="Produce 50 new active learning examples"',
                              uiOutput("CL_dict_ui")
             )
      )
    ),
    bsButton(inputId = "CL_Submit_Script",label = "Submit Request",icon = icon("play-circle"),type = "primary")
  )
})

observeEvent(input$CL_Context_Unit,ignoreInit = T,{
  selected<-input$CL_Mode
  if(input$CL_Context_Unit=="Sentence"){
    choices = c("Produce 50 new active learning examples","Active learning on whole documents","Evaluate Training Set","Classify on entire collection")
    if(selected%in%choices){
      updateSelectizeInput(session = session,inputId = "CL_Mode",choices=choices,selected=selected)
    }
    else{
      updateSelectizeInput(session = session,inputId = "CL_Mode",choices=choices)
    }
  }
  else{
    choices = c("Produce 50 new active learning examples","Evaluate Training Set","Classify on entire collection")
    if(selected%in%choices){
      updateSelectizeInput(session = session,inputId = "CL_Mode",choices=choices,selected=selected)
    }
    else{
      updateSelectizeInput(session = session,inputId = "CL_Mode",choices=choices)
    }
  }
})


#render categories for chosen project
output$CL_UI_Category<-renderUI({
  if(length(input$CL_Project)>0){
    load(paste0("collections/annotation_schemes/",input$CL_Project,".RData"))
    names<-names(unlist(anno))
    anno<-unlist(anno)
    name_tag<-names[which(grepl(pattern = paste0(NULL,".name"),x = names))]
    name<-anno[name_tag]
    ui<-selectizeInput(inputId = "CL_Category",label="Category",choices=as.character(name))%>%
      shinyInput_label_embed(
        shiny_iconlink() %>%
          bs_embed_popover(
            title = "For which category do you wish to generate active learning examples for?",
            placement = "right",
            html="true"
          )
      )
    return(ui)
  }
})

output$CL_dict_ui<-renderUI({
  values$update_dicts
  shinyWidgets::prettyRadioButtons(inputId = "CL_dict",label = "Dictionaries",
                                   choices = stringr::str_replace_all(string = list.files("collections/dictionaries/"),pattern = ".RData",replacement = ""),
                                   fill=T,animation = "tada",selected = NULL,inline = T)  
  
})



output$CL_whitelist_UI<-renderUI({
  values$invalidate_whitelists
  if(length(list.files("collections/whitelists/"))==0){
    return(HTML("No whitelists available. You can create whitelist in the Scripts-Whitelist Tab"))
  }
  else{
    return(
      shinyWidgets::prettyRadioButtons(inputId = "CL_whitelist",label = "Whitelists",
                                       choices = stringr::str_replace_all(string = list.files("collections/whitelists/"),pattern = ".txt",replacement = ""),
                                       fill=T,animation = "tada",selected = NULL)
    )
  }
})

output$CL_blacklist_UI<-renderUI({
  values$invalidate_blacklists
  if(length(list.files("collections/blacklists/"))==0){
    return(HTML("No blacklists available. You can create blacklists in the Scripts-Blacklist Tab"))
  }
  else{
    return(
      shinyWidgets::prettyRadioButtons(inputId = "CL_blacklist",label = "Blacklists",
                                       choices = stringr::str_replace_all(string = list.files("collections/blacklists/"),pattern = ".txt",replacement = ""),
                                       fill=T,animation = "tada",selected = NULL)
    )
  }
})




#start classification script, if submit button is clicked
observeEvent(ignoreInit = T,input$CL_Submit_Script,{
  #check if input is correct
  continue=TRUE
  if(input$CL_Mode=="Produce 50 new active learning examples"){
    if(input$CL_Category==""){
      shinyWidgets::sendSweetAlert(session=session,title = "No Category specified!",text = "Please specify a category!",type = "warning")
      continue=FALSE
    }
    else{
      if(input$CL_use_dict==T){
        load(paste0("collections/dictionaries/",input$CL_dict,".RData"))
        if(!input$CL_Category%in%names(dict)){
          shinyWidgets::sendSweetAlert(session=session,title = "The Category can't be found in the dictionary",text = "Check the category names of the dictionary in 'Scripts - Dictionaries - Change dictionary'!",type = "warning")
          continue=FALSE
        }
      }
    }
  }
  if(continue==TRUE){
    valid<-check_pruning_parameters(min_t_c = input$CL_min_termfreq_c,max_t_c = input$CL_max_termfreq_c,min_t_p =input$CL_min_termfreq_p,max_t_p =  input$CL_max_termfreq_p
                                    ,min_t_r =input$CL_min_termfreq_r,max_t_r = input$CL_max_termfreq_r,min_t_q = input$CL_min_termfreq_q, max_t_q = input$CL_max_termfreq_q
                                    ,min_d_c = input$CL_min_docfreq_c,max_d_c = input$CL_max_docfreq_c,min_d_p = input$CL_min_docfreq_p,max_d_p = input$CL_max_docfreq_p
                                    ,min_d_r = input$CL_min_docfreq_r,max_d_r = input$CL_max_docfreq_r,min_d_q = input$CL_min_docfreq_q,max_d_q = input$CL_max_docfreq_q)
    if(isFALSE(valid)){
      shinyWidgets::confirmSweetAlert(session = session,title = "Check pruning settings!",text = HTML("It seems your current pruning input parameters don't make sense. It's very likely, that the whole vocabulary will be removed.
                           Check <a href='https://quanteda.io/reference/dfm_trim.html' title='quanteda pruning'> Quanteda Pruning Settings </a>"),html=T,inputId="CL_pruning_continue",
                                      type="warning",closeOnClickOutside = T,btn_labels = c("Change Settings","Continue anyway!"))
    }
    else{
      #get pruning parameters
      if(input$CL_termfreq_type=="count"){
        min_t<-input$CL_min_termfreq_c
        max_t<-input$CL_max_termfreq_c
      }
      if(input$CL_docfreq_type=="count"){
        min_d<-input$CL_min_docfreq_c
        max_d<-input$CL_max_docfreq_c
      }
      if(input$CL_termfreq_type=="rank"){
        min_t<-input$CL_min_termfreq_r
        max_t<-input$CL_max_termfreq_r
      }
      if(input$CL_docfreq_type=="rank"){
        min_d<-input$CL_min_docfreq_r
        max_d<-input$CL_max_docfreq_r
      }
      if(input$CL_termfreq_type=="prop"){
        min_t<-input$CL_min_termfreq_p
        max_t<-input$CL_max_termfreq_p
      }
      if(input$CL_docfreq_type=="prop"){
        min_d<-input$CL_min_docfreq_p
        max_d<-input$CL_max_docfreq_p
      }
      if(input$CL_termfreq_type=="quantile"){
        min_t<-input$CL_min_termfreq_q
        max_t<-input$CL_max_termfreq_q
      }
      if(input$CL_docfreq_type=="quantile"){
        min_d<-input$CL_min_docfreq_q
        max_d<-input$CL_max_docfreq_q
      }
      if(is.null(min_t))min_t<-NA
      if(is.null(max_t))max_t<-NA
      if(is.null(min_d))min_d<-NA
      if(is.null(max_d))max_d<-NA
      #save needed parameters
      parameters<-list(collection=input$collection_selected,
                       baseform_reduction=input$CL_baseform,
                       min_char=input$CL_min_char,
                       ngrams=input$CL_ngram,
                       remove_stopwords=input$CL_remove_stopwords,
                       lowercase=input$CL_lowercase,
                       remove_numbers=input$CL_remove_numbers,
                       remove_numbers_all=input$CL_remove_numbers_all,
                       remove_punctuation=input$CL_remove_punctuation,
                       remove_hyphenation=input$CL_remove_hyphenation,
                       min_term=min_t,
                       max_term=max_t,
                       min_document=min_d,
                       max_document=max_d,
                       remove_custom=input$CL_remove_custom,
                       blacklist=input$CL_blacklist,
                       reduce_POS=input$CL_POS_TYPES,
                       reduce_NER=input$CL_ENTITY_TYPES,
                       termfreq_type=input$CL_termfreq_type,
                       docfreq_type=input$CL_docfreq_type,
                       keep_custom=input$CL_keep_custom,
                       use_blacklist=input$CL_use_custom_blacklist,
                       use_whitelist=input$CL_use_custom_whitelist,
                       whitelist=input$CL_whitelist,
                       whitelist_only=input$CL_whitelist_only,
                       whitelist_expand=input$CL_whitelist_expand,
                       cooc_window=input$CL_Context_Unit,
                       Project=input$CL_Project,
                       cl_Mode=input$CL_Mode,
                       cl_Category=input$CL_Category,
                       cl_positive_Threshold=input$CL_Threshold,
                       use_dictionary=input$CL_use_dict,
                       Dictioanry=input$CL_dict,
                       cl_active_learning_strategy=input$CL_active_learning_strategy,
                       cl_c=input$CL_c
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
        system(paste0('Rscript collections/scripts/Classification/',input$custom_script_options,' &'))
      }
      else{
        system(paste('Rscript collections/scripts/Classification_Script.R','&'))
        #show modal when process is started
        showModal(modalDialog(
          title = "Process started",
          "The process was succesfully started. Check details in 'My Tasks'."
        ))
      }
    }
  }
})




#start script after continue anyway is clicked even though pruning settings seem to be wrong
observeEvent(ignoreInit=T,input$CL_pruning_continue,{
  validate(
    need(isTRUE(input$CL_pruning_continue),message=F)
  )
  #check if input is correct
  continue=TRUE
  if(input$CL_Mode=="Produce 50 new active learning examples"){
    if(input$CL_Category==""){
      shinyWidgets::sendSweetAlert(session=session,title = "No Category specified!",text = "Please specify a category!",type = "warning")
      continue=FALSE
    }
    else{
      if(input$CL_use_dict==T){
        load(paste0("collections/dictionaries/",input$CL_dict,".RData"))
        if(!input$CL_Category%in%names(dict)){
          shinyWidgets::sendSweetAlert(session=session,title = "The Category can't be found in the dictionary",text = "Check the category names of the dictionary in 'Scripts - Dictionaries - Change dictionary'!",type = "warning")
          continue=FALSE
        }
      }
    }
  }
  if(continue==TRUE){
    #get pruning parameters
    if(input$CL_termfreq_type=="count"){
      min_t<-input$CL_min_termfreq_c
      max_t<-input$CL_max_termfreq_c
    }
    if(input$CL_docfreq_type=="count"){
      min_d<-input$CL_min_docfreq_c
      max_d<-input$CL_max_docfreq_c
    }
    if(input$CL_termfreq_type=="rank"){
      min_t<-input$CL_min_termfreq_r
      max_t<-input$CL_max_termfreq_r
    }
    if(input$CL_docfreq_type=="rank"){
      min_d<-input$CL_min_docfreq_r
      max_d<-input$CL_max_docfreq_r
    }
    if(input$CL_termfreq_type=="prop"){
      min_t<-input$CL_min_termfreq_p
      max_t<-input$CL_max_termfreq_p
    }
    if(input$CL_docfreq_type=="prop"){
      min_d<-input$CL_min_docfreq_p
      max_d<-input$CL_max_docfreq_p
    }
    if(input$CL_termfreq_type=="quantile"){
      min_t<-input$CL_min_termfreq_q
      max_t<-input$CL_max_termfreq_q
    }
    if(input$CL_docfreq_type=="quantile"){
      min_d<-input$CL_min_docfreq_q
      max_d<-input$CL_max_docfreq_q
    }
    if(is.null(min_t))min_t<-NA
    if(is.null(max_t))max_t<-NA
    if(is.null(min_d))min_d<-NA
    if(is.null(max_d))max_d<-NA
    #save needed parameters
    parameters<-list(collection=input$collection_selected,
                     baseform_reduction=input$CL_baseform,
                     min_char=input$CL_min_char,
                     ngrams=input$CL_ngram,
                     remove_stopwords=input$CL_remove_stopwords,
                     lowercase=input$CL_lowercase,
                     remove_numbers=input$CL_remove_numbers,
                     remove_numbers_all=input$CL_remove_numbers_all,
                     remove_punctuation=input$CL_remove_punctuation,
                     remove_hyphenation=input$CL_remove_hyphenation,
                     min_term=min_t,
                     max_term=max_t,
                     min_document=min_d,
                     max_document=max_d,
                     remove_custom=input$CL_remove_custom,
                     blacklist=input$CL_blacklist,
                     reduce_POS=input$CL_POS_TYPES,
                     reduce_NER=input$CL_ENTITY_TYPES,
                     termfreq_type=input$CL_termfreq_type,
                     docfreq_type=input$CL_docfreq_type,
                     keep_custom=input$CL_keep_custom,
                     use_blacklist=input$CL_use_custom_blacklist,
                     use_whitelist=input$CL_use_custom_whitelist,
                     whitelist=input$CL_whitelist,
                     whitelist_only=input$CL_whitelist_only,
                     whitelist_expand=input$CL_whitelist_expand,
                     da_Context_Unit=input$CL_Context_Unit,
                     Project=input$CL_Project,
                     cl_Mode=input$CL_Mode,
                     cl_Category=input$CL_Category,
                     cl_positive_Threshold=input$CL_Threshold,
                     use_dictionary=input$CL_use_dict,
                     Dictioanry=input$CL_dict,
                     cl_active_learning_strategy=input$CL_active_learning_strategy,
                     cl_c=input$CL_c
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
      system(paste0('Rscript collections/scripts/Classification/',input$custom_script_options,' &'))
    }
    else{
      system(paste('Rscript collections/scripts/Classification_Script.R','&'))
      #show modal when process is started
      showModal(modalDialog(
        title = "Process started",
        "The process was succesfully started. Check details in 'My Tasks'."
      ))
    }
  }
})
