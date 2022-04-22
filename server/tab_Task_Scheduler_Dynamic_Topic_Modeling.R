#' render the parameter set for dynamic topic modeling
output$Analysis_Parameter_DTM<-renderUI({
  tagList(
    tags$hr(),
    #standard parameters
    fluidRow(
      column(2,
             checkboxInput(inputId="DTM_use_fixed_vocab",label="use fixed vocabulary?",value=F)   %>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Use an already existing vocabulary instead of performing a parameterized preprocessing. Vocabularies can be added and adjusted in Scripts > Vocabularies.", placement = "right"
                   )
               )
      ),
      column(2,
             conditionalPanel(condition = 'input.DTM_use_fixed_vocab==true',
                              selectInput(inputId="DTM_fixed_vocab",label="found vocabularies",choices=list.files("collections/vocabularies/",full.names = F),multiple=F)
             )
      )
    ),
    fluidRow(
      column(1,
             selectInput(inputId = "DTM_baseform",label = "Baseform Reduction",choices = c("lemma","stemming","none"),selected = "none")#,
             #shinyBS::bsPopover(id = "DTM_baseform", title = "Specify whether and how to do baseform reduction. The options are stemming, lemmatization or no baseorm reduction.",
             #                    content = '<a href="https://www.google.com/">Google Homepage</a>',placement = "right",trigger = "hover",list(container = "body"))
             %>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Specify whether and how to do baseform reduction. The options are stemming, lemmatization or no baseorm reduction.", placement = "right"
                   )
               )
      ),
      column(1,
             selectInput(inputId = "DTM_ngram",label = "N-grams",choices = c(1,2,3),selected = 1,multiple = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Should n-grams be included in the analysis? They will be connected with '_'.", placement = "right"
                   )
               )),
      column(2,
             conditionalPanel(condition='input.DTM_use_fixed_vocab==false',
                              sliderInput(inputId = "DTM_min_char",label = "Min #chars for words",value = c(2,50),min = 1,step = 1,max = 100)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Set the minimum and maximum number of characters of a word to be included in the analysis.", placement = "right"
                                      ,html=T)
                                ))
      ),
      column(2,
             conditionalPanel(condition='input.DTM_use_fixed_vocab==false',
                              textInput(inputId = "DTM_remove_custom",label = HTML("Remove custom words"),placeholder ="Add words (Seperated by ,)")%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Delete specific words from the analysis. Seperate them with ','.", placement = "right"
                                    )
                                )
             )
      ),
      column(2,
             conditionalPanel(condition='input.DTM_use_fixed_vocab==false',
                              textInput(inputId = "DTM_keep_custom",label = HTML("Keep custom words"),placeholder ="Add words (Seperated by ,)")%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Keep specific words in the analysis. Seperate them with ','. If both custom words and whitelist are specified, they will be merged together.", placement = "right"
                                    )
                                )
             )
      )
    ),
    fluidRow(
      column(1,
             checkboxInput(inputId = "DTM_lowercase",label = "Transform tokens to lowercase?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Should lowercasing (set all characters to lower case) be applied?", placement = "left"
                   )
               )),
      column(1,
             conditionalPanel(condition='input.DTM_use_fixed_vocab==false',
                              checkboxInput(inputId = "DTM_remove_stopwords",label = "Remove Stopwords",value = T)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Should stopwords be removed from the analysis? The stopwords of the language specified during dataimport for this dataset will be used.", placement = "top"
                                    )
                                )
             )),
      column(1,
             conditionalPanel(condition='input.DTM_use_fixed_vocab==false',
                              checkboxInput(inputId = "DTM_remove_numbers",label = "Remove Numbers?",value = T)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Remove types which consist of numbers only from the analysis.", placement = "bottom"
                                    )
                                )
             )),
      column(1,
             conditionalPanel(condition='input.DTM_use_fixed_vocab==false',
                              checkboxInput(inputId = "DTM_remove_numbers_all",label = "Remove every word containing a number?",value = T)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Remove words which are composed of at least one number.", placement = "right"
                                    )
                                )
             )),
      column(1,
             conditionalPanel(condition='input.DTM_use_fixed_vocab==false',
                              checkboxInput(inputId = "DTM_remove_punctuation",label = "Remove Punctuation?",value = T)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Remove words, which reflect punctuation from the analysis.", placement = "right"
                                    )
                                )
             )),
      column(1,
             conditionalPanel(condition='input.DTM_use_fixed_vocab==false',
                              checkboxInput(inputId = "DTM_remove_hyphenation",label = "Remove Hyphenation?",value = T)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Remove words, which reflect hyphens from the analysis.", placement = "right"
                                    )
                                )
             )),
      column(1,
             checkboxInput(inputId = "DTM_consolidate_entities",label = "Consolidate Entities?",value = F)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Consolidate found entities by binding them together with '_' and treat them as a single word afterwards.", placement = "right"
                   )
               ))
    ),
    conditionalPanel(condition='input.DTM_use_fixed_vocab==false',
                     fluidRow(
                       column(1,
                              checkboxInput(inputId = "DTM_use_custom_blacklist",label = "use custom blacklist?",value = F)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Use one of the specified blacklists to remove a set of words from the analysis.", placement = "right"
                                    )
                                )
                       ),
                       column(2,
                              uiOutput(outputId = "DTM_blacklist_UI")
                       ),
                       column(1,
                              checkboxInput(inputId = "DTM_use_custom_whitelist",label = "use custom whitelist?",value = F)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Use of the specified whitelists to protect a set of words from the removal during the pre-processing or to calculate the analysis for these words exclusivly.", placement = "right"
                                    )
                                )
                       ),
                       column(2,
                              uiOutput(outputId = "DTM_whitelist_UI")
                       )
                     ),
                     fluidRow(
                       conditionalPanel(condition = "input.DTM_use_custom_whitelist==true || input.DTM_keep_custom.length>=1",
                                        tags$br(),
                                        tags$h5("Whitelist Options:"),
                                        column(1,
                                               checkboxInput(inputId = "DTM_whitelist_only",label = "Exclude all words apart from the whitelist entries",value = FALSE)%>%
                                                 shinyInput_label_embed(
                                                   shiny_iconlink() %>%
                                                     bs_embed_popover(
                                                       title = "Should all words apart from the entries in the whitelist be excluded from the analysis?", placement = "right"
                                                     )
                                                 )
                                        ),
                                        column(1,
                                               conditionalPanel(condition = "(input.DTM_use_custom_whitelist==true || input.DTM_keep_custom.length>=1) && (input.DTM_ngram.includes('2') || input.DTM_ngram.includes('3'))",
                                                                checkboxInput(inputId = "DTM_whitelist_expand",label = "Expand whitelist?",value = FALSE)%>%
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
                                selectInput(inputId = "DTM_termfreq_type",label = "term frequency type",choices = c("count","prop","rank","quantile"),multiple = F,selected = "count")%>%
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
                              conditionalPanel(condition = 'input.DTM_termfreq_type=="count"',
                                               numericInput(inputId = "DTM_min_termfreq_c",label = "min. term frequency",min = 0,step = 1,value=NULL),
                                               numericInput(inputId = "DTM_max_termfreq_c",label = "max. term frequency",min = 1,step = 1,value=NULL)
                              ),
                              conditionalPanel(condition = 'input.DTM_termfreq_type=="prop"',
                                               numericInput(inputId = "DTM_min_termfreq_p",label = "min. term probability",min = 0,step = 0.01,max = 1,value=NULL),
                                               numericInput(inputId = "DTM_max_termfreq_p",label = "max. term probability",min = 0.001,step = 0.01,max = 1,value=NULL)
                              ),
                              conditionalPanel(condition = 'input.DTM_termfreq_type=="rank"',
                                               numericInput(inputId = "DTM_min_termfreq_r",label = "min. term rank",min = 1,step = 1,value=NULL),
                                               numericInput(inputId = "DTM_max_termfreq_r",label = "max. term rank",min = 1,step = 1,value=NULL)
                              ),
                              conditionalPanel(condition = 'input.DTM_termfreq_type=="quantile"',
                                               numericInput(inputId = "DTM_min_termfreq_q",label = "min. term quantile",min = 0,max = 1,step = 0.25,value=NULL),
                                               numericInput(inputId = "DTM_max_termfreq_q",label = "max. term quantile",min = 0,max = 1,step = 0.25,value=NULL)
                              )
                       ),
                       column(1,
                              tags$div(
                                selectInput(inputId = "DTM_docfreq_type",label = "doc frequency type",choices = c("count","prop","rank","quantile"),multiple = F,selected = "prop")%>%
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
                              conditionalPanel(condition = 'input.DTM_docfreq_type=="count"',
                                               numericInput(inputId = "DTM_min_docfreq_c",label = "min. doc frequency",min = 0,step = 1,value=NULL),
                                               numericInput(inputId = "DTM_max_docfreq_c",label = "max. doc frequency",min = 1,step = 1,value=NULL)
                              ),
                              conditionalPanel(condition = 'input.DTM_docfreq_type=="prop"',
                                               numericInput(inputId = "DTM_min_docfreq_p",label = "min. doc probability",min = 0,step = 0.01,max = 1,value=NULL),
                                               numericInput(inputId = "DTM_max_docfreq_p",label = "max. doc probability",min = 0.001,step = 0.01,max = 1,value=NULL)
                              ),
                              conditionalPanel(condition = 'input.DTM_docfreq_type=="rank"',
                                               numericInput(inputId = "DTM_min_docfreq_r",label = "min. doc rank",min = 1,step = 1,value=NULL),
                                               numericInput(inputId = "DTM_max_docfreq_r",label = "max. doc rank",min = 1,step = 1,value=NULL)
                              ),
                              conditionalPanel(condition = 'input.DTM_docfreq_type=="quantile"',
                                               numericInput(inputId = "DTM_min_docfreq_q",label = "min. doc quantile",min = 0,max = 1,step = 0.25,value=NULL),
                                               numericInput(inputId = "DTM_max_docfreq_q",label = "max. doc quantile",min = 0,max = 1,step = 0.25,value=NULL)
                              )
                       )
                     )
    ),
    #specific parameters
    tags$hr(),
    tags$h4("Dynamic Topic Modeling parameters"),
    conditionalPanel(condition='input.DTM_use_fixed_vocab==false',
                     fluidRow(
                       column(1,
                              selectInput(inputId = "DTM_POS_TYPES",label = "Include POS-Types",
                                          choices =c("all","NOUN","VERB","ADJ","PUNCT","SYM","ADP","PART","ADV","INTJ","X") ,selected = "all",multiple = T)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Should the analysis be limited to words from a certain range of POS-Types. If this is the case make sure to exclude 'all' from the selection.",
                                      placement = "right"
                                    )
                                )
                       ),
                       column(1,
                              selectInput(inputId = "DTM_ENTITY_TYPES",label = " Include NER-Tags",
                                          choices =c("all","PER","ORG","GPE","PRODUCT","NORP","FACILITY","LOC","EVENT","WORK_OF_ART","LAW",
                                                     "LANGUAGE","DATE","TIME","PERCENT","MONEY","QUANTITY","ORDINAL","CARDINAL") ,selected = "all",multiple=T)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Should the analysis be limited to words from a certain range of NER-Tags. If this is the case make sure to exclude 'all' from the selection. Using the NER-Tag option causes the consolidation of entities.",
                                      placement = "right"
                                    )
                                )
                       )
                     ),
                     fluidRow(
                       column(1,
                              selectInput(inputId = "DTM_POS_TYPES_exclude",label = "Exclude POS-Types",
                                          choices =c("NOUN","VERB","ADJ","PUNCT","SYM","ADP","PART","ADV","INTJ","X"), selected=character(0),multiple = T)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Remove words with a certain POS-Tag from the analysis.",
                                      placement = "right"
                                    )
                                )
                       ),
                       column(1,
                              selectInput(inputId = "DTM_ENTITY_TYPES_exclude",label = "Exclude NER-Tags",
                                          choices =c("PER","ORG","GPE","PRODUCT","NORP","FACILITY","LOC","EVENT","WORK_OF_ART","LAW",
                                                     "LANGUAGE","DATE","TIME","PERCENT","MONEY","QUANTITY","ORDINAL","CARDINAL") ,selected = character(0),multiple=T)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Remove words with a certain NER-Tag from the analysis. Using this option causes the consolitation of entities.",
                                      placement = "right"
                                    )
                                )
                       )
                     )
    ),
    fluidRow(
      column(1,
             numericInput(inputId = "DTM_alpha",label = "alpha",value=0.05,min=0.01,max=2,step=0.001)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Alpha represents document-topic density - with a higher alpha, documents are made up of more topics, and with lower alpha, documents contain fewer topics.",
                     placement = "right"
                   )
               )
      ),
      column(1,
             numericInput(inputId = "DTM_number_of_topics",label = "number of topics",value=10,min = 2,step = 1)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Set the number of topics.",
                     placement = "right"
                   )
               )
      ),
      column(1,
             numericInput(inputId = "DTM_chain_variance",label = "chain variance",value=0.5,step = 0.1)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "This hyperparameter controls one of the key aspect of topic evolution which is the speed at which these topics evolve.
                                      A smaller chain vairance leads to similar word distributions over multiple timeslice.",
                     placement = "right"
                   )
               )
      ),
      # column(2,
      #        selectInput(inputId = "DTM_Mode",choices=c("fit","time"),multiple = F,label="Mode")%>%
      #          shinyInput_label_embed(
      #            shiny_iconlink() %>%
      #              bs_embed_popover(
      #                title = "Controls the mode of the mode: ‘fit’ is for training, ‘time’ for analyzing documents through time according to a DTM, basically a held out set.",
      #                placement = "right"
      #              )
      #          )
      # ),
      
      column(2,
             selectInput(inputId = "DTM_Date_Split_How",label = "How to split the dates?",choices=c("By Date","Automatic Chunking"))%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "In Dynamic Topic Modeling multiple timespans need to be specified.
                                      They can either be splitted using a certain time interval or create the timestamps automatically based on the number of given documents. The second option will lead to chunks of balanced time intervalls that represent compareable number of documents.",
                     placement = "right"
                   )
               )
      ),
      column(2,
             selectInput(inputId = "DTM_ByDate_Type",label="Time Intervall",choices=c("Decade","Year","Month","Week","Day"),selected="Year")%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "What timeintervall shall be used to split the data?",
                     placement = "right"
                   )
               )
      ),
      column(2,
             conditionalPanel(condition = 'input.DTM_Date_Split_How=="Automatic Chunking"',
                              numericInput(inputId = "DTM_Chunksize",label="Number of Chunks",value=5,min=1,step=1)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Here you can specify the number of chunks the dates for the given documents will be splitted into. The fewer chunks, the more documents are present in each chunk.",
                                      placement = "right"
                                    )
                                )
             ),
             conditionalPanel(condition = 'input.DTM_Date_Split_How=="By Date"',
                              numericInput(inputId = "DTM_ByDate_n",label="number of specified time intervalls per used period",value=1,min=1,step=1)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Shall multiple specified time units be aggregated? E.G. 6 months as half year time span.",
                                      placement = "right"
                                    )
                                )
             )
             
      )
      
      
    ),
    bsButton(inputId = "DTM_Submit_Script",label = "Submit Request",icon = icon("play-circle"),type = "primary")
  )
})

#' show whitelists stored in collections/whitelists
output$DTM_whitelist_UI<-renderUI({
  if(length(list.files("collections/whitelists/"))==0){
    return(HTML("No whitelists available. You can create whitelist in the Scripts-Whitelist Tab"))
  }
  else{
    return(shinyjs::hidden(
      prettyRadioButtons(inputId = "DTM_whitelist",label = "Whitelists",
                         choices = stringr::str_replace_all(string = list.files("collections/whitelists/"),pattern = ".txt",replacement = ""),
                         fill=T,animation = "tada",selected = NULL,inline = T)
    ))  
  }
})

#' show whitelist options when whitelist checkbox is TRUE
#' depends on:
#'  input$DTM_use_custom_whitelist: should a customed withelist for the DTMquency extraction be used
observeEvent(ignoreNULL = T,input$DTM_use_custom_whitelist,{
  if(isTRUE(input$DTM_use_custom_whitelist)){
    shinyjs::show(id = "DTM_whitelist")
  }
  else{
    shinyjs::hide(id = "DTM_whitelist")
  }
})

#' show blacklists stored in collections/blacklists
output$DTM_blacklist_UI<-renderUI({
  if(length(list.files("collections/blacklists/"))==0){
    return(HTML("No blacklists available. You can create blacklists in the Scripts-Blacklist Tab"))
  }
  else{
    return(shinyjs::hidden(
      prettyRadioButtons(inputId = "DTM_blacklist",label = "Blacklists",
                         choices = stringr::str_replace_all(string = list.files("collections/blacklists/"),pattern = ".txt",replacement = ""),
                         fill=T,animation = "tada",selected = NULL,inline = T)
    ))  
  }
})

#' show blacklist options when blacklist checkbox is TRUE
#' depends on: 
#'  input$DTM_use_custom_blacklist: should a customed blacklist be used for dynamic topic modeling
observeEvent(ignoreNULL = T,input$DTM_use_custom_blacklist,{
  if(isTRUE(input$DTM_use_custom_blacklist)){
    shinyjs::show(id = "DTM_blacklist")
  }
  else{
    shinyjs::hide(id = "DTM_blacklist")
  }
})




#' start cooccurrence analysis script, if submit button is clicked
#' depends on:
#'   input$DTM_min_termfreq_c: minimum term frequency (count)
#'   input$DTM_max_termfreq_c: maximum term frequency (count)
#'   input$DTM_min_termfreq_p: minimum term probability
#'   input$DTM_max_termfreq_p: maximum term probalility
#'   input$DTM_min_termfreq_r: minimum term rank
#'   input$DTM_max_termfreq_r: maximum term rank
#'   input$DTM_min_termfreq_q: minimum term quantile
#'   input$DTM_max_termfreq_q: maximum term quantile
#'   input$DTM_min_docfreq_c: minimum document frequency (count)
#'   input$DTM_max_docfreq_c: maximum document frequency (count)
#'   input$DTM_min_docfreq_p: minimum document probability
#'   input$DTM_max_docfreq_p: maximum document probability
#'   input$DTM_min_docfreq_r: minimum document rank 
#'   input$DTM_max_docfreq_r: maximum document rank
#'   input$DTM_min_docfreq_q: minimum document quantile
#'   input$DTM_max_docfreq_q: maximum document quantile
#'   input$DTM_use_fixed_vocab: should a fixed vocabulary be used?
#'   input$DTM_fixed_vocab: the fixed vocabulary list
#'   input$DTM_termfreq_type: choose a term frequency type (count, quantile, rank, probability)
#'   input$collection_selected: selected collection
#'   input$DTM_baseform: should words be reduced to their baseform
#'   input$DTM_min_char: select minimum of characters
#'   input$DTM_ngram: choose size of n-grams
#'   input$DTM_remove_stopwords: should stopwords be removed
#'   input$DTM_lowercase: shoult all words be put in lowercase
#'   input$DTM_remove_numbers: should numbers in the documents be removed?
#'   input$DTM_remove_numbers_all: should all words be removed that contain numbers?
#'   input$DTM_remove_punctuation: should the punctuation be removed?
#'   input$DTM_remove_hyphenation: should hyphenation be removed?
#'   input$DTM_remove_custom: should custom words be removed
#'   input$DTM_consolidate_entities: should entities be consolidated?
#'   input$DTM_blacklist: blacklist of words that should be removed from the texts 
#'   input$DTM_POS_TYPES: select part of speech types that should be used
#'   input$DTM_POS_TYPES_exclude: select part of speech types that should be excluded
#'   input$DTM_ENTITY_TYPES: select entity (NER) types that should be used
#'   input$DTM_ENTITY_TYPES_exclude: select entity (NER) types that should be excluded
#'   input$DTM_docfreq_type: choosen document frequence type 
#'   input$DTM_keep_custom: are there custome words that should stay in the documents
#'   input$DTM_use_custom_blacklist: should a custom blacklist be used
#'   input$DTM_use_custom_whitelist: should a custom whitelist be used
#'   input$DTM_whitelist: whitelist for words that should stay in the documents
#'   input$DTM_whitelist_expand: expand the whitelist?
#'   input$DTM_whitelist_only: just use words from whitelist for analysis
#'   input$analysis_selected: selected analysis type
#'   input$use_custom_script: should a customed script be used?
#'   input$custom_script_options: options for the custom script
observeEvent(input$DTM_Submit_Script,{
  valid<-check_pruning_parameters(min_t_c = input$DTM_min_termfreq_c,max_t_c = input$DTM_max_termfreq_c,min_t_p =input$DTM_min_termfreq_p,max_t_p =  input$DTM_max_termfreq_p
                                  ,min_t_r =input$DTM_min_termfreq_r,max_t_r = input$DTM_max_termfreq_r,min_t_q = input$DTM_min_termfreq_q, max_t_q = input$DTM_max_termfreq_q
                                  ,min_d_c = input$DTM_min_docfreq_c,max_d_c = input$DTM_max_docfreq_c,min_d_p = input$DTM_min_docfreq_p,max_d_p = input$DTM_max_docfreq_p
                                  ,min_d_r = input$DTM_min_docfreq_r,max_d_r = input$DTM_max_docfreq_r,min_d_q = input$DTM_min_docfreq_q,max_d_q = input$DTM_max_docfreq_q)
  valid_vocab<-check_if_predefined_vocabulary_is_valid(use_predefined_vocab = input$DTM_use_fixed_vocab, vocabulary = input$DTM_fixed_vocab)
  if(isFALSE(valid)){
    shinyWidgets::confirmSweetAlert(session = session,title = "Check pruning settings!",text = HTML("It seems your current pruning input parameters don't make sense. It's very likely, that the whole vocabulary will be removed.
                           Check <a href='https://quanteda.io/reDTMrence/dfm_trim.html' title='quanteda pruning'> Quanteda Pruning Settings </a>"),html=T,inputId="DTM_pruning_continue",
                                    type="warning",closeOnClickOutside = T,btn_labels = c("Change Settings","Continue anyway!"))
  }
  else if(isFALSE(valid_vocab)){
    shinyWidgets::confirmSweetAlert(session = session,title = "Check vocabulary",text = HTML("You chose to use a predefined vocabulary. It seems this vocabulary is not present."),html=T,inputId="DTM_pruning_continue",
                                    type="warning",closeOnClickOutside = T,btn_labels = c("Change Settings","Continue anyway!"))
  }
  else{
    #get pruning parameters
    if(input$DTM_termfreq_type=="count"){
      min_t<-input$DTM_min_termfreq_c
      max_t<-input$DTM_max_termfreq_c
    }
    if(input$DTM_docfreq_type=="count"){
      min_d<-input$DTM_min_docfreq_cgens
      max_d<-input$DTM_max_docfreq_c
    }
    if(input$DTM_termfreq_type=="rank"){
      min_t<-input$DTM_min_termfreq_r
      max_t<-input$DTM_max_termfreq_r
    }
    if(input$DTM_docfreq_type=="rank"){
      min_d<-input$DTM_min_docfreq_r
      max_d<-input$DTM_max_docfreq_r
    }
    if(input$DTM_termfreq_type=="prop"){
      min_t<-input$DTM_min_termfreq_p
      max_t<-input$DTM_max_termfreq_p
    }
    if(input$DTM_docfreq_type=="prop"){
      min_d<-input$DTM_min_docfreq_p
      max_d<-input$DTM_max_docfreq_p
    }
    if(input$DTM_termfreq_type=="quantile"){
      min_t<-input$DTM_min_termfreq_q
      max_t<-input$DTM_max_termfreq_q
    }
    if(input$DTM_docfreq_type=="quantile"){
      min_d<-input$DTM_min_docfreq_q
      max_d<-input$DTM_max_docfreq_q
    }
    if(is.null(min_t))min_t<-NA
    if(is.null(max_t))max_t<-NA
    if(is.null(min_d))min_d<-NA
    if(is.null(max_d))max_d<-NA
    #save needed parameters
    parameters<-list(collection=input$collection_selected,
                     baseform_reduction=input$DTM_baseform,
                     min_char=input$DTM_min_char,
                     ngrams=input$DTM_ngram,
                     remove_stopwords=input$DTM_remove_stopwords,
                     lowercase=input$DTM_lowercase,
                     remove_numbers=input$DTM_remove_numbers,
                     remove_numbers_all=input$DTM_remove_numbers_all,
                     remove_punctuation=input$DTM_remove_punctuation,
                     remove_hyphenation=input$DTM_remove_hyphenation,
                     min_term=min_t,
                     max_term=max_t,
                     min_document=min_d,
                     max_document=max_d,
                     remove_custom=input$DTM_remove_custom,
                     consolidate_entities=input$DTM_consolidate_entities,
                     blacklist=input$DTM_blacklist,
                     reduce_POS=input$DTM_POS_TYPES,
                     reduce_POS_exclude=input$DTM_POS_TYPES_exclude,
                     reduce_NER=input$DTM_ENTITY_TYPES,
                     reduce_NER_exclude=input$DTM_ENTITY_TYPES_exclude,
                     termfreq_type=input$DTM_termfreq_type,
                     docfreq_type=input$DTM_docfreq_type,
                     keep_custom=input$DTM_keep_custom,
                     use_blacklist=input$DTM_use_custom_blacklist,
                     use_whitelist=input$DTM_use_custom_whitelist,
                     whitelist=input$DTM_whitelist,
                     whitelist_expand=input$DTM_whitelist_expand,
                     whitelist_only=input$DTM_whitelist_only,
                     use_fixed_vocab=input$DTM_use_fixed_vocab,
                     fixed_vocab=input$DTM_fixed_vocab,
                     tm_number_of_topics=input$DTM_number_of_topics,
                     tm_alpha=input$DTM_alpha,
                     dtm_top_chain_variance=input$DTM_chain_variance,
                     dtm_split_how=input$DTM_Date_Split_How,
                     dtm_chunksize=input$DTM_Chunksize,
                     dtm_Date_Type=input$DTM_ByDate_Type,
                     dtm_Date_n=input$DTM_ByDate_n
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
      system(paste0('Rscript collections/scripts/Dynamic_Topic_Model/',input$custom_script_options,' &'))
    }
    else{
      system(paste('Rscript collections/scripts/Dynamic_Topic_Model_Script.R','&'))
      #show modal when process is started
      showModal(modalDialog(
        title = "Process started",
        "The process was succesfully started. Check details in 'My Tasks'."
      ))
    }
  }
})



#' start script after continue anyway is clicked even though pruning settings seem to be wrong
#' depends on:
#'   input$DTM_pruning_continue: do you want to continue the pruning?
#'   input$DTM_termfreq_type: choose a term frequency type (count, quantile, rank, probability)
#'   input$DTM_docfreq_type: choosen document frequence type 
#'   input$DTM_min_termfreq_c: minimum term frequency (count)
#'   input$DTM_max_termfreq_c: maximum term frequency (count)
#'   input$DTM_min_docfreq_c: minimum document frequency (count)
#'   input$DTM_max_docfreq_c: maximum document frequency (count)
#'   input$DTM_min_termfreq_r: minimum term rank
#'   input$DTM_max_termfreq_r: maximum term rank
#'   input$DTM_min_docfreq_r: minimum document rank 
#'   input$DTM_max_docfreq_r: maximum document rank
#'   input$DTM_min_termfreq_p: minimum term probability
#'   input$DTM_max_termfreq_p: maximum term probalility
#'   input$DTM_min_docfreq_p: minimum document probability
#'   input$DTM_max_docfreq_p: maximum document probability
#'   input$DTM_min_termfreq_q: minimum term quantile
#'   input$DTM_max_termfreq_q: maximum term quantile
#'   input$DTM_min_docfreq_q: minimum document quantile
#'   input$DTM_max_docfreq_q: maximum document quantile
#'   input$collection_selected: selected collection
#'   input$DTM_baseform: should words be reduced to their baseform?
#'   input$DTM_min_char: select minimum of characters
#'   input$DTM_ngram: choose size of n-grams
#'   input$DTM_remove_stopwords: should stopwords be removed
#'   input$DTM_lowercase: shoult all words be put in lowercase
#'   input$DTM_remove_numbers: should numbers in the documents be removed?
#'   input$DTM_remove_numbers_all: should all words be removed that contain numbers?
#'   input$DTM_remove_punctuation: should the punctuation be removed?
#'   input$DTM_remove_hyphenation: should hyphenation be removed?
#'   input$DTM_cooc_type: which type of cooccurrence analysis should be used?
#'   input$DTM_remove_custom: should custom words be removed
#'   input$DTM_consolidate_entities: should entities be consolidated?
#'   input$DTM_blacklist: blacklist of words that should be removed from the texts 
#'   input$DTM_POS_TYPES: select part of speech types that should be used
#'   input$DTM_POS_TYPES_exclude: select part of speech types that should be excluded
#'   input$DTM_ENTITY_TYPES: select entity (NER) types that should be used
#'   input$DTM_ENTITY_TYPES_exclude: select entity (NER) types that should be excluded
#'   input$DTM_keep_custom: are there custome words that should stay in the documents?
#'   input$DTM_use_custom_blacklist: should a custom blacklist be used?
#'   input$DTM_use_custom_whitelist: should a custom whitelist be used?
#'   input$DTM_whitelist: whitelist for words that should stay in the documents
#'   input$DTM_whitelist_expand: expand the whitelist?
#'   input$DTM_whitelist_only: just use words from whitelist for analysis
#'   input$DTM_use_fixed_vocab: should a fixed vocabulary be used?
#'   input$DTM_fixed_vocab: the fixed vocabulary list
#'   input$analysis_selected: selected analysis type
#'   input$use_custom_script: should a customed script be used?
#'   input$custom_script_options: options for the custom script
observeEvent(input$DTM_pruning_continue,ignoreInit = T,{
  validate(
    need(isTRUE(input$DTM_pruning_continue),message=F)
  )
  #get pruning parameters
  if(input$DTM_termfreq_type=="count"){
    min_t<-input$DTM_min_termfreq_c
    max_t<-input$DTM_max_termfreq_c
  }
  if(input$DTM_docfreq_type=="count"){
    min_d<-input$DTM_min_docfreq_c
    max_d<-input$DTM_max_docfreq_c
  }
  if(input$DTM_termfreq_type=="rank"){
    min_t<-input$DTM_min_termfreq_r
    max_t<-input$DTM_max_termfreq_r
  }
  if(input$DTM_docfreq_type=="rank"){
    min_d<-input$DTM_min_docfreq_r
    max_d<-input$DTM_max_docfreq_r
  }
  if(input$DTM_termfreq_type=="prop"){
    min_t<-input$DTM_min_termfreq_p
    max_t<-input$DTM_max_termfreq_p
  }
  if(input$DTM_docfreq_type=="prop"){
    min_d<-input$DTM_min_docfreq_p
    max_d<-input$DTM_max_docfreq_p
  }
  if(input$DTM_termfreq_type=="quantile"){
    min_t<-input$DTM_min_termfreq_q
    max_t<-input$DTM_max_termfreq_q
  }
  if(input$DTM_docfreq_type=="quantile"){
    min_d<-input$DTM_min_docfreq_q
    max_d<-input$DTM_max_docfreq_q
  }
  if(is.null(min_t))min_t<-NA
  if(is.null(max_t))max_t<-NA
  if(is.null(min_d))min_d<-NA
  if(is.null(max_d))max_d<-NA
  #save needed parameters
  parameters<-list(collection=input$collection_selected,
                   baseform_reduction=input$DTM_baseform,
                   min_char=input$DTM_min_char,
                   ngrams=input$DTM_ngram,
                   remove_stopwords=input$DTM_remove_stopwords,
                   lowercase=input$DTM_lowercase,
                   remove_numbers=input$DTM_remove_numbers,
                   remove_numbers_all=input$DTM_remove_numbers_all,
                   remove_punctuation=input$DTM_remove_punctuation,
                   remove_hyphenation=input$DTM_remove_hyphenation,
                   min_term=min_t,
                   max_term=max_t,
                   min_document=min_d,
                   max_document=max_d,
                   cooc_window=input$DTM_cooc_type,
                   remove_custom=input$DTM_remove_custom,
                   consolidate_entities=input$DTM_consolidate_entities,
                   blacklist=input$DTM_blacklist,
                   reduce_POS=input$DTM_POS_TYPES,
                   reduce_POS_exclude=input$DTM_POS_TYPES_exclude,
                   reduce_NER=input$DTM_ENTITY_TYPES,
                   reduce_NER_exclude=input$DTM_ENTITY_TYPES_exclude,
                   termfreq_type=input$DTM_termfreq_type,
                   docfreq_type=input$DTM_docfreq_type,
                   keep_custom=input$DTM_keep_custom,
                   use_blacklist=input$DTM_use_custom_blacklist,
                   use_whitelist=input$DTM_use_custom_whitelist,
                   whitelist=input$DTM_whitelist,
                   whitelist_expand=input$DTM_whitelist_expand,
                   whitelist_only=input$DTM_whitelist_only,
                   use_fixed_vocab=input$DTM_use_fixed_vocab,
                   fixed_vocab=input$DTM_fixed_vocab,
                   tm_number_of_topics=input$DTM_number_of_topics,
                   tm_alpha=input$DTM_alpha,
                   dtm_top_chain_variance=input$DTM_chain_variance,
                   dtm_split_how=input$DTM_Date_Split_How,
                   dtm_chunksize=input$DTM_Chunksize,
                   dtm_Date_Type=input$DTM_ByDate_Type,
                   dtm_Date_n=input$DTM_ByDate_n
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
    system(paste0('Rscript collections/scripts/Dynamic_Topic_Model/',input$custom_script_options,' &'))
  }
  else{
    system(paste('Rscript collections/scripts/Dynamic_Topic_Model_Script.R','&'))
    #show modal when process is started
    showModal(modalDialog(
      title = "Process started",
      "The process was succesfully started. Check details in 'My Tasks'."
    ))
  }
})
