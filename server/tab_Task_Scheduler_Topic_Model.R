
#render the parameter set for topic modeling
output$Analysis_Parameter_TM<-renderUI({
  tagList(
    tags$hr(),
    #standard parameters
    fluidRow(
      column(1,
             selectInput(inputId = "TM_baseform",label = "Baseform Reduction",choices = c("lemma","stemming","none"),selected = "none")#,
             #shinyBS::bsPopover(id = "TM_baseform", title = "Specify whether and how to do baseform reduction. The options are stemming, lemmatization or no baseorm reduction.",
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
             sliderInput(inputId = "TM_min_char",label = "Min #chars for words",value = c(2,50),min = 1,step = 1,max = 100)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Set the minimum and maximum number of characters of a word to be included in the analysis.", placement = "right"
                     ,html=T)
               )),
      column(1,
             selectInput(inputId = "TM_ngram",label = "N-grams",choices = c(1,2,3),selected = 1,multiple = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Should n-grams be included in the analysis? They will be connected with '_'.", placement = "right"
                   )
               )),
      column(2,
             textInput(inputId = "TM_remove_custom",label = HTML("Remove custom words"),placeholder ="Add words (Seperated by ,)")%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Delete specific words from the analysis. Seperate them with ','.", placement = "right"
                   )
               )
      ),
      column(2,
             textInput(inputId = "TM_keep_custom",label = HTML("Keep custom words"),placeholder ="Add words (Seperated by ,)")%>%
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
             checkboxInput(inputId = "TM_remove_stopwords",label = "Remove Stopwords",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Should stopwords be removed from the analysis? The stopwords of the language specified during dataimport for this dataset will be used.", placement = "top"
                   )
               )),
      column(1,
             checkboxInput(inputId = "TM_lowercase",label = "Transform tokens to lowercase?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Should lowercasing (set all characters to lower case) be applied?", placement = "left"
                   )
               )),
      column(1,
             checkboxInput(inputId = "TM_remove_numbers",label = "Remove Numbers?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Remove types which consist of numbers only from the analysis.", placement = "bottom"
                   )
               )),
      column(1,
             checkboxInput(inputId = "TM_remove_numbers_all",label = "Remove everything containing a number number?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Remove words which are composed of at least one number.", placement = "right"
                   )
               )),
      column(1,
             checkboxInput(inputId = "TM_remove_punctuation",label = "Remove Punctuation?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Remove words, which reflect punctuation from the analysis.", placement = "right"
                   )
               )),
      column(1,
             checkboxInput(inputId = "TM_remove_hyphenation",label = "Remove Hyphenation?",value = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Remove words, which reflect hyphens from the analysis.", placement = "right"
                   )
               )),
      column(1,
             checkboxInput(inputId = "TM_consolidate_entities",label = "Consolidate Entities?",value = F)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Consolidate found entities by binding them together with '_' and treat them as a single word afterwards.", placement = "right"
                   )
               ))
    ),
    fluidRow(
      column(1,
             checkboxInput(inputId = "TM_use_custom_blacklist",label = "use custom blacklist?",value = F)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Use one of the specified blacklists to remove a set of words from the analysis.", placement = "right"
                   )
               )
      ),
      column(2,
             conditionalPanel(condition = "input.TM_use_custom_blacklist==true",
                              uiOutput(outputId = "TM_blacklist_UI")
             )
      ),
      column(1,
             checkboxInput(inputId = "TM_use_custom_whitelist",label = "use custom whitelist?",value = F)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Use of the specified whitelists to protect a set of words from the removal during the pre-processing or to calculate the analysis for these words exclusivly.", placement = "right"
                   )
               )
      ),
      column(2,
             conditionalPanel(condition = "input.TM_use_custom_whitelist==true",
                              uiOutput(outputId = "TM_whitelist_UI")
             )
      )
    ),
    fluidRow(
      conditionalPanel(condition = "input.TM_use_custom_whitelist==true || input.TM_keep_custom.length>=1",
                       tags$br(),
                       tags$h5("Whitelist Options:"),
                       column(1,
                              checkboxInput(inputId = "TM_whitelist_only",label = "Exclude all words apart from the whitelist entries",value = FALSE)%>%
                                shinyInput_label_embed(
                                  shiny_iconlink() %>%
                                    bs_embed_popover(
                                      title = "Should all words apart from the entries in the whitelist be excluded from the analysis?", placement = "right"
                                    )
                                )
                       ),
                       column(1,
                              conditionalPanel(condition = "(input.TM_use_custom_whitelist==true || input.TM_keep_custom.length>=1) && (input.TM_ngram.includes('2') || input.TM_ngram.includes('3'))",
                                               checkboxInput(inputId = "TM_whitelist_expand",label = "Expand whitelist?",value = FALSE)%>%
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
               selectInput(inputId = "TM_termfreq_type",label = "term frequency type",choices = c("count","prop","rank","quantile"),multiple = F,selected = "count")%>%
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
             conditionalPanel(condition = 'input.TM_termfreq_type=="count"',
                              numericInput(inputId = "TM_min_termfreq_c",label = "min. term frequency",min = 0,step = 1,value=NULL),
                              numericInput(inputId = "TM_max_termfreq_c",label = "max. term frequency",min = 1,step = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.TM_termfreq_type=="prop"',
                              numericInput(inputId = "TM_min_termfreq_p",label = "min. term probability",min = 0,step = 0.01,max = 1,value=NULL),
                              numericInput(inputId = "TM_max_termfreq_p",label = "max. term probability",min = 0.001,step = 0.01,max = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.TM_termfreq_type=="rank"',
                              numericInput(inputId = "TM_min_termfreq_r",label = "min. term rank",min = 1,step = 1,value=NULL),
                              numericInput(inputId = "TM_max_termfreq_r",label = "max. term rank",min = 1,step = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.TM_termfreq_type=="quantile"',
                              numericInput(inputId = "TM_min_termfreq_q",label = "min. term quantile",min = 0,max = 1,step = 0.25,value=NULL),
                              numericInput(inputId = "TM_max_termfreq_q",label = "max. term quantile",min = 0,max = 1,step = 0.25,value=NULL)
             )
      ),
      column(1,
             tags$div(
               selectInput(inputId = "TM_docfreq_type",label = "doc frequency type",choices = c("count","prop","rank","quantile"),multiple = F,selected = "prop")%>%
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
             conditionalPanel(condition = 'input.TM_docfreq_type=="count"',
                              numericInput(inputId = "TM_min_docfreq_c",label = "min. doc frequency",min = 0,step = 1,value=NULL),
                              numericInput(inputId = "TM_max_docfreq_c",label = "max. doc frequency",min = 1,step = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.TM_docfreq_type=="prop"',
                              numericInput(inputId = "TM_min_docfreq_p",label = "min. doc probability",min = 0,step = 0.01,max = 1,value=NULL),
                              numericInput(inputId = "TM_max_docfreq_p",label = "max. doc probability",min = 0.001,step = 0.01,max = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.TM_docfreq_type=="rank"',
                              numericInput(inputId = "TM_min_docfreq_r",label = "min. doc rank",min = 1,step = 1,value=NULL),
                              numericInput(inputId = "TM_max_docfreq_r",label = "max. doc rank",min = 1,step = 1,value=NULL)
             ),
             conditionalPanel(condition = 'input.TM_docfreq_type=="quantile"',
                              numericInput(inputId = "TM_min_docfreq_q",label = "min. doc quantile",min = 0,max = 1,step = 0.25,value=NULL),
                              numericInput(inputId = "TM_max_docfreq_q",label = "max. doc quantile",min = 0,max = 1,step = 0.25,value=NULL)
             )
      )
      
    ),
    #Topic Model specific parameters
    tags$hr(),
    tags$h4("Topic Model parameters"),
    fluidRow(
      column(2,
             numericInput(inputId = "TM_number_of_topics",label = "number of topics",value=20,min = 3,step = 1)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Set the number of topics.",
                     placement = "right"
                   )
               )
      ),
      column(2,
             selectInput(inputId = "TM_method",label = "method",choices = tmodel$list_methods())%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Choose one of the topic modelling backends.",
                     placement = "right"
                   )
               )
      ),
      column(2,
             
             numericInput(inputId = "TM_alpha",label = "alpha",value=0.05,min=0.01,max=2,step=0.001)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Alpha represents document-topic density - with a higher alpha, documents are made up of more topics, and with lower alpha, documents contain fewer topics.",
                     placement = "right"
                   )
               )
      ),
      
      column(2,
             materialSwitch(inputId = "TM_detailed_meta_dist",label = "detailed meta distribution analysis?",value = F,status="info")%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Analyse the resulting document distribution with respect to their corresponding meta data.",
                     placement = "right"
                   )
               )
      ),
      column(2,
             selectInput(inputId = "TM_POS_TYPES",label = "POS-Types",
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
             selectInput(inputId = "TM_ENTITY_TYPES",label = "NER-Tags",
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
    
    # Structural Topic Model
    fluidRow(
      
             conditionalPanel(condition = 'input.TM_method=="stm"',
                              tags$hr(),
                              tags$h4("Parameters specific for Structural Topic Model"),
                              
                              #documents
                              #vocabulary
                              #K
                              column(2,
                                     # prevalenceFormula = NULL,
                                     textInput(inputId = "stm_prevalenceFormula",label = "prevalence formula")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             title = "A formula object with no response variable or a matrix containing topic prevalence covariates. Use s, ns or bs to specify smooth terms. See details of stm for more information. \nThe model for topical prevalence includes covariates which the analyst believes may influence the frequency with which a topic is discussed. This is specified as a formula which can contain smooth terms using splines or by using the function s. The response portion of the formula should be left blank. See the examples in stm documentation. These variables can include numeric and factor variables. While including variables of class Dates or other non-numeric, non-factor types will work in stm it may not always work for downstream functions such as estimateEffect. Example: '~ countryName + s(date)'.",
                                             placement = "right"
                                           )
                                       ),
                                     
                                     # contentFormula = NULL,
                                     textInput(inputId = "stm_contentFormula",label = "content formula")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             title = "A formula containing a single variable, a factor variable or something which can be coerced to a factor indicating the category of the content variable for each document. \nThe topical convent covariates are those which affect the way in which a topic is discussed. As currently implemented this must be a single variable which defines a discrete partition of the dataset (each document is in one and only one group). STM may relax this in the future. While including more covariates in topical prevalence will rarely affect the speed of the model, including additional levels of the content covariates can make the model much slower to converge. This is due to the model operating in the much higher dimensional space of words in dictionary (which tend to be in the thousands) as opposed to topics. Example: '~countryName'",
                                             placement = "right"
                                           )
                                       ),
                                     
                                     #data => is meta data
                                     
                                     #init.type = Spectral
                                     
                                     selectInput(inputId = "stm_init_type",label = "init type",choices = c("Spectral", "LDA", "Random", "Custom"), selected = "Spectral")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             title = "The method of initialization, by default the spectral initialization. Must be either Latent Dirichlet Allocation (\"LDA\"), \"Random\", \"Spectral\" or \"Custom\". See details of stm for more info. If you want to replicate a previous result, see the argument seed. For \"Custom\" see the format described in stm help under the custom.beta option of the control parameters",
                                             placement = "right"
                                           )
                                       )
                                     
                                     # # seed = NULL,
                                     # textInput(inputId = "stm_seed",label = "seed")%>%
                                     #   shinyInput_label_embed(
                                     #     shiny_iconlink() %>%
                                     #       bs_embed_popover(
                                     #         title = "Seed for the random number generator. stm saves the seed it uses on every run so that any result can be exactly reproduced. When attempting to reproduce a result with that seed, it should be specified here",
                                     #         placement = "right"
                                     #       )
                                     #   ),
                                     
                                    
                                     
                              
                              ),
                              column(2,
                                     #verbose = TRUE A logical flag indicating whether information should be printed to the screen. During the E-step (iteration over documents) a dot will print each time 1% of the documents are completed. At the end of each iteration the approximate bound will also be printed.
                                     #reportevery = 5 An integer determining the intervals at which labels are printed to the screen during fitting. Defaults to every 5 iterations.
                                     
                                     #max.em.its = 500
                                     numericInput(inputId = "stm_max_em_its",label = "maximum number of EM iterations",value=500,step = 50)%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             title = "The maximum number of EM iterations. If convergence has not been met at this point, a message will be printed. If you set this to 0 it will return the initialization",
                                             placement = "right"
                                           )
                                       ),
                                     
                                     #emtol
                                     numericInput(inputId = "stm_emtol",label = "Convergence tolerance",value=0.00001)%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             title = "Convergence tolerance. EM stops when the relative change in the approximate bound drops below this level. Defaults to .00001. You can set it to 0 to have the algorithm run max.em.its number of steps.",
                                             placement = "right"
                                           )
                                       ),
                                     
                                     #ngroups
                                     numericInput(inputId = "stm_ngroups",label = "Number of groups for memorized inference", value = 1)%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             title = "Specifying an integer greater than 1 for the argument ngroups causes the corpus to be broken into the specified number of groups. Global updates are then computed after each group in turn. This approach, called memoized variational inference in Hughes and Sudderth (2013), can lead to more rapid convergence when the number of documents is large. Note that the memory requirements scale linearly with the number of groups so this provides a tradeoff between memory efficiency and speed. The claim of speed here is based on the idea that increasing the number of global updates should help the model find a solution in fewer passes through the document set. However, itt is worth noting that for any particular case the model need not converge faster and definitely won't converge to the same location. This functionality should be considered somewhat experimental and stm team encourages users to let them know what their experiences are like here in practice.",
                                             placement = "right"
                                           )
                                       ),
                                     
                                     #LDAbeta
                                     checkboxInput(inputId = "stm_LDAbeta",label = "LDA beta", value = TRUE)%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             title = "a logical that defaults to TRUE when there are no content covariates. When set to FALSE the model performs SAGE style topic updates (sparse deviations from a baseline)",
                                             placement = "right"
                                           )
                                       ),
                                     
                                     #interactions
                                     checkboxInput(inputId = "stm_interactions",label = "interactions", value = TRUE)%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             title = "a logical that defaults to TRUE. This automatically includes interactions between content covariates and the latent topics. Setting it to FALSE reduces to a model with no interactive effects.",
                                             placement = "right"
                                           )
                                       )
                                     
                                     
                                     
                                     # model = NULL, #Models can now be restarted by passing an STM object to the argument model. This is particularly useful if you run a model to the maximum iterations and it terminates without converging
                              ),
                              
                              
                              column(2,
                                     #gamma.prior
                                     selectInput(inputId = "stm_gamma_prior",label = "gamma prior",choices = c("Pooled", "L1"),selected = "Pooled")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             title = "sets the prior estimation method for the prevalence covariate model. The default Pooled options uses Normal prior distributions with a topic-level pooled variance which is given a moderately regularizing half-cauchy(1,1) prior. The alternative L1 uses glmnet to estimate a grouped penalty between L1-L2. If your code is running slowly immediately after \"Completed E-Step\" appears, you may want to switch to the L1 option. See details in stm help",
                                             placement = "right"
                                           )
                                       ),
                                     
                                     #sigma.prior
                                     numericInput(inputId = "stm_sigma_prior",label = "sigma prior",value=0)%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             title = "a scalar between 0 and 1 which defaults to 0. This sets the strength of regularization towards a diagonalized covariance matrix. Setting the value above 0 can be useful if topics are becoming too highly correlated",
                                             placement = "right"
                                           )
                                       ),
                                     
                                     
                                     #kappa.prior
                                     selectInput(inputId = "stm_kappa_prior",label = "kappa prior",choices = c("L1", "Jeffreys"),selected = "L1")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             title = "a scalar between 0 and 1 which defaults to 0. This sets the strength of regularization towards a diagonalized covariance matrix. Setting the value above 0 can be useful if topics are becoming too highly correlated",
                                             placement = "right"
                                           )
                                       )
                                     
                                     
                                     
                                     # control = list() a list of additional advanced parameters. See details of stm

                              )
                              
             )
             
              
             
      
      
      
    ),
    
    
    
    bsButton(inputId = "TM_Submit_Script",label = "Submit Request",icon = icon("play-circle"),type = "primary")
  )
})

output$TM_whitelist_UI<-renderUI({
  values$invalidate_whitelists
  if(length(list.files("collections/whitelists/"))==0){
    return(HTML("No whitelists available. You can create whitelist in the Scripts-Whitelist Tab"))
  }
  else{
    return(
      shinyWidgets::prettyRadioButtons(inputId = "TM_whitelist",label = "Whitelists",
                                       choices = stringr::str_replace_all(string = list.files("collections/whitelists/"),pattern = ".txt",replacement = ""),
                                       fill=T,animation = "tada",selected = NULL)
    )
  }
})

output$TM_blacklist_UI<-renderUI({
  values$invalidate_blacklists
  if(length(list.files("collections/blacklists/"))==0){
    return(HTML("No blacklists available. You can create blacklists in the Scripts-Blacklist Tab"))
  }
  else{
    return(
      shinyWidgets::prettyRadioButtons(inputId = "TM_blacklist",label = "Blacklists",
                                       choices = stringr::str_replace_all(string = list.files("collections/blacklists/"),pattern = ".txt",replacement = ""),
                                       fill=T,animation = "tada",selected = NULL)
    )
  }
})


#start topic model analysis script, if submit button is clicked
observeEvent(input$TM_Submit_Script,{
  valid<-check_pruning_parameters(min_t_c = input$TM_min_termfreq_c,max_t_c = input$TM_max_termfreq_c,min_t_p =input$TM_min_termfreq_p,max_t_p =  input$TM_max_termfreq_p
                                  ,min_t_r =input$TM_min_termfreq_r,max_t_r = input$TM_max_termfreq_r,min_t_q = input$TM_min_termfreq_q, max_t_q = input$TM_max_termfreq_q
                                  ,min_d_c = input$TM_min_docfreq_c,max_d_c = input$TM_max_docfreq_c,min_d_p = input$TM_min_docfreq_p,max_d_p = input$TM_max_docfreq_p
                                  ,min_d_r = input$TM_min_docfreq_r,max_d_r = input$TM_max_docfreq_r,min_d_q = input$TM_min_docfreq_q,max_d_q = input$TM_max_docfreq_q)
  if(isFALSE(valid)){
    shinyWidgets::confirmSweetAlert(session = session,title = "Check pruning settings!",text = HTML("It seems your current pruning input parameters don't make sense. It's very likely, that the whole vocabulary will be removed.
                           Check <a href='https://quanteda.io/reference/dfm_trim.html' title='quanteda pruning'> Quanteda Pruning Settings </a>"),html=T,inputId="TM_pruning_continue",
                                    type="warning",closeOnClickOutside = T,btn_labels = c("Change Settings","Continue anyway!"))
  }
  else{
    #get pruning parameters
    if(input$TM_termfreq_type=="count"){
      min_t<-input$TM_min_termfreq_c
      max_t<-input$TM_max_termfreq_c
    }
    if(input$TM_docfreq_type=="count"){
      min_d<-input$TM_min_docfreq_c
      max_d<-input$TM_max_docfreq_c
    }
    if(input$TM_termfreq_type=="rank"){
      min_t<-input$TM_min_termfreq_r
      max_t<-input$TM_max_termfreq_r
    }
    if(input$TM_docfreq_type=="rank"){
      min_d<-input$TM_min_docfreq_r
      max_d<-input$TM_max_docfreq_r
    }
    if(input$TM_termfreq_type=="prop"){
      min_t<-input$TM_min_termfreq_p
      max_t<-input$TM_max_termfreq_p
    }
    if(input$TM_docfreq_type=="prop"){
      min_d<-input$TM_min_docfreq_p
      max_d<-input$TM_max_docfreq_p
    }
    if(input$TM_termfreq_type=="quantile"){
      min_t<-input$TM_min_termfreq_q
      max_t<-input$TM_max_termfreq_q
    }
    if(input$TM_docfreq_type=="quantile"){
      min_d<-input$TM_min_docfreq_q
      max_d<-input$TM_max_docfreq_q
    }
    if(is.null(min_t))min_t<-NA
    if(is.null(max_t))max_t<-NA
    if(is.null(min_d))min_d<-NA
    if(is.null(max_d))max_d<-NA
    #save needed parameters
    parameters<-list(collection=input$collection_selected,
                     baseform_reduction=input$TM_baseform,
                     min_char=input$TM_min_char,
                     ngrams=input$TM_ngram,
                     remove_stopwords=input$TM_remove_stopwords,
                     lowercase=input$TM_lowercase,
                     remove_numbers=input$TM_remove_numbers,
                     remove_numbers_all=input$TM_remove_numbers_all,
                     remove_punctuation=input$TM_remove_punctuation,
                     remove_hyphenation=input$TM_remove_hyphenation,
                     min_term=min_t,
                     max_term=max_t,
                     min_document=min_d,
                     max_document=max_d,
                     min_cooc_freq=input$TM_min_Cooc_Freq,
                     remove_custom=input$TM_remove_custom,
                     consolidate_entities=input$TM_consolidate_entities,
                     blacklist=input$TM_blacklist,
                     reduce_POS=input$TM_POS_TYPES,
                     reduce_NER=input$TM_ENTITY_TYPES,
                     termfreq_type=input$TM_termfreq_type,
                     docfreq_type=input$TM_docfreq_type,
                     keep_custom=input$TM_keep_custom,
                     use_blacklist=input$TM_use_custom_blacklist,
                     use_whitelist=input$TM_use_custom_whitelist,
                     whitelist=input$TM_whitelist,
                     whitelist_expand=input$TM_whitelist_expand,
                     whitelist_only=input$TM_whitelist_only,
                     tm_number_of_topics=input$TM_number_of_topics,
                     tm_alpha=input$TM_alpha,
                     tm_method=input$TM_method,
                     tm_detailed_meta=input$TM_detailed_meta_dist
    )
    
    if(input$TM_method == "stm"){
      #add stm parameters
      stm_parameters <- list(
        stm_prevalenceFormula = input$stm_prevalenceFormula,
        stm_contentFormula = input$stm_contentFormula,
        stm_init_type = input$stm_init_type,
        stm_max_em_its = input$stm_max_em_its,
        stm_emtol = input$stm_emtol,
        stm_LDAbeta = input$stm_LDAbeta,
        stm_interactions = input$stm_interactions,
        stm_ngroups = input$stm_ngroups,
        stm_gamma_prior = input$stm_gamma_prior,
        stm_sigma_prior = input$stm_sigma_prior,
        stm_kappa_prior = input$stm_kappa_prior
      )
      parameters <- c(parameters, stm_parameters)
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
      system(paste0('Rscript collections/scripts/Topic_Model/',input$custom_script_options,' &'))
    }
    else{
      system(paste('Rscript collections/scripts/Topic_Model_Script.R','&'))
      #show modal when process is started
      showModal(modalDialog(
        title = "Process started",
        "The process was succesfully started. Check details in 'My Tasks'."
      ))
    }
  }
})



#start script after continue anyway is clicked even though pruning settings seem to be wrong
observeEvent(input$TM_pruning_continue,ignoreInit = T,{
  validate(
    need(isTRUE(input$TM_pruning_continue),message=F)
  )
  #get pruning parameters
  if(input$TM_termfreq_type=="count"){
    min_t<-input$TM_min_termfreq_c
    max_t<-input$TM_max_termfreq_c
  }
  if(input$TM_docfreq_type=="count"){
    min_d<-input$TM_min_docfreq_c
    max_d<-input$TM_max_docfreq_c
  }
  if(input$TM_termfreq_type=="rank"){
    min_t<-input$TM_min_termfreq_r
    max_t<-input$TM_max_termfreq_r
  }
  if(input$TM_docfreq_type=="rank"){
    min_d<-input$TM_min_docfreq_r
    max_d<-input$TM_max_docfreq_r
  }
  if(input$TM_termfreq_type=="prop"){
    min_t<-input$TM_min_termfreq_p
    max_t<-input$TM_max_termfreq_p
  }
  if(input$TM_docfreq_type=="prop"){
    min_d<-input$TM_min_docfreq_p
    max_d<-input$TM_max_docfreq_p
  }
  if(input$TM_termfreq_type=="quantile"){
    min_t<-input$TM_min_termfreq_q
    max_t<-input$TM_max_termfreq_q
  }
  if(input$TM_docfreq_type=="quantile"){
    min_d<-input$TM_min_docfreq_q
    max_d<-input$TM_max_docfreq_q
  }
  if(is.null(min_t))min_t<-NA
  if(is.null(max_t))max_t<-NA
  if(is.null(min_d))min_d<-NA
  if(is.null(max_d))max_d<-NA
  #save needed parameters
  parameters<-list(collection=input$collection_selected,
                   baseform_reduction=input$TM_baseform,
                   min_char=input$TM_min_char,
                   ngrams=input$TM_ngram,
                   remove_stopwords=input$TM_remove_stopwords,
                   lowercase=input$TM_lowercase,
                   remove_numbers=input$TM_remove_numbers,
                   remove_numbers_all=input$TM_remove_numbers_all,
                   remove_punctuation=input$TM_remove_punctuation,
                   remove_hyphenation=input$TM_remove_hyphenation,
                   min_term=min_t,
                   max_term=max_t,
                   min_document=min_d,
                   max_document=max_d,
                   min_cooc_freq=input$TM_min_Cooc_Freq,
                   remove_custom=input$TM_remove_custom,
                   consolidate_entities=input$TM_consolidate_entities,
                   blacklist=input$TM_blacklist,
                   reduce_POS=input$TM_POS_TYPES,
                   reduce_NER=input$TM_ENTITY_TYPES,
                   termfreq_type=input$TM_termfreq_type,
                   docfreq_type=input$TM_docfreq_type,
                   keep_custom=input$TM_keep_custom,
                   use_blacklist=input$TM_use_custom_blacklist,
                   use_whitelist=input$TM_use_custom_whitelist,
                   whitelist=input$TM_whitelist,
                   whitelist_expand=input$TM_whitelist_expand,
                   whitelist_only=input$TM_whitelist_only,
                   tm_number_of_topics=input$TM_number_of_topics,
                   tm_alpha=input$TM_alpha,
                   tm_method=input$TM_method,
                   tm_detailed_meta=input$TM_detailed_meta_dist
  )
  
  if(input$tm_method == "stm"){
    #add stm parameters
    stm_parameters <- list(
      stm_prevalenceFormula = input$stm_prevalenceFormula,
      stm_contentFormula = input$stm_contentFormula,
      stm_init_type = input$stm_init_type,
      stm_max_em_its = input$stm_max_em_its,
      stm_emtol = input$stm_emtol,
      stm_LDAbeta = input$stm_LDAbeta,
      stm_interactions = input$stm_interactions,
      stm_ngroups = input$stm_ngroups,
      stm_gamma_prior = input$stm_gamma_prior,
      stm_sigma_prior = input$stm_sigma_prior,
      stm_kappa_prior = input$stm_kappa_prior
    )
    parameters <- c(parameters, stm_parameters)
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
  save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
  #start script
  if(input$use_custom_script==TRUE && !is.null(input$custom_script_options)){
    shinyWidgets::sendSweetAlert(session=session,title = "Starting a custom script",text = "You are about to start a custom script. Caution with the calculation and results!",type = "info")
    system(paste0('Rscript collections/scripts/Topic_Model/',input$custom_script_options,' &'))
  }
  else{
    system(paste('Rscript collections/scripts/Topic_Model_Script.R','&'))
    #show modal when process is started
    showModal(modalDialog(
      title = "Process started",
      "The process was succesfully started. Check details in 'My Tasks'."
    ))
  }
})
