#' select Input to choose a collection
#' choices: check files in "collections/collections"
#' depends on: values$coll_saved: reactive variable that invalidates when a new collection is saved
output$Task_Scheduler_Collection<-renderUI({
  values$coll_saved
  selectizeInput(inputId = "collection_selected",label = "Collection:",choices = stringr::str_replace_all(string = list.files(path ="collections/collections/"),pattern = ".RData",replacement = ""))
})

#' if the user wants to use a custom script, show select statement with existing custom scripts
#' depends on: 
#'   values$new_script_saved: reactive variable that invalidates when a new custom script got saved
#'   input$analysis_selected: selected analysis method
output$custom_script_options_UI<-renderUI({
  values$new_script_saved
  if(input$analysis_selected==""){
    shinyWidgets::sendSweetAlert(session=session,title = "Please select an analysis",text = "Please choose a type of analysis",type = "warning")
    return(NULL)
  }
  else{
    choices<-list.files(paste0("collections/scripts/",input$analysis_selected,"/"))
    
    if(length(choices)==0){
      shinyWidgets::sendSweetAlert(session=session,title = "no custom script found!",text = "You can edit and save scipts in the 'Scripts-Tab' in the sidebar. Otherwise the standard script will be used.",type = "warning")
      return(NULL)
    }
    else{
      return(
        selectInput(inputId = "custom_script_options",label = "available custom scripts",choices = choices)
      )
    }
  }
})

#' select input showing the available parameter presets with option to save the current setting as preset
#' depends on:
#'   values$reload_presets: reactive variable that invalidates if a new parameter preset got saved
output$parameter_preset_UI<-renderUI({
  values$reload_presets
  available_presets<-stringr::str_remove(string = list.files(paste0("collections/parameter_settings/",input$analysis_selected)),pattern = ".RData")
  return(
    tagList(
      selectInput(inputId = "parameter_preset",label = "found presets:",choices = available_presets,multiple = F)%>%
        shinyInput_label_embed(
          shiny_iconlink() %>%
            bs_embed_popover(
              title = "Using a parameter preset sets the preprocessing and analysis parameters as they were set in a stored setting",
              placement = "right"
            )
        ),
      shinyBS::bsButton(inputId = "parameter_preset_set",label = "apply preset",icon = icon("play"))%>%
        bs_embed_tooltip(
          title = "Careful! The current setting will be replaced",
          placement = "right"
        ),
      shinyBS::bsButton(inputId = "parameter_preset_save",label = "save",icon = icon("save"))%>%
        bs_embed_tooltip(
          title = "Save the current settings as a parameter preset",
          placement = "right"
        )
    )
  )
})


#' save current parameter setting as a parameter preset
#' depends on:
#'   input$parameter_preset_save: saved preset parameter for analysis
#'   input$analysis_selected: selected analysis method
#'   values$preset_parameter: list of preset parameters for choosen analysis method
#'  parameter for Cooccurrence analysis:
#'    input$CA_baseform: should words be reduced to their baseform?
#'    input$CA_min_char: select minimum of characters a word needs to contain 
#'    input$CA_ngram: choose size of n-grams 
#'    input$CA_remove_custom: remove custom words? 
#'    input$CA_keep_custom: keep customed words? 
#'    input$CA_remove_stopwords:  should stopwords be removed? 
#'    input$CA_lowercase: should all words be put in lowercase
#'    input$CA_remove_numbers: should numbers in the documents be removed?
#'    input$CA_remove_numbers_all: should all words containing number be removed?
#'    input$CA_remove_punctuation: should the punctuation be removed?
#'    input$CA_remove_hyphenation: should hyphenation be removed?
#'    input$CA_consolidate_entities: should entities be consolidated?
#'    input$CA_use_custom_blacklist: should a custome blacklist be used?
#'    input$CA_use_custom_whitelist: should a custome whitelist be used?
#'    input$CA_blacklist: blacklist of words that should be removed from the texts
#'    input$CA_whitelist: selected whitelist of words to keep in analysis
#'    input$CA_whitelist_only: just use words on whitelist
#'    input$CA_whitelist_expand: expand current whitelist
#'    input$CA_termfreq_type: choose a term frequency type (count, quantile, rank, probability)
#'    input$CA_min_termfreq_c: minimum term frequency (count)
#'    input$CA_max_termfreq_c: maximum term frequency (count)
#'    input$CA_min_termfreq_p: minimum term probability
#'    input$CA_max_termfreq_p: maximum term probalility
#'    input$CA_min_termfreq_r: minimum term rank
#'    input$CA_max_termfreq_r: maximum term rank
#'    input$CA_min_termfreq_q: minimum term quantile
#'    input$CA_max_termfreq_q: maximum document quantile
#'    input$CA_docfreq_type: choosen document frequency type
#'    input$CA_min_docfreq_c: minimum document frequency (count)
#'    input$CA_max_docfreq_c: maximum document frequency (count)
#'    input$CA_min_docfreq_p: minimum document probability
#'    input$CA_max_docfreq_p: maximum document probability
#'    input$CA_min_docfreq_r: minimum document rank
#'    input$CA_max_docfreq_r: maximum document rank
#'    input$CA_min_docfreq_q: minimum document quantile
#'    input$CA_max_docfreq_q: maximum document quantile
#'    input$CA_cooc_type: which type of cooccurrence analysis should be used? 
#'    input$CA_min_Cooc_Freq: select a minimum frequence for words for cooccurrence calculation
#'    input$CA_POS_TYPES: select part of speech types that should be used
#'    input$CA_ENTITY_TYPES:  select entity (NER) types that should be used
#'  parameter for classification (only new parameter mentioned)
#'    input$CL_Context_Unit: select context unit
#'    input$CL_Project: select project for classification
#'    input$CL_Mode: select classification mode
#'    input$CL_c: parameter to tell how much to avoid missclassifying each training example
#'    input$CL_Threshold: classification threshold
#'    input$CL_active_learning_strategy: selected strategy on which the document selection for active learning examples based on
#'    input$CL_use_dict: use a dictionary?
#'    input$CL_Category: classification category for generating active learning examples
#'    input$CL_dict: selected dictionary
#'  parameter for dictionary extraction (only new parameter mentioned)
#'    input$DE_use_reg_exp: use regular expressions?
#'    input$DE_regexp_input: input for regular expressions
#'    input$DE_use_context_filter: should a context filter be used?
#'    input$DE_Context_Filter: use context filter to just count occurrence of dictionary term 
#'    input$DE_reg_exp: use regular expressions?
#'    input$DE_context_unit: specify the window within the specific context has to co-occure within dictionary terms (sentence or document)
#'  parameter for volatility analysis (only new parameter mentioned)
#'    input$VA_min_Cooc_Freq: minimum cooccurrence frequency
#'    input$VA_Cooc_Measure:  which distand measurement should be used? (manhatten distance, eucledean distance, etc.)
#'    input$VA_timeintervall: which time intervall (day, month, year) should be used?
#'    input$VA_history: how large should the memory storage be?
#'    input$VA_method: which method should be used? (significance or ranks)
#'    input$VA_cooc_type: which Co-occurrence significance measure should be used? (Dice, log likelihood, mutual information)
#'    input$VA_weightfactor: selected weight factor
#'  parameter for keyword extraction (only new parameter mentioned)
#'    input$KE_Mode: select keyword extraction mode (RAKE, PMI collocation, phrase sequence, text rank)
#'    input$KE_no_ref_method: referenced methods
#'    input$KE_no_ref_n_min: referenced minimal n
#'    input$KE_no_ref_ngram_max: referenced n-gram maximum
#'    input$KE_seperator: choosen seperator for phrases
#'    input$KE_filter: choosen key extraction filter
#'    input$KE_phrase: choosen phrase pattern 
#'  parameter for sentiment analysis (only new mentioned)
#'    input$SA_sentiment_dictionary: choose sentiment dictionary (depends on language of the text)
#'    input$SA_avg:  selected document score aggregation
#'  parameter for document deduplication  
#'    input$DD_similarity_measure: selected similarity measurement
#'  parameter for syntactic parsing (only new mentioned)
#'    input$SP_cores: select a number of cores to start the calculation
#'  parameter for vector space representation (only new mentioned)
#'    input$VS_min_occ: set minimum occurrence for words
#'    input$VS_vectors: set number of vectors for the output
#'    input$VS_threads: set a number of threads to run the trainingsprocess on
#'    input$VS_window: set the window size used in training
#'    input$VS_iter: number of passes to make over the corpus during training
#'    input$VS_neg_samples:negative sectioned samples
#'    input$VS_train_dim_reduction: reduction of vector dimension for training 
#'  parameter for topic analysis
#'    input$TM_number_of_topics: how many topics should be calculated?
#'    input$TM_method: choose a backend method to calculate the topic models
#'    input$TM_alpha: set document topic density
#'    input$TM_detailed_meta_dist: choose a distance masurement for detailed meta data analysis
#'    input$stm_prevalenceFormula: formula with no response variable or a matrix containing topic prevalence covariates
#'    input$stm_contentFormula: formula containing a single variable or something which can be coerced to a factor indicating the category of the content variable for each document
#'    input$stm_init_type: choose method of initialization (spectral or LDA or custom or random option possible)
#'    input$stm_max_em_its: maximum number of EM iterations
#'    input$stm_emtol: convergence tolerance - EM stops when the relatice change in the approximate bound drops below this level
#'    input$stm_ngroups: Number of groups for memorized inference
#'    input$stm_LDAbeta: is LDAbeta selected? automatically ticket when there are no content covariates
#'    input$stm_interactions: are the interactions between content covariates and the latent topic activated?
#'    input$stm_gamma_prior: set the prior estimation method for the prevalence covariate model (pooled or L1)
#'    input$stm_sigma_prior: set a value for the strength of regularization toward a diagonalized covariance matrix
#'    input$stm_kappa_prior: set method for structural topic modeling (L1 or Jeffreys)
#'          
observeEvent(input$parameter_preset_save,{
  chosen_analysis<-input$analysis_selected
  if(input$analysis_selected=="Cooccurrence_Analysis"){
    values$preset_parameter=list(
      CA_baseform=input$CA_baseform,
      CA_min_char=input$CA_min_char,
      CA_ngram=input$CA_ngram,
      CA_remove_custom=input$CA_remove_custom,
      CA_keep_custom=input$CA_keep_custom,
      CA_remove_stopwords=input$CA_remove_stopwords,
      CA_lowercase=input$CA_lowercase,
      CA_remove_numbers=input$CA_remove_numbers,
      CA_remove_numbers_all=input$CA_remove_numbers_all,
      CA_remove_punctuation=input$CA_remove_punctuation,
      CA_remove_hyphenation=input$CA_remove_hyphenation,
      CA_consolidate_entities=input$CA_consolidate_entities,
      CA_use_custom_blacklist=input$CA_use_custom_blacklist,
      CA_use_custom_whitelist=input$CA_use_custom_whitelist,
      CA_blacklist=input$CA_blacklist,
      CA_whitelist=input$CA_whitelist,
      CA_whitelist_only=input$CA_whitelist_only,
      CA_whitelist_expand=input$CA_whitelist_expand,
      CA_termfreq_type=input$CA_termfreq_type,
      CA_min_termfreq_c=input$CA_min_termfreq_c,
      CA_max_termfreq_c=input$CA_max_termfreq_c,
      CA_min_termfreq_p=input$CA_min_termfreq_p,
      CA_max_termfreq_p=input$CA_max_termfreq_p,
      CA_min_termfreq_r=input$CA_min_termfreq_r,
      CA_max_termfreq_r=input$CA_max_termfreq_r,
      CA_min_termfreq_q=input$CA_min_termfreq_q,
      CA_max_termfreq_q=input$CA_max_termfreq_q,
      CA_docfreq_type=input$CA_docfreq_type,
      CA_min_docfreq_c=input$CA_min_docfreq_c,
      CA_max_docfreq_c=input$CA_max_docfreq_c,
      CA_min_docfreq_p=input$CA_min_docfreq_p,
      CA_max_docfreq_p=input$CA_max_docfreq_p,
      CA_min_docfreq_r=input$CA_min_docfreq_r,
      CA_max_docfreq_r=input$CA_max_docfreq_r,
      CA_min_docfreq_q=input$CA_min_docfreq_q,
      CA_max_docfreq_q=input$CA_max_docfreq_q,
      CA_cooc_type=input$CA_cooc_type,
      CA_min_Cooc_Freq=input$CA_min_Cooc_Freq,
      CA_POS_TYPES=input$CA_POS_TYPES,
      CA_ENTITY_TYPES=input$CA_ENTITY_TYPES
    )
  }
  if(input$analysis_selected=="Classification"){
    values$preset_parameter=list(
      CL_baseform=input$CL_baseform,
      CL_min_char=input$CL_min_char,
      CL_ngram=input$CL_ngram,
      CL_remove_custom=input$CL_remove_custom,
      CL_keep_custom=input$CL_keep_custom,
      CL_remove_stopwords=input$CL_remove_stopwords,
      CL_lowercase=input$CL_lowercase,
      CL_remove_numbers=input$CL_remove_numbers,
      CL_remove_numbers_all=input$CL_remove_numbers_all,
      CL_remove_punctuation=input$CL_remove_punctuation,
      CL_remove_hyphenation=input$CL_remove_hyphenation,
      CL_use_custom_blacklist=input$CL_use_custom_blacklist,
      CL_use_custom_whitelist=input$CL_use_custom_whitelist,
      CL_whitelist_only=input$CL_whitelist_only,
      CL_whitelist_expand=input$CL_whitelist_expand,
      CL_POS_TYPES=input$CL_POS_TYPES,
      CL_ENTITY_TYPES=input$CL_ENTITY_TYPES,
      CL_termfreq_type=input$CL_termfreq_type,
      CL_min_termfreq_c=input$CL_min_termfreq_c,
      CL_max_termfreq_c=input$CL_max_termfreq_c,
      CL_min_termfreq_p=input$CL_min_termfreq_p,
      CL_max_termfreq_p=input$CL_max_termfreq_p,
      CL_min_termfreq_r=input$CL_min_termfreq_r,
      CL_max_termfreq_r=input$CL_max_termfreq_r,
      CL_min_termfreq_q=input$CL_min_termfreq_q,
      CL_max_termfreq_q=input$CL_max_termfreq_q,
      CL_docfreq_type=input$CL_docfreq_type,
      CL_min_docfreq_c=input$CL_min_docfreq_c,
      CL_max_docfreq_c=input$CL_max_docfreq_c,
      CL_min_docfreq_p=input$CL_min_docfreq_p,
      CL_max_docfreq_p=input$CL_max_docfreq_p,
      CL_min_docfreq_r=input$CL_min_docfreq_r,
      CL_max_docfreq_r=input$CL_max_docfreq_r,
      CL_min_docfreq_q=input$CL_min_docfreq_q,
      CL_max_docfreq_q=input$CL_max_docfreq_q,
      CL_Context_Unit=input$CL_Context_Unit,
      CL_Project=input$CL_Project,
      CL_Mode=input$CL_Mode,
      CL_c=input$CL_c,
      CL_Threshold=input$CL_Threshold,
      CL_active_learning_strategy=input$CL_active_learning_strategy,
      CL_use_dict=input$CL_use_dict,
      CL_Category=input$CL_Category,
      CL_dict=input$CL_dict,
      CL_whitelist=input$CL_whitelist,
      CL_blacklist=input$CL_blacklist
    )
  }
  if(input$analysis_selected=="Dictionary_Extraction"){
    values$preset_parameter=list(
      DE_baseform=input$DE_baseform,
      DE_min_char=input$DE_min_char,
      DE_ngram=input$DE_ngram,
      DE_remove_custom=input$DE_remove_custom,
      DE_keep_custom=input$DE_keep_custom,
      DE_remove_stopwords=input$DE_remove_stopwords,
      DE_lowercase=input$DE_lowercase,
      DE_remove_numbers=input$DE_remove_numbers,
      DE_remove_numbers_all=input$DE_remove_numbers_all,
      DE_remove_punctuation=input$DE_remove_punctuation,
      DE_remove_hyphenation=input$DE_remove_hyphenation,
      DE_consolidate_entities=input$DE_consolidate_entities,
      DE_use_custom_blacklist=input$DE_use_custom_blacklist,
      DE_blacklist=input$DE_blacklist,
      DE_use_custom_whitelist=input$DE_use_custom_whitelist,
      DE_whitelist=input$DE_whitelist,
      DE_whitelist_expand=input$DE_whitelist_expand,
      DE_termfreq_type=input$DE_termfreq_type,
      DE_min_termfreq_c=input$DE_min_termfreq_c,
      DE_max_termfreq_c=input$DE_max_termfreq_c,
      DE_min_termfreq_p=input$DE_min_termfreq_p,
      DE_max_termfreq_p=input$DE_max_termfreq_p,
      DE_min_termfreq_r=input$DE_min_termfreq_r,
      DE_max_termfreq_r=input$DE_max_termfreq_r,
      DE_min_termfreq_q=input$DE_min_termfreq_q,
      DE_max_termfreq_q=input$DE_max_termfreq_q,
      DE_docfreq_type=input$DE_docfreq_type,
      DE_min_docfreq_c=input$DE_min_docfreq_c,
      DE_max_docfreq_c=input$DE_max_docfreq_c,
      DE_min_docfreq_p=input$DE_min_docfreq_p,
      DE_max_docfreq_p=input$DE_max_docfreq_p,
      DE_min_docfreq_r=input$DE_min_docfreq_r,
      DE_max_docfreq_r=input$DE_max_docfreq_r,
      DE_min_docfreq_q=input$DE_min_docfreq_q,
      DE_max_docfreq_q=input$DE_max_docfreq_q,
      DE_POS_TYPES=input$DE_POS_TYPES,
      DE_ENTITY_TYPES=input$DE_ENTITY_TYPES,
      DE_use_reg_exp=input$DE_use_reg_exp,
      DE_regexp_input=input$DE_regexp_input,
      DE_use_context_filter=input$DE_use_context_filter,
      DE_Context_Filter=input$DE_Context_Filter,
      DE_reg_exp=input$DE_reg_exp,
      DE_context_unit=input$DE_context_unit,
      DE_dict=input$DE_dict
    )
  } 
  if(input$analysis_selected=="Frequency_Extraction"){
    values$preset_parameter=list(
      FE_baseform=input$FE_baseform,
      FE_min_char=input$FE_min_char,
      FE_ngram=input$FE_ngram,
      FE_remove_custom=input$FE_remove_custom,
      FE_keep_custom=input$FE_keep_custom,
      FE_remove_stopwords=input$FE_remove_stopwords,
      FE_lowercase=input$FE_lowercase,
      FE_remove_numbers=input$FE_remove_numbers,
      FE_remove_numbers_all=input$FE_remove_numbers_all,
      FE_remove_punctuation=input$FE_remove_punctuation,
      FE_remove_hyphenation=input$FE_remove_hyphenation,
      FE_consolidate_entities=input$FE_consolidate_entities,
      FE_use_custom_blacklist=input$FE_use_custom_blacklist,
      FE_blacklist=input$FE_blacklist,
      FE_use_custom_whitelist=input$FE_use_custom_whitelist,
      FE_whitelist=input$FE_whitelist,
      FE_whitelist_expand=input$FE_whitelist_expand,
      FE_termfreq_type=input$FE_termfreq_type,
      FE_min_termfreq_c=input$FE_min_termfreq_c,
      FE_max_termfreq_c=input$FE_max_termfreq_c,
      FE_min_termfreq_p=input$FE_min_termfreq_p,
      FE_max_termfreq_p=input$FE_max_termfreq_p,
      FE_min_termfreq_r=input$FE_min_termfreq_r,
      FE_max_termfreq_r=input$FE_max_termfreq_r,
      FE_min_termfreq_q=input$FE_min_termfreq_q,
      FE_max_termfreq_q=input$FE_max_termfreq_q,
      FE_docfreq_type=input$FE_docfreq_type,
      FE_min_docfreq_c=input$FE_min_docfreq_c,
      FE_max_docfreq_c=input$FE_max_docfreq_c,
      FE_min_docfreq_p=input$FE_min_docfreq_p,
      FE_max_docfreq_p=input$FE_max_docfreq_p,
      FE_min_docfreq_r=input$FE_min_docfreq_r,
      FE_max_docfreq_r=input$FE_max_docfreq_r,
      FE_min_docfreq_q=input$FE_min_docfreq_q,
      FE_max_docfreq_q=input$FE_max_docfreq_q,
      FE_POS_TYPES=input$FE_POS_TYPES,
      FE_ENTITY_TYPES=input$FE_ENTITY_TYPES
    )
  }  
  if(input$analysis_selected=="Dynamic_Topic_Model"){
    values$preset_parameter=list(
      DTM_baseform=input$DTM_baseform,
      DTM_min_char=input$DTM_min_char,
      DTM_ngram=input$DTM_ngram,
      DTM_remove_custom=input$DTM_remove_custom,
      DTM_keep_custom=input$DTM_keep_custom,
      DTM_remove_stopwords=input$DTM_remove_stopwords,
      DTM_lowercase=input$DTM_lowercase,
      DTM_remove_numbers=input$DTM_remove_numbers,
      DTM_remove_numbers_all=input$DTM_remove_numbers_all,
      DTM_remove_punctuation=input$DTM_remove_punctuation,
      DTM_remove_hyphenation=input$DTM_remove_hyphenation,
      DTM_consolidate_entities=input$DTM_consolidate_entities,
      DTM_use_custom_blacklist=input$DTM_use_custom_blacklist,
      DTM_blacklist=input$DTM_blacklist,
      DTM_use_custom_whitelist=input$DTM_use_custom_whitelist,
      DTM_whitelist=input$DTM_whitelist,
      DTM_whitelist_expand=input$DTM_whitelist_expand,
      DTM_termfreq_type=input$DTM_termfreq_type,
      DTM_min_termfreq_c=input$DTM_min_termfreq_c,
      DTM_max_termfreq_c=input$DTM_max_termfreq_c,
      DTM_min_termfreq_p=input$DTM_min_termfreq_p,
      DTM_max_termfreq_p=input$DTM_max_termfreq_p,
      DTM_min_termfreq_r=input$DTM_min_termfreq_r,
      DTM_max_termfreq_r=input$DTM_max_termfreq_r,
      DTM_min_termfreq_q=input$DTM_min_termfreq_q,
      DTM_max_termfreq_q=input$DTM_max_termfreq_q,
      DTM_docfreq_type=input$DTM_docfreq_type,
      DTM_min_docfreq_c=input$DTM_min_docfreq_c,
      DTM_max_docfreq_c=input$DTM_max_docfreq_c,
      DTM_min_docfreq_p=input$DTM_min_docfreq_p,
      DTM_max_docfreq_p=input$DTM_max_docfreq_p,
      DTM_min_docfreq_r=input$DTM_min_docfreq_r,
      DTM_max_docfreq_r=input$DTM_max_docfreq_r,
      DTM_min_docfreq_q=input$DTM_min_docfreq_q,
      DTM_max_docfreq_q=input$DTM_max_docfreq_q,
      DTM_POS_TYPES=input$DTM_POS_TYPES,
      DTM_ENTITY_TYPES=input$DTM_ENTITY_TYPES,
      DTM_number_of_topics=input$DTM_number_of_topics,
      DTM_alpha=input$DTM_alpha,
      DTM_chain_variance=input$DTM_chain_variance,
      DTM_Date_Split_How=input$DTM_Date_Split_How,
      DTM_Chunksize=input$DTM_Chunksize,
      DTM_ByDate_Type=input$DTM_ByDate_Type,
      DTM_ByDate_n=input$DTM_ByDate_n,
      DTM_chunk_documents_n=input$DTM_chunk_documents_n,
      DTM_chunk_documents=input$DTM_chunk_documents
    )
  }  
  if(input$analysis_selected=="Volatility_Analysis"){
    values$preset_parameter=list(
      VA_baseform=input$VA_baseform,
      VA_min_char=input$VA_min_char,
      VA_ngram=input$VA_ngram,
      VA_remove_custom=input$VA_remove_custom,
      VA_keep_custom=input$VA_keep_custom,
      VA_remove_stopwords=input$VA_remove_stopwords,
      VA_lowercase=input$VA_lowercase,
      VA_remove_numbers=input$VA_remove_numbers,
      VA_remove_numbers_all=input$VA_remove_numbers_all,
      VA_remove_punctuation=input$VA_remove_punctuation,
      VA_remove_hyphenation=input$VA_remove_hyphenation,
      VA_consolidate_entities=input$VA_consolidate_entities,
      VA_use_custom_blacklist=input$VA_use_custom_blacklist,
      VA_blacklist=input$VA_blacklist,
      VA_use_custom_whitelist=input$VA_use_custom_whitelist,
      VA_whitelist=input$VA_whitelist,
      VA_whitelist_expand=input$VA_whitelist_expand,
      VA_termfreq_type=input$VA_termfreq_type,
      VA_min_termfreq_c=input$VA_min_termfreq_c,
      VA_max_termfreq_c=input$VA_max_termfreq_c,
      VA_min_termfreq_p=input$VA_min_termfreq_p,
      VA_max_termfreq_p=input$VA_max_termfreq_p,
      VA_min_termfreq_r=input$VA_min_termfreq_r,
      VA_max_termfreq_r=input$VA_max_termfreq_r,
      VA_min_termfreq_q=input$VA_min_termfreq_q,
      VA_max_termfreq_q=input$VA_max_termfreq_q,
      VA_docfreq_type=input$VA_docfreq_type,
      VA_min_docfreq_c=input$VA_min_docfreq_c,
      VA_max_docfreq_c=input$VA_max_docfreq_c,
      VA_min_docfreq_p=input$VA_min_docfreq_p,
      VA_max_docfreq_p=input$VA_max_docfreq_p,
      VA_min_docfreq_r=input$VA_min_docfreq_r,
      VA_max_docfreq_r=input$VA_max_docfreq_r,
      VA_min_docfreq_q=input$VA_min_docfreq_q,
      VA_max_docfreq_q=input$VA_max_docfreq_q,
      VA_POS_TYPES=input$VA_POS_TYPES,
      VA_ENTITY_TYPES=input$VA_ENTITY_TYPES,
      VA_min_Cooc_Freq=input$VA_min_Cooc_Freq,
      VA_Cooc_Measure=input$VA_Cooc_Measure,
      VA_timeintervall=input$VA_timeintervall,
      VA_history=input$VA_history,
      VA_method=input$VA_method,
      VA_cooc_type=input$VA_cooc_type,
      VA_weightfactor=input$VA_weightfactor
    )
  }  
  if(input$analysis_selected=="Keyword_Extraction"){
    values$preset_parameter=list(
      KE_baseform=input$KE_baseform,
      KE_lowercase=input$KE_lowercase,
      KE_Mode=input$KE_Mode,
      KE_no_ref_method=input$KE_no_ref_method,
      KE_no_ref_n_min=input$KE_no_ref_n_min,
      KE_no_ref_ngram_max=input$KE_no_ref_ngram_max,
      KE_seperator=input$KE_seperator,
      KE_filter=input$KE_filter,
      KE_phrase=input$KE_phrase
    )
  }
  if(input$analysis_selected=="Sentiment_Analysis"){
    values$preset_parameter=list(
      SA_baseform=input$SA_baseform,
      SA_lowercase=input$SA_lowercase,
      SA_ngram=input$SA_ngram,
      SA_remove_custom=input$SA_remove_custom,
      SA_sentiment_dictionary=input$SA_sentiment_dictionary,
      SA_avg=input$SA_avg
    )
  }
  if(input$analysis_selected=="Document_Deduplication"){
    values$preset_parameter=list(
      DD_similarity_measure=input$DD_similarity_measure
    )
  }  
  if(input$analysis_selected=="Syntactic_Parsing"){
    values$preset_parameter=list(
      SP_cores=input$SP_cores
    )
  } 
  if(input$analysis_selected=="Vector_Space_Representation"){
    values$preset_parameter=list(
      VS_use_model=input$VS_use_model,
      VS_model=input$VS_model,
      VS_lowercase=input$VS_lowercase,
      VS_ngram=input$VS_ngram,
      VS_stopwords=input$VS_stopwords,
      VS_punctuation=input$VS_punctuation,
      VS_min_occ=input$VS_min_occ,
      VS_vectors=input$VS_vectors,
      VS_threads=input$VS_threads,
      VS_window=input$VS_window,
      VS_iter=input$VS_iter,
      VS_neg_samples=input$VS_neg_samples,
      VS_train_dim_reduction=input$VS_train_dim_reduction
    )
  }
  if(input$analysis_selected=="Topic_Model"){
    values$preset_parameter=list(
      TM_baseform=input$TM_baseform,
      TM_min_char=input$TM_min_char,
      TM_ngram=input$TM_ngram,
      TM_remove_custom=input$TM_remove_custom,
      TM_keep_custom=input$TM_keep_custom,
      TM_remove_stopwords=input$TM_remove_stopwords,
      TM_lowercase=input$TM_lowercase,
      TM_remove_numbers=input$TM_remove_numbers,
      TM_remove_numbers_all=input$TM_remove_numbers_all,
      TM_remove_punctuation=input$TM_remove_punctuation,
      TM_remove_hyphenation=input$TM_remove_hyphenation,
      TM_consolidate_entities=input$TM_consolidate_entities,
      TM_use_custom_blacklist=input$TM_use_custom_blacklist,
      TM_blacklist=input$TM_blacklist,
      TM_use_custom_whitelist=input$TM_use_custom_whitelist,
      TM_whitelist=input$TM_whitelist,
      TM_whitelist_expand=input$TM_whitelist_expand,
      TM_termfreq_type=input$TM_termfreq_type,
      TM_min_termfreq_c=input$TM_min_termfreq_c,
      TM_max_termfreq_c=input$TM_max_termfreq_c,
      TM_min_termfreq_p=input$TM_min_termfreq_p,
      TM_max_termfreq_p=input$TM_max_termfreq_p,
      TM_min_termfreq_r=input$TM_min_termfreq_r,
      TM_max_termfreq_r=input$TM_max_termfreq_r,
      TM_min_termfreq_q=input$TM_min_termfreq_q,
      TM_max_termfreq_q=input$TM_max_termfreq_q,
      TM_docfreq_type=input$TM_docfreq_type,
      TM_min_docfreq_c=input$TM_min_docfreq_c,
      TM_max_docfreq_c=input$TM_max_docfreq_c,
      TM_min_docfreq_p=input$TM_min_docfreq_p,
      TM_max_docfreq_p=input$TM_max_docfreq_p,
      TM_min_docfreq_r=input$TM_min_docfreq_r,
      TM_max_docfreq_r=input$TM_max_docfreq_r,
      TM_min_docfreq_q=input$TM_min_docfreq_q,
      TM_max_docfreq_q=input$TM_max_docfreq_q,
      TM_POS_TYPES=input$TM_POS_TYPES,
      TM_ENTITY_TYPES=input$TM_ENTITY_TYPES,
      TM_number_of_topics=input$TM_number_of_topics,
      TM_method=input$TM_method,
      TM_alpha=input$TM_alpha,
      TM_detailed_meta_dist=input$TM_detailed_meta_dist,
      stm_prevalenceFormula=input$stm_prevalenceFormula,
      stm_contentFormula=input$stm_contentFormula,
      stm_init_type=input$stm_init_type,
      stm_max_em_its=input$stm_max_em_its,
      stm_emtol=input$stm_emtol,
      stm_ngroups=input$stm_ngroups,
      stm_LDAbeta=input$stm_LDAbeta,
      stm_interactions=input$stm_interactions,
      stm_gamma_prior=input$stm_gamma_prior,
      stm_sigma_prior=input$stm_sigma_prior,
      stm_kappa_prior=input$stm_kappa_prior,
      TM_chunk_documents_n=input$TM_chunk_documents_n,
      TM_chunk_documents=input$TM_chunk_documents
    )
  }
  # show modal view where user can specify the name of the preset
  shiny::showModal(
    shiny::modalDialog(
      textInput(inputId = "parameter_preset_name",label = "name for preset",value = ""),
      shinyBS::bsButton(inputId = "parameters_preset_save_file",label = "save",icon = icon("save"))
    )
  )
})

#' check whether file name is already used; if no file with that name exists: save file
#' else ask user to confirm overwrite
#' depends on:
#'   input$parameters_preset_save_file: preset parameter to save an file
#'   input$analysis_selected: selected analysis method
#'   input$parameter_preset_name: preset name for the file
#'   values$preset_parameter: preset parameter for calculation
#'   values$reload_presets: reload presets
observeEvent(input$parameters_preset_save_file,{
  filename<-paste0("collections/parameter_settings/",input$analysis_selected,"/",input$parameter_preset_name,".RData")
  preset_parameter<-values$preset_parameter
  if(!file.exists(filename)){
    save(preset_parameter,file = filename)
    values$reload_presets<-runif(1,0,1)
    removeModal()
    shinyWidgets::sendSweetAlert(session = session,title = "preset saved!",text = "",type = "success",closeOnClickOutside = T)
  }
  else{
    shinyWidgets::confirmSweetAlert(session=session, inputId = "parameter_preset_confirm_overwrite",type="warning",title = "File with the specified name already exists.",
                                    text = "Do you want to overwrite the file?",btn_labels = c("Change Name","Overwrite existing file"))
  }
})

#' if user wants to overwrite existing parameter preset --> overwrite existing preset with current setting
#' depends on:
#'   input$parameter_preset_confirm_overwrite: confirm to overwrite the preset parameters
#'   input$analysis_selected: selected analysis model
#'   input$parameter_preset_name: preset parameter names
#'   values$preset_parameter: preset parameter (with values)
#'   values$reload_presets: reload the preset parameters
observeEvent(input$parameter_preset_confirm_overwrite,{
  validate(
    need(isTRUE(input$parameter_preset_confirm_overwrite),message=F)
  )
  filename<-paste0("collections/parameter_settings/",input$analysis_selected,"/",input$parameter_preset_name,".RData")
  preset_parameter<-values$preset_parameter
  save(preset_parameter,file = filename)
  values$reload_presets<-runif(1,0,1)
  removeModal()
  shinyWidgets::sendSweetAlert(session = session,title = "preset saved!",text = "",type = "success",closeOnClickOutside = T)
})



#' try to update parameters corresponding to chosen preset
#' depends on:
#'   input$parameter_preset_set: set of preset parameters
observeEvent(input$parameter_preset_set,{
  if(input$parameter_preset==""){
    shinyWidgets::sendSweetAlert(session = session,title = "No preset found for this analysis!",text = "You need to create at least one parameter preset before you can apply presets",
                                 type="warning",closeOnClickOutside = T)
  }else{
    load(paste0("collections/parameter_settings/",input$analysis_selected,"/",input$parameter_preset,".RData"))
    updateShinyInputs(session, preset_parameter)
  }
})






source(file.path("server","tab_Task_Scheduler_Coocs.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Frequency_Extraction.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Dictionary_Extraction.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Sentiment_Analysis.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Volatility_Analysis.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Topic_Model.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Classification.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Rest.R"),local = T)$value
#source(file.path("server","tab_Task_Scheduler_Factorial_Analysis.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Vector_Space_Representation.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Document_Deduplication.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Keyword_Extraction.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Syntactic_Parsing.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Geocoding.R"),local = T)$value
source(file.path("server","tab_Task_Scheduler_Dynamic_Topic_Modeling.R"),local=T)$value



#' check if analysis help button was clicked, if yes,open modal with rmd.files
#' depends on:
#'   input$analysis_help: help files with information about analysis
#'   input$analysis_selected: selected analysis method
observeEvent(input$analysis_help,ignoreInit = T,{
  path_to_tutorial<-paste0("Tutorials/",input$analysis_selected,".md")
  if(file.exists(path_to_tutorial)){
    showModal(
      modalDialog(title = paste0("Tutorial for ",input$analysis_selected),size = "l",fade = T,
                  includeMarkdown(path_to_tutorial)
                  
      )
    )
  }
  else{
    shinyWidgets::sendSweetAlert(session=session,title = "No Tutorial found",text = "For your chosen analysis, there is no tutorial .md file located in the tutorials directory Sorry!",type = "warning")
  }
  
})



#' check if option "all" is selected along with an other option in include POS input
#' if this is the case, remove "all"
#' depends on:
#'   input$CA_POS_TYPES: which POS-Tags to use in analysis
observe({
  validate(
    need(!is.null(input$CA_POS_TYPES),message=F),
    need(length(input$CA_POS_TYPES)>1,message=F),
    need("all"%in%input$CA_POS_TYPES,message=F)
  )
  choices_update<-input$CA_POS_TYPES[-which(input$CA_POS_TYPES=="all")]
  updateSelectInput(session = session,inputId = "CA_POS_TYPES",selected = choices_update)
})

#' check if option "all" is selected along with an other option in include NE input
#' if this is the case, remove "all"
#' depends on:
#'   input$CA_ENTITY_TYPES: which NE-Tags to use in analysis
observe({
  validate(
    need(!is.null(input$CA_ENTITY_TYPES),message=F),
    need(length(input$CA_ENTITY_TYPES)>1,message=F),
    need("all"%in%input$CA_ENTITY_TYPES,message=F)
  )
  choices_update<-input$CA_ENTITY_TYPES[-which(input$CA_ENTITY_TYPES=="all")]
  updateSelectInput(session = session,inputId = "CA_ENTITY_TYPES",selected = choices_update)
})


#' check if option "all" is selected along with an other option in include POS input
#' if this is the case, remove "all"
#' depends on:
#'   input$VA_POS_TYPES: which POS-Tags to use in analysis
observe({
  validate(
    need(!is.null(input$VA_POS_TYPES),message=F),
    need(length(input$VA_POS_TYPES)>1,message=F),
    need("all"%in%input$VA_POS_TYPES,message=F)
  )
  choices_update<-input$VA_POS_TYPES[-which(input$VA_POS_TYPES=="all")]
  updateSelectInput(session = session,inputId = "VA_POS_TYPES",selected = choices_update)
})

#' check if option "all" is selected along with an other option in include NE input
#' if this is the case, remove "all"
#' depends on:
#'   input$VA_ENTITY_TYPES: which NE-Tags to use in analysis
observe({
  validate(
    need(!is.null(input$VA_ENTITY_TYPES),message=F),
    need(length(input$VA_ENTITY_TYPES)>1,message=F),
    need("all"%in%input$VA_ENTITY_TYPES,message=F)
  )
  choices_update<-input$VA_ENTITY_TYPES[-which(input$VA_ENTITY_TYPES=="all")]
  updateSelectInput(session = session,inputId = "VA_ENTITY_TYPES",selected = choices_update)
})


#' check if option "all" is selected along with an other option in include POS input
#' if this is the case, remove "all"
#' depends on:
#'   input$FE_POS_TYPES: which POS-Tags to use in analysis
observe({
  validate(
    need(!is.null(input$FE_POS_TYPES),message=F),
    need(length(input$FE_POS_TYPES)>1,message=F),
    need("all"%in%input$FE_POS_TYPES,message=F)
  )
  choices_update<-input$FE_POS_TYPES[-which(input$FE_POS_TYPES=="all")]
  updateSelectInput(session = session,inputId = "FE_POS_TYPES",selected = choices_update)
})

#' check if option "all" is selected along with an other option in include NE input
#' if this is the case, remove "all"
#' depends on:
#'   input$FE_ENTITY_TYPES: which NE-Tags to use in analysis
observe({
  validate(
    need(!is.null(input$FE_ENTITY_TYPES),message=F),
    need(length(input$FE_ENTITY_TYPES)>1,message=F),
    need("all"%in%input$FE_ENTITY_TYPES,message=F)
  )
  choices_update<-input$FE_ENTITY_TYPES[-which(input$FE_ENTITY_TYPES=="all")]
  updateSelectInput(session = session,inputId = "FE_ENTITY_TYPES",selected = choices_update)
})



#' check if option "all" is selected along with an other option in include POS input
#' if this is the case, remove "all"
#' depends on:
#'   input$DE_POS_TYPES: which POS-Tags to use in analysis
observe({
  validate(
    need(!is.null(input$DE_POS_TYPES),message=F),
    need(length(input$DE_POS_TYPES)>1,message=F),
    need("all"%in%input$DE_POS_TYPES,message=F)
  )
  choices_update<-input$DE_POS_TYPES[-which(input$DE_POS_TYPES=="all")]
  updateSelectInput(session = session,inputId = "DE_POS_TYPES",selected = choices_update)
})

#' check if option "all" is selected along with an other option in include NE input
#' if this is the case, remove "all"
#' depends on:
#'   input$DE_ENTITY_TYPES: which NE-Tags to use in analysis
observe({
  validate(
    need(!is.null(input$DE_ENTITY_TYPES),message=F),
    need(length(input$DE_ENTITY_TYPES)>1,message=F),
    need("all"%in%input$DE_ENTITY_TYPES,message=F)
  )
  choices_update<-input$DE_ENTITY_TYPES[-which(input$DE_ENTITY_TYPES=="all")]
  updateSelectInput(session = session,inputId = "DE_ENTITY_TYPES",selected = choices_update)
})


#' check if option "all" is selected along with an other option in include POS input
#' if this is the case, remove "all"
#' depends on:
#'   input$TM_POS_TYPES: which POS-Tags to use in analysis
observe({
  validate(
    need(!is.null(input$TM_POS_TYPES),message=F),
    need(length(input$TM_POS_TYPES)>1,message=F),
    need("all"%in%input$TM_POS_TYPES,message=F)
  )
  choices_update<-input$TM_POS_TYPES[-which(input$TM_POS_TYPES=="all")]
  updateSelectInput(session = session,inputId = "TM_POS_TYPES",selected = choices_update)
})

#' check if option "all" is selected along with an other option in include NE input
#' if this is the case, remove "all"
#' depends on:
#'   input$TM_ENTITY_TYPES: which NE-Tags to use in analysis
observe({
  validate(
    need(!is.null(input$TM_ENTITY_TYPES),message=F),
    need(length(input$TM_ENTITY_TYPES)>1,message=F),
    need("all"%in%input$TM_ENTITY_TYPES,message=F)
  )
  choices_update<-input$TM_ENTITY_TYPES[-which(input$TM_ENTITY_TYPES=="all")]
  updateSelectInput(session = session,inputId = "TM_ENTITY_TYPES",selected = choices_update)
})


#' check if option "all" is selected along with an other option in include POS input
#' if this is the case, remove "all"
#' depends on:
#'   input$CL_POS_TYPES: which POS-Tags to use in analysis
observe({
  validate(
    need(!is.null(input$CL_POS_TYPES),message=F),
    need(length(input$CL_POS_TYPES)>1,message=F),
    need("all"%in%input$CL_POS_TYPES,message=F)
  )
  choices_update<-input$CL_POS_TYPES[-which(input$CL_POS_TYPES=="all")]
  updateSelectInput(session = session,inputId = "CL_POS_TYPES",selected = choices_update)
})

#' check if option "all" is selected along with an other option in include NE input
#' if this is the case, remove "all"
#' depends on:
#'   input$CL_ENTITY_TYPES: which NE-Tags to use in analysis
observe({
  validate(
    need(!is.null(input$CL_ENTITY_TYPES),message=F),
    need(length(input$CL_ENTITY_TYPES)>1,message=F),
    need("all"%in%input$CL_ENTITY_TYPES,message=F)
  )
  choices_update<-input$CL_ENTITY_TYPES[-which(input$CL_ENTITY_TYPES=="all")]
  updateSelectInput(session = session,inputId = "CL_ENTITY_TYPES",selected = choices_update)
})


#' check if option "all" is selected along with an other option in include POS input
#' if this is the case, remove "all"
#' depends on:
#'   input$DTM_POS_TYPES: which POS-Tags to use in analysis
observe({
  validate(
    need(!is.null(input$DTM_POS_TYPES),message=F),
    need(length(input$DTM_POS_TYPES)>1,message=F),
    need("all"%in%input$DTM_POS_TYPES,message=F)
  )
  choices_update<-input$DTM_POS_TYPES[-which(input$DTM_POS_TYPES=="all")]
  updateSelectInput(session = session,inputId = "DTM_POS_TYPES",selected = choices_update)
})

#' check if option "all" is selected along with an other option in include NE input
#' if this is the case, remove "all"
#' depends on:
#'   input$DTM_ENTITY_TYPES: which NE-Tags to use in analysis
observe({
  validate(
    need(!is.null(input$DTM_ENTITY_TYPES),message=F),
    need(length(input$DTM_ENTITY_TYPES)>1,message=F),
    need("all"%in%input$DTM_ENTITY_TYPES,message=F)
  )
  choices_update<-input$DTM_ENTITY_TYPES[-which(input$DTM_ENTITY_TYPES=="all")]
  updateSelectInput(session = session,inputId = "DTM_ENTITY_TYPES",selected = choices_update)
})