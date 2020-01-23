# select Input to choose a collection
# choices: check files in "colelctions/collections"
# @ values$coll_saved: reactive variable that invalidates when a new collection is saved
output$Task_Scheduler_Collection<-renderUI({
  values$coll_saved
  selectizeInput(inputId = "collection_selected",label = "Collection:",choices = stringr::str_replace_all(string = list.files(path ="collections/collections/"),pattern = ".RData",replacement = ""))
})

# if the user wants to use a custom script, show select statement with existing custom scripts
# @values$new_script_saved: reactive variable that invalidates when a new custom script got saved
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

# select input showing the available parameter presets with option to save the current setting as preset
# @values$reload_presets: reactive variable that invalidates if a new parameter preset got saved
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


# save current parameter setting as a parameter preset
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
      stm_kappa_prior=input$stm_kappa_prior
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

# check whether file name is already used; if no file with that name exists: save file
# else ask user to confirm overwrite
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

# if user wants to overwrite existing parameter preset --> overwrite existing preset with current setting
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



# try to update parameters corresponding to chosen preset
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


#check if analysis help button was clicked, if yes,open modal with rmd.files
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


