values$tm_number_of_topics<-0
values$TM_Coherence_show<-FALSE
values$TM_Intrusion_show<-FALSE
values$TM_topic_intrusion_run<-NULL
values$TM_Intrusion_word_show<-FALSE
values$TM_word_intrusion_run<-NULL
values$tm_stm_parameters_contentFormula <- ""
#render parameter tagset depending on the process
output$details_parameter<-renderUI({
  if(!is.null(values$Details_Analysis)){
    if(values$Details_Analysis=="SP"){
      load(paste0(values$Details_Data_SP,"/annotations.RData"))
      load(paste0(values$Details_Data_SP,"/meta.RData"))
      values$Det_SP_annotations<-annotation
      values$Det_SP_meta<-meta
      return(
        tagList(
          checkboxInput(inputId = "Det_SP_baseform",label = "Use lemmata?",value = TRUE),
          selectizeInput(inputId = "Det_SP_subject",label="Subjects:",choices=NULL,multiple=T,width="90%"),
          selectizeInput(inputId = "Det_SP_predicate",label="Predicate:",choices=NULL,multiple=T,width="90%"),
          selectizeInput(inputId = "Det_SP_object",label="Objects:",choices=NULL,multiple=T,width="90%")
        )
      )
    }
    if(values$Details_Analysis=="KE"){
      load(paste0(values$Details_Data_KE,"/stats.RData"))
      values$Det_KE_stats<-stats
      values$Det_KE_method<-method
      return(
        tagList(
          sliderInput(inputId = "Det_KE_min_freq",label = "min frequency",min = 1,max = max(as.numeric(stats$freq)),value = c(3,50)),
          sliderInput(inputId = "Det_KE_ngram",label = "number of words",min = 1,max = max(as.numeric(stats$ngram)),value = c(2,(max(as.numeric(stats$ngram))-1))),
          conditionalPanel(condition = "input.tabBox_keyword_extraction=='Plot'",
                           numericInput(inputId = "Det_KE_n",label = "number of keyword included in plot",value = 20,min = 1,max = 10000)
          )
        )
      )
    }
    if(values$Details_Analysis=="DD"){
      load(paste0(values$Details_Data_DD,"/info_and_removal_candidates.RData"))
      values$Det_DD_info<-info
      values$Det_DD_results<-results
      values$Det_DD_meta<-meta
      values$DD_whitelist<-NULL
      values$DD_blacklist<-NULL
      values$Det_DD_current_table<-NULL
      return(
        tagList(
          selectInput(inputId = "Det_DD_strategy",label = "Default strategy",choices = c("longest","shortest","latest","earliest","maximum node degree","random"),multiple = F),
          numericInput(inputId = "Det_DD_threshold",label="Threshold",value = 0.4,min=0,max=1,step = 0.05),
          bsButton(inputId = "Det_DD_reset_user_input",label = "Reset user input",icon = icon("refresh"),style = "primary"),
          bsButton(inputId = "Det_DD_save_collection",label = "Save Collection",icon = icon("save"),style = "success"),
          conditionalPanel(condition = 'input.tabBox_deduplication=="Graph"',
                           checkboxInput(inputId = "Det_DD_use_igraph_layout",label = "use iGraph-layout?",value = T),
                           bsButton("DD_help", label = "Help", icon = icon("question"), style = "info", size = "small")
          )
        )
      )
    }
    if(values$Details_Analysis=="CL"){
      if(values$Details_CL_mode=="whole_collection"){
        load(paste0(values$Details_Data_CL,"/feature_matrix.RData"))
        values$Det_CL_feature_matrix<-feature_matrix
        values$Det_CL_word_counts<-word_counts
        load(paste0(values$Details_Data_CL,"/results_complete.RData"))
        values$Det_CL_results_complete<-results_complete
        return(
          tagList(
            conditionalPanel(condition = 'input.tabBox_classification=="Date distribution"',
                             selectInput(inputId = "Det_CL_Time",label = "Timeintervall",choices = c("Day","Month","Year"),selected = "Month")         
            ),
            conditionalPanel(condition = 'input.tabBox_classification=="Classifier performance"',
                             selectInput(inputId = "Det_CL_c",label = "C Parameter",choices=setNames(1:10,c(0.003, 0.01, 0.03, 0.1, 0.3, 1, 3 , 10, 30, 100)),multiple = F),
                             selectInput(inputId = "Det_CL_fold",label = "Fold of Cross Validation",choices = 1:length(results_complete[[1]]),multiple = F)           
            ),
            conditionalPanel(condition = 'input.tabBox_classification=="Feature breakdown"',
                             #NEG examles not included in predictions; therefore removed from selection even though feature matrix used row for class NEG
                             selectInput(inputId = "Det_CL_feature_class",label = "Category",choices = rownames(feature_matrix),multiple = F),
                             sliderInput(inputId = "Det_CL_number_of_features",label = "number of features",min = 2,value = 20,step = 2,max = (dim(feature_matrix)[2]-1),width = "96%")%>%
                               shinyInput_label_embed(
                                 shiny_iconlink() %>%
                                   bs_embed_popover(
                                     title = "the most distinctive features pro and contra the selected category are shown", placement = "left"
                                   )
                               ),
                             shinyWidgets::materialSwitch(inputId = "Det_CL_feature_show_labels",label = "show labels",value = TRUE,status = "default"),
                             downloadButton(outputId = "Det_CL_download_feature_matrix",label = "Download feature matrix",icon=icon("download"), style="position:fixed;
  bottom:120px;")
            ),
            conditionalPanel(condition = 'input.tabBox_classification=="Validation"',
                             selectInput(inputId = "Det_CL_feature_class2",label = "Category",choices = setdiff(rownames(feature_matrix),"NEG"),multiple = F),
                             uiOutput(outputId = "Det_CL_validation_document_UI")
            ),
            downloadButton(outputId = "Det_CL_download_texts",label = "Download examples",icon=icon("download"), style="position:fixed;
  bottom:75px;")
          )
        )
      }
      if(values$Details_CL_mode=="evaluate"){
        load(paste0(values$Details_Data_CL,"/results_complete.RData"))
        values$Det_CL_results_complete<-results_complete
        return(
          tagList(
            selectInput(inputId = "Det_CL_c",label = "C Parameter",choices=setNames(1:10,c(0.003, 0.01, 0.03, 0.1, 0.3, 1, 3 , 10, 30, 100)),multiple = F),
            selectInput(inputId = "Det_CL_fold",label = "Fold of Cross Validation",choices = 1:length(results_complete[[1]]),multiple = F)
          )
        )
      }
      
      
      else{
        return(NULL)
      }
    }
    if(values$Details_Analysis=="SA"){
      return(
        tagList(
          conditionalPanel(condition = 'input.tabBox_senti=="Time Series"',
                           selectInput(inputId = "Det_SA_Time",label = "Timeintervall",choices = c("Day","Week","Month","Year"),selected = "Month")
          ),
          conditionalPanel(condition = 'input.tabBox_senti=="Document Length"',
                           numericInput(inputId = "Det_SA_max_breaks",label = "Maximal number of breaks",value = 25,min = 2,max = 1000)
          ),
          # conditionalPanel(condition = 'input.tabBox_senti=="Section"',
          #                  numericInput(inputId = "Det_SA_min_Section",label = "minimal number of occurrences to be shown in plot",value = 10,min = 2)
          # ),
          # conditionalPanel(condition = 'input.tabBox_senti=="Author"',
          #                  numericInput(inputId = "Det_SA_min_Author",label = "minimal number of occurrences to be shown in plot",value = 10,min = 2)
          # ),
          conditionalPanel(condition='input.tabBox_senti!="Author" && input.tabBox_senti!="Section"',
                           checkboxInput(inputId = "Det_SA_Lines",label = "Draw Lines",value = FALSE)
          ),
          conditionalPanel(condition='input.tabBox_senti!="Timeintervall" && input.tabBox_senti!="Document Length"',
                           numericInput(inputId = "Det_SA_min",label = "minimal number of occurrences to be shown in plot",value = 10,min = 2)
          ),
          downloadButton(outputId = "Det_SA_download_timeseries",label = HTML("current plot data"),icon=icon("download"))
        )
      )
    }
    if(values$Details_Analysis=="CO"){
      #load data needed for selection on input words
      load(paste0(values$Details_Data_CO,"/data_Coocs.RData"))
      values$coocs_terms<-terms
      values$coocs_token<-token
      values$coocs_load_examples<-FALSE
      values$coocs_load_top<-FALSE
      #tagList for cooccurrence analysis parameters for visualisation
      return(
        tagList(
          conditionalPanel(condition = 'input.tabBox_coocs=="Co-occurrence Graph" || input.tabBox_coocs=="Co-occurrence Matrix"',
                           actionButton(inputId = "Det_CO_Update",label = "Update Plot",styleclass = "primary"),
                           selectInput(inputId = "Det_CO_Measure",label = "Significance measure",
                                       choices = c("Dice","Mutual information","Log-likelihood","Count"),multiple = F,width="90%"),
                           #selectizeInput(inputId = "Det_CO_Word",label="Words:",choices=terms,width = "90%",multiple=F),
                           
                           selectizeInput(inputId = "Det_CO_Word",label="Word:",choices=NULL,multiple=F,width="90%"),
                           sliderInput(inputId = "Det_CO_RootLinks",label = "Root max Links:",min=1,max = 19,value = 4,step = 1,width = "90%"),
                           sliderInput(inputId = "Det_CO_LeavesLinks",label = "Leaves max Links:",min=1,max = 19,value = 4,step = 1,width = "90%"),
                           sliderInput(inputId = "Det_CO_Depth",label = "Depth of Graph",min=1,max = 4,value = 2,step = 1,width = "90%")
          ),
          conditionalPanel(condition = 'input.tabBox_coocs=="Co-occurrence Graph"',
                           checkboxInput(inputId = "Det_CO_use_igraph_layout",label = "use iGraph-layout?",value = T),
                           sliderInput(inputId = "Det_CO_Max_Edges",label = "Maximum Number of Edges",min=10,max = 1000,value = 40,step = 1,width = "90%"),
                           sliderInput(inputId = "Det_CO_gravity",label = "Charge of Graph",value = -2500,min = -10000,max = 0,width = "90%"),
                           checkboxInput(inputId = "Det_CO_Layout",label = "Layout hierarchical",width = "90%"),
                           checkboxInput(inputId = "Det_CO_smooth",label = "smooth edges",width = "90%",value = T),
                           shinyWidgets::prettyRadioButtons(inputId = "Det_CO_node_scaling",label = "Node size basis",
                                                            choices = c("degree","betweenness","centrality"),
                                                            fill=T,animation = "tada",selected = "degree",width = "90%"),
                           numericInput(inputId = "Det_CO_Threshold",label = "Significance Threshold",value = 0,min = 0,max = 1,step = 0.01,width="90%")
          ),
          # conditionalPanel(condition='input.tabBox_coocs=="Keyword and Context"',
          #                  shinyBS::bsButton(inputId = "coocs_knk_calc",label = "calculate keyword & context",style = "primary"),
          #                  uiOutput(outputId = "coocs_knk_word_ui"),
          #                  numericInput(inputId = "coocs_knk_n",label = "number of examples",value = 10,min = 1,max = 50,step = 1),
          #                  numericInput(inputId = "coocs_knk_k",label = "context size",value = 8,min = 1,max = 20,step = 1)
          # ),
          conditionalPanel(condition='input.tabBox_coocs=="Co-occurrence Matrix"',
                           downloadButton(outputId = "Det_CO_download_coocs",label = "Download matrix",icon=icon("download"))
          ),
          conditionalPanel(condition='input.tabBox_coocs=="Kwic"',
                           selectizeInput(inputId = "coocs_examples_words",label="Words:",choices=NULL,width = "90%",multiple=T),
                           numericInput(inputId = "coocs_examples_n",label = "number of examples",value = 10,min = 1,max = 50,step = 1),
                           checkboxInput(inputId = "coocs_examples_all",label = "show all",value = F),
                           numericInput(inputId = "coocs_examples_k",label = "context size",value = 5,min = 1,max = 500,step = 1),
                           downloadButton(outputId = "Det_CO_download_examples",label = "Download examples",icon=icon("download"))
          ),
          conditionalPanel(condition='input.tabBox_coocs=="Top-Co-occurrences"',
                           selectizeInput(inputId = "coocs_top_word1",label="first word:",choices=NULL,width = "90%",multiple=F,
                                          options=list(create=T,placeholder = 'Please select a word',onInitialize = I('function() { this.setValue(""); }'))),
                           selectizeInput(inputId = "coocs_top_word2",label="second word:",choices=NULL,width = "90%",multiple=F,
                                          options=list(create=T,placeholder = 'Please select a word',onInitialize = I('function() { this.setValue(""); }'))),
                           numericInput(inputId = "coocs_top_max",label = "maximum number of co-ooccurrences shown",value = 1000,min = 10,max = 1000000,step = 1),
                           downloadButton(outputId = "coocs_download_dice",label = "Download Dice",icon=icon("download")),
                           downloadButton(outputId = "coocs_download_mi",label = "Download MI",icon=icon("download")),
                           downloadButton(outputId = "coocs_download_log",label = "Download Log-Like",icon=icon("download"))
          )
        )
      )
    }
    if(values$Details_Analysis=="VS"){
      #load data needed for selection on input words
      #browser()
      try({
        model<-read.vectors(paste0(values$Details_Data_VS,"//vectors.bin"))
      })
      try({
        model<-read.vectors(paste0(values$Details_Data_VS,"//subset_labels.w2v"),binary=F)
      })
      try({
        load(paste0(values$Details_Data_VS,"//model.RData"))
        model<-model
      })
      try({
        load(paste0(values$Details_Data_VS,"//pca.RData"))
        values$vs_pca<-pca
      })
      try({
        load(paste0(values$Details_Data_VS,"//tsne.RData"))
        values$vs_tsne<-as.data.frame(tsne)
      })
      values$vs_model = model
      selectList <-rownames(model)
      values$VS_selectList <- selectList
      #tagList for vector space representation parameters for visualisation
      return(
        tagList(
          conditionalPanel(condition = 'input.tabBox_vector=="Similarity"',
                           #selectInput(inputId = "Det_VS_words",label = "words",choices =  selectList,multiple = T),
                           selectizeInput(inputId = "Det_VS_words",label = "words",choices =  NULL,multiple = T),
                           sliderInput(inputId = "Det_VS_n",label = "Number of Results",min = 1,max = 100,value = 10,step = 1),
                           tags$hr(),
                           radioButtons(inputId = "Det_vs_dimension_reduction",label = "Dimension reduction using:",choices = c("pca","tsne"))
                           
          ),
          conditionalPanel(condition = 'input.tabBox_vector=="Arithmetics"',
                           tags$div(icon("plus")),
                           selectizeInput(inputId = "Det_VS_arith_words_pos",label = "words to add",choices =  NULL,multiple = T),
                           tags$div(icon("minus")),
                           selectizeInput(inputId = "Det_VS_arith_words_neg",label = "words to substract",choices =  NULL,multiple = T),
                           numericInput(inputId = "Det_VS_arith_n",label = "Number of Results",min = 1,max = 1000,value = 10,step = 1)
          ),
          conditionalPanel(condition = 'input.tabBox_vector=="Linear Substructures"',
                           selectizeInput(inputId = "Det_VS_linS_words1",label = "word pairs/triples #1",choices =  NULL,multiple = T),
                           selectizeInput(inputId = "Det_VS_linS_words2",label = "word pairs/triples #2",choices =  NULL,multiple = T),
                           selectizeInput(inputId = "Det_VS_linS_words3",label = "word pairs/triples #3",choices =  NULL,multiple = T),
                           selectizeInput(inputId = "Det_VS_linS_words4",label = "word pairs/triples #4",choices =  NULL,multiple = T),
                           selectizeInput(inputId = "Det_VS_linS_words5",label = "word pairs/triples #5",choices =  NULL,multiple = T),
                           radioButtons(inputId = "Det_vs_linS_dimension_reduction",label = "Dimension reduction using:",choices = c("pca","tsne"))
          )#,
          #conditionalPanel(condition = 'input.tabBox_vector=="Clustering"',
          #                 selectInput(inputId = "Det_VS_words",label = "words",choices = c("a","b"))
          #),
          #conditionalPanel(condition = 'input.tabBox_vector=="Graph"',
          #                 selectInput(inputId = "Det_VS_words",label = "words",choices = c("a","b"))
          #)
        )
      )
    }
    if(values$Details_Analysis=="FE"){
      #load data needed for selection on input words
      load(paste0(values$Details_Data_FE,"/vocab.RData"))
      values$fe_vocab<-vocab
      #tagList for frequency extraction parameters for visualisation
      return(
        tagList(
          selectInput(inputId = "Det_FE_REL_ABS",label = "relative or absolute",choices = c("relative","absolute")),
          selectInput(inputId = "Det_FE_Time",label = "Timeintervall",choices = c("Day","Week","Month","Year"),selected = "Month"),
          selectInput(inputId = "Det_FE_Term_Doc",label = "Word- or Documentlevel",choices = c("Word","Document")),
          checkboxInput(inputId="Det_FE_use_regexp",label = "use regexp?",value = FALSE),
          conditionalPanel(condition='input.Det_FE_use_regexp==true',
                           textInput(inputId = "Det_FE_regexp",label = "reg exp:"),
                           materialSwitch(inputId = "Det_FE_calc",label = "calculate",value = FALSE,status = "primary")
          ),
          conditionalPanel(condition='input.Det_FE_use_regexp==false',
                           selectizeInput(inputId = "Det_FE_Word",label="Words:",choices=NULL,multiple=T)
          )
        )
      )
    }
    if(values$Details_Analysis=="DE"){
      #load data needed for selection on input words
      load(paste0(values$Details_Data_DE,"/vocab.RData"))
      values$de_vocab<-vocab
      #tagList for frequency extraction parameters for visualisation
      return(
        tagList(
          selectInput(inputId = "Det_DE_REL_ABS",label = "relative or absolute",choices = c("relative","absolute")),
          selectInput(inputId = "Det_DE_Time",label = "Timeintervall",choices = c("Day","Week","Month","Year"),selected = "Month"),
          selectInput(inputId = "Det_DE_Term_Doc",label = "Word- or Documentlevel",choices = c("Word","Document")),
          selectizeInput(inputId = "Det_DE_Word",label="Dicts:",choices=vocab,multiple=T)
        )
      )
    }
    if(values$Details_Analysis=="VA"){
      #load data needed for selection on input words
      withProgress(message = 'Loading data', value = 0, {
        load(paste0((values$Details_Data_VA),"/freq.RData"))
        incProgress(0.1, detail = "Getting frequencies")
        load(paste0((values$Details_Data_VA),"/labels.RData"))
        incProgress(0.1, detail = "Getting labels")
        load(paste0((values$Details_Data_VA),"/tags.RData"))
        incProgress(0.1, detail = "Getting NER- and POS Tags")
        load(paste0((values$Details_Data_VA),"/voldata.RData"))
        incProgress(0.7, detail = "Getting diachron co-occurrence information")
        load(paste0((values$Details_Data_VA),"/CoocYears.RData"))
      })
      
      values$va_cy<-diachron_Coocs
      values$va_freq<-freq
      values$va_words<-rownames(freq)[order(rowSums(freq),decreasing = T)]
      values$va_ner_tags<-ner_tags
      values$va_pos_tags<-pos_tags
      values$va_voldata<-voldata
      values$va_un_dates<-un_dates
      intervall<-"year"
      if(nchar(un_dates[1])==10){
        intervall<-c("month","year")
      }
      #tagList for volatility analysis parameters for visualisation
      return(
        tagList(
          actionButton(inputId = "Det_VA_Update",label = "Update Plot"),
          tags$br(),
          conditionalPanel(condition = 'input.tabBox_volat=="Multi Words"',
                           selectizeInput(inputId = "Det_VA_Select_Words",label="Word:",choices=NULL,multiple=T)
          ),
          conditionalPanel(condition = 'input.tabBox_volat=="Single Word"',
                           selectizeInput(inputId = "Det_VA_Select_Word",label="Words:",choices=NULL,multiple=F)
          ),
          conditionalPanel(condition = 'input.tabBox_volat=="Highest Volatility"',
                           numericInput(inputId="Det_VA_maxFreq_high",label="max frequency",value = 1000,step = 1,min = 1),
                           numericInput(inputId="Det_VA_minFreq_high",label="min frequency",value = 1,step = 1,min = 1)
          ),
          conditionalPanel(condition = 'input.tabBox_volat=="Lowest Volatility"',
                           numericInput(inputId="Det_VA_maxFreq_low",label="max frequency",value = 1000,step = 1,min = 1),
                           numericInput(inputId="Det_VA_minFreq_low",label="min frequency",value = 1,step = 1,min = 1)
          ),
          conditionalPanel(condition = 'input.tabBox_volat=="Highest Volatility periodwise"',
                           selectizeInput(inputId = "Det_VA_time",label = "which time?",choices=un_dates,multiple=F),
                           selectizeInput(inputId = "Det_VA_NER",label = "specify NER Tags",choices=c("all",as.character(unique(ner_tags[,2]))),multiple=F),
                           selectizeInput(inputId = "Det_VA_POS",label = "specify POS Tags",choices=c("all",as.character(unique(pos_tags[,2]))),multiple=F)
          ),
          conditionalPanel(condition = 'input.tabBox_volat=="Wordclouds for Highest Volat"',
                           selectizeInput(inputId = "Det_VA_WC_TIME",label = "which time?",choices=intervall,multiple=F),
                           selectizeInput(inputId = "Det_VA_WC_NER",label = "specify NER Tags",choices=c("all",as.character(unique(ner_tags[,2]))),multiple=F),
                           selectizeInput(inputId = "Det_VA_WC_POS",label = "specify POS Tags",choices=c("all",as.character(unique(pos_tags[,2]))),multiple=F),
                           numericInput(inputId="Det_VA_WC_maxFreq_low",label="max frequency",value = 1000,step = 1,min = 1),
                           numericInput(inputId="Det_VA_WC_minFreq_low",label="min frequency",value = 1,step = 1,min = 1),
                           knobInput(inputId = "DET_VA_WC_N",label = "Number of Words per Plot",value = 15,min = 1,max = 100,step = 1)
          )
        )
      )
    }
    if(values$Details_Analysis=="TM"){
      #load tm resultset and make it available
      load(paste0(values$Details_Data_TM,"/data_TM.RData"))
      updateSelectizeInput(session = session,inputId = "Det_TM_ewf_word",server = T,choices = colnames(phi))
      values$TM_Coherence_show<-FALSE
      values$TM_Intrusion_show<-FALSE
      values$TM_topic_intrusion_run<-NULL
      values$TM_Intrusion_word_show<-FALSE
      values$TM_word_intrusion_run<-NULL
      values$tm_rel_counts <- round((colSums(theta * doc.length))*phi,digits = 2)
      values$tm_freqs<-term.frequency
      rownames(phi)<-as.character(1:dim(phi)[1])
      colnames(theta)<-as.character(1:dim(theta)[2])
      values$tm_phi<-phi
      values$tm_info<-info
      values$tm_theta<-theta
      values$tm_doc.length<-doc.length
      values$tm_vocab<-vocab
      values$tm_json<-json
      values$tm_term.frequency<-term.frequency
      
      
      # #stm
      load(paste0(values$Details_Data_TM,"/parameters.RData"))
      values$tm_parameters <- parameters
      values$tm_method <- parameters$tm_method
      if(values$tm_method == "stm"){
        values$tm_stm_model <- model$stm_model # loaded via data_TM.RData
        load(paste0(values$Details_Data_TM,"/dtm_TM.RData"))
        values$dtm <- dtm
        values$tm_stm_visu_topicCorr_show <- FALSE
        values$tm_stm_visu_estimateEffect_plot_show <- FALSE
        
      }

      topic.frequency <- colSums(theta * doc.length)
      topic.proportion <- topic.frequency/sum(topic.frequency)
      o <- order(topic.proportion, decreasing = TRUE)
      #phi <- phi[o, ]
      #theta <- theta[, o]
      topic.frequency <- topic.frequency[o]
      topic.proportion <- topic.proportion[o]
      
      #values$tm_probability_distribution<-(table(c(apply(X=theta,1,FUN = function(i){return(which.max(i))}),1:dim(theta)[2]))-1)/(dim(theta)[1])
      values$tm_probability_distribution<-topic.proportion
      values$tm_number_of_topics<-dim(phi)[1]
      values$tm_timeline_ids<-NULL
      values$tm_random<-runif(n = 1,min = 0,max = 1)
      if(class(info[[6]])!="data.frame"){
        isolate(values$tm_dates<-data.frame(as.character(info[[6]]),stringsAsFactors = F))
      }
      else{
        isolate(values$tm_dates<-info[[6]])
      }
      isolate(colnames(values$tm_dates)<-"date")
      #create output objects for wordclouds
      wordcloud_list<-lapply(1:dim(phi)[1],function(i){
        box_title<-paste0("Topic: ",i)
        wc_id<-paste0("tm_wc_",i)
        ac_id<-paste0("tm_ac_",values$tm_random,"_",i)
        sp_id<-paste0("tm_sp_",i)
        return(box(title = box_title,width = 12,
                   tags$head(
                     tags$style(HTML('div#wcLabel {display: none;}'))
                   ),
                   fluidRow(
                     column(1,offset = 1,
                            shinyBS::bsButton(inputId = ac_id,label = NULL,icon = icon(name = "plus",lib = "font-awesome"),style = "success",size = "extra-small")
                     ),
                     column(width = 8,   offset = 2, 
                            sparklineOutput(outputId = sp_id)
                     )
                   ),
                   tags$h5(paste0("Probability: ",round(isolate(values$tm_probability_distribution[i]),digits = 4))),
                   wordcloud2Output(outputId = wc_id,width = "100%",height = "150px")
        )
        )
      }
      )
      if(!is.null(isolate(values$observers))){
        for(i in 1:length(isolate(values$observers))){
          isolate(values$observers[[i]]$destroy())
          #isolate(shinyjs::runjs(paste0('Shiny.onInputChange(\"tm_ac_',i,'\",  0)')))
        }
      }
      #panels for topic modeling parameters for visualisation
      
          panelLDAVIs <- conditionalPanel(condition = 'input.tabBox_tm=="LDA-Vis"',
                           sliderInput("nTerms", "Number of terms to display", min = 20, max = 40, value = 30),
                           tags$br(),
                           downloadButton(outputId = "download_phi",label = "phi",icon=icon("download")),
                           downloadButton(outputId = "download_theta",label = "theta",icon=icon("download"))
                           
          )
          panelEstWordFrequencies <- conditionalPanel(condition = 'input.tabBox_tm=="Estimated word frequencies"',
                           selectizeInput(inputId = "Det_TM_ewf_word",label = "word:",choices =  NULL,multiple = T),
                           materialSwitch(inputId = "Det_TM_emf_rel",label = "relative distribution over topics?",value = F),
                           tags$br(),
                           downloadButton(outputId = "download_rel_counts",label = "estimated frequencies",icon=icon("download"))
          )
          panelCoherence <- conditionalPanel(condition = 'input.tabBox_tm=="Coherence"',
                           sliderInput("TM_Coherence_runs", "Number of runs for interactive coherence measure", min = 5, max = 50, value = 10,step=1),
                           sliderInput("TM_Coherence_setsize", "set size for interactive coherence measure", min = 3, max = values$tm_number_of_topics, value = 4,step = 1),
                           checkboxInput("TM_Coherence_chance_correction",label = "chance correction?",value = F)
          )
          panelDateDistribution <- conditionalPanel(condition = 'input.tabBox_tm=="Date distribution"',
                           tags$head(tags$style(HTML("div.col-sm-12 {padding:1px}; float:up;"))),
                           shinyBS::bsButton(inputId = "TM_Timeline_Reset",label = "reset chart",style = "success",size = "extra-small"),
                           tags$br(),
                           sliderTextInput(inputId = "TM_Timeline_Range",label = NULL,choices = c("Day","Month","Year"),force_edges = T,grid = T,selected = "Month"),
                           sliderTextInput(inputId = "TM_Timeline_Measure",label = NULL,choices = c("Document Count","Document Probability","relative Document Count"),force_edges = T,grid=T),
                           materialSwitch(inputId = "TM_Timeline_Rank1",label = "Use Rank1 for selecting document membership",value = T,status = "warning"),
                           conditionalPanel(condition = 'input.TM_Timeline_Rank1==false',
                                            knobInput(inputId = "TM_Timeline_Prob",label = "Minimal Probability",value = 0.5,min = 0,max = 1,step = 0.01)
                           ),
                           do.call(tagList,wordcloud_list)
          )
          panelExtractDictionaries <- conditionalPanel(condition = 'input.tabBox_tm=="Extract Dictionaries"',
                           numericInput("TM_dict_number_of_words", "Number of words included in dictionary from each topic", min = 1, max = 5000, value = 25,step=1)
          )
          panelDetailedMetaDataAnalysis <- conditionalPanel(condition = 'input.tabBox_tm=="Detailed Metadata Distribution"',
                           uiOutput(outputId = "Det_meta_select_ui")
          )
          panelValidation <- conditionalPanel(condition = 'input.tabBox_tm=="Validation"',
                           selectizeInput(inputId = "Det_TM_validation_document",label="Document:",choices=NULL,multiple=F),
                           sliderInput(inputId = "Det_TM_validation_topic",label = "Topic:",min = 1,value = 1,max = dim(phi)[1],step = 1),
                           tags$hr(),
                           uiOutput("Det_TM_validation_metadata_UI")%>%withSpinner()
          )

          #stm
          if(values$tm_method == "stm"){
            panelSTM <- conditionalPanel(condition = 'input.tabBox_tm=="Structural Topic Model"',
                             
                             # stm number of words to label topic
                             conditionalPanel(condition = 'input.stm_visu=="Topics" || input.stm_visu=="Labels" || input.stm_visu=="Histogramm"|| input.stm_visu=="Perspectives"',
                                              numericInput(inputId = "tm_stm_visu_numberOfWordsToLabelTopic",label="number of words to label topics", value = 10, min = 2, max = 50, step = 1)
                             ),
                             #stm labeltype (for sub tabs Topics, Labels and Histogram - but only if selected calculated structural topic model didn't have a content formula set. If a content formula was set, the label type is not selectable)
                             conditionalPanel(condition = '(input.stm_visu=="Topics" || input.stm_visu=="Labels" || input.stm_visu=="Histogramm") && output.tm_stm_parameters_contentFormulaIsSet==false',
                                              selectInput(inputId = "tm_stm_visu_labeltype",label="method to choose most important words",choices=c("prob", "frex", "lift", "score"),multiple=F, selected = "prob")%>%
                                                shinyInput_label_embed(
                                                  shiny_iconlink() %>%
                                                    bs_embed_popover(
                                                      title = "PROB: words within each topic with the highest probability. FREX: words which are both frequent in and exclusive to a topic of interest identifying words that distinguish topics. SCORE: divides the log frequency of the word in the topic by the log frequency of the word in other topics. For more information on score, see the lda R package. LIFT: words according to the lift metric: Lift is calculated by dividing the topic-word distribution by the empirical word count probability distribution. For more information see R calclift",
                                                      placement = "right"
                                                    )
                                                )
                             ),
                             # stm perspectives topic 1 and 2
                             conditionalPanel(condition = 'input.stm_visu=="Perspectives"',
                                              selectInput(inputId = "tm_stm_visu_perspectives_topic1",label="select topic 1",choices=c(1:values$tm_stm_model$settings$dim$K),multiple=F, selected = 1),
                                              selectInput(inputId = "tm_stm_visu_perspectives_topic2",label="select topic 2",choices=c(1:values$tm_stm_model$settings$dim$K),multiple=F, selected = 2)
                             ),
                             # stm frexweight (if labeltype == frex)
                             conditionalPanel(condition = 'input.tm_stm_visu_labeltype=="frex" && (input.stm_visu=="Topics" || input.stm_visu=="Labels" || input.stm_visu=="Histogramm") && output.tm_stm_parameters_contentFormulaIsSet==false', 
                                              numericInput(inputId = "tm_stm_visu_frexweight",label="frex weight",value = 0.5, min = 0, max = 1, step = 0.1)%>%
                                                shinyInput_label_embed(
                                                  shiny_iconlink() %>%
                                                    bs_embed_popover(
                                                      title = "the proportion of the weight assigned to frequency",
                                                      placement = "right"
                                                    )
                                                )
                             )
            )
            
          } # end: if tm_method == stm
          
          
          if(values$tm_method =="stm"){
            returnValue <-  tagList(
                   panelLDAVIs,
                   panelEstWordFrequencies,
                   panelCoherence,
                   panelDateDistribution,
                   panelExtractDictionaries,
                   panelDetailedMetaDataAnalysis,
                   panelValidation,
                   panelSTM
            )
          }else{
            returnValue <-  tagList(
                   panelLDAVIs,
                   panelEstWordFrequencies,
                   panelCoherence,
                   panelDateDistribution,
                   panelExtractDictionaries,
                   panelDetailedMetaDataAnalysis,
                   panelValidation
            )
          }
    
    
    return (returnValue)
        
          
    }
  }
}
)

##########################################################################################
#                                details_visu                                            #
##########################################################################################

#render the visulisation depending on the process
output$details_visu<-renderUI({
  if(!is.null(values$Details_Analysis)){
    if(values$Details_Analysis=="TM"){
      validate(
        need(!is.null(values$tm_theta),message=F)
      )

      updateSelectizeInput(session = session,inputId = "Det_TM_validation_document",server = T,choices = rownames(values$tm_theta))
      
      #stm

      #return visu for topic modeling
      tabPanelLDAVis <- tabPanel("LDA-Vis",
                                 #use the d3.js from ldavis library
                                 tags$script(src="d3.v3.js"),
                                 LDAvis::visOutput('TM_LDAvis')
      )
      tabPanelEstWordFrequencies <-  tabPanel("Estimated word frequencies",
                                              fluidRow(style="margin-left:0px;margin-right:0px",
                                                       plotlyOutput(outputId = "TM_ewf_bar")
                                              ),
                                              tags$br(),
                                              fluidRow(style="margin-left:0px;margin-right:0px",
                                                       DT::dataTableOutput(outputId = "TM_ewf_table")
                                              )
      )
      tabPanelDateDistribution <- tabPanel("Date distribution",
                                           plotlyOutput(outputId = "TM_Timeline"),
                                           DT::dataTableOutput(outputId = "TM_Subcollection_Table"),
                                           uiOutput("TM_subColl_UI")
      )
      tabPanelCoherence <- tabPanel("Coherence",
               busyIndicator(text = "loading data for calculating coherence",wait = 0),
               bsButton(inputId = "TM_Coherence_start",label = "start Coherence Calculation",style = "primary",icon=icon("play")),
               conditionalPanel(condition = "output.TM_Coherence_show==true",
                                tabsetPanel(type="tabs",
                                            tabPanel(title = "Topic Coherence",
                                                     plotlyOutput(outputId = "TM_Coherence_topic_coherence"),
                                                     valueBoxOutput(outputId = "TM_Coherence_topic_coherence_mean_box")
                                            ),
                                            tabPanel(title="Topic Intrusion",
                                                     uiOutput(outputId = "TM_Coherence_topic_intrusion")
                                            ),
                                            tabPanel(title="Word Intrusion",
                                                     uiOutput(outputId = "TM_Coherence_word_intrusion")
                                            )
                                )
               )

      )
      tabPanelExtractDictionaries <- tabPanel("Extract Dictionaries",
               uiOutput("TM_dict_topics_ui"),
               uiOutput("TM_dict_categories_names"),
               uiOutput("TM_dict_save_ui")
      )
      tabPanelDetailedMetaData <- tabPanel("Detailed Metadata Distribution",
               uiOutput("TM_meta_ui")
      )
      tabPanelValidation <- tabPanel("Validation",
               uiOutput("TM_validation_UI")%>%withSpinner()
      )

      # additional panels specific for stm
      # needs values$tm_parameters and values$tm_method to be set (via load parameters from file). This is currently performed in output$details_parameter above, so doesn't need to be performed again here
      if(values$tm_method == "stm"){
        
        # load meta data
        load(paste0(values$Details_Data_TM,"/meta_TM.RData"))
        values$tm_stm_metaData <- combineMetaDataWithMetaNamesForMDEs(meta = meta, meta_names = meta_names)
        values$tm_stm_parameters_contentFormula <- values$tm_parameters$stm_contentFormula
        # general STM tab panel
        tabPanelSTM <- tabPanel("Structural Topic Model",
                                tabsetPanel(type="tabs", id = "stm_visu",
                                            tabPanel(title = "Topics", plotOutput(outputId = "TM_stm_visu_summary")),
                                            tabPanel(title = "Labels", plotOutput(outputId = "TM_stm_visu_labels")),
                                            tabPanel(title = "Perspectives", 
                                                     conditionalPanel(condition = "output.tm_stm_parameters_contentFormulaIsSet==true",
                                                                      textInput(inputId = "tm_stm_visu_perspectives_covariateValue1",label = "covariate value 1")%>%
                                                                        shinyInput_label_embed(
                                                                          shiny_iconlink() %>%
                                                                            bs_embed_popover(
                                                                              title = "The covariate value 1 of interest. Example if the content covariate in the stm model was the country name: 'Russia'.",
                                                                              placement = "right"
                                                                            )
                                                                        ),
                                                                      textInput(inputId = "tm_stm_visu_perspectives_covariateValue2",label = "covariate value 1")%>%
                                                                        shinyInput_label_embed(
                                                                          shiny_iconlink() %>%
                                                                            bs_embed_popover(
                                                                              title = "The covariate value 2 of interest. Example if the content covariate in the stm model was the country name: 'Turkey'.",
                                                                              placement = "right"
                                                                            )
                                                                        )
                                                                  
                                                     ),
                                                     plotOutput(outputId = "TM_stm_visu_perspectives")
                                                     
                                                     ),
                                            tabPanel(title = "Histogramm", plotOutput(outputId = "TM_stm_visu_hist")),
                                            tabPanel(title = "Topic Correlation",
                                                     busyIndicator(text = "calculating topic correlation",wait = 0),
                                                     bsButton(inputId = "tm_stm_visu_topicCorr_start",label = "start calculation of Topic Correlation",style = "primary",icon=icon("play")),
                                                     conditionalPanel(condition = "output.TM_stm_visu_topicCorr_show==true",
                                                                      plotOutput(outputId = "TM_stm_visu_topicCorr_calc")
                                                     )
                                            ),
                                            tabPanel(title = "Estimate Effect",
                                                     
                                                     busyIndicator(text = "calculating estimateEffect",wait = 0),
                                                     # formula
                                                     textInput(inputId = "tm_stm_visu_estimateEffect_calcParam_formula",label = "formula")%>%
                                                                        shinyInput_label_embed(
                                                                          shiny_iconlink() %>%
                                                                            bs_embed_popover(
                                                                              title = "A formula for the regression. It should have an integer or vector of numbers on the left-hand side and an equation with covariates on the right hand side. See Details of STM documentation for more information. Typically, users will pass the same model of topical prevalence used in estimating the STM to the estimateEffect function. The syntax of the estimateEffect function is designed so users specify the set of topics they wish to use for estimation, and then a formula for metadata of interest. Examples: 'c(1) ~ treatment', '1:3 ~ treatment' , c(1,3,5) ~ treatment + s(day)",
                                                                              placement = "right"
                                                                            )
                                                                        ),
                                                     # meta vars to convert to factors 
                                                     selectInput(inputId = "tm_stm_visu_estimateEffect_metaVarsToConvertToFactor", label = "meta variables to convert to factor", choices = names(values$tm_stm_metaData), multiple = T)%>%
                                                       shinyInput_label_embed(
                                                         shiny_iconlink() %>%
                                                           bs_embed_popover(
                                                             title = "before the estimate effect can be calculated, the variables used in the formula above should be converted to factors or numeric. Chosse those which should be converted to factors here",
                                                             placement = "right"
                                                           )
                                                       ),
                                                     # meta vars to convert to numeric
                                                     selectInput(inputId = "tm_stm_visu_estimateEffect_metaVarsToConvertToNumeric", label = "meta variables to convert to numeric (continuous variables like dates)", choices = names(values$tm_stm_metaData), multiple = T)%>%
                                                       shinyInput_label_embed(
                                                         shiny_iconlink() %>%
                                                           bs_embed_popover(
                                                             title = "please select the variables to convert to numeric. These are continuous variables like e.g. dates",
                                                             placement = "right"
                                                           )
                                                       ),
 
                                                     bsButton(inputId = "tm_stm_visu_estimateEffect_calcButton",label = "start calculation of estimate effect",style = "primary",icon=icon("play")),
                                                     conditionalPanel(condition = "output.TM_stm_visu_estimateEffect_show==true",
                                                                      tabsetPanel(type="tabs",
                                                                                  tabPanel(title="Summary",
                                                                                           verbatimTextOutput (outputId = "TM_stm_visu_estimateEffect_summary")
                                                                                  ),
                                                                                  tabPanel(title="Plot",
                                                                                           textInput(inputId = "tm_stm_visu_estimateEffect_plot_covariate",label = "covariate")%>%
                                                                                               shinyInput_label_embed(
                                                                                               shiny_iconlink() %>%
                                                                                                 bs_embed_popover(
                                                                                                   title = "The covariate of interest. Example: 'treatment'. All other covariates within the formula specified in estimateEffect will be kept at their median.",
                                                                                                   placement = "right"
                                                                                                 )
                                                                                             ),
                                                                                           checkboxGroupInput(inputId = "tm_stm_visu_estimateEffect_plot_topics", label = "topics to plot", choices = c(1:values$tm_stm_model$settings$dim$K), selected = 1)%>%
                                                                                             shinyInput_label_embed(
                                                                                               shiny_iconlink() %>%
                                                                                                 bs_embed_popover(
                                                                                                   title = "Topics to plot. Please be aware that only topics can be plotted also used in the calculation of estimateEffect. So if you have used e.g. '1:3~treatment' for estimate effect only the first 3 topics can be selected to be plotted.",
                                                                                                   placement = "right"
                                                                                                 )
                                                                                             ),
                                                                                           selectInput(inputId = "tm_stm_visu_estimateEffect_plot_method",label="method used for plotting",choices=c("pointestimate", "difference", "continuous"),multiple=F, selected = "pointestimate")%>%shinyInput_label_embed(
                                                                                             shiny_iconlink() %>%
                                                                                               bs_embed_popover(
                                                                                                 title = "Method used for plotting. 'pointestimate' estimates mean topic proportions for each value of the covariate. 'difference' estimates the mean difference in topic proportions for two different values of the covariate (cov.value1 and cov.value2 must be specified). 'continuous' estimates how topic proportions vary over the support of a continuous covariate",
                                                                                                 placement = "right"
                                                                                               )
                                                                                           ),
                                                                                           conditionalPanel(condition = 'input.tm_stm_visu_estimateEffect_plot_method=="difference"',
                                                                                                            textInput(inputId = "tm_stm_visu_estimateEffect_plot_difference_covValue1",label = "covariate value 1")%>%
                                                                                                              shinyInput_label_embed(
                                                                                                                shiny_iconlink() %>%
                                                                                                                  bs_embed_popover(
                                                                                                                    title = "the value or set of values of interest at which to set the covariate. In the case of calculating a treatment/control contrast, set the treatment to cov.value1.",
                                                                                                                    placement = "right"
                                                                                                                  )
                                                                                                              ),
                                                                                                            textInput(inputId = "tm_stm_visu_estimateEffect_plot_difference_covValue2",label = "covariate value 2")%>%
                                                                                                              shinyInput_label_embed(
                                                                                                                shiny_iconlink() %>%
                                                                                                                  bs_embed_popover(
                                                                                                                    title = "the value or set of values which will be set as the comparison group. cov.value1 and cov.value2 must be vectors of the same length",
                                                                                                                    placement = "right"
                                                                                                                  )
                                                                                                              )
                                                                                           ),
                                                                                           bsButton(inputId = "tm_stm_visu_estimateEffect_plotupdate",label = "Show plot for given parameters",style = "primary",icon=icon("play")),
                                                                                           conditionalPanel(condition = 'output.TM_stm_visu_estimateEffect_plot_show==true',
                                                                                                            plotOutput(outputId = "TM_stm_visu_estimateEffect_plot")
                                                                                                            )
                                                                                  )
                                                                      )
                                                     )
                                            )

                                )
        )



        returnValue <-  tagList(
                   tabBox(id="tabBox_tm",width = 12,
                          tabPanelLDAVis,
                          tabPanelEstWordFrequencies,
                          tabPanelDateDistribution,
                          tabPanelCoherence,
                          tabPanelExtractDictionaries,
                          tabPanelDetailedMetaData,
                          tabPanelValidation,
                          tabPanelSTM
                   )

        )

      }else{

        returnValue <-  tagList(
          tabBox(id="tabBox_tm",width = 12,
                 tabPanelLDAVis,
                 tabPanelEstWordFrequencies,
                 tabPanelDateDistribution,
                 tabPanelCoherence,
                 tabPanelExtractDictionaries,
                 tabPanelDetailedMetaData,
                 tabPanelValidation
                 )
        )
      }

      return (returnValue)
      
    }
    
    
    if(values$Details_Analysis=="CO"){
      validate(
        need(!is.null(values$coocs_terms),message=F)
      )
      updateSelectizeInput(session = session,inputId = "Det_CO_Word",server = T,choices = values$coocs_terms)
      updateSelectizeInput(session = session,inputId = "coocs_examples_words",server = T,choices = values$coocs_terms)
      updateSelectizeInput(session = session,inputId = "coocs_top_word1",server = T,choices = values$coocs_terms)
      updateSelectizeInput(session = session,inputId = "coocs_top_word2",server = T,choices = values$coocs_terms)
      
      #return cooccurrence graph depending on the chosen Display Type
      return(
        tagList(
          tagList(
            tabBox(id="tabBox_coocs",width = 12,
                   tabPanel("Co-occurrence Graph",
                            busyIndicator(text = "loading data",wait = 0),
                            visNetwork::visNetworkOutput(outputId = "visNetwork_cooc_net",height = "70vh")
                   ),
                   # tabPanel("Keyword and Context",
                   #          busyIndicator(text = "loading data",wait = 0),
                   #          DT::dataTableOutput(outputId = "cooc_kwic_table")
                   # ),
                   tabPanel("Co-occurrence Matrix",
                            busyIndicator(text = "loading data",wait = 0),
                            plotlyOutput(outputId = "cooc_heatmap",height = "70vh") 
                   ),
                   tabPanel("Kwic",
                            busyIndicator(text = "loading data",wait = 0),
                            uiOutput(outputId = "coocs_examples_stats"),
                            DT::dataTableOutput(outputId = "cooc_examples_table")
                   ),
                   tabPanel("Top-Co-occurrences",
                            busyIndicator(text = "loading data",wait = 0),
                            box(title = tags$h4("Dice",style="color:white;"),solidHeader = T,width = 4,status="primary",
                                DT::dataTableOutput(outputId = "coocs_top_dice_table")%>% withSpinner(type = 6)
                            ),
                            box(title = tags$h4("Mutual information",style="color:white;"),solidHeader = T,width = 4,status="primary",
                                DT::dataTableOutput(outputId = "coocs_top_mi_table")%>% withSpinner(type = 6)
                            ),
                            box(title = tags$h4("Log-likelihood",style="color:white;"),solidHeader = T,width = 4,status = "primary",
                                DT::dataTableOutput(outputId = "coocs_top_log_table")%>% withSpinner(type = 6)
                            )
                   )
            )
          ),
          tags$script("$('.forceNetwork').css('background-color', 'black');")
        )
        
        
      )  
    }
    if(values$Details_Analysis=="VS"){
      #return cooccurrence graph depending on the chosen Display Type
      validate(
        need(!is.null(values$VS_selectList),message=F)
      )
      updateSelectizeInput(session = session,inputId = "Det_VS_words",server = T,choices = values$VS_selectList)
      updateSelectizeInput(session = session,inputId = "Det_VS_arith_words_pos",server = T,choices = values$VS_selectList)
      updateSelectizeInput(session = session,inputId = "Det_VS_arith_words_neg",server = T,choices = values$VS_selectList)
      updateSelectizeInput(session = session,inputId = "Det_VS_linS_words1",server = T,choices = values$VS_selectList)
      updateSelectizeInput(session = session,inputId = "Det_VS_linS_words2",server = T,choices = values$VS_selectList)
      updateSelectizeInput(session = session,inputId = "Det_VS_linS_words3",server = T,choices = values$VS_selectList)
      updateSelectizeInput(session = session,inputId = "Det_VS_linS_words4",server = T,choices = values$VS_selectList)
      updateSelectizeInput(session = session,inputId = "Det_VS_linS_words5",server = T,choices = values$VS_selectList)
      return(
        tagList(
          tagList(
            tabBox(id="tabBox_vector",width = 12,
                   tabPanel("Similarity",
                            tags$br(),
                            fluidRow(style="margin-left:0px;margin-right:0px",
                                     column(5,
                                            DT::dataTableOutput(outputId = "vs_similar_table")%>% withSpinner(type = 1)
                                     ),
                                     column(7,
                                            tags$br(),
                                            conditionalPanel(condition = 'input.Det_vs_dimension_reduction=="pca"',
                                                             plotlyOutput(outputId = "vs_similar_plotly_pca")%>% withSpinner(type = 1)
                                            ),
                                            conditionalPanel(condition = 'input.Det_vs_dimension_reduction=="tsne"',
                                                             plotlyOutput(outputId = "vs_similar_plotly_tsne")%>% withSpinner(type = 1)
                                            )
                                            
                                     )
                            )
                   ),
                   tabPanel("Linear Substructures",
                            tags$br(),
                            fluidRow(style="margin-left:0px;margin-right:0px",
                                     
                                     column(12,
                                            tags$br(),
                                            conditionalPanel(condition = 'input.Det_vs_linS_dimension_reduction=="pca"',
                                                             plotlyOutput(outputId = "vs_linS_plotly_pca")%>% withSpinner(type = 1)
                                            ),
                                            conditionalPanel(condition = 'input.Det_vs_linS_dimension_reduction=="tsne"',
                                                             plotlyOutput(outputId = "vs_linS_plotly_tsne")%>% withSpinner(type = 1)
                                            )
                                            
                                     )
                            )
                   ),
                   tabPanel("Arithmetics",
                            DT::dataTableOutput(outputId = "vs_arithmetics")%>% withSpinner(type = 1)
                   )#,
                   #tabPanel("Clustering",
                   #          busyIndicator(text = "loading data",wait = 0),
                   #          plotOutput(outputId = "vs_cluster",height = "70vh") 
                   # ),
                   # tabPanel("Graph",
                   #          busyIndicator(text = "loading data",wait = 0),
                   #          visNetwork::visNetworkOutput(outputId = "vs_graph")
                   # )
            )
          )
        )
      )  
    }
    if(values$Details_Analysis=="FE"){
      validate(
        need(!is.null(values$fe_vocab),message=F)
      )
      choices<-values$fe_vocab[,1]
      names<-paste(values$fe_vocab[,1]," (",values$fe_vocab[,2],")",sep="")
      names(choices)<-names
      updateSelectizeInput(session = session,inputId = "Det_FE_Word",server = T,choices = choices)
      #load and set the frequency data
      load(paste0(values$Details_Data_FE,"/frequencies.RData"))
      values$FE_freqs_day<-freqs_day
      values$FE_freqs_week<-freqs_week
      values$FE_freqs_month<-freqs_month
      values$FE_freqs_year<-freqs_year
      values$FE_rel_doc_freqs_day<-rel_doc_freqs_day
      values$FE_rel_doc_freqs_week<-rel_doc_freqs_week
      values$FE_rel_doc_freqs_month<-rel_doc_freqs_month
      values$FE_rel_doc_freqs_year<-rel_doc_freqs_year
      values$FE_rel_freqs_day<-rel_freqs_day
      values$FE_rel_freqs_week<-rel_freqs_week
      values$FE_rel_freqs_month<-rel_freqs_month
      values$FE_rel_freqs_year<-rel_freqs_year
      values$FE_doc_freqs_day<-doc_freqs_day
      values$FE_doc_freqs_week<-doc_freqs_week
      values$FE_doc_freqs_month<-doc_freqs_month
      values$FE_doc_freqs_year<-doc_freqs_year
      #load(paste0(values$Details_Data_FE,"/dtm.RData"))
      #values$FE_dtm<-dtm
      #return the Frequency Extraction plot
      return(
        tagList(
          tabBox(id = "tabBox_FE",width=12,
                 tabPanel(title = "Frequency Plot",
                          busyIndicator(text = "loading data",wait = 0),
                          plotlyOutput(outputId = "FE_plot"),
                          conditionalPanel(condition='input.Det_FE_use_regexp==true',
                                           uiOutput("Det_FE_regexp_words")
                          ),
                          conditionalPanel(condition = 'input.Det_FE_Word!=null',
                                           tags$br(),
                                           tags$div("Select terms in the list below"),
                                           tags$div("The Time Series Plot will be exported according to the graph settings"),
                                           tags$br(),
                                           uiOutput(outputId = "FE_words_to_export"),
                                           downloadButton(outputId = "download_FE_frequencies",label = "Download Time Series")
                          )
                 )
          )
        )
      )
    }
    if(values$Details_Analysis=="DE"){
      #load and set the frequency data
      load(paste0(values$Details_Data_DE,"/frequencies.RData"))
      values$DE_freqs_day<-freqs_day_dict
      values$DE_freqs_week<-freqs_week_dict
      values$DE_freqs_month<-freqs_month_dict
      values$DE_freqs_year<-freqs_year_dict
      values$DE_rel_doc_freqs_day<-rel_doc_freqs_day_dict
      values$DE_rel_doc_freqs_week<-rel_doc_freqs_week_dict
      values$DE_rel_doc_freqs_month<-rel_doc_freqs_month_dict
      values$DE_rel_doc_freqs_year<-rel_doc_freqs_year_dict
      values$DE_rel_freqs_day<-rel_freqs_day_dict
      values$DE_rel_freqs_week<-rel_freqs_week_dict
      values$DE_rel_freqs_month<-rel_freqs_month_dict
      values$DE_rel_freqs_year<-rel_freqs_year_dict
      values$DE_doc_freqs_day<-doc_freqs_day_dict
      values$DE_doc_freqs_week<-doc_freqs_week_dict
      values$DE_doc_freqs_month<-doc_freqs_month_dict
      values$DE_doc_freqs_year<-doc_freqs_year_dict
      if(use_reg_exp_as_input==TRUE){
        values$Det_DE_regexp_words<-dict_terms
      }
      else{
        values$Det_DE_regexp_words<-NULL
      }
      #return the Frequency Extraction plot
      return(
        tagList(
          tabBox(id = "tabBox_DE",width=12,
                 tabPanel(title = "Dictionary Frequency Plot",
                          busyIndicator(text = "loading data",wait = 0),
                          plotlyOutput(outputId = "DE_plot"),
                          uiOutput("Det_DE_regexp_words"),
                          conditionalPanel(condition = 'input.Det_DE_Word!=null',
                                           tags$br(),
                                           tags$div("Select terms in the list below"),
                                           tags$div("The Time Series Plot will be exported according to the graph settings"),
                                           tags$br(),
                                           uiOutput(outputId = "DE_words_to_export"),
                                           downloadButton(outputId = "download_DE_frequencies",label = "Download Time Series")
                          )
                 )
          )
        )
      )
    }
    if(values$Details_Analysis=="CL"){
      if(values$Details_CL_mode=="whole_collection"){
        return(
          tagList(
            tabBox(id="tabBox_classification",width=12,
                   tabPanel("Date distribution",
                            plotlyOutput(outputId = "Det_Class_date_distribution"),
                            downloadButton(outputId = "Det_CL_download_timeseries",label = "Download timeseries data",icon=icon("download")),
                            tags$br(),
                            plotlyOutput(outputId = "Det_Class_pie")
                   ),
                   tabPanel("Classifier performance",
                            uiOutput(outputId = "Det_Class_classifier_performance")
                            
                   ),
                   tabPanel("Feature breakdown",
                            tags$br(),
                            uiOutput("Det_CL_feature_UI")   
                   ),
                   tabPanel("Validation",
                            uiOutput("Det_CL_validation")%>%withSpinner()
                   )
            )
          )
        )
      }
      if(values$Details_CL_mode=="evaluate"){
        return(
          tagList(
            tabBox(id="tabBox_classification",width=12,
                   tabPanel("Classifier performance",
                            uiOutput(outputId = "Det_Class_classifier_performance")
                            
                   )
            )
          )
        )
      }
    }
    if(values$Details_Analysis=="KE"){
      return(
        tagList(
          tabBox(id="tabBox_keyword_extraction",width=12,
                 tabPanel("Plot",
                          tags$hr(),
                          plotly::plotlyOutput(outputId = "Det_KE_plot",width = "auto")%>%
                            withSpinner()
                 ),
                 tabPanel("Table", 
                          tags$br(),
                          DT::dataTableOutput(outputId = "Det_KE_table")%>%
                            withSpinner()
                 )
          )
        )
      )
    }
    if(values$Details_Analysis=="SP"){
      validate(
        need(
          !is.null(values$Det_SP_annotations),message=F
        ),
        need(!is.null(input$Det_SP_baseform),message=F)
      )
      annotation<-values$Det_SP_annotations
      if(input$Det_SP_baseform==TRUE){
        #predicates<-annotation$lemma[union(which(annotation$dep_rel=="cop"),which(annotation$dep_rel=="aux"))]
        predicates<-unique(annotation$lemma[which(annotation$upos=="VERB")])
        subjects<-unique(annotation$lemma[which(annotation$dep_rel=="nsubj")])
        objects<-unique(annotation$lemma[which(annotation$dep_rel=="obj")])
      }
      else{
        #predicates<-annotation$token[union(which(annotation$dep_rel=="cop"),which(annotation$dep_rel=="aux"))]
        predicates<-unique(annotation$token[which(annotation$upos=="VERB")])
        subjects<-unique(annotation$token[which(annotation$dep_rel=="nsubj")])
        objects<-unique(annotation$token[which(annotation$dep_rel=="obj")])
      }
      
      updateSelectizeInput(session = session,inputId = "Det_SP_subject",server = T,choices = unique(subjects))
      updateSelectizeInput(session = session,inputId = "Det_SP_predicate",server = T,choices = unique(predicates))
      updateSelectizeInput(session = session,inputId = "Det_SP_object",server = T,choices = unique(objects))
      return(
        tagList(
          tabBox(id="tabBox_syntactic_parsing",width=12,
                 tabPanel("Search",
                          tags$br(),
                          uiOutput(outputId = "Det_SP_number_of_results"),
                          tags$h4("Sentences"),
                          uiOutput(outputId = "Det_SP_sentences"),
                          uiOutput(outputId = "Det_SP_slider_ui")#,
                          # tags$h4("Relations table"),
                          #                         DT::dataTableOutput(outputId = "Det_SP_search_table")%>%
                          #   withSpinner()
                          
                 )
          )
        )
      )
    }
    if(values$Details_Analysis=="DD"){
      return(
        tagList(
          tabBox(id="tabBox_deduplication",width=12,
                 tabPanel("Graph",
                          visNetwork::visNetworkOutput(outputId = "Det_DD_Network",height = "68vh")%>%
                            withSpinner()
                 ),
                 tabPanel("Table",
                          column(6,
                                 DT::dataTableOutput(outputId = "Det_DD_Table_White")%>%
                                   withSpinner()
                          ),
                          column(6,
                                 DT::dataTableOutput(outputId = "Det_DD_Table_Black")%>%
                                   withSpinner()
                          )
                 )
          )
        )
      )
    }
    if(values$Details_Analysis=="SA"){
      load(paste0(values$Details_Data_SA,"/data_Senti.RData"))
      values$Det_Senti_meta<-meta
      return(
        tagList(
          tabBox(id="tabBox_senti",width=12,
                 tabPanel("Time Series",
                          plotlyOutput(outputId = "Det_Senti_date")
                 ),
                 tabPanel("Document Length",
                          plotlyOutput(outputId = "Det_Senti_length")
                 ),
                 tabPanel("Other meta data",
                          uiOutput("tab_Panels_senti_mde")
                 )
                 # tabPanel("Section",
                 #          plotlyOutput(outputId = "Det_Senti_section")
                 # ),
                 # tabPanel("Author",
                 #          plotlyOutput(outputId = "Det_Senti_author")
                 # )
          )
        )
      )
    }
    if(values$Details_Analysis=="VA"){
      validate(
        need(!is.null(values$va_words),message=F)
      )
      updateSelectizeInput(session = session,inputId = "Det_VA_Select_Words",server = T,choices = values$va_words)
      updateSelectizeInput(session = session,inputId = "Det_VA_Select_Word",server = T,choices = values$va_words)
      #return results for volatility analysis
      return(
        tagList(
          tabBox(id="tabBox_volat",width=12,
                 tabPanel("Single Word",
                          #volatility plot for one word with frequency included
                          busyIndicator(text = "loading data",wait = 0),
                          plotlyOutput(outputId = "VA_plot"),
                          #show top co-occurrences for clicked on point in time in table and wordcloud
                          fluidRow(style="margin-left:0px;margin-right:0px",
                                   box(title=tags$h4("top co-occurrences",style="color:white"),status = "warning",solidHeader = T,collapsible = F,width=12,collapsed = T,
                                       uiOutput(outputId = "volat_coocs_headline"),
                                       column(4,
                                              dataTableOutput(outputId = "coocs")
                                       ),
                                       column(8,
                                              wordcloud2Output(outputId = "wc")
                                       )
                                   )
                          ),
                          #show top co-occurrences missing and new for 2 points in time
                          fluidRow(style="margin-left:0px;margin-right:0px",
                                   box(title=tags$h4("most divergent co-occurrences",style="color:white"),status = "danger",solidHeader = T,collapsible = F,width = 12,collapsed = T,
                                       uiOutput(outputId = "volat_divergent_headline"),
                                       column(4,
                                              dataTableOutput(outputId = "coocs2")
                                       ),
                                       column(8,
                                              #how many words shall be plotted
                                              textInput(inputId = "max_words",label = "max. Anzahl Worte",value = 100),
                                              selectInput(inputId = "was",label = "what shall be displayed",choices = c("everything","new","missing"),selected = "everything"),
                                              wordcloud2Output(outputId = "wc2")
                                       )
                                   )
                          )
                 ),
                 tabPanel("Multi Words",
                          plotlyOutput(outputId = "VA_Multi_plot") %>% withSpinner(type = 6)
                 ),
                 tabPanel("Highest Volatility",
                          DT::dataTableOutput(outputId = "Det_VA_highV") %>% withSpinner(type = 6)
                 ),
                 tabPanel("Lowest Volatility",
                          DT::dataTableOutput(outputId = "Det_VA_lowV") %>% withSpinner(type = 6)
                 ),
                 tabPanel("Highest Volatility periodwise",
                          plotlyOutput(outputId = "Det_VA_highest_period") %>% withSpinner(type = 6)
                 ),
                 tabPanel("Wordclouds for Highest Volat",
                          uiOutput(outputId = "Det_VA_WC") %>% withSpinner(type = 6)
                 )
                 
          )   
        )
      )
    }
  }
  else{
    return(tags$img(src="busy4.gif"))
  }
})



###########################################
#     create output objects              #
###########################################
source(file.path("server","tab_Details_Frequency_Extraction.R"),local = T)$value
source(file.path("server","tab_Details_Dictionary_Extraction.R"),local = T)$value
source(file.path("server","tab_Details_Coocs.R"),local = T)$value
source(file.path("server","tab_Details_Classification.R"),local = T)$value
source(file.path("server","tab_Details_Sentiment_Analysis.R"),local = T)$value
source(file.path("server","tab_Details_Topic_Model.R"),local = T)$value
source(file.path("server","tab_Details_Volatility.R"),local = T)$value
source(file.path("server","tab_Details_Vector.R"),local = T)$value
source(file.path("server","tab_Details_Deduplication.R"),local = T)$value
source(file.path("server","tab_Details_Keyword_Extraction.R"),local = T)$value
source(file.path("server","tab_Details_Syntactic_Parsing.R"),local = T)$value
#source(file.path("server","tab_Details_Facto.R"),local = T)$value


output$Details_FA_selected<-reactive({
  values$Details_Analysis=="FA"
})

outputOptions(output,"details_visu",suspendWhenHidden=FALSE)
outputOptions(output,"TM_LDAvis",suspendWhenHidden=FALSE)
outputOptions(output, "Details_FA_selected", suspendWhenHidden = FALSE)





