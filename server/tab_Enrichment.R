

output$enrichment_configuration_UI<-renderUI({
  validate(
    need(!is.null(values$topic_enrichment_topic_model_theta),message = "Please specify a Topic Model!")
  )
  return(tagList(
    fluidRow(style="margin-left:0px;margin-right:0px",
             column(2,
                    numericInput("enrichment_chunksize","Chunksize",min=1,max=1000,value=25,step=5)
             ),
             column(2,
                    numericInput("enrichment_output_table_n","Max number of Topic-Tags per Chunk:",min=1,max=ncol(values$topic_enrichment_topic_model_theta),value=3,step=1)
             ),
             column(2,
                    numericInput(inputId = "enrichment_output_table_threshold", label = "Min. Probability",min=0,max=1,value=0,step = 0.05)
             )
             
    )
  ))
  
})



output$enrichment_topic_control_UI<-renderUI({
  validate(
    need(!is.null( values$topic_enrichment_topic_model_phi),message=F)
  )
  return(tagList(
    sliderInput(inputId="enrichment_topic_n","Select Topic:",min=1,max = ncol(values$topic_enrichment_topic_model_theta),value=1,step=1),
    fluidRow(style="margin-left:0px;margin-right:0px",
             column(4,
                    sliderInput(inputId = "enrichment_topic_lambda","Lambda:",min=0,max=1,step=0.05,value=0.5)
             ),
             column(5,
                    uiOutput(outputId = "enrichment_topic_label")
             )),
    uiOutput(outputId = "enrichment_topic_control_details_UI")
  ))
})

output$enrichment_topic_label<-renderUI({
  validate(need(!is.null(values$topic_enrichment_topic_model_topic_labels),message=F))
  return(
    tags$h3(HTML(paste0("Details for <b>",values$topic_enrichment_topic_model_topic_labels[input$enrichment_topic_n],"</b>")))
  )
})


output$enrichment_topic_control_details_UI<-renderUI({
  validate(
    need(!is.null( input$enrichment_topic_n),message=F)
  )
  return(tagList(
    column(4,
           DT::dataTableOutput("enrichment_topic_control_details_table")  
    ),
    column(8,
           wordcloud2Output(outputId = "enrichment_topic_control_details_wordcloud")
    )
  ))
})

output$enrichment_topic_control_details_wordcloud <- renderWordcloud2({
  relevance<-calculate_topic_relevance(lambda=input$enrichment_topic_lambda,phi=values$topic_enrichment_topic_model_phi,theta=values$topic_enrichment_topic_model_theta,doc.length=values$topic_enrichment_topic_model_doc.length)
  data <- relevance[,as.numeric(input$enrichment_topic_n)]
  data <- data[order(data,decreasing=T)][1:100]
  data <- data.frame(cbind(names(data),data),stringsAsFactors = FALSE)
  class(data$data) <- "numeric"
  # normalize weights for wordcloud
  data$data <- data$data-min(data$data)
  data$data <- data$data/max(data$data)
  shinyjs::runjs(paste0("Math.seedrandom('",values$random_seed,"')"))
  hoverFunction = htmlwidgets :: JS ( "function hover() {}" )
  wordcloud2(data = data,size=0.62,fontFamily = "Helvetica",color = "random-dark",minSize = 0.2,minRotation = -pi/2,maxRotation = -pi/2,hoverFunction = hoverFunction)
})


output$enrichment_topic_control_details_table <- DT::renderDataTable({
  relevance<-calculate_topic_relevance(lambda=input$enrichment_topic_lambda,phi=values$topic_enrichment_topic_model_phi,theta=values$topic_enrichment_topic_model_theta,doc.length=values$topic_enrichment_topic_model_doc.length)
  data <- relevance[,as.numeric(input$enrichment_topic_n)]
  data <- data[order(data,decreasing=T)]
  data <- data.frame(cbind(names(data),data),stringsAsFactors = FALSE)
  class(data$data) <- "numeric"
  colnames(data)<-c("Word","Relevance")
  datatable(data,rownames = F)
})



observeEvent(input$enrichment_import_topic,{
  validate(
    need(!is.null(input$enrichment_import_topic),message=F)
  )
  withBusyIndicatorServer("enrichment_import_topic", {
    load(paste0("collections/results/topic-model/",input$enrichment_topic,"/data_TM.RData"))
    values$topic_enrichment_topic_model_phi <- phi
    values$topic_enrichment_topic_model_theta <- theta
    values$topic_enrichment_topic_model_doc.length <- doc.length
    values$topic_enrichment_topic_model_model <- model
    values$topic_enrichment_topic_model_t <- t
    topic_labels <- rep(paste0("Topic: ",1:ncol(theta)))
    try({
      load(paste0("collections/results/topic-model/",input$enrichment_topic,"/Topic_Labels.RData"))
      
    })
    values$topic_enrichment_topic_model_topic_labels<-topic_labels
  })
})



#load chosen collection
observeEvent(input$enrichment_load_collection,{
  validate(
    need(!is.null(input$enrichment_collection),message = "No collection specified.")
  )
  withBusyIndicatorServer("enrichment_load_collection", {
    load(paste0("collections/collections/",input$enrichment_collection,".RData"))
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
    RMariaDB::dbBegin(conn = mydb)
    RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
    dataset = info[[2]][1,1]
    doc_ids <- info[[1]][,1]
    doc_ids <- paste0(doc_ids,collapse=",")
    statement <-  paste("select * from interview_info where dataset='",dataset,"' and id_doc in (",doc_ids,");",sep="")
    interview_data <- RMariaDB::dbGetQuery(mydb,statement = statement)
    # try to get entities from DB
    if(isTRUE(input$enrichment_include_ner_tags)){
      statement <-  paste("select * from token where dataset='",dataset,"' and id in (",doc_ids,");",sep="")
      token <- RMariaDB::dbGetQuery(mydb,statement = statement)
      if(length(which(token$entity==""))>0){
        token <- token[-which(token$entity==""),]
      }
      toks<-token[,c("id"  ,    "sid", "tid"  ,  "word"    ,   "lemma"     ,  "pos"    ,     "entity",  "idx","id_interview","id_interview_row")]
      colnames(toks) <-c("doc_id"  ,    "sentence_id", "token_id"  ,  "token"    ,   "lemma"     ,  "pos"    ,     "entity",  "idx","id_interview","id_interview_row")
      #toks$sentence_id <- paste0(toks$sentence_id, "_",toks$token_id)
      class(toks)<-c("spacyr_parsed","data.frame")
      
      ents<-spacyr::entity_consolidate(x = toks)[c("doc_id","sentence_id","token_id","token","entity_type")]
      ents2 <- merge(x=ents,y=toks[,c("doc_id","sentence_id","id_interview","id_interview_row")],by=c("doc_id","sentence_id"),all.x=T,all.y=F)
      ents2 <- unique(ents2)
      ners <- ents2[order(as.numeric(ents2$doc_id),as.numeric(ents2$sentence_id)),]
      
      values$enrichment_ner <- ners
    }
    RMariaDB::dbDisconnect(mydb)
    try({
      interview_data <- interview_data[,c("id_interview","band","timecode","sprecher","transkript","uebersetzung","hauptueberschrift","zwischenueberschrift","hauptueberschrift_uebersetzung",
                                          "zwischenueberschrift_uebersetzung","registerverknüpfungen","anmerkungen","anmerkungen_uebersetzung")]
    })
    interview_data[which(is.na(interview_data))]<-""
    values$enrichment_result <- NULL
    values$enrichment_data <- interview_data
  })
})


#show table of imported data
output$enrichment_data_import_table<-DT::renderDataTable({
  validate(
    need(!is.null(values$enrichment_data),message="No data imported yet.")
  )
  data <- values$enrichment_data
  data<-data[1:min(5,nrow(data)),]
  DT::datatable(data = data, width = "100%",
                selection = "none",escape=F,class = 'cell-border stripe hover',
                options = list(scrollX=T, scrollY="42vh"))
})


# load new uploaded csv data
observeEvent(input$enrichment_new_transcript,{
  withBusyIndicatorServer("enrichment_new_transcript", {
    if(grepl(x = input$enrichment_new_transcript$name,pattern = ".csv$",ignore.case = T)){
      data_interview <- readr::read_delim(file = input$enrichment_new_transcript$datapath,
                                          delim = "\t", na = character(), quote = "\\\"", escape_double = FALSE )
      id_interview <- rep(substr(input$enrichment_new_transcript$name,1,7),nrow(data_interview))
      data_interview <- cbind(id_interview,data_interview)
    }
    if(grepl(x = input$enrichment_new_transcript$name,pattern = ".zip$",ignore.case = T)){
      try({
        do.call(file.remove, list(list.files("collections/tmp/ohd_enrichment/", full.names = TRUE)))
      })
      unzip(zipfile = input$enrichment_new_transcript$datapath,exdir = "collections/tmp/ohd_enrichment/")
      files <- list.files("collections/tmp/ohd_enrichment/",full.names = T)
      file_names <-list.files("collections/tmp/ohd_enrichment/",full.names = F)
      data_interview <- NULL
      for(i in 1:length(files)){
        data_single_interview <- readr::read_delim(file = files[i],
                                                   delim = "\t", na = character(), quote = "\\\"", escape_double = FALSE )
        id_interview <- rep(substr(file_names[i],1,7),nrow(data_single_interview))
        data_single_interview<-cbind(id_interview,data_single_interview)
        data_interview<-rbind(data_interview,data_single_interview)
      }
    }
    try({
      colnames(data_interview)<-c("id_interview","band","timecode","sprecher","transkript","uebersetzung","hauptueberschrift","zwischenueberschrift","hauptueberschrift_uebersetzung",
                                  "zwischenueberschrift_uebersetzung","registerverknüpfungen","anmerkungen","anmerkungen_uebersetzung")
    })
    values$enrichment_data <- data_interview
    if(isTRUE(input$enrichment_include_ner_tags)){
      withProgress(min = 0,max = (length(unique(data_interview$id_interview))+2),message = "Preparing new Interviews for NER-Extraction...",{
        
        # TODO add entities by spacy
        data_interview$transkript<-stringr::str_replace_all(string = data_interview$transkript,pattern = '"',"'")
        data_interview$transkript<-stringr::str_replace_all(string = data_interview$transkript,pattern = '\\\\0'," ")
        token <- NULL
        data=data_interview
        data$doc_id <- as.numeric(factor(data$id_interview))
        spacyr::spacy_initialize(model = "de_core_news_sm",python_executable = reticulate_python_path)
        token_grouped=list()
        ids <- unique(data$doc_id)
        for(i in 1:length(ids)){
          gc()
          print(ids[i])
          data_indiv <- data[which(data$doc_id==ids[i]),]
          token_mc <- parallel::mclapply(mc.cleanup = T, mc.cores = min(c(10, nrow(data_indiv),( parallel::detectCores()-1))),1:nrow(data_indiv),FUN = function(x){
            row<-iconv(data_indiv$transkript[x], "UTF-8", "UTF-8",sub='')
            toks<-spacyr::spacy_parse(row,pos = T,tag = F,lemma = T,entity = T,dependency = F,additional_attributes = "idx")
            toks$doc_id = rep(data_indiv$doc_id[x],nrow(toks))
            toks$id_interview <- rep(data_indiv$id_interview[x],nrow(toks))
            toks$id_interview_row <- rep(x,nrow(toks))
            if(ncol(toks)==9){
              toks$token_id = character(0)
            }
            return(toks)
          })
          sents = 0
          for(j in 1:length(token_mc)){
            token_indiv <- token_mc[[j]]
            if(nrow(token_indiv)>0){
              addition = max(token_indiv[,2])
              token_indiv[,2] = as.numeric(token_indiv[,2]) + sents
              token_mc[[j]]<-token_indiv
              sents = sents + addition
            }
          }
          token <- data.table::rbindlist(token_mc,use.names = T)
          token <- data.frame(token)
          token_grouped[[i]] <- token
          incProgress(session = session,amount = 1,message = paste0("Finished Pre-Processing for ",i," of ",length(ids)," Interviews"))
        }
        incProgress(session = session,amount = 1,message = paste0("Extracting Named Entites from processed Interviews..."))
        token <- data.table::rbindlist(token_grouped,use.names = T)
        token <- data.frame(token)
        
        if(length(which(token$entity==""))>0){
          token <- token[-which(token$entity==""),]
        }
        toks<-token
        colnames(toks) <-c("doc_id"  ,    "sentence_id", "token_id"  ,  "token"    ,   "lemma"     ,  "pos"    ,     "entity",  "idx","id_interview","id_interview_row")
        class(toks)<-c("spacyr_parsed","data.frame")
        
        ents<-spacyr::entity_consolidate(x = toks)[c("doc_id","sentence_id","token_id","token","entity_type")]
        ents2 <- merge(x=ents,y=toks[,c("doc_id","sentence_id","id_interview","id_interview_row")],by=c("doc_id","sentence_id"),all.x=T,all.y=F)
        ents2 <- unique(ents2)
        ners <- ents2[order(as.numeric(ents2$doc_id),as.numeric(ents2$sentence_id)),]
        values$enrichment_ner <- ners    
      })
    }
    values$enrichment_result <- NULL
  })
})


#start enrichment
observeEvent(input$enrichment_start_enrichment,{
  withBusyIndicatorServer("enrichment_start_enrichment", {
    if(!is.null(values$enrichment_data)){
      if(!is.null(values$topic_enrichment_topic_model_phi)){
        data = values$enrichment_data
        model = values$topic_enrichment_topic_model_model
        load(paste0("collections/results/topic-model/",input$enrichment_topic,"/dtm_TM.RData"))
        t <- values$topic_enrichment_topic_model_t 
        theta <-values$topic_enrichment_topic_model_theta 
        #spacyr::spacy_initialize(model = "de_core_news_sm")
        
        vocab <-colnames(dtm)
        interview_ids <- unique(data$id_interview)
        topic_values <-NULL
        withProgress(min = 0,max = (length(interview_ids)+1),message = "Starting Topic Enrichment",{
          for(j in 1:length(interview_ids)){
            data_interview <- data[which(data$id_interview==interview_ids[j]),]
            tokens <- quanteda::tokenize_word(x = data_interview$transkript)
            new_dtm <- Matrix(c(0),nrow = length(tokens),ncol = ncol(dtm))
            present <- parallel::mclapply(X = 1:nrow(data_interview),
                                          mc.cores = min(c(10,nrow(data_interview),( parallel::detectCores()-1))),
                                          mc.cleanup = T,
                                          FUN = function(x){
                                            token = tolower(tokens[[x]]) 
                                            
                                            present_words <- which(vocab %in% token)
                                            return(present_words)
                                          }
            )
            for(i in 1:length(present)){
              if(length(present[[i]])>0){
                new_dtm[i,present[[i]]] <- 1
              }
            }
            colnames(new_dtm)<-colnames(dtm)
            chunksize = input$enrichment_chunksize
            if(chunksize>nrow(new_dtm)){
              chunksize = nrow(new_dtm)
            }
            if(chunksize>1){
              res <- matrix(c(0),nrow = length(tokens),ncol = ncol(theta))
              splits <- split(1:nrow(new_dtm), ceiling(seq_along(1:nrow(new_dtm))/chunksize))
              res_splits <- parallel::mclapply(X = 1:length(splits),
                                               mc.cores = min(c(10,length(splits),( parallel::detectCores()-1))),
                                               mc.cleanup = T,
                                               FUN = function(x){
                                                 idx <- as.numeric(splits[[x]])
                                                 new_dtm_aggregated <- Matrix::colSums(new_dtm[idx,,drop=F])
                                                 if(sum(new_dtm_aggregated,na.rm = T)==0){
                                                   res <- rep(NA,ncol(theta))
                                                   return(matrix(rep(res, length(idx)),nrow=length(idx),byrow = T))
                                                 }
                                                 else{
                                                   new_dtm_aggregated <- Matrix(new_dtm_aggregated,nrow = 1,ncol = ncol(dtm))
                                                   res <-t$infer_topics(dtm = new_dtm_aggregated)
                                                   return(matrix(rep(res, length(idx)),nrow=length(idx),byrow = T))
                                                 }
                                               })
              for(i in 1:length(splits)){
                res[as.numeric(splits[[i]]),]<-res_splits[[i]]
              }
            }
            else{
              zero_idx <- which(rowSums(new_dtm)==0)
              a <- unlist(lapply(present,FUN=function(x){
                paste0(x,collapse = "_")
              }))
              l <- do.call(paste, data.frame((a)))
              d = match(l, l)
              
              tab <-as.data.frame((table(d)))
              single_idx = setdiff(as.character(tab$d[which(tab$Freq==1)]),zero_idx)
              multis <-as.character(tab$d[which(tab$Freq>1)])
              if(length(zero_idx)>0){
                multis <- multis[-which(multis%in%zero_idx)]
              }
              multi_idx <- lapply(multis,FUN = function(x){
                which(d==x)
              })
              
              res <- matrix(c(0),nrow = length(tokens),ncol = ncol(theta))
              res[zero_idx,] <- NA
              res[as.numeric(single_idx),] <- t$infer_topics(dtm = new_dtm[as.numeric(single_idx),,drop=F])
              
              res_multi <- parallel::mclapply(X = 1:length(multi_idx),
                                              mc.cores = min(c(10,nrow(data_interview),( parallel::detectCores()-1))),
                                              mc.cleanup = T,
                                              FUN = function(x){
                                                idx <- as.numeric(multi_idx[[x]])
                                                res <-t$infer_topics(dtm = new_dtm[as.numeric(idx)[1],,drop=F])
                                                return(matrix(rep(res, length(idx)),nrow=length(idx),byrow = T))
                                              })
              for(i in 1:length(res_multi)){
                res[as.numeric(multi_idx[[i]]),]<-res_multi[[i]]
              }
            }
            topic_values_interview <- res
            topic_values <- rbind(topic_values, topic_values_interview)
            incProgress(session = session,amount = 1,message = paste0("Finished Topic Enrichment for ",j," of ",length(interview_ids)," Interviews"))
          }
          # re order topics
          topic_values <- topic_values[,as.numeric(colnames(theta),decreasing = F)]
          colnames(topic_values) <- values$topic_enrichment_topic_model_topic_labels
          values$enrichment_result <- topic_values
        })
      }
      else{
        shinyWidgets::sendSweetAlert(session = session,title = "No Topic Model found",text = "Please choose one exisiting topic model to use for enrichment",type = "warning")
      }
    }
    else{
      shinyWidgets::sendSweetAlert(session = session,title = "No Data found",text = "Please upload new transcripts or use existing collections of interviews",type = "warning")
    }
  })
})


output$enrichment_topic_UI <- renderUI({
  input$enrichment_reload_avail_topics
  selectInput(inputId="enrichment_topic","Choose Topic Model:",choices = rev(list.files(path = "collections/results/topic-model/")))
})

# parameters for enrichment output plot
output$enrichment_output_plot_parameters_UI<-renderUI({
  validate(
    need(!is.null(values$enrichment_data),message=F)
  )
  document_choices <- unique(values$enrichment_data$id_interview)
  names(document_choices) <- paste0("Interview: ",unique(values$enrichment_data$id_interview))
  return(tagList(
    fluidRow(style="margin-left:0px;margin-right:0px",
             column(2,
                    selectizeInput(inputId="enrichment_output_plot_parameters_document_selection","Document:",choices=document_choices, multiple=F)
             ),
             column(2,
                    checkboxInput(inputId="enrichment_output_plot_parameters_exclude_NA","Exclude NAs?",value=T)
             ),
             column(2,
                    checkboxInput(inputId="enrichment_output_plot_parameters_use_only_max","Just use max. Value?",value=F)
             ),
             column(2,
                    checkboxInput(inputId="enrichment_output_plot_parameters_use_sliding_window","Use Sliding-Window?",value=F)
             ),
             column(2,
                    conditionalPanel(condition = "input.enrichment_output_plot_parameters_use_sliding_window==true",
                                     numericInput(inputId="enrichment_output_plot_parameters_sliding_window_size","Sliding-Window Size",min=2,max=50,value=5)
                    )
             )
    )
  ))
  
})



# results table 
output$enrichment_output_table<-DT::renderDataTable({
  validate(
    need(!is.null(values$enrichment_result),message = "Enrichment not finished yet.")
  )
  withProgress(session = session, message = "Preparing Results Table...",{expr = 

  interview_data <- values$enrichment_data
  topic_annotations <- values$enrichment_result
  n = input$enrichment_output_table_n
  chosen_topics <- lapply(X = 1:nrow(topic_annotations),  FUN = function(x){
    x = topic_annotations[x,]
    if(is.na(x[1])){
      return(NA)
    }else{
      topics <-as.numeric(which(x>input$enrichment_output_table_threshold))
      if(length(topics)>0){
      topic_probability <- x[topics]
      o <- order(topic_probability,decreasing = T)[1:min(input$enrichment_output_table_n,length(topics))]
      topics <- topics[o]
      return(topics)
      }
      else{
        return(NA)
      }
    }
  })
  topic_labels <-rep(c(""),length(chosen_topics))
  topic_ids <- rep(c(""),length(chosen_topics))
  topic_model_id <- stringr::str_extract(string = input$enrichment_topic,pattern = "^[0-9]{1,5}(?=_)")
  for(i in 1:length(chosen_topics)){
    topic_probability <- topic_annotations[i,chosen_topics[[i]]]
    if(any(!is.na(topic_probability))){
    #o <- order(topic_probability,decreasing = T)[1:input$enrichment_output_table_n]
    topic_label = paste0(values$topic_enrichment_topic_model_topic_labels[chosen_topics[[i]]],collapse = ", ")
    topic_labels[i] <-topic_label
    topic_id <- paste0(topic_model_id,"_",chosen_topics[[i]],"(",round(topic_probability,digits = 2),")",collapse=", ")
    topic_ids[i] <- topic_id
    }
  }
  topic_ids[which(is.na(chosen_topics))]<-""
  topic_labels[which(is.na(chosen_topics))]<-""
  
  interview_data$zwischenueberschrift<-topic_labels
  interview_data$registerverknüpfungen<-topic_ids
  # add ner data
  if(input$enrichment_include_ner_tags==TRUE){
    ner <- values$enrichment_ner[c("token","entity_type","id_interview","id_interview_row")]
    ner <- aggregate(token ~ entity_type + id_interview + id_interview_row, data=ner,FUN = function(x){
      paste0(x, collapse=" # ")
    })
    help <- interview_data
    id_interview_row <- unlist(lapply(unique(help$id_interview),FUN=function(x){
      return(1:length(which(help$id_interview==x)))
    }))
    help$id_interview_row = id_interview_row
    locations <- ner[which(ner$entity_type=="LOC"),c(2,3,4)]
    persons <- ner[which(ner$entity_type=="PER"),c(2,3,4)]
    orgs <- ner[which(ner$entity_type=="ORG"),c(2,3,4)]
    locations_merged <- merge(x=help, y = locations, by=c("id_interview","id_interview_row"),all.x=T)
    persons_merged <- merge(x=help, y = persons, by=c("id_interview","id_interview_row"),all.x=T)
    orgs_merged <- merge(x=help, y = orgs, by=c("id_interview","id_interview_row"),all.x=T)
    interview_data$ner_loc <- locations_merged$token
    interview_data$ner_per <- persons_merged$token
    interview_data$ner_org <- orgs_merged$token
  }
  values$enrichment_result_interview<-interview_data
  })
  DT::datatable(data = interview_data, width = "100%",
                selection = "none",escape=F,class = 'cell-border stripe hover',
                options = list(scrollX=T, scrollY="42vh"))
})

#enrichment results UI
output$enrichment_results_UI<-renderUI({
  validate(
    need(!is.null(values$enrichment_result),message=F)
  )
  return(tagList(
    tabsetPanel(id = "enrichment_results",
                tabPanel(title = "Resulting Transcript Table",
                         DT::dataTableOutput(outputId = "enrichment_output_table")
                ),
                tabPanel(title = "Document Overview",
                         uiOutput("enrichment_output_plot_parameters_UI"),
                         plotlyOutput("enrichment_output_plot",height = "65vh")%>%withSpinner()
                ),
                tabPanel(title = "Document Details",
                         uiOutput("enrichment_output_document_parameters_UI"),
                         uiOutput(outputId = "enrichment_document_details")%>%withSpinner()
                )
    )
  ))
})


# overview heatmap
output$enrichment_output_plot<-renderPlotly({
  validate(
    need(!is.null(input$enrichment_output_plot_parameters_document_selection),message=F)
  )
  topic_dist <- values$enrichment_result
  data <- values$enrichment_data
  topic_dist <- topic_dist[which(data$id_interview%in%input$enrichment_output_plot_parameters_document_selection),]
  data <- data[which(data$id_interview%in%input$enrichment_output_plot_parameters_document_selection),]
  text = data$transkript
  colnames(topic_dist) <- stringr::str_replace_all(string = colnames(topic_dist),pattern = "\n",replacement = " ")
  if(input$enrichment_output_plot_parameters_exclude_NA){
    if(length(which(is.na(topic_dist[,1])))>0){
      text = text[-which(is.na(topic_dist[,1]))]
      topic_dist <- topic_dist[-which(is.na(topic_dist[,1])),]
      
    }
  }
  if(input$enrichment_output_plot_parameters_use_sliding_window){
    window_size=input$enrichment_output_plot_parameters_sliding_window_size
    topic_dist_new <- matrix(c(NA),(nrow(topic_dist)-(window_size)-1),ncol(topic_dist))
    text_new <- NULL
    for(i in 1:nrow(topic_dist_new)){
      topic_dist_new[i,]<-colMeans(topic_dist[i:(i+(window_size-1)),])
      text_window <- paste0(text[i:(i+(window_size-1))],collapse="\n")
      text_new <- c(text_new,text_window)
    }
    colnames(topic_dist_new) <- colnames(topic_dist)
    topic_dist <- topic_dist_new
    text = text_new
  }
  if(input$enrichment_output_plot_parameters_use_only_max){
    topic_dist_new<-matrix(c(0),nrow(topic_dist),ncol(topic_dist))
    for(i in 1:nrow(topic_dist)){
      topic_dist_new[i,which.max(topic_dist[i,])]<-1
    }
    colnames(topic_dist_new) <- colnames(topic_dist)
    topic_dist <- topic_dist_new
  }
  rownames(topic_dist)<-1:nrow(topic_dist)
  text = data.frame(rows = 1:nrow(topic_dist),text=text)
  
  data <- reshape2::melt(topic_dist)
  data <- merge(data,text,by.x="Var1",by.y="rows")
  ######################
  p <- plotly::plot_ly(data = data, x = ~Var2, y = ~Var1, z = ~value,type="heatmap", hoverinfo = 'text',
                       key=~text,
                       text = ~paste("<b> Row:</b>", data$Var1,
                                     "<br><b> Topic:</b>", data$Var2,
                                     "<br><b> Text:</b>", data$text,
                                     "<br><b> Value:</b>", round(data$value,digits = 4)))%>%
    layout(xaxis=list(title="Topics"),yaxis=list(title="Row"))
  p
  return(p)
})

output$enrichment_output_document_parameters_UI<-renderUI({
  validate(
    need(!is.null(values$enrichment_data),message=F)
  )
  document_choices <- unique(values$enrichment_data$id_interview)
  names(document_choices) <- paste0("Interview: ",unique(values$enrichment_data$id_interview))
  return(tagList(
    fluidRow(style="margin-left:0px;margin-right:0px",
             column(2,
                    selectizeInput(inputId="enrichment_output_document_parameters_document_selection","Document:",choices=document_choices, multiple=F)
             )
    )
  ))
  
  
  
})


# enrichment output detailed document 
output$enrichment_document_details<-renderUI({
  validate(
    need(!is.null(input$enrichment_output_document_parameters_document_selection),message=F)
  )
  topic_dist <- values$enrichment_result
  data <- values$enrichment_data
  topic_dist <- topic_dist[which(data$id_interview%in%input$enrichment_output_document_parameters_document_selection),]
  data <- data[which(data$id_interview%in%input$enrichment_output_document_parameters_document_selection),]
  
  document <- ""
  colors <- rainbow(ncol(topic_dist))
  text = data$transkript
  speaker <- data$sprecher
  topic_labels <- values$topic_enrichment_topic_model_topic_labels
  topic_labels <- topic_labels[unlist(apply(X = topic_dist, MARGIN = 1, FUN = function(x){
    if(is.na(x[1])){
      return(NA)
    }else{
      which.max(x)
    }
  }))]
  topic_propbs <- apply(X = topic_dist,MARGIN = 1,max)
  for(i in 1:length(text)){
    if(is.na(topic_dist[i,1])){
      span <- paste0("<span> <b>",speaker[i],":&nbsp;&nbsp;&nbsp;</b><span style=border-bottom: 2px solid ","white",';">',text[i],"</span>","</span><br>")
    }
    else{
      color = colors[which.max(topic_dist[i,])]
      span <- paste0("<span>  <b>",speaker[i],":&nbsp;&nbsp;&nbsp;</b> <span style=\"border-bottom: 2px solid ",color,';" Title=\"',topic_labels[i],' (Prob:',round(topic_propbs[i],digits = 2),")\">",text[i],"</span>","</span><br>")
      
      document <- paste0(document, span,collapse = "\n")
    }   
  }
  
  return(HTML(document))
})



#export UI
output$enrichment_download_UI<-renderUI({
  validate(
    need(!is.null(values$enrichment_result),message=F)
  )
  return(tagList(
    fluidRow(style="margin-left:0px;margin-right:0px",
             # column(3,
             #        downloadButton(outputId = "enrichment_export_single_CSV",label = "All Interviews as single CSV")
             # ),
             column(2,
                    downloadButton(outputId = "enrichment_export_zip",label = "Transcript(s)")
             ),
             column(2,
                    downloadButton(outputId = "enrichment_export_topic",label = "Topic Model ") 
             ),
             column(2,
                    textInput(inputId="enrichment_topic_name","Specify a name for the chosen Topic Model",placeholder=paste0("iLCM ID of chosen Topic Model: ",stringr::str_extract(string = input$enrichment_topic,pattern = "^[0-9]{1,5}(?=_)")))
                    ),
             column(2,
                    numericInput(inputId = "enrichment_topic_words_n","Number of Top Words per Topic Description",value=10,min=1,max=ncol(values$topic_enrichment_topic_model_phi),step=5)
                    )
             
    )
  ))
})


output$enrichment_export_zip <- downloadHandler(
  filename = function(){
    if(input$enrichment_upload_new_csv){
      data_name <- input$enrichment_new_transcript$name
    }
    else{
      data_name <- input$enrichment_collection
    }
    return(
      paste0("Topic_Enrichment_",data_name,"_Model_",stringr::str_extract(string = input$enrichment_topic,pattern = "^[0-9]{1,5}(?=_)"),".zip")
    )
    
  },
  content = function(file){
    data<-values$enrichment_result_interview
    try({
      if(input$enrichment_include_ner_tags){
        colnames(data)<-c("Interview_Id", "Band","Timecode","Sprecher","Transkript","Übersetzung","Hauptüberschrift","Zwischenüberschrift","Hauptüberschrift (Übersetzung)","Zwischenüberschrift (Übersetzung)","Registerverknüpfungen", "Anmerkungen", "Anmerkungen (Übersetzung)","NER LOC","NER PER","NER ORG")
        
      }
      else{
        colnames(data)<-c("Interview_Id", "Band","Timecode","Sprecher","Transkript","Übersetzung","Hauptüberschrift","Zwischenüberschrift","Hauptüberschrift (Übersetzung)","Zwischenüberschrift (Übersetzung)","Registerverknüpfungen", "Anmerkungen", "Anmerkungen (Übersetzung)")
      }
    })
    try({
      do.call(file.remove, list(list.files("collections/tmp/ohd_enrichment/", full.names = TRUE)))
    })
    interviews <- unique(data$Interview_Id)
    
    fs = NULL
    for(i in 1:length(interviews)){
      data_interview <- data[which(data$Interview_Id==interviews[i]),]
      #remove interview id
      data_interview <- data_interview[,-1]
      readr::write_tsv(data_interview,path=paste0("collections/tmp/ohd_enrichment/",interviews[i],".csv"))
      fs=c(fs,paste0("collections/tmp/ohd_enrichment/",interviews[i],".csv"))
    }
    zip::zipr(zipfile=file, files=fs,include_directories = F)
  },contentType = "application/zip"
)

#TODO
output$enrichment_export_topic <- downloadHandler(
  filename = function(){
    if(input$enrichment_upload_new_csv){
      data_name <- input$enrichment_new_transcript$name
    }
    else{
      data_name <- input$enrichment_collection
    }
    paste0("TopicDetails","_Model_",stringr::str_extract(string = input$enrichment_topic,pattern = "^[0-9]{1,5}(?=_)"),".tsv")
  },
  content = function(file){
    phi<-values$topic_enrichment_topic_model_phi
    top_words <- apply(X = phi,1,FUN = function(x){
      scores <-round(x[order(x,decreasing = T)][1:input$enrichment_topic_words_n],digits = 3)
      names <- colnames(phi)[order(x,decreasing = T)][1:input$enrichment_topic_words_n]
      paste0(names," (",scores,")",collapse=", ")
    })
    data <- data.frame(id=1:nrow(phi),description=top_words)
    if(input$enrichment_topic_name==""){
      model_name <- stringr::str_extract(string = input$enrichment_topic,pattern = "^[0-9]{1,5}(?=_)")
    }
    else{
      model_name <- input$enrichment_topic_name
    }
    # create registry template format
    topic_labels <- values$topic_enrichment_topic_model_topic_labels
    data$id <- paste0(model_name,"_",data$id)
    data$parent_name <- "Topics"
    data$parent_id = 82
    data$name <- topic_labels
    data$latitude = ""
    data$longitude = ""
    data$'GND ID' = ""
    data$'OSM ID' = ""
    data <- data[,c("parent_name","parent_id","name","id","description","latitude","longitude","GND ID","OSM ID")]
    readr::write_tsv(data,path=file)
  }
)


# output$enrichment_export_single_CSV <- downloadHandler(
#   filename = function(){
#     if(input$enrichment_upload_new_csv){
#       data_name <- input$enrichment_new_transcript$name
#     }
#     else{
#       data_name <- input$enrichment_collection
#     }
#     paste0("Topic_Enrichment_",data_name,"_Model_",stringr::str_extract(string = input$enrichment_topic,pattern = "^[0-9]{1,5}(?=_)"),".csv")
#   },
#   content = function(file){
#     data<-values$enrichment_result_interview
#     try({
#       colnames(data)<-c("Interview_Id", "Band","Timecode","Sprecher","Transkript","Übersetzung","Hauptüberschrift","Zwischenüberschrift","Hauptüberschrift (Übersetzung)","Zwischenüberschrift (Übersetzung)","Registerverknüpfungen", "Anmerkungen", "Anmerkungen (Übersetzung)")
#     })
#     write.csv2(data,file=file,row.names = F)
#   }
# )