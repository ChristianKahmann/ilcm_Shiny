DF_prepare_dataframe_for_plain_text_selection <- function(dataframe) {
  if (!"end_position" %in% colnames(dataframe)){
    dataframe$end_eosition <- str_count(dataframe$body)
  }
  return(dataframe)
}

DF_loadRData <- function(filename){
  load(filename)
  get(ls()[ls() != "filename"])
}

DF_info_to_dataframe <- function(df){
  d <- data.frame(
    'id_doc'=df[[1]]$x....id_doc_i..
    , 'dataset'=df[[2]]$x....dataset_s..
    , 'global_doc_id'=df[[3]]$x....id..
    , 'date'=df[[6]]$substr.x....date_dt....1..10.
    , 'score'=df[[7]]$x....score..
  )
  return(d)
}

DF_create_refi_users <- function(usernames){
  user_name <- paste0(unique(usernames))
  user_guid <- sapply(seq(length(user_name)), function(x) UUIDgenerate(TRUE))
  users <- data.frame(guid = user_guid, name = user_name)
  return(users)
}

DF_create_refi_sets <- function(collection_names){
  set_name <- c(collection_names)
  set_guid <- sapply(seq(length(set_name)), function(x) UUIDgenerate(TRUE))
  sets <- data.frame(guid = set_guid, name = set_name)
  return(sets)
}

DF_create_refi_codes_from_anno_schemes <- function(anno_scheme){
  if (!is.null(anno_scheme)){
    anno_file <- file.path(ANNO_SCHEME_HOME, paste0(anno_scheme, ".RData"))
    if (file.exists(anno_file)){
      load(anno_file)
      codes <- IO_anno_scheme_to_dataframe(anno)
    }else {
      codes <- data.frame()
    }
  } else {
    codes <- data.frame()
  }
  return(codes)
}

DF_create_refi_text_sources <- function(documents){
  text_sources_guids <- sapply(seq(nrow(documents)), function(x) UUIDgenerate(TRUE))
  text_sources_paths <- paste0("internal://", text_sources_guids, ".txt")
  text_sources_id <- documents$id
  text_sources_content <- documents$body
  text_sources <- data.frame(
    guid = text_sources_guids
    , plaintext_path = text_sources_paths
    , plaintext_content = text_sources_content
    , id = text_sources_id
  )
  return(text_sources)
}

DF_create_refi_plaintext_selections <- function(annotations, text_sources, users, codes){
  if (!is.data.frame(codes) && nrow(codes) <= 0) {
    return(data.frame())
  }
  plaintext_selections <- select(annotations, guid = anno_id, start_position = from, end_position = to, text_source_guid = global_doc_id, creation_datetime = Annotation_Date, creating_user = User, code_ref = Annotation)
  for (row in 1:nrow(plaintext_selections)){
    ts <- text_sources[text_sources$id == plaintext_selections[row, "text_source_guid"],]
    ts_guid <- paste0(ts[1, "guid"])
    plaintext_selections[row, "text_source_guid"] <- ts_guid
    plaintext_selections[row, "creation_datetime"] <- str_replace(plaintext_selections[row, "creation_datetime"], " ", "T")
    plaintext_selections[row, "creating_user"] <- paste0(filter(users, name == plaintext_selections[row, "creating_user"])$guid)
    plaintext_selections[row, "code_ref"] <- paste0(filter(codes, name == annotations[row, "Annotation"])$guid)
  }
  return(plaintext_selections)
}

DF_create_refi_plaintext_selections_for_topic_models2 <- function(text_sources, users, codes, topic_model_file,token){
  load(topic_model_file)
  user <- users[1]
  plaintext_selections <- data.frame(
    guid = character(0)
    , start_position = character(0)
    , end_position = character(0)
    , text_source_guid = character(0)
    , creation_datetime = character(0)
    , creating_user = character(0)
    , code_ref = character(0)
  )
  for (row in 1:nrow(text_sources)){
    doc <- DB_get_document_by_id(text_sources[row, "id"])
    token_local<-token[which(token$id==paste0(doc[1,"dataset"],"_",doc[1,"id_doc"])),]
    dataset_doc_id <- paste0(doc$dataset, "_", doc$id_doc)
    doc_topics <- theta[dataset_doc_id,]
    max_topic <- as.numeric(names(sort(doc_topics, decreasing=T)[1]))
    topic <- codes$name[max_topic]
    code <- filter(codes, name==topic)
    if (is.data.frame(code) && nrow(code) > 0){
      code <- paste0(code$guid)
    } else {
      code <- paste0("topic '", topic,"' not found in codebook")
    }
    plaintext_selection <- data.frame(
      guid = UUIDgenerate(TRUE)
      , start_position = "0"
      , end_position = (as.numeric(token_local[nrow(token_local),"idx"])+nchar(token_local[nrow(token_local),"word"])-1)
      , text_source_guid = paste0(text_sources[row, "guid"])
      , creation_datetime = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      , creating_user = user$guid
      , code_ref = code
    )
    
    plaintext_selections <- rbind(plaintext_selections, plaintext_selection)
  }
  return(plaintext_selections)
}

# DF_create_refi_plaintext_selections_for_topic_models <- function(annotations, text_sources, users, codes, topic_model_file){
#   load(topic_model_file)
#   browser()
#   plaintext_selections <- select(annotations, guid = anno_id, start_position = from, end_position = to, text_source_guid = global_doc_id, creation_datetime = Annotation_Date, creating_user = User, code_ref = Annotation, dataset = dataset, id = id)
#   for (row in 1:nrow(plaintext_selections)){
#     ts <- text_sources[text_sources$id == plaintext_selections[row, "text_source_guid"],]
#     ts_guid <- paste0(ts[1, "guid"])
#     plaintext_selections[row, "text_source_guid"] <- ts_guid
#     doc <- paste0(plaintext_selections[row, "dataset"], "_", plaintext_selections[row, "id"])
#     doc_topics <- theta[doc,]
#     max_topic <- as.numeric(names(sort(doc_topics, decreasing=T)[1]))
#     topic <- paste0(vocab[order(phi[max_topic,],decreasing = T)[1:5]], collapse = "-")
#     plaintext_selections[row, "creation_datetime"] <- str_replace(plaintext_selections[row, "creation_datetime"], " ", "T")
#     plaintext_selections[row, "creating_user"] <- paste0(filter(users, name == plaintext_selections[row, "creating_user"])$guid)
#     code <- filter(codes, name==topic)
#     if (is.data.frame(code) && nrow(code) > 0){
#       plaintext_selections[row, "code_ref"] <- paste0(code$guid)
#     }
#   }
#   return(plaintext_selections)
# }

DF_create_refi_plaintext_selections_for_classification2 <- function(original_text, text_sources, users, codes, classification, anno_scheme,token){
  user <- users[1]
  plaintext_selections <- data.frame(
    guid = character(0)
    , start_position = character(0)
    , end_position = character(0)
    , text_source_guid = character(0)
    , creation_datetime = character(0)
    , creating_user = character(0)
    , code_ref = character(0)
  )
  for (row in 1:nrow(original_text)){
    ot <- original_text[row,]
    dataset<-stringr::str_split(ot$doc_id,pattern = "_",simplify = T)[1,1]
    doc_id<-stringr::str_split(ot$doc_id,pattern = "_",simplify = T)[1,2]
    s_id<-NULL
    try({
      s_id<-stringr::str_split(ot$doc_id,pattern = "_",simplify = T)[1,3]
    },silent = T)
    # if document annotations
    if(is.null(s_id)){
      token_local<-token[which(token$id==paste0(dataset,"_",doc_id)),]
    }
    else{
      token_local<-token[intersect(which(token$id==paste0(dataset,"_",doc_id)),which(token$sid==s_id)),]
    }
    if (is.data.frame(ot) && nrow(ot) == 0){
      # print(paste0("can not find '", dataset_doc_id, "' in original_text!"))
      next
    }
    code <- filter(codes, name==paste0(ot$V1))
    if (is.data.frame(code) && nrow(code) > 0){
      code <- paste0(code$guid)
    } else {
      code <- paste0("not found in codebook")
    }
    start<-token_local[1,"idx"]
    end<-  as.numeric(token_local[nrow(token_local),"idx"]) + nchar(token_local[nrow(token_local),"word"])-1
    plaintext_selection <- data.frame(
      guid = UUIDgenerate(TRUE)
      , start_position = start
      , end_position = end
      , text_source_guid = filter(text_sources,id==ot$global_ids)$guid
      , creation_datetime = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      , creating_user = user$guid
      , code_ref = code
     # , description = paste0("probability: ", ot$probabilities)
    )
    plaintext_selections <- rbind(plaintext_selections, plaintext_selection)
  }
  return(plaintext_selections)
}

DF_create_refi_plaintext_selections_for_classification <- function(annotations, text_sources, users, codes, classification, anno_scheme){
  classification_file <- file.path(ANALYSIS_RESULTS_HOME, "classification", "classifyCollection", classification, "texts.RData")
  if (!file.exists(classification_file)){
    return()
  }
  load(classification_file)
  plaintext_selections <- select(annotations, guid = anno_id, start_position = from, end_position = to, text_source_guid = global_doc_id, creation_datetime = Annotation_Date, creating_user = User, code_ref = Annotation, description = id, dataset = dataset)
  for (row in 1:nrow(plaintext_selections)){
    id <- paste0(plaintext_selections[row, "description"])
    dataset <- paste0(plaintext_selections[row, "dataset"])
    ts <- text_sources[text_sources$id == plaintext_selections[row, "text_source_guid"],]
    ts_guid <- paste0(ts[1, "guid"])
    plaintext_selections[row, "text_source_guid"] <- ts_guid
    plaintext_selections[row, "creation_datetime"] <- str_replace(plaintext_selections[row, "creation_datetime"], " ", "T")
    plaintext_selections[row, "creating_user"] <- paste0(filter(users, name == plaintext_selections[row, "creating_user"])$guid)
    plaintext_selections[row, "code_ref"] <- paste0(filter(codes, name == annotations[row, "Annotation"])$guid)
    probability <- paste0(filter(original_text, doc_id == paste0(dataset, "_", id))$probabilities)
    probability <- if (probability != "") probability else ""
    plaintext_selections[row, "description"] <- paste0("probability: ", probability)
  }
  return(plaintext_selections)
}

DF_create_refi_codes_from_topics <- function(selected_topics, topic_model_file){
  load(topic_model_file)
  topics <- c()
  phi<-phi[order(as.numeric(rownames(phi)),decreasing=F),]
  for (i in 1:ncol(theta)){
    topic <- paste0(vocab[order(phi[as.numeric(i),], decreasing = T)[1:5]], collapse = "-")
    topics <- c(topics, topic)
  }
  colors<-randomcoloR::randomColor(count = ncol(theta))
  df_topics <- data.frame(
    guid = sapply(seq(length(topics)), function(x) UUIDgenerate(TRUE))
    , name = topics
    , is_codable = "true"
    , color = colors
    , parent_guid = ""
    , description = paste0("Topic ", seq(from = 1, to = ncol(theta)))
  )
  return(df_topics)
}



