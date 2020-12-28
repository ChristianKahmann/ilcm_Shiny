#' export collection
#' @param output_directory
#' @param projectname
#' @param collection_name
#' @param annotation_scheme
#' @param session
#' 
#' @return out (collection to refi)
export_collection <- function(output_directory, projectname, collection_name, annotation_scheme = "all",session) {
  if (annotation_scheme == "all") {
    # change this method
    annotation_schemes <- DB_get_used_annotation_schemes(collection_name)
    if (nrow(annotation_schemes) != 0){
      for (anno_scheme in annotation_schemes$Anno_set){
        out<-collection_to_refi(output_directory, projectname = paste0(projectname, "-", anno_scheme), collection_name = collection_name, anno_scheme = anno_scheme, session = session)
      }
    } else {
      out<-collection_to_refi(output_directory, projectname = projectname, collection_name = collection_name, session = session)
    }
  } else {
    out<-collection_to_refi(output_directory, projectname = projectname, collection_name = collection_name, anno_scheme = annotation_scheme, session = session)
  }
  return(out)
}

#' collection to refi
#' @param output_directory
#' @param projectname
#' @param collection_name
#' @param anno_scheme
#' @param session
#' 
#' @return refi_export_collection_success and datframe with collection-details
#' 
#' @export
#' @example 
collection_to_refi <- function(output_directory, projectname, collection_name, anno_scheme = NULL,session){
  refi_export_collection_success=F
  collection_file <- file.path(COLLECTIONS_HOME, paste0(collection_name, ".RData"))
  if (!file.exists(collection_file)){
    return(refi_export_collection_success)
  }
  load(collection_file)
  out<-tryCatch(
    {
      info_data <- DF_info_to_dataframe(info)
      documents <- DB_get_documents_by_id(info_data$global_doc_id)
      # get token objects from database
      db_data<-get_token_meta_and_language_from_db(get_meta = F,get_language = T,get_global_doc_ids = F,host=host,port=db_port,id=info[[1]],dataset=info[[2]])
      text_sources <- DF_create_refi_text_sources(documents)
      sets <- DF_create_refi_sets(c(collection_name))
      
      if (is.null(anno_scheme)){
        users = NULL
        codes = NULL
        plaintext_selections = NULL
      } else {
        codes <- DF_create_refi_codes_from_anno_schemes(anno_scheme)
        annotations <- DB_get_annotations_by_global_document_ids_with_anno_scheme(info_data$global_doc_id, anno_scheme)
        users <- DF_create_refi_users(annotations$User)
        #transform word offset annotations into charater offsets 
        annotations <- transform_word_offset_to_character_offset(annotations = annotations, token=db_data$token)
        plaintext_selections <- DF_create_refi_plaintext_selections(annotations, text_sources, users, codes)
      }
      refi::create_project(
        name = projectname
        , output_directory = output_directory
        , text_sources = text_sources
        , plaintext_selections = plaintext_selections
        , users = users
        , sets = sets
        , codes = codes
      )
      refi_export_collection_success=T
      return(refi_export_collection_success)
    }, error=function(cond){
      shinyWidgets::sendSweetAlert(session=session,title = "REFI-QDA Project Export", text = paste0("Error while creating collection '", collection_name, "' to ", output_directory, "."), type = "error")
      message(cond)
      return(refi_export_collection_success)
    }
  )
  return(out)
}


#' export topic model
#' @param output_directory
#' @param projectname
#' @param collection_name
#' @param topic_model_file
#' @param selected_topics
#' @param session
#' 
#' @return refi_export_collection_success and dataframe with topic model information
export_topic_model <- function(output_directory, projectname, collection_name, topic_model_file, selected_topics,session){
  refi_export_collection_success=F
  collection_file <- file.path(COLLECTIONS_HOME, paste0(collection_name, ".RData"))
  if (!file.exists(topic_model_file)){
    return(refi_export_collection_success)
  }
  load(topic_model_file)
  if (!file.exists(collection_file)){
    return(refi_export_collection_success)
  }
  load(collection_file)
  out <- tryCatch(
    {
      info_data <- DF_info_to_dataframe(info)
      documents <- DB_get_documents_by_id(info_data$global_doc_id)
      # get token objects from database
      db_data<-get_token_meta_and_language_from_db(get_meta = F,get_language = T,get_global_doc_ids = F,host=host,port=db_port,id=info[[1]],dataset=info[[2]])
      text_sources <- DF_create_refi_text_sources(documents)
      sets <- DF_create_refi_sets(c(collection_name))
      codes <- DF_create_refi_codes_from_topics(selected_topics, topic_model_file)
      #annotations <- DB_get_annotations_by_global_document_ids(info_data$global_doc_id)
      #transform word offset annotations into charater offsets 
      #annotations <- transform_word_offset_to_character_offset(annotations = annotations, token=db_data$token)
      #users <- "topic model extraction"
      users <- data.frame(guid=uuid::UUIDgenerate(),name="topic model extraction",stringsAsFactors = F)
      plaintext_selections <- DF_create_refi_plaintext_selections_for_topic_models2(text_sources, users, codes, topic_model_file,token=db_data$token)
      refi::create_project(
        name = projectname
        , output_directory = output_directory
        , text_sources = text_sources
        , sets = sets
        , codes = codes
        , plaintext_selections = plaintext_selections
      )
      refi_export_collection_success=T
      return(refi_export_collection_success)
    }, error=function(cond){
      shinyWidgets::sendSweetAlert(session=session,title = "REFI-QDA Project Export", text = paste0("Error while creating collection '", collection_name, "' to ", output_directory, "."), type = "error")
      message(cond)
      return(refi_export_collection_success)
    }
  )
  return(out)
}
#' export classification
#' @param output_directory
#' @param projectnme
#' @param collection_name
#' @param classification
#' @param session
#' 
#' @return refi_export_collection_success and out (datframe with classification information)
export_classification <- function(output_directory, projectname, collection_name, classification,session){
  refi_export_collection_success=F
  collection_file <- file.path(COLLECTIONS_HOME, paste0(collection_name, ".RData"))
  if (!file.exists(collection_file)){
    return(refi_export_collection_success)
  }
  load(collection_file)
  classification_results <- file.path(ANALYSIS_RESULTS_HOME, "classification", "classifyCollection", classification, "texts.RData")
  classification_parameters <- file.path(ANALYSIS_RESULTS_HOME, "classification", "classifyCollection", classification, "parameters.RData")
  if (!file.exists(classification_results)){
    return(refi_export_collection_success)
  } 
  if (!file.exists(classification_parameters)){
    return(refi_export_collection_success)
  }
  load(classification_results)
  load(classification_parameters)
  anno_scheme <- parameters$Project
  out <- tryCatch(
    {
      info_data <- DF_info_to_dataframe(info)
      documents <- DB_get_documents_by_id(info_data$global_doc_id)
      # get token objects from database
      db_data<-get_token_meta_and_language_from_db(get_meta = F,get_language = T,get_global_doc_ids = F,host=host,port=db_port,id=info[[1]],dataset=info[[2]])
      
      text_sources <- DF_create_refi_text_sources(documents)
      codes <- DF_create_refi_codes_from_anno_schemes(anno_scheme)
      #match global_id to id_doc of original_texts
      doc_ids<-stringr::str_split(as.character(original_text[,3]),pattern = "_",simplify = T)[,1:2]
      global_ids<-NULL
      for(i in 1:nrow(doc_ids)){
        global_id<-NA
        try({
          global_id<-info[[3]][intersect(which(info[[2]]==doc_ids[i,1]),which(info[[1]]==doc_ids[i,2])),1]
        })
        global_ids<-c(global_ids,global_id)
      }
      
      original_text<-cbind(original_text,global_ids)
      
      users <- data.frame(guid=uuid::UUIDgenerate(),name="classification result",stringsAsFactors = F)
      plaintext_selections <- DF_create_refi_plaintext_selections_for_classification2(original_text=original_text, text_sources, users, codes, classification, anno_scheme,token=db_data$token)
      
      sets <- DF_create_refi_sets(c(collection_name))
      refi::create_project(
        name = projectname
        , output_directory = output_directory
        , text_sources = text_sources
        , sets = sets
        , codes = codes
        , users = users
        , plaintext_selections = plaintext_selections
      )
      refi_export_collection_success=T
      return(refi_export_collection_success)
    }, error=function(cond){
      shinyWidgets::sendSweetAlert(session=session,title = "REFI-QDA Project Export", text = paste0("Error while creating collection '", collection_name, "' to ", output_directory, "."), type = "error")
      message(cond)
      return(refi_export_collection_success)
    }
  )
  return(out)
}

#' export_codebook
#' @param projectname
#' @param annotation_scheme
#' @param output_directory
#' @param session
#' 
#' @return refi_export_codebook_success and out (dataframe for codebook information)
export_codebook <- function(projectname, annotation_scheme, output_directory,session){
  refi_export_codebook_success<-F
  out<-tryCatch(
    { 
      codes <- DF_create_refi_codes_from_anno_schemes(annotation_scheme)
      dir.create(path = output_directory)
      refi::create_codebook(output_directory = output_directory, projectname = projectname, codes = codes)
      refi_export_codebook_success<-T
      return(refi_export_codebook_success)
    }, error=function(cond){
      shinyWidgets::sendSweetAlert(session=session,title = "REFI-QDA Codebook Export", text = paste0("Error while exporting annotation scheme '", annotation_scheme, "' to ", output_directory, "."), type = "error")
      message(cond)
      return(refi_export_codebook_success)
    }
  )
  return(out)
}

