#' IO unpack project
#' @param qdpx_file
#' 
#' @return temporary_working_directory
#' 
#' @export
#' @example 
IO_unpack_project <- function(qdpx_file) {
  temporary_working_directory <- IO_create_tempdir(parent_dir = "data_import/refi/")
  unzip(zipfile = qdpx_file, exdir = temporary_working_directory)
  # check if qde file and sources directory are positioned directly inside temporary_working_directorry
  files<-list.files(temporary_working_directory)
  if(any(grepl(pattern = ".qde",x = files))&& dir.exists(paste0(temporary_working_directory,"/Sources"))){
    dir.create(path = paste0(temporary_working_directory,"/help_directory/"))
    for(i in 1:length(files)){
      file.copy(from = paste0(temporary_working_directory,"/",files[i]),to = paste0(temporary_working_directory,"/help_directory/"),recursive = T)
      unlink(paste0(temporary_working_directory,"/",files[i]),recursive = T)
    }
    
  }
  temporary_working_directory <- list.dirs(temporary_working_directory,recursive = F)
  return(temporary_working_directory)
}
#' IO create temporary directory
#' @param parent_dir
#' 
#' @return temporary working directory
#' 
#' @export
#' @example 
IO_create_tempdir <- function(parent_dir) {
  temporary_working_directory <- file.path(parent_dir, stri_rand_strings(1, 8, "[A-Za-z]"))
  if(!dir.exists(temporary_working_directory)){
    dir.create(temporary_working_directory, recursive = TRUE, showWarnings = SHOW_WARNINGS)
  }
  return(temporary_working_directory)
}

#' IO get annotation schemes
#' 
#' @return annotation schemes
#' 
#' @export
#' @example 
IO_get_annotation_schemes <- function(){
  anno_schemes <- list.files(ANNO_SCHEME_HOME, pattern = ".RData")
  anno_schemes <- lapply(anno_schemes, FUN = function(x){tools::file_path_sans_ext(x)})
  anno_schemes <- unlist(anno_schemes, use.names = FALSE)
  anno_schemes <- data.frame(schemes=anno_schemes)
  return(anno_schemes$schemes)
}

#' IO get topics
#' @param topic_model
#' 
#' @return topics or nothing if the topic model is empty
#' 
#' @export
#' @example 
IO_get_topics <- function(topic_model){
  if (is.null(topic_model)){
    return(c(""))
  }
  topic_model_file <- file.path("collections/results", "topic-model", topic_model, "data_TM.RData")
  if (!file.exists(topic_model_file)){
    return()
  }
  load(topic_model_file)
  number_of_topics <- nrow(phi)
  topics <- c()
  for (i in 1:nrow(phi)){
    topic <- paste0(vocab[order(phi[i,],decreasing = T)[1:5]], collapse = "-")
    topics <- c(topics, topic)
  }
  return(topics)
}

#' IO get analysis
#' @param collection
#' @param analysis
#' 
#' @return analysis files
#' 
#' @export
#' @example 
IO_get_analysis <- function(collection, analysis){
  if (analysis == 'Classfication') {
    path_analysis_rdata <- file.path("collections/results", "classification", "classifyCollection")
  } else if (analysis == 'Topic Model') {
    path_analysis_rdata <- file.path("collections/results", "topic-model")
  } else {
    path_analysis_rdata <- ""
  }
  analysis_files <- list.files(path_analysis_rdata, pattern = paste0('.*', collection, '.*'))
  return(analysis_files)
}

#' IO get classification project
#' 
#' @return annotation schemes
#' 
#' @export
#' @example 
IO_get_classification_projects <- function(){
  anno_schemes <- list.files(file.path("collections/results", "classification", "classifyCollection"))
  return(anno_schemes)
}

#' IO is valid qde files
#' @param qde_file
#' 
#' @return qde
#' 
#' @export
#' @example 
IO_is_valid_qde_file <- function(qde_file) {
  qde <- FALSE
  tryCatch(
    {
      xml_doc <- read_xml(qde_file)
      qde <- XML_validate_project_scheme(xml_doc)
    }, error=function(cond){
      message(cond)
    }
  )
  return(qde)
}

#' IO get classification
#' @param collection
#' 
#' @return annotation schemes
#' 
#' @export
#' @example 
IO_get_classifications <- function(collection){
  anno_schemes <- list.files(file.path("collections/results", "classification", "classifyCollection"), pattern = paste0("[0-9]*_", collection, "_[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}"))
  return(anno_schemes)
}

#' IO annotation scheme to dataframe
#' @param anno
#' 
#' @return codes
#' 
#' @export
#' @example 
IO_anno_scheme_to_dataframe <- function(anno){
  codes <- data.frame(
    guid = character(0)
    , name = character(0)
    , is_codable = character(0)
    , color = character(0)
    , description = character(0)
    , parent_guid = character(0)
  )
  codes <- IO_get_anno_df(anno = anno, codes = codes)
  return(codes)
}

#' IO get annotation data frame
#' @param anno
#' @param parent
#' @param parent_guid
#' @param codes
#' 
#' @return codes
#' 
#' @export
#' @example
IO_get_anno_df <- function(anno, parent = "", parent_guid = "", codes){
  ids <- names(anno)
  if (length(ids) > 0) {
    for (id in ids) {
      current_guid <- UUIDgenerate(TRUE)
      df <- data.frame(
        guid = current_guid
        , name = anno[[id]]$name
        , is_codable = "true"
        , color = anno[[id]]$color
        , description = anno[[id]]$description
        , parent_guid = if (parent_guid == "") "" else parent_guid
      )
      codes <- rbind(codes, df)
      codes <- IO_get_anno_df(anno = anno[[id]]$sublist, parent = anno[[id]], parent_guid = current_guid, codes = codes)
    }
  }
  return(codes)
}





