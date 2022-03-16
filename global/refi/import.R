#' refi to collection
#' @param qdpx_file
#' @param dataset
#' 
#' @return r as list depending on:
#' list:
#'       data
#'       xml_document
#'       importDirectory
#' @export
#' @example 
refi_to_collection <- function(qdpx_file, dataset){
  unpacked_dir <- IO_unpack_project(qdpx_file$datapath)
  qde_file <- file.path(unpacked_dir, list.files(unpacked_dir,recursive = T, pattern = "qde"))
  if (!file.exists(qde_file)) {
    message("Project does not contain a .qde-file!")
    return()
  }
  
  if (!IO_is_valid_qde_file(qde_file)) {
    message(".qde-file is does not corrospond to the REFI-scheme file!")
    #return()
  }
  
  
  xml_document <- read_xml(qde_file)
  project_node <- xml_name(xml_document)
  text_sources <- xml_find_all(xml_document, "//d1:Project/d1:Sources/d1:TextSource")
  codebook <- xml_find_first(xml_document, "//d1:CodeBook")
  users <- xml_find_all(xml_document, "//d1:User")
  sets <- xml_find_all(xml_document, "//d1:Set")
  plaintext_selections <- xml_find_all(xml_document, "//d1:PlainTextSelection")
  data <- import_text_sources(text_sources, unpacked_dir, dataset)
  
  # unlink(unpacked_dir, recursive = TRUE)  
  
  r <- list("data" = data, "xmlDoc" = xml_document, "importDirectory" = unpacked_dir)
  return(r)
}

#' import text sources
#' @param test_sources
#' @param unpacked_dir
#' @param dataset
#' 
#' @return data (imported documents/texts as dataframe)
#' 
#' @export
#' @example 
import_text_sources <- function(text_sources, unpacked_dir, dataset){
  id_doc <- 0
  data <- data.frame(
    dataset = character(0)
    , id_doc = character(0)
    , title = character(0)
    , body = character(0)
    , date = character(0)
    , token = character(0)
    , language = character(0)
    , mde1 = character(0)
    , mde2 = character(0)
    , mde3 = character(0)
    , mde4 = character(0)
    , mde5 = character(0)
    , mde6 = character(0)
    , mde7 = character(0)
    , mde8 = character(0)
    , mde9 = character(0)
  )
  for (text_source in text_sources) {
    id_doc <- id_doc + 1
    if (xml_has_attr(text_source, 'plainTextPath')) {
      plain_text_path <- xml_attr(text_source, 'plainTextPath')
      if("Sources"%in% list.files(unpacked_dir)){
        body_file <- file.path(unpacked_dir, "Sources", str_replace(plain_text_path, 'internal://', ""))
      }
      else{
        body_file <- file.path(unpacked_dir, "sources", str_replace(plain_text_path, 'internal://', ""))
      }
      if (!file_ext(body_file) == "txt") {
        message(paste0("Skipping TextSource. Could not open file extension ", file_ext(body_file), "!"))
        next
      }
      ts_body <- read_file(body_file)
    } else if (XML_has_plain_text_content(text_source)) {
      ptc <- xml_find_all(text_source, './d1:PlainTextContent')
      ts_body <- xml_text(ptc)
      
    } else {
      # missing plainTextPath attribute or plainTextContent
      message("Missing plainTextPath-attribute or plainTextContent-element. TextSource should contains at least one of that. Skipping TextSource ...")
      next
    }
    
    if (xml_has_attr(text_source, "name")) {
      ts_title <- xml_attr(text_source, "name")
    } else {
      ts_title <- paste0("title-", id_doc)
    }
    
    if (xml_has_attr(text_source, "creationDateTime")) {
      ts_date <- xml_attr(text_source, "creationDateTime")
    } else {
      ts_date <- format(Sys.Date(), "%Y-%m-%d")
    }
    ts_body <- trimws(ts_body)
    
    ts_body <- gsub("'", "\'", ts_body)
    df <- data.frame(
      dataset = dataset
      , id_doc = id_doc
      , title = ts_title
      , body = as.character(ts_body)
      , date = ts_date
      , token = as.numeric(sapply(strsplit(as.character(ts_body), " "), length))
      , language = cld2::detect_language(text = as.character(ts_body))
      , mde1 = ""
      , mde2 = ""
      , mde3 = ""
      , mde4 = ""
      , mde5 = ""
      , mde6 = ""
      , mde7 = ""
      , mde8 = ""
      , mde9 = ""
    )
    xml_set_attr(text_source, "id_doc", id_doc )
    data <- rbind(data, df)
  }
  return(data)
}

# muss eventuell nach dem import extra gemacht werden, damit die ids von den datasets da sind, sowie die globaldoc_ids
# import_annotations <- function(text_source){
#   annotations <- data.frame(
#     anno_id = character(0) # guid von plaintext_selection
#     , User = character(0) # von plaintext_selection
#     , dataset = character(0) # neu
#     , id = character(0) #
#     , from = character(0) # startPosition
#     , to = character(0) # endPosition
#     , Annotation = character(0) # von Codebook targetGuid
#     , color = character(0) # von plaintext_selection
#     , Annotation_Date = character(0)
#     , Anno_set = character(0)
#     , collection = character(0)
#     , global_doc_id = character(0)
#     , text = character(0)
#     , document_annotation = character(0)
#   )
# }
#' get codebook
#' @param node
#' 
#' @return annotation scheme
#' 
#' @export
#' @example 
get_codebook <- function(node){
  anno_scheme <- list()
  hash <- NULL
  if (xml_name(node) == 'Code') {
    desc_node <- xml_find_first(node, ".//d1:Description")
    desc <- xml_text(desc_node)
    name <- xml_attr(node, 'name')
    if (xml_has_attr(node, 'color')) {
      color <- xml_attr(node, 'color')
    } else {
      color <- paste0("#",stringi::stri_rand_strings(1, 6, '[A-F0-9]'))
    }
    cat <- add_category(anno_scheme, name = name, color = color, description = desc)
    hash <- cat$hash
    anno_scheme <- cat$newList
  }
  children <- xml_children(node)
  print(xml_attr(node, 'name'))
  for (child in children) {
    child_anno <- get_codebook(node = child)
    if (!is.null(hash)) {
      anno_scheme[[hash]]$sublist <- append(anno_scheme[[hash]]$sublist, child_anno)
    }
    else {
      anno_scheme <- append(anno_scheme, child_anno)
    }
  }
  return(anno_scheme)
}

#' add_category
#' @param edit_list
#' @param name
#' @param color
#' @param description
#' 
#' @return list depending on:
#' list:
#'          hash
#'          temporary list (name, color, isDocumentAnnotation, description)
#'          edit_list
#'
#' @export
#' @example 
add_category <- function(edit_list, name = NULL, color = NULL, description = NULL){
  tmp <- list()
  hash <- stringi::stri_rand_strings(1, 5, '[A-Z0-9]')
  tmp[[hash]] <- list(
    name = name,
    color = color,
    isDocumentAnnotation = F,
    sublist = list(),
    description = description
  )
  return(list(hash = hash, newList = append(tmp,edit_list)))
}

#' import codebook
#' @param codebook
#' @param name
#' 
#' @return 
#' @export
#' @example 
import_codebook <- function(codebook, name){
  codes <- xml_find_first(codebook, ".//d1:Codes")
  anno <- list()
  anno <- get_codebook(node = codes)
  tryCatch({
    save(anno, file = paste0(ANNO_SCHEME_HOME, '/', name, '.RData'))
    message("Successfully imported new anno scheme.")
  }, error=function(cond){
    message(cond)
  })
}

#' import function
#' @param 
#' 
#' @return 
#' @export
#' @example 
import_function <- function(){
  source("global/text_functions.R")
  source("global/log_to_file.R")
  source("global/preprocess_data.R")
  source("config_file.R")
  #process
  error<-try(expr = {
    load("collections/tmp/tmp.RData")
    library(Matrix)
    library(dplyr)
    library(spacyr)
    library(RMariaDB)
    
    metadata<-parameters[[1]]
    write_to_db<-parameters[[2]]
    language<-parameters[[3]]
    date_format<-parameters[[4]]
    meta_metadata<-parameters[[5]]
    
    #reduce metadata object to the metadata columns the user specified
    if(dim(meta_metadata)[2]==1){
      metadata<-metadata[,c("dataset","id_doc","title","body","date","token","language")]
    }
    else{
      metadata<-metadata[,c("dataset","id_doc","title","body","date","token","language",paste(colnames(meta_metadata)[2:dim(meta_metadata)[2]],sep=""))]
    }
    
    if(language%in%c("en","de","es","fr","it","nl","pt","el","xx")){
      avail_models <-  stringr::str_remove_all(string=stringr::str_split(
        stringr::str_replace_all(string = system(command = "python -m spacy info",intern = T)[8],pattern = "Pipelines[ ]+",replacement = "")
        ,pattern = ", ",simplify = T),pattern = " ")
      fitting_model <- avail_models[grepl(pattern = language,x = substr(avail_models,1,3))]
      if(length(fitting_model>0)){
        langauge=fitting_model[1]
      }
      else{
        stop("No model for specified language found")
      }
    }
    
    

    spacy_initialize(model = stringr::str_remove_all(string = language, pattern = "\\(.+\\)"))
    log_to_file(message = "spacy initialized",logfile)
    
    #write import csv for meta and token information
    preprocess_data(text = metadata[,"body"],metadata = metadata,process_id = process_info[[1]],offset = (min(as.numeric(metadata[,"id_doc"]))-1),logfile = logfile,date_format = date_format)
    #write meta metadata csv
    write.csv(x = parameters[[5]],file=paste0("data_import/processed_data/metameta_",metadata[1,"dataset"],"_",process_info[[1]],".csv"),row.names = F)
    
    log_to_file(message = "finished writing results metadata to database",logfile)
    if(write_to_db==T){
      mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
      rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
      
      log_to_file(message = "Sending data to db",logfile)
      a<-readr::read_csv(file = paste0("data_import/processed_data/meta_",metadata[1,"dataset"],"_",process_info[[1]],".csv"),col_names = FALSE)[1,c(1,2)]
      b<-dbGetQuery(mydb,paste0("Select title from documents where id_doc=",a[1,2]," and dataset='",a[1,1],"' limit 1;"))
      if(dim(b)[1]==0){
        if(dim(meta_metadata)[2]==1){
          query<-paste0("LOAD DATA LOCAL INFILE '","data_import/processed_data/meta_",metadata[1,"dataset"],"_",process_info[[1]],".csv","' INTO TABLE ilcm.documents CHARACTER SET utf8mb4 FIELDS TERMINATED BY ',' ENCLOSED BY '\"' LINES TERMINATED BY '","\n","' 
                        (dataset,id_doc,title,body,date,token,language",",entities) ;")
          rs<- dbSendQuery(mydb, query)
        }
        else{
          query<-paste0("LOAD DATA LOCAL INFILE '","data_import/processed_data/meta_",metadata[1,"dataset"],"_",process_info[[1]],".csv","' INTO TABLE ilcm.documents CHARACTER SET utf8mb4  FIELDS TERMINATED BY ',' ENCLOSED BY '\"' LINES TERMINATED BY '","\n","' 
                        (dataset,id_doc,title,body,date,token,language,",paste(colnames(meta_metadata)[2:dim(meta_metadata)[2]],collapse=","),",entities) ;")
          rs<- dbSendQuery(mydb, query)
        }
        query<-paste0("LOAD DATA LOCAL INFILE '","data_import/processed_data/token_",metadata[1,"dataset"],"_",process_info[[1]],".csv","' INTO TABLE ilcm.token CHARACTER SET utf8mb4  FIELDS TERMINATED BY ',' ENCLOSED BY '\"' LINES TERMINATED BY '","\n","';")
        rs<- dbSendQuery(mydb, query)
        
        try({
          if(dim(meta_metadata)[2]==1){
            query<-paste0("LOAD DATA LOCAL INFILE '","data_import/processed_data/metameta_",metadata[1,"dataset"],"_",process_info[[1]],".csv","' INTO TABLE ilcm.metadata_names CHARACTER SET utf8mb4  FIELDS TERMINATED BY ',' ENCLOSED BY '\"' LINES TERMINATED BY '","\n","' IGNORE 1 LINES (dataset",");")
            rs<- dbSendQuery(mydb, query)
          }
          else{
            query<-paste0("LOAD DATA LOCAL INFILE '","data_import/processed_data/metameta_",metadata[1,"dataset"],"_",process_info[[1]],".csv","' INTO TABLE ilcm.metadata_names  CHARACTER SET utf8mb4 FIELDS TERMINATED BY ',' ENCLOSED BY '\"' LINES TERMINATED BY '","\n","' IGNORE 1 LINES (dataset,",paste(colnames(meta_metadata)[2:dim(meta_metadata)[2]],collapse=","),");")
            rs<- dbSendQuery(mydb, query)
          }
        })
        
        
        #update meta tables in database
        data<-data.frame(readtext::readtext(file = paste0("data_import/processed_data/meta_",metadata[1,"dataset"],"_",process_info[[1]],".csv") ),stringsAsFactors = F)
        #date
        dates<-unique(data[,6])
        dates<-cbind(rep(data[1,2],length(dates)),dates)
        rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_date (dataset, date) values ",paste(sprintf("('%s', %s)", dates[,1], dates[,2]), collapse=', ') ,";"))
        #token
        token<-unique(data[,7])
        token<-cbind(rep(data[1,2],length(token)),token)
        rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_token (dataset, token) values ",paste(sprintf("('%s', %s)", token[,1], token[,2]), collapse=', ') ,";"))
        #mde1
        try({
          mde1<-unique(data[,9])
          mde1<-cbind(rep(data[1,2],length(mde1)),mde1)
          rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_token (dataset, mde1) values ",paste(sprintf("('%s', %s)", mde1[,1], mde1[,2]), collapse=', ') ,";"))
        })
        #mde2
        try({
          mde2<-unique(data[,10])
          mde2<-cbind(rep(data[1,2],length(mde2)),mde2)
          rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_token (dataset, mde2) values ",paste(sprintf("('%s', %s)", mde2[,1], mde2[,2]), collapse=', ') ,";"))
        })
        #mde3
        try({
          mde3<-unique(data[,11])
          mde3<-cbind(rep(data[1,2],length(mde3)),mde3)
          rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_token (dataset, mde3) values ",paste(sprintf("('%s', %s)", mde3[,1], mde3[,2]), collapse=', ') ,";"))
        })
        #mde4
        try({
          mde4<-unique(data[,12])
          mde4<-cbind(rep(data[1,2],length(mde4)),mde4)
          rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_token (dataset, mde4) values ",paste(sprintf("('%s', %s)", mde4[,1], mde4[,2]), collapse=', ') ,";"))
        })
        #mde5
        try({
          mde5<-unique(data[,13])
          mde5<-cbind(rep(data[1,2],length(mde5)),mde5)
          rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_token (dataset, mde5) values ",paste(sprintf("('%s', %s)", mde5[,1], mde5[,2]), collapse=', ') ,";"))
        })
        #mde6
        try({
          mde6<-unique(data[,14])
          mde6<-cbind(rep(data[1,2],length(mde6)),mde6)
          rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_token (dataset, mde6) values ",paste(sprintf("('%s', %s)", mde6[,1], mde6[,2]), collapse=', ') ,";"))
        })
        #mde7
        try({
          mde7<-unique(data[,15])
          mde7<-cbind(rep(data[1,2],length(mde7)),mde7)
          rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_token (dataset, mde7) values ",paste(sprintf("('%s', %s)", mde7[,1], mde7[,2]), collapse=', ') ,";"))
        })
        #mde8
        try({
          mde8<-unique(data[,16])
          mde8<-cbind(rep(data[1,2],length(mde8)),mde8)
          rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_token (dataset, mde8) values ",paste(sprintf("('%s', %s)", mde8[,1], mde8[,2]), collapse=', ') ,";"))
        })
        #mde9
        try({
          mde9<-unique(data[,17])
          mde9<-cbind(rep(data[1,2],length(mde9)),mde9)
          rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_token (dataset, mde9) values ",paste(sprintf("('%s', %s)", mde9[,1], mde9[,2]), collapse=', ') ,";"))
        })
        log_to_file(message = "Finished sending data to db",logfile)
        
        log_to_file(message = "Importing data from Database to solr",logfile)
        url<-stringr::str_replace(string = url,pattern = "select/",replacement = "")
        z<-RCurl::getURL(
          paste0(url,"dataimport?command=full-import"),followlocation=TRUE
        )
        #initiate suggest
        z<-RCurl::getURL(
          paste0(url,"suggest?suggest.build=true"),followlocation=TRUE
        )
        log_to_file(message = "Finished importing data from database to solr",logfile)
      }
      else{
        log_to_file(message = "Error: combination of dataset and doc_id seems to be already used",logfile)
        
      }
      RMariaDB::dbDisconnect(mydb)
    }
    log_to_file(message = "Finished preprocessing. Restart App to work with the new data",logfile)
    system(paste("mv ",logfile," collections/logs/finished/",sep=""))
    
  }) 
}

