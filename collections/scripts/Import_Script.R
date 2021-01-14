source("global/text_functions.R")
source("global/log_to_file.R")
source("global/preprocess_data.R")
source("global/functions_used_in_scripts.R")
source("config_file.R")
source("global/change_annotation_offset_methods.R")

#process
error<-try(expr = {
  library(Matrix)
  library(dplyr)
  library(spacyr)
  library(readr)
  library(RMariaDB)
  
  #load parameters
  load("collections/tmp/tmp.RData")
  
  
  metadata<-parameters[[1]]
  write_to_db<-parameters[[2]]
  language<-parameters[[3]]
  date_format<-parameters[[4]]
  meta_metadata<-parameters[[5]]
  slow_mode<-F
  try({
    slow_mode<-parameters[[6]]
  })
  #reduce metadata object to the metadata columns the user specified
  if(dim(meta_metadata)[2]==1){
    metadata<-metadata[,c("dataset","id_doc","title","body","date","token","language")]
  }
  else{
    metadata<-metadata[,c("dataset","id_doc","title","body","date","token","language",paste(colnames(meta_metadata)[2:dim(meta_metadata)[2]],sep=""))]
  }
  
  
  spacy_initialize(model = language)
  log_to_file(message = "spacy initialized",logfile)
  # write import csv for meta and token information
  preprocess_data(text = metadata[,"body"],metadata = metadata,process_id = process_info[[1]],offset = (min(as.numeric(metadata[,"id_doc"]))-1),logfile = logfile,date_format = date_format,slow_mode=slow_mode)
  # write meta metadata csv
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
      rs<-RMariaDB::dbSendStatement(mydb, query)
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
      #data<-data.frame(readtext::readtext(file = paste0("data_import/processed_data/meta_",metadata[1,"dataset"],"_",process_info[[1]],".csv") ),stringsAsFactors = F)
      data<-data.frame(readr::read_delim(file = paste0("data_import/processed_data/meta_",metadata[1,"dataset"],"_",process_info[[1]],".csv"), delim=',',
                                         escape_double=FALSE, escape_backslash=TRUE, quote='"',col_names = F),
                       stringsAsFactors = F)
      data<-cbind(rep(paste0("data_import/processed_data/meta_",metadata[1,"dataset"],"_",process_info[[1]],".csv"),nrow(data)),data)%>%
        mutate_all(as.character)
      #date
      dates<-unique(data[,6])
      dates<-cbind(rep(data[1,2],length(dates)),dates)
      rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_date (dataset, date) values ",paste(sprintf("('%s', '%s')", dates[,1], dates[,2]), collapse=', ') ,";"))
      #token
      token<-unique(data[,7])
      token<-cbind(rep(data[1,2],length(token)),token)
      rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_token (dataset, token) values ",paste(sprintf("('%s', %s)", token[,1], token[,2]), collapse=', ') ,";"))
      #mde1
      try({
        mde1<-unique(data[,9])
        mde1<-cbind(rep(data[1,2],length(mde1)),mde1)
        #check if only NA
        if(any(!is.na(mde1[,2]))){
          mde1<-mde1[which(!is.na(mde1[,2])),]
          rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde1 (dataset, mde1) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde1[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde1[,2])), collapse=', ') ,";"))
        }
      })
      #mde2
      try({
        mde2<-unique(data[,10])
        mde2<-cbind(rep(data[1,2],length(mde2)),mde2)
        #check if only NA
        if(any(!is.na(mde2[,2]))){
          mde2<-mde2[which(!is.na(mde2[,2])),]
          rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde2 (dataset, mde2) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde2[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde2[,2])), collapse=', ') ,";"))
        }
      })
      #mde3
      try({
        mde3<-unique(data[,11])
        mde3<-cbind(rep(data[1,2],length(mde3)),mde3)
        #check if only NA
        if(any(!is.na(mde3[,2]))){
          mde3<-mde3[which(!is.na(mde3[,2])),]
          rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde3 (dataset, mde3) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde3[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde3[,2])), collapse=', ') ,";"))
        }
      })
      #mde4
      try({
        mde4<-unique(data[,12])
        mde4<-cbind(rep(data[1,2],length(mde4)),mde4)
        #check if only NA
        if(any(!is.na(mde4[,2]))){
          mde4<-mde4[which(!is.na(mde4[,2])),]
          rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde4 (dataset, mde4) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde4[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde4[,2])), collapse=', ') ,";"))
        }
      })
      #mde5
      try({
        mde5<-unique(data[,13])
        mde5<-cbind(rep(data[1,2],length(mde5)),mde5)
        #check if only NA
        if(any(!is.na(mde5[,2]))){
          mde5<-mde5[which(!is.na(mde5[,2])),]
          rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde5 (dataset, mde5) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde5[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde5[,2])), collapse=', ') ,";"))
        }
      })
      #mde6
      try({
        mde6<-unique(data[,14])
        mde6<-cbind(rep(data[1,2],length(mde6)),mde6)
        #check if only NA
        if(any(!is.na(mde6[,2]))){
          mde6<-mde6[which(!is.na(mde6[,2])),]
          rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde6 (dataset, mde6) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde6[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde6[,2])), collapse=', ') ,";"))
        }
      })
      #mde7
      try({
        mde7<-unique(data[,15])
        mde7<-cbind(rep(data[1,2],length(mde7)),mde7)
        #check if only NA
        if(any(!is.na(mde7[,2]))){
          mde7<-mde7[which(!is.na(mde7[,2])),]
          rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde7 (dataset, mde7) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde7[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde7[,2])), collapse=', ') ,";"))
        }
      })
      #mde8
      try({
        mde8<-unique(data[,16])
        mde8<-cbind(rep(data[1,2],length(mde8)),mde8)
        #check if only NA
        if(any(!is.na(mde8[,2]))){
          mde8<-mde8[which(!is.na(mde8[,2])),]
          rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde8 (dataset, mde8) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde8[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde8[,2])), collapse=', ') ,";"))
        }
      })
      #mde9
      try({
        mde9<-unique(data[,17])
        mde9<-cbind(rep(data[1,2],length(mde9)),mde9)
        #check if only NA
        if(any(!is.na(mde9[,2]))){
          mde9<-mde9[which(!is.na(mde9[,2])),]
          rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde9 (dataset, mde9) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde9[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde9[,2])), collapse=', ') ,";"))
        }
      })
      rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
      
      log_to_file(message = "Finished sending data to db",logfile)
      
      log_to_file(message = "Importing data from Database to solr",logfile)
      url<-stringr::str_replace(string = url,pattern = "select/",replacement = "")
      z<-RCurl::getURL(
        paste0(url,"dataimport?command=delta-import"),followlocation=TRUE
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
  if(write_to_db==T){
    log_to_file(message = paste0("Finished preprocessing. You can now select ",metadata[1,"dataset"]," in the Explorer Corpus Selection"),logfile)
  }
  else{
    log_to_file(message = paste0("Finished preprocessing. You can now import the created csv files in Import/Export Importer Upload Data to DB and
                                 Solr with the name:",metadata[1,"dataset"],"_",process_info[[1]] ),logfile)
  }
  
  # save annotations?
  if(length(parameters)==6){
    if(parameters$save_annotations==TRUE){
      log_to_file(message = "Save Annotations from imported data to database",logfile)
      load("collections/tmp/tmp_annotations.RData")
      source("global/refi/db.R")
      source("global/refi/xml.R")
      source("global/refi/io.R")
      source("global/refi/import.R")
      source("global/refi/plaintext_selection.R")
      source("global/refi/interface.R")
      library(xml2)
      annotations <- data.frame(
        anno_id = character(0) # guid von plaintext_selection
        , User = character(0) # von plaintext_selection
        , dataset = character(0) # neu
        , id = character(0) #
        , from = character(0) # startPosition
        , to = character(0) # endPosition
        , Annotation = character(0) # von Codebook targetGuid
        , color = character(0) # von plaintext_selection
        , Annotation_Date = character(0)
        , Anno_set = character(0)
        , collection = character(0)
        , global_doc_id = character(0)
        , text = character(0)
        , document_annotation = character(0)
      )
      
      # 1. codebook importieren, da annotations sich darauf beziehen
      xml_document<-xml2::as_xml_document(xml_document)
      codebook <- xml_find_first(xml_document, "//d1:CodeBook")
      import_codebook(codebook, name = dataset)
      
      load("collections/tmp/tmp.RData")
      id_docs <-parameters[[1]]$id_doc
      text_sources <- xml_find_all(xml_document, "//d1:Project/d1:Sources/d1:TextSource")
      for (text_source in text_sources) {
        
        plaintext_selections <- xml_find_all(text_source, "./d1:PlainTextSelection")
        
        
        if (length(plaintext_selections) == 0) {
          
        } else if (length(plaintext_selections) >= 1) {
          # check if plaintext_selection exists, if not --> continue loop
          plaintext_path <- xml_attr(text_source, 'plainTextPath')
          plaintext_content <- xml_find_first(text_source, './d1:PlainTextContent')
          
          for (selection in plaintext_selections) {
            
            
            if (!is.na(plaintext_path)) {
              text_source_content <- ""
              if (stringr::str_detect(plaintext_path, "internal://")) {
                text_source_filename <- strsplit(plaintext_path, "://")[[1]][2]
                if("Sources"%in% list.files(import_directory)){
                  text_source_filepath <- file.path(import_directory, "Sources", text_source_filename)
                }
                else{
                  text_source_filepath <- file.path(import_directory, "sources", text_source_filename)
                }
                text_source_content <- readr::read_file(text_source_filepath)
              }
              
            } else if (is.na(plaintext_content)) {
              text_source_content <- xml_text(plaintext_content)
            } else {
              text_source_content <- ""
            }
            
            guid <- xml_attr(selection, "guid")
            from <- xml_attr(selection, "startPosition")
            to <- xml_attr(selection, "endPosition")
            if (stringi::stri_length(text_source_content) >= as.numeric(to)) {
              text <- substr(text_source_content, as.numeric(from), as.numeric(to))
            } else {
              text <- ""
            }
            
            user <- xml_attr(selection, "creatingUser")
            anno_date <- xml_attr(selection, "creationDateTime")
            
            if (is.na(anno_date)) {
              anno_date <- format(Sys.Date(), "%Y-%m-%d")
            }
            
            if (is.na(user)) {
              user = "unknown"
            }
            
            id_doc <- xml_attr(text_source, "id_doc")
            document <- DB_get_document_by_id_and_dataset(id_doc, dataset)
            global_doc_id <- document$id
            
            code_ref <- xml_find_first(selection, './d1:Coding/d1:CodeRef')
            
            if (!is.null(code_ref)) {
              coderef_target_guid <- xml_attr(code_ref, 'targetGUID')
              code <- xml_find_first(codebook, paste0("//d1:Code[@guid='", coderef_target_guid, "']"))
              code_name <- xml_attr(code, "name")
              code_color <- xml_attr(code, 'color')
            } else {
              code_name <- "no reference to any code"
              code_color <- paste0('#', stringi::stri_rand_strings(1,6, "[A-Fa-f0-9]"))
            }
            anno <- data.frame(
              anno_id = guid
              , User = user
              , dataset = dataset
              , id = id_doc
              , from = from
              , to = to
              , Annotation = code_name
              , color = code_color
              , Annotation_Date = substr(anno_date,1,min(19,nchar(anno_date)))
              , Anno_set = dataset
              , collection = dataset
              , global_doc_id = global_doc_id
              , text = text
              , document_annotation = FALSE
              ,stringsAsFactors = F
            )
            print(id_doc)
            db_data<-get_token_meta_and_language_from_db_refi(host=host,port=db_port,id=id_doc,dataset=dataset)
            # change character offset annotations to word offsets
            anno <- transform_character_offset_to_word_offset(annotations = anno, token = db_data$token)
            # check if document_annotation==T
            # assume document annotation if annotation starts with first word and ends with last word
            if(anno[1,"from"]==1 && anno[1,"to"]==nrow(db_data$token)){
              anno$document_annotation <- TRUE
            }
            annotations <- rbind(annotations, anno)
          }
          
        } else {
          
        }
        
      }
      # limit text field to max 10000 chars
      annotations[which(nchar(annotations$text)>10000),"text"]<-paste0(substr(annotations[which(nchar(annotations$text)>10000),"text"],0,9997),"...")
      DB_import_annotations(annotations)
      log_to_file(message = paste0("Succesfully imported ",nrow(annotations)," annotations to the database"),logfile)
    }
  }
  
  system(paste("mv ",logfile," collections/logs/finished/",sep=""))
  
}) 

if(class(error)=="try-error"){
  system(paste("mv ",logfile," collections/logs/failed/",sep=""))
  log_to_file(message=error[[1]],file = stringr::str_replace(string = logfile,pattern = "running",replacement = "failed"))
}