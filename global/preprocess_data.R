#' preprocess data
#' @param text
#' @param metadata
#' @param process_id
#' @param offset
#' @param logfile
#' @param date_format
#' @param slow_mode
#' 
#' @return 
#' @export
#' @example 
preprocess_data<-function(text,metadata,process_id,offset,logfile,date_format,slow_mode=F){
  metadata$body<-stringr::str_replace_all(string = metadata$body,pattern = '"',"'")
  metadata$body<-stringr::str_replace_all(string = metadata$body,pattern = '\\\\0'," ")
  text<-stringr::str_replace_all(string = text,pattern = '"',"'")
  text<-stringr::str_replace_all(string = text,pattern = '\\\\0'," ")
  
  empty_text<-which(text=="")
  if(length(empty_text)>0){
    log_to_file(message = paste0(length(empty_text)," empty doucments found and removed"),logfile)
    text<-text[-empty_text]
    metadata<-metadata[-empty_text,,drop=F]
  }
  mean_doc_length=ceiling(mean(as.numeric(metadata$token)))
  split_documents=F
  if(slow_mode==T){
    split_size=1
    # too long documents can cause spacy to take to much RAM, which will then cause the import process to fail
    # in order to limit this, very long documents will be processed in several chunks
    split_documents=T
  }
  else{
    split_size=100
    if(mean_doc_length>1000){
      split_size=50
    }
    if(mean_doc_length>10000){
      split_size=15
    }
    if(mean_doc_length>50000){
      split_size=8
    }
    if(mean_doc_length>75000){
      split_size=1
    }
    if(mean_doc_length>100000){
      split_size=1
      # too long documents can cause spacy to take to much RAM, which will then cause the import process to fail
      # in order to limit this, very long documents will be processed in several chunks
      split_documents=T
    }
    
    
  }
  split<-split(1:length(text), ceiling(seq_along(1:length(text))/min(length(text),split_size)))
  token<-NULL
  count=0
  log_to_file(paste0("Input splitted into ",length(split)," parts"),logfile)
  for(i in split){
    count=count+1
    if(split_documents==F){
      toks<-spacyr::spacy_parse(iconv(text[i], "UTF-8", "UTF-8",sub=''),pos = T,tag = F,lemma = T,entity = T,dependency = F,additional_attributes = "idx")
      toks[,1]<-stringr::str_replace_all(string = toks[,1],pattern = "text",replacement = "")
      toks[,1]<-metadata[((as.numeric(toks[,1])+(count-1)*min(length(text),split_size))),"id_doc"]
      token<-rbind(token,toks)
      log_to_file(paste(count,"of:",length(split)),logfile)
      if(count%%10==0)gc()
    }
    else{
      #get single document
      document<-iconv(text[i], "UTF-8", "UTF-8",sub='')
      #split in chunks of 10000 chars
      offset=0
      chunk_char_size=10000
      if(nchar(document)>chunk_char_size){
        document_chunk<-substr(document,offset,min(nchar(document),(offset+chunk_char_size)))
        token_doc<-spacyr::spacy_parse(document_chunk,pos = T,tag = F,lemma = T,entity = T,dependency = F,additional_attributes = "idx")
        
        boundary<-max(which(token_doc$sentence_id==(max(token_doc$sentence_id)-3)))
        sentence_offset<-token_doc$sentence_id[boundary]
        new_offset<-token_doc[(1+boundary),"idx"]+1
        token_doc<-token_doc[1:boundary,]
        k=1
        while(nchar(document)>new_offset){
          k=k+1
          document_chunk<-substr(document,new_offset,min(nchar(document),(new_offset+chunk_char_size)))
          toks<-spacyr::spacy_parse(document_chunk,pos = T,tag = F,lemma = T,entity = T,dependency = F,additional_attributes = "idx")
          toks$sentence_id<-toks$sentence_id+sentence_offset
          toks$idx<-toks$idx+new_offset-1
          
          if(nchar(document)>(new_offset+chunk_char_size)){
            boundary<-max(which(toks$sentence_id==(max(toks$sentence_id)-3)))
            new_offset<-toks[(1+boundary),"idx"]+1
            sentence_offset<-toks$sentence_id[boundary]
            toks<-toks[1:boundary,]
          }
          else{
            boundary<-nrow(toks)
            new_offset<-nchar(document)+1
          }
          token_doc<-rbind(token_doc,toks)
        }
        token_doc[,1]<-stringr::str_replace_all(string = token_doc[,1],pattern = "text",replacement = "")
        token_doc[,1]<-metadata[((as.numeric(token_doc[,1])+(count-1)*min(length(text),split_size))),"id_doc"]
        token<-rbind(token,token_doc)
      }
      else{
        token_doc<-spacyr::spacy_parse(document,pos = T,tag = F,lemma = T,entity = T,dependency = F,additional_attributes = "idx")
        token_doc[,1]<-stringr::str_replace_all(string = token_doc[,1],pattern = "text",replacement = "")
        token_doc[,1]<-metadata[((as.numeric(token_doc[,1])+(count-1)*min(length(text),split_size))),"id_doc"]
        token<-rbind(token,token_doc)
      }
      if(length(split)>=100){
        log_sequence<-round(seq(from=1,to=length(split),length.out=101))[2:101]
        if(count%in%log_sequence){
          log_to_file(paste(which(log_sequence==count),"% finished (",count,")"),logfile)
        }
      }
      else{
        log_to_file(paste(count,"of:",length(split)),logfile)
        if(count%%10==0)gc()
      }
    }
    
  }  
  token<-token[,c("doc_id","sentence_id","token_id","token","lemma","pos","entity","idx")]
  #toDelete<-which(token[,6]=="SPACE")
  #if(length(toDelete)>0){
  #  token<-token[-which(token[,6]=="SPACE"),]
  #}
  token[which(token[,6]=="SPACE"),4:5]<-""
  
  token<-cbind(rep(metadata[1,"dataset"],dim(token)[1]),token)
  
  
  log_to_file(message = "data preprocessed",logfile)
  
  print("converting dates")
  formatted_dates<-as.character(as.Date(as.matrix(metadata[,"date"]),format =date_format ))
  if(all(is.na(formatted_dates))){
    # if dates only consist of 4 characters, assume only year is given
    if(nchar(metadata[1,"date"])==4){
      formatted_dates<-as.character(as.Date(as.matrix(metadata[,"date"]),format ="%Y" ))
    }
  }
  metadata[,"date"] <- formatted_dates
  
  log_to_file(message = "extracting entities...",logfile)
  metadata<-cbind(metadata,rep(0,dim(metadata)[1]))
  count=0
  for(j in unique(token[,2])){
    count=count+1
    toks<-token[which(token[,2]==j),c("doc_id"  ,    "sentence_id", "token_id"  ,  "token"    ,   "lemma"     ,  "pos"    ,     "entity",  "idx")]
    class(toks)<-c("spacyr_parsed","data.frame")
    entities<-unique(spacyr::entity_extract(toks,type = "named")[,3])
    entities<-stringr::str_replace_all(string = entities,pattern = " ",replacement = "_")
    entities<-paste(entities,collapse=" ")
    metadata[count,dim(metadata)[2]]<-entities
  }
  # ensure no quotes detroy csv structure
  for(i in 1:ncol(metadata)){
    metadata[,i]<-stringr::str_replace_all(string = metadata[,i],pattern = '"',replacement = "'")
  }
  log_to_file(message = "Writing data...",logfile)
  write.table(x = metadata,file = paste("data_import/processed_data/meta_",metadata[1,"dataset"],"_",process_id,".csv",sep=""),row.names = F,col.names = F,sep = ",",fileEncoding = "UTF-8",quote = TRUE)
  write.table(x = token,file = paste("data_import/processed_data/token_",metadata[1,"dataset"],"_",process_id,".csv",sep=""),row.names = F,col.names = F,sep = ",",fileEncoding = "UTF-8")
  log_to_file(message = "Finished writing data",logfile)
}