preprocess_data<-function(text,metadata,process_id,offset,logfile,date_format){
  split_size=100
  split<-split(1:length(text), ceiling(seq_along(1:length(text))/min(length(text),split_size)))
  token<-NULL
  count=0
  for(i in split){
    count=count+1
    toks<-spacyr::spacy_parse(iconv(text[i], "UTF-8", "UTF-8",sub=''),pos = T,tag = F,lemma = T,entity = T,dependency = F)
    toks[,1]<-stringr::str_replace_all(string = toks[,1],pattern = "text",replacement = "")
    toks[,1]<-metadata[((as.numeric(toks[,1])+(count-1)*min(length(text),split_size))),"id_doc"]
    token<-rbind(token,toks)
    log_to_file(paste(count,"of:",length(split)),logfile)
    if(count%%10==0)gc()
  }  
  token<-token[,c("doc_id","sentence_id","token_id","token","lemma","pos","entity")]
  #toDelete<-which(token[,6]=="SPACE")
  #if(length(toDelete)>0){
  #  token<-token[-which(token[,6]=="SPACE"),]
  #}
  token[which(token[,6]=="SPACE"),4:5]<-""
 
  token<-cbind(rep(metadata[1,"dataset"],dim(token)[1]),token)
  
  
  log_to_file(message = "data preprocessed",logfile)

  print("converting dates")
  metadata[,"date"]<-as.character(as.Date(as.matrix(metadata[,"date"]),format =date_format ))

  log_to_file(message = "extracting entities",logfile)
  metadata<-cbind(metadata,rep(0,dim(metadata)[1]))
  count=0
  for(j in unique(token[,2])){
    count=count+1
    toks<-token[which(token[,2]==j),c("doc_id"  ,    "sentence_id", "token_id"  ,  "token"    ,   "lemma"     ,  "pos"    ,     "entity")]
    class(toks)<-c("spacyr_parsed","data.frame")
    entities<-unique(spacyr::entity_extract(toks,type = "named")[,3])
    entities<-stringr::str_replace_all(string = entities,pattern = " ",replacement = "_")
    entities<-paste(entities,collapse=" ")
    metadata[count,dim(metadata)[2]]<-entities
  }
  
  
  log_to_file(message = "Writing data",logfile)
  
  write.table(x = metadata,file = paste("data_import/processed_data/meta_",metadata[1,"dataset"],"_",process_id,".csv",sep=""),row.names = F,col.names = F,sep = ",")
  write.table(x = token,file = paste("data_import/processed_data/token_",metadata[1,"dataset"],"_",process_id,".csv",sep=""),row.names = F,col.names = F,sep = ",")
  log_to_file(message = "Finished writing data",logfile)
}