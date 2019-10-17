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
  
  
  spacy_initialize(model = language)
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
      data<-data.frame(readtext::readtext(file = paste0("data_import/processed_data/meta_",metadata[1,"dataset"],"_",process_info[[1]],".csv") ),stringsAsFactors = F)
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
  system(paste("mv ",logfile," collections/logs/finished/",sep=""))
  
}) 

if(class(error)=="try-error"){
  system(paste("mv ",logfile," collections/logs/failed/",sep=""))
  log_to_file(message=error[[1]],file = stringr::str_replace(string = logfile,pattern = "running",replacement = "failed"))
}