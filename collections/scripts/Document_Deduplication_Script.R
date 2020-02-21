source("global/text_functions.R")
source("global/log_to_file.R")
source("config_file.R")
source("global/functions_used_in_scripts.R")
source("global/create_body_for_solr_atomic_update.R")
source("global/save_collection_to_db.R")

error<-try(expr = {
  library(Matrix)
  library(dplyr)
  library(readr)
  library(textreuse)
  library(magrittr)
  library(plyr)
  
  #load parameters
  load("collections/tmp/tmp.RData")
  parameters_original<-parameters
  
  #load collection 
  log_to_file(message = "<b>Step 1/8: Loading collection</b>",file = logfile)
  load(paste("collections/collections/",unlist(parameters[1]),".RData",sep=""))
  parameters$id<-process_info[[1]]
  parameters$task<-process_info[[3]]
  parameters$started<-process_info[[4]]
  log_to_file(message = "  <b style='color:green'> ✔ </b> Finished loading collection",file = logfile)
  
  
  
  #load data from database
  log_to_file(message = "<b>Step 2/8: Loading data from database</b>",file = logfile)
  path0<-paste0("collections/results/document-deduplication/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
  rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
  ids<-info[[3]]
  ids<- paste(ids[,1],collapse=", ")
  meta<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from documents where id in (",ids,");"))
  RMariaDB::dbDisconnect(mydb)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished loading data from database",file = logfile)
  
  
  #sanity check
  log_to_file(message = "<b>Step 3/8: Sanity check</b>",file = logfile)
  #token object not empty
  log_to_file(message = "&emsp; meta object not empty?",logfile)
  if(dim(meta)[1]>1){
    log_to_file(message = "&emsp; ✔",logfile)
  }
  else{
    log_to_file(message = "&emsp;<b style='color:red'>&#10008; No documents were found in the database for the specified collection.</b>",logfile)
    stop("Meta empty")
  }

  
  #create minhashes
  log_to_file(message = "<b>Step 4/8: Creating hash fucntion</b>",file = logfile)
  minhash <- minhash_generator(n = 480, seed = 3552)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  ",file = logfile)
  
  # create corpus object
  log_to_file(message = "<b>Step 5/8: Creating corpus object and calculating hashes</b>",file = logfile)
  corpus <- TextReuseCorpus(text=meta$body, tokenizer = tokenize_ngrams, n = 5,
                            minhash_func = minhash, keep_tokens = TRUE,
                            progress = TRUE)
  
  buckets <- lsh(corpus, bands = 160, progress = TRUE)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  ",file = logfile)
  
  
  # find candidates
  log_to_file(message = "<b>Step 6/8: Extract possible candidates</b>",file = logfile)
  candidates <- lsh_candidates(buckets)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  ",file = logfile)
  
  
  # calculate similarity
  log_to_file(message = "<b>Step 7/8: Calculate similarity score for candidates</b>",file = logfile)
  if(parameters$DD_similarity_measure=="jaccard bag similarity"){
    results<-data.frame(lsh_compare(candidates, corpus, jaccard_bag_similarity, progress = FALSE),stringsAsFactors = F)
  }
  if(parameters$DD_similarity_measure=="jaccard similarity"){
    results<-data.frame(lsh_compare(candidates, corpus, jaccard_similarity, progress = FALSE),stringsAsFactors = F)
  }
  if(parameters$DD_similarity_measure=="ratio of matches"){
    results<-data.frame(lsh_compare(candidates, corpus, ratio_of_matches, progress = FALSE),stringsAsFactors = F)
  }
  
  results$a<-stringr::str_remove(string = results$a,pattern = "doc-")
  results$b<-stringr::str_remove(string = results$b,pattern = "doc-")
  log_to_file(message = paste0("  <b style='color:green'> ✔ </b>  ",dim(candidates)[1]," candidates were found"),file = logfile)
  
  if(dim(candidates)[1]>0){
    if(parameters$DD_interactive==FALSE){
      remove<-which(as.numeric(results[,3])>=parameters$DD_treshold)
      if(length(remove)==0){
        log_to_file(message = "<b>No document pair had a higher similarity than the specified threshold. As a result no new collection was created.</b>",file = logfile)
      }
      else{
        log_to_file(message = "<b>Step 8/8: Determine which documents to keep and which to remove from collection</b>",file = logfile)
        ####non interactive mode###
        log_to_file(message = "<b>Non-interactive mode chosen</b>",file = logfile)
        results<-results[remove,,drop=F]
        final_remove<-rep(0,dim(results)[1])
        
        
        ######
        data<-data.frame(a=results[,1],token_a=0,date_a=0,b=results[,2],token_b=0,date_b=0,similarity=results[,3],keep_a=0,keep_b=0,stringsAsFactors = F)
        data<-data[order(data$similarity,decreasing = T),]
        
        x<-as.data.frame(table(c(data[,"a"],data[,"b"])))
        node_degree<-x
        data<-merge(data,y = x,by.x = "a",by.y = "Var1")
        data<-merge(data,y = x,by.x = "b",by.y = "Var1")
        data<-data[,c("a","token_a","date_a","b","token_b","date_b","similarity","keep_a","keep_b","Freq.x","Freq.y")]
        final_remove<-matrix(c(0),dim(data)[1],2)
        for(i in 1:dim(data)[1]){
          if(parameters$DD_strategy=="longest"){
            try({
              remove<-which.max(c(meta[data[i,1],"token"],meta[data[i,4],"token"]))
            })
            if(length(remove)==0){
              remove<-sample(x = c(1,2),size = 1)
            }
          }  
          if(parameters$DD_strategy=="shortest"){
            try({
              remove<-which.min(c(meta[data[i,1],"token"],meta[data[i,4],"token"]))
            })
            if(length(remove)==0){
              remove<-sample(x = c(1,2),size = 1)
            }
          }
          if(parameters$DD_strategy=="latest"){
            try({
              remove<-which.max(c(meta[data[i,1],"date"],meta[data[i,4],"date"]))
            })
            if(length(remove)==0){
              remove<-sample(x = c(1,2),size = 1)
            }
          }
          if(parameters$DD_strategy=="earliest"){
            try({
              remove<-which.min(c(meta[data[i,1],"date"],meta[data[i,4],"date"]))
            })
            if(length(remove)==0){
              remove<-sample(x = c(1,2),size = 1)
            }
          }
          if(parameters$DD_strategy=="maximum node degree"){
            #browser()
            try({
              remove<-which.max(c(data[i,"Freq.x"],data[i,"Freq.y"]))
            })
            if(length(remove)==0){
              remove<-sample(x = c(1,2),size = 1)
            }
          }
          if(parameters$DD_strategy=="random"){
            remove<-sample(x = c(1,2),size = 1)
          }
          final_remove[i,remove]<-1
        }
        data[,8:9]<-final_remove
        data[,2]<-meta[data[,1],"token"]
        data[,3]<-meta[data[,1],"date"]
        data[,5]<-meta[data[,4],"token"]
        data[,6]<-meta[data[,4],"date"]
        
        
        ##sort data by according to chosen strategy
        rownames(data)<-1:dim(data)[1]
        if(parameters$DD_strategy=="latest"){
          sort<-apply(X = data,MARGIN = 1,FUN = function(x){max(x[c(3,6)])})
        }
        if(parameters$DD_strategy=="earliest"){
          sort<-apply(X = data,MARGIN = 1,FUN = function(x){min(x[c(3,6)])})
        }
        if(parameters$DD_strategy=="longest"){
          sort<-as.numeric(apply(X = data,MARGIN = 1,FUN = function(x){max(x[c(2,5)])}))
        }
        if(parameters$DD_strategy=="shortest"){
          sort<-as.numeric(apply(X = data,MARGIN = 1,FUN = function(x){min(x[c(2,5)])}))
        }
        if(parameters$DD_strategy=="maximum node degree"){
          sort<-as.numeric(apply(X = data,MARGIN = 1,FUN = function(x){max(x[c(10,11)])}))
        }
        if(parameters$DD_strategy=="random"){
          sort<-sample(x = 1:dim(data)[1],size = dim(data)[1],replace = T)
        }
        
        data<-data[order(sort,decreasing = T),]
        
        d_tmp<-data
        documents<-unique(union(d_tmp[,"a"],d_tmp[,"b"]))
        #set by user
        DD_whitelist<-NULL
        DD_blacklist<-NULL
        DD_whitelist<-unique(c(union(d_tmp[which(d_tmp[,"keep_a"]==9),"a"],d_tmp[which(d_tmp[,"keep_b"]==9),"b"]),(DD_whitelist)))
        DD_blacklist<-setdiff(unique(c(union(d_tmp[which(d_tmp[,"keep_a"]==8),"a"],d_tmp[which(d_tmp[,"keep_b"]==8),"b"]),(DD_blacklist))),DD_whitelist)
        
        blacklist<-setdiff(unique(c(DD_blacklist,union(d_tmp[which(d_tmp[,c("a")]%in%as.numeric((DD_whitelist))),"b"],d_tmp[which(d_tmp[,c("b")]%in%as.numeric((DD_whitelist))),"a"]))),DD_whitelist)
        whitelist<-unique(c(DD_whitelist,setdiff(union(d_tmp[which(d_tmp[,c("a")]%in%as.numeric(blacklist)),"b"],d_tmp[which(d_tmp[,c("b")]%in%as.numeric(blacklist)),"a"]),union(DD_whitelist,blacklist))[1]))
        whitelist<-whitelist[!is.na(whitelist)]
        
        repeat({
          repeat({
            lenB<-length(blacklist)
            blacklist<-unique(c(blacklist,setdiff(union(d_tmp[which(d_tmp[,c("a")]%in%as.numeric(whitelist)),"b"],d_tmp[which(d_tmp[,c("b")]%in%as.numeric(whitelist)),"a"]),union(blacklist,whitelist))))
            blacklist<-blacklist[!is.na(blacklist)]
            if(length(blacklist)==lenB ){break}
          })
          lenW<-length(whitelist)
          whitelist<-unique(c(whitelist,setdiff(union(d_tmp[which(d_tmp[,c("a")]%in%as.numeric(blacklist)),"b"],d_tmp[which(d_tmp[,c("b")]%in%as.numeric(blacklist)),"a"]),union(whitelist,blacklist))[1]))
          whitelist<-whitelist[!is.na(whitelist)]
          if(length(whitelist)==lenW ){break}
        })
        
        repeat({
          seen<-unique(union(c(which(d_tmp[,"a"]%in%whitelist),which(d_tmp[,"b"]%in%whitelist)),c(which(d_tmp[,"a"]%in%blacklist),which(d_tmp[,"b"]%in%blacklist))))
          if(length(seen)==0){
            d_rest<-d_tmp
          }
          else{
            d_rest<-d_tmp[-seen,]
          }
          if(dim(d_rest)[1]==0){break}
          if(d_rest[1,"keep_a"]==1){
            whitelist<-unique(c(whitelist,d_rest[1,"a"]))
          }
          else{
            whitelist<-unique(c(whitelist,d_rest[1,"b"]))
          }
          repeat({
            repeat({
              lenB<-length(blacklist)
              blacklist<-unique(c(blacklist,setdiff(union(d_tmp[which(d_tmp[,c("a")]%in%as.numeric(whitelist)),"b"],d_tmp[which(d_tmp[,c("b")]%in%as.numeric(whitelist)),"a"]),union(blacklist,whitelist))))
              blacklist<-blacklist[!is.na(blacklist)]
              if(length(blacklist)==lenB ){break}
            })
            lenW<-length(whitelist)
            whitelist<-unique(c(whitelist,setdiff(union(d_tmp[which(d_tmp[,c("a")]%in%as.numeric(blacklist)),"b"],d_tmp[which(d_tmp[,c("b")]%in%as.numeric(blacklist)),"a"]),union(whitelist,blacklist))[1]))
            whitelist<-whitelist[!is.na(whitelist)]
            if(length(whitelist)==lenW ){break}
          })
        })
        
        
        final_remove<-as.numeric(blacklist)
        
        
        #####
        log_to_file(message = "  <b style='color:green'> ✔ </b>  ",file = logfile)
        
        
        log_to_file(message = "<b>Step 8/8: Removing documents and saving new collection</b>",file = logfile)
        info[[1]]<-info[[1]][-as.numeric(final_remove),1,drop=F]
        info[[2]]<-info[[2]][-as.numeric(final_remove),1,drop=F]
        info[[3]]<-info[[3]][-as.numeric(final_remove),1,drop=F]
        info[[4]]<-paste0(info[[4]]," | ",dim(results)[1]," documents removed in document deduplication with taskID:",process_info[[1]], " | Documents removed:(",paste(final_remove,collapse=", ") ,")" )
        info[[5]]<-paste0(info[[5]], " deduplicated")
        if(file.exists(paste("collections/collections/",info[[5]],".RData",sep = ""))){
          info[[5]]<-paste0(info[[5]], " ",Sys.time())
        }
        info[[6]]<-info[[6]][-as.numeric(final_remove),1,drop=F]
        info[[7]]<-info[[7]][-as.numeric(final_remove),1,drop=F]
        
        host=update_solr_url
        port=update_solr_port
        
        
        log_to_file(message = "&emsp; updating solr collection-affiliation",file = logfile)
        body<-create_body_solr_update_add(ids = info[[3]][,1],field_name = "collections",values = rep(info[[5]],length(info[[3]][,1])))
        conn<-solrium::SolrClient$new(host = host ,port = port ,path="search")
        try(silent = T,{
          rm(solr_update_working)
          conn$update_atomic_json(name = "iLCM",body = body)->solr_update_working
        })
        if(!exists("solr_update_working")){
          conn$update_atomic_json(name = "iLCM",body = body)
        }
        solrium::commit(conn = conn,name="iLCM")
        
        
        
        
        save(info,file=paste("collections/collections/",info[[5]],".RData",sep = ""))
        save_collection_to_db(info)
        log_to_file(message = paste0("  <b style='color:green'> ✔ </b>",dim(results)[1]," Documents removed and collection saved"  ),file = logfile)
        

        log_to_file(message = paste0(" <b style='color:green'>Process finished successfully. You can use now use the collection ",info[[5]]," for other tasks. </b>"),logfile)
        
      }
    }
    else{
      ###interactive mode###
      log_to_file(message = "<b>Interactive mode chosen</b>",file = logfile)
      
      log_to_file(message = "<b>Step 8/8: Saving results for interactive adjustments</b>",file = logfile)
      dir.create(path0)
      save(info,results,meta,file=paste0(path0,"info_and_removal_candidates.RData"))
      parameters<-parameters_original
      save(parameters,file=paste0(path0,"parameters.RData"))
      log_to_file(message = "   <b style='color:green'> ✔ </b> Finished saving results",logfile)
      
      

      log_to_file(message = paste0(" <b style='color:green'>Process finished successfully. You can use now interactivly operate the document deduplication in the results tab. </b>"),logfile)
    }
  }
  else{
    log_to_file(message = "<b>No candidate for a possible dublicate could be detected!</b>",file = logfile)
    write_metadata_to_database(parameters)
  }
  system(paste("mv ",logfile," collections/logs/finished/",sep=""))
  
}) 

if(class(error)=="try-error"){
  system(paste("mv ",logfile," collections/logs/failed/",sep=""))
  RMariaDB::dbDisconnect(mydb)
  log_to_file(message=error[[1]],file = stringr::str_replace(string = logfile,pattern = "running",replacement = "failed"))
}