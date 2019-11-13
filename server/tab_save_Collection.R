observeEvent(input$save_Collection,{
  withBusyIndicatorServer("save_Collection", {
    #check if collection name is still avaiable
    if(values$numFound==0){
      shinyWidgets::sendSweetAlert(type = "warning",session = session,title = "No documents to save",text = "The last search did not return any document!")
    }
    else{
      if(stringr::str_detect(string = input$Collection_Name,pattern = "_")){
        shinyWidgets::sendSweetAlert(type = "warning",session = session,title = "'_' used in collection name",text = "Please don't use '_' in the collection name.")
      }
      else{
        mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
        coll_names_in_db<-RMariaDB::dbGetQuery(mydb, 'Select distinct name from Collections')
        if(isolate(input$Collection_Name)%in%coll_names_in_db[,1]){
          shinyWidgets::sendSweetAlert(type = "warning",session = session,title = "Collection Name is already used. Please use another one.")
        }
        else{
          try({
            if(values$custom==TRUE){
              url<-isolate(values$custom_inputtext)
              if(stringr::str_detect(string = url,pattern = "fl=")){
                url<-stringr::str_replace_all(string = url,pattern = "fl=[a-z_,]+",replacement = "fl=id_doc_i,dataset_s,date_dt,id")
              }
              else{
                url<-paste(url,"&fl=id_doc_i,dataset_s,date_dt,id,score")
              }
              x<-data.frame(data.table::rbindlist(solr_custom(url = paste0(url,"&rows=",values$numFound),start=0)$response[[3]],use.names = T))
            }
            else{
              if(input$Collection_limit_binary==T){
                #just save number of relevant doucments limited to by user
                if(input$Collection_limit_method=="most relevant documents"){
                  x<-solr::solr_search(base = values$solr_url,q = values$q,fl="id_doc_i , dataset_s , date_dt , id,score",fq=values$fq,rows=input$Collection_limit,start = 0)
                }
                #generate random sample of found documents
                if(input$Collection_limit_method=="random sample"){
                  x<-(solr::solr_search(base = values$solr_url,q = values$q,fl="id_doc_i , dataset_s ,date_dt,id,score",fq=values$fq,rows=input$Collection_limit,start = 0,sort = "random_12345 DESC"))
                }
              }
              else{
                x<-(solr::solr_search(base = values$solr_url,q = values$q,fl="id_doc_i , dataset_s ,date_dt,id,score",fq=values$fq,rows=isolate(values$numFound),start = 0))
              }
            }
            if(length(isolate(values$delete_documents))>0){
              x<-x[-which(x[,"id"]%in%isolate(values$delete_documents)),]
            }
            
            host<-values$update_solr_url
            port<-values$update_solr_port
            rm(solr_update_working)
            try({future::future(expr = {
              body<-create_body_solr_update_add(ids = x[,"id"],field_name = "collections",values = rep(input$Collection_Name,length(x[,"id"])))
              conn<-solrium::SolrClient$new(host = host,port = port,path="search")
             try({
                conn$update_atomic_json(name = "iLCM",body = body)->solr_update_working
              })
              if(!exists("solr_update_working")){
                conn$update_atomic_json(name = "iLCM",body = body)
              }
              solrium::commit(conn = conn,name="iLCM")
            }) %...>% future:::ClusterRegistry("stop")
            })
            x<-x[order(x[,"id_doc_i"]),]
            indices<-data.frame(x[,"id_doc_i"],stringsAsFactors = F)
            dataset<-data.frame(x[,"dataset_s"],stringsAsFactors = F)
            ids<-data.frame(x[,"id"],stringsAsFactors = F)
            score<-data.frame(x[,"score"],stringsAsFactors = F)
            url<-isolate(values$solr_query)
            if(length(isolate(values$delete_documents))>0){
              url<-paste0(url," \n documents with id: ",paste0(isolate(values$delete_documents),collapse=", ")," deleted by user:",values$user ,sep="")
            }
            name<-isolate(input$Collection_Name)
            dates<-data.frame(substr(x[,"date_dt"],1,10),stringsAsFactors = F)
            q<-values$q
            fq<-values$fq
            del<-values$delete_documents
            
            info<-list(indices,dataset,ids,url,name,dates,score,q,fq,del)
            if(length(indices)>=1){
              save(info,file=paste("collections/collections/",isolate(input$Collection_Name),".RData",sep = ""))
              shinyWidgets::sendSweetAlert(type = "success",session = session,title = "Collection saved")
              values$coll_saved<-runif(1)
              values$num_collections<-length(list.files("collections/collections/"))
            }
            else{
              shinyWidgets::sendSweetAlert(type = "warning",session = session,title = "Collection could not be saved")
            }
            save_collection_to_db(info)
          },silent = F)
        }
        RMariaDB::dbDisconnect(mydb)
      }
    }
  })
})


output$Collection_limit_UI<-renderUI({
  validate(
    need(values$numFound>0,message = "no document in result set")
  )
  numericInput(inputId = "Collection_limit",label = "limit",value = values$numFound,min = 1,max=values$numFound)
})