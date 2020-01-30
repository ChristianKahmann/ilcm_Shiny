


#render parameters for downloading a collection
output$Analysis_Parameter_DL<-renderUI({
  #browser()
  load(paste0("collections/collections/",input$collection_selected,".RData"))
  ids<-as.numeric(as.character(info[[3]]$x....id..))
  number_of_buttons<-ceiling(length(ids)/input$download_batch_size)
  values$number_of_buttons<-number_of_buttons
  dl_button_ids<-paste(input$collection_selected,"NR",1:number_of_buttons,sep="")
  actionbutton_list<-lapply(X =1:number_of_buttons,FUN = function(x){
    return(tagList(downloadButton(outputId = dl_button_ids[x],label = paste0("Token Part ",x),icon = icon("download"),class = "dl1"),
                   downloadButton(outputId = paste0(dl_button_ids[x],"meta"),label = paste0("Meta Part ",x),icon = icon("download"),class = "dl2"),
                   tags$br()
    ))
  })
  tagList(
    tags$div(paste(length(ids)," documents were found. They were split into ",number_of_buttons,"parts.")), 
    busyIndicator(text = "Retrieving documents from database",wait = 0),
    shinyBS::bsButton(inputId = "Prepare_Documents",label = "Prepare Documents for Download (might take some minutes)",icon = icon("database"),style = "primary"),
    tags$br(),
    tags$br(),
    do.call(tagList,actionbutton_list)
  )
})


#get documents from database
observeEvent(input$Prepare_Documents,{
  load(paste0("collections/collections/",input$collection_selected,".RData"))
  #token object
  token<-NULL
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
  rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
  d<-data.frame(id=info[[1]],dataset=info[[2]])
  for(i in 1:length(unique(d[,2]))){
    ids<-paste(d[which(d[,2]==unique(d[,2])[i]),1],collapse = " ")
    ids<-stringr::str_replace_all(string = as.character(ids),pattern = " ",",")
    token<-rbind(token,RMariaDB::dbGetQuery(mydb, paste("select * from token where dataset='",unique(d[,2])[i],"' and id in (",ids,");",sep="")))
  }
  #meta object
  meta<-NULL
  d<-data.frame(id=info[[3]])
  ids<-paste(d[,1],collapse = ", ")
  meta<-rbind(meta,RMariaDB::dbGetQuery(mydb, paste("select * from documents where id in (",ids,");",sep="")))
  RMariaDB::dbDisconnect(mydb)
  values$token_tmp<-token
  values$meta_tmp<-meta[,2:13]
  shinyWidgets::sendSweetAlert(session=session,title = "Data ready for Download",text=paste0(dim(meta)[1]," Documents could be retrieved from database"),type = "success")
})





#create download functionality for downloading token objects
observe({
  validate(
    need(!is.null(values$number_of_buttons),message=FALSE)
  )
  lapply(1:values$number_of_buttons,FUN = function(i){
    output[[paste(input$collection_selected,"NR",i,sep="")]]<-downloadHandler(
      filename = function(con){
        paste("token_",input$collection_selected,"_",i,".csv",sep="")
      },
      content = function(con) {
        if(dim(values$token_tmp)[1]==0){
          shinyWidgets::sendSweetAlert(session=session,title = "no documents found.",text = "Have you clicked 'Prepare Documents'?",type = "warning")
        }
        else{
          write.table(values$token_tmp[((floor((dim(values$token_tmp)[1]/values$number_of_buttons)*(i-1))+1):floor((dim(values$token_tmp)[1]/values$number_of_buttons)*(i))),], con,col.names = F,row.names = F,sep=",")
        }
      }
    )
  })
})


#create download functionality for downloading meta objects
observe({
  validate(
    need(!is.null(values$number_of_buttons),message=FALSE)
  )
  lapply(1:values$number_of_buttons,FUN = function(i){
    output[[paste(input$collection_selected,"NR",i,"meta",sep="")]]<-downloadHandler(
      filename = function(con){
        paste("meta_",input$collection_selected,"_",i,".csv",sep="")
      },
      content = function(con) {
        if(dim(values$meta_tmp)[1]==0){
          shinyWidgets::sendSweetAlert(session=session,title = "no documents found.",text = "Have you clicked 'Prepare Documents'?",type = "warning")
        }
        else{
          write.table(values$meta_tmp[((floor((dim(values$meta_tmp)[1]/values$number_of_buttons)*(i-1))+1):floor((dim(values$meta_tmp)[1]/values$number_of_buttons)*(i))),], con,col.names = F,row.names = F,sep=",")
        }
      }
    )
  })
})


observeEvent(input$start_token_saving,{
  load(paste0("collections/collections/",input$collection_selected,".RData"))
  token<-NULL
  #getting data from db
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
  rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
  d<-data.frame(id=info[[1]],dataset=info[[2]])
  for(i in 1:length(unique(d[,2]))){
    ids<-paste(d[which(d[,2]==unique(d[,2])[i]),1],collapse = " ")
    ids<-stringr::str_replace_all(string = as.character(ids),pattern = " ",",")
    token<-rbind(token,RMariaDB::dbGetQuery(mydb, paste("select * from token where dataset='",unique(d[,2])[i],"' and id in (",ids,");",sep="")))
  }
  RMariaDB::dbDisconnect(mydb)
  save(token,file=paste0("collections/results/extracted_collections/token_",input$collection_selected,".RData"))
  shinyWidgets::sendSweetAlert(session=session,title = "Token object saved to results",text = "accessible in RStudio-Server",type = "success")
})


observeEvent(input$start_meta_saving,{
  load(paste0("collections/collections/",input$collection_selected,".RData"))
  #getting data from db
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
  rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
  ids<-info[[3]]
  ids<- paste(ids[,1],collapse=", ")
  meta<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from documents where id in (",ids,");"))
  RMariaDB::dbDisconnect(mydb)
  save(meta,file=paste0("collections/results/extracted_collections/meta_",input$collection_selected,".RData"))
  shinyWidgets::sendSweetAlert(session=session,title = "Meta object saved to results",text = "accessible in RStudio-Server",type = "success")
})
