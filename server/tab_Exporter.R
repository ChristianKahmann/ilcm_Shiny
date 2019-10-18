output$export_results_resultset_ui<-renderUI({
  return(tagList(
    conditionalPanel(condition = "input.export_results_analysis=='classification'",
                     selectInput(inputId = "export_results_resultset_class",label = "Result sets",choices = setNames(nm = list.files("collections/results/classification/classifyCollection/"),object =list.files("collections/results/classification/classifyCollection/",full.names = T) ),width="25%"
                     )
    ),
    conditionalPanel(condition = "input.export_results_analysis!='classification'",        
                     selectInput(inputId = "export_results_resultset",label="Result sets",choices = setNames(nm=list.files(paste0("collections/results/",input$export_results_analysis)),object = list.files(paste0("collections/results/",input$export_results_analysis),full.names = T)),width="25%"
                     )
    )
  )
  )
})


output$export_results_files_ui<-DT::renderDataTable({
  validate(
    need(!is.null(input$export_results_resultset),message=F)
  )
  if(input$export_results_analysis=='classification'){
    path<-input$export_results_resultset_class
  }
  else{
    path<-input$export_results_resultset
  }
  
  files<-list.files(path = path,full.names = T)
  values$export_results_files<-files
  info<-cbind(list.files(path = path),file.info(files)[,c("size","mtime","uname")])
  info[,"size"]<-info[,"size"]/1000000
  colnames(info)<-c("filename","size in MB","creation time","created by:")
  values$export_results_table<-info
  Download = shinyInput(
    shinyBS::bsButton,
    dim(info)[1],
    'export_results_files_dl_',
    label = "",
    style="info",
    icon=icon("download"),
    onclick = 'Shiny.onInputChange(\"export_results_buttons\",  this.id)'
  )
  return(datatable(data = cbind(info,Download),rownames=F,options=list(dom="tp"),escape = F,selection = "none"))
},server=F)




observeEvent(ignoreInit = T,input$export_results_buttons,{
  validate(
    need(input$export_results_buttons!="export_results_files_dl_0",message=F)
  )
  values$export_results_download_selected<-as.numeric(stringr::str_split(string =input$export_results_buttons,pattern = "_",simplify = T )[1,5])
  isolate(shinyjs::runjs('Shiny.onInputChange(\"export_results_buttons\",  "export_results_files_dl_0")'))
  showModal(
    modalDialog(title = "Download",easyClose = T,
                downloadButton(outputId = "export_results_final_dl",label = "Download")
    )
  )
})

output$export_results_final_dl<-downloadHandler(
  filename = function(){
    as.character(values$export_results_table[values$export_results_download_selected,"filename"])
  },
  content = function(file){
    file.copy(values$export_results_files[values$export_results_download_selected], file)
  })


output$export_results_extra<-renderUI({
  validate(
    need(!is.null(input$export_results_resultset),message=F)
  )
  L<-NULL
  if(input$export_results_analysis=="topic-model"){
     L<-tagList(
      downloadButton(outputId = "export_results_special_topic_theta",label = "theta"),
      downloadButton(outputId = "export_results_special_topic_phi",label = "phi")
    )
  }
  return(L)
})

output$export_results_special_topic_theta<-downloadHandler(
  filename = function(){
      paste0(input$export_results_resultset,"_theta",".csv")
  },
  content = function(file){
    load(paste0(input$export_results_resultset,"/data_TM.RData"))
    data<-as.matrix(theta)
    write.csv(data, file)
  }
)

output$export_results_special_topic_phi<-downloadHandler(
  filename = function(){
    paste0(input$export_results_resultset,"_phi",".csv")
  },
  content = function(file){
    load(paste0(input$export_results_resultset,"/data_TM.RData"))
    data<-as.matrix(phi)
    write.csv(data, file)
  }
)



#get annotations from database and render them in a datatable
output$export_annotations<-renderDataTable({
  input$export_update_annotations
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
  rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
  data<-RMariaDB::dbGetQuery(mydb, paste("select * from Annotations",sep=""))
  RMariaDB::dbDisconnect(mydb)
  validate(
    need(length(data)>0,"no annotations found!")
  )
  #filter annotations by project
  #data<-data[which(data[,10]==paste0((input$project_selected))),]
  validate(
    need(dim(data)[1]>0,"no annotations for this project found!")
  )
  #filetr annotations by category
  # if(length(input$annotation_filter)>0){
  #   data<-data[which(data[,7]%in%input$annotation_filter),]
  # }
  #values$annotations_all<-data
  for(i in 1:dim(data)[1]){
    data[i,8]<-paste0('<b style="background-color:',data[i,8],';">',data[i,8],'</b>')
  }
  colnames(data) = str_wrap(colnames(data),width = 12)
  #browser()
  data$User<-as.factor(data$User)
  data$dataset<-as.factor(data$dataset)
  data$Anno_set<-as.factor(data$Anno_set)
  data$Annotation<-as.factor(data$Annotation)
  dt<-datatable(data.frame(data),rownames = F,escape = F,selection = "none",class = "row-border compact",extensions = c('Buttons','Responsive'),filter = 'top',
                options=list(
                  dom='Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'print')
                )
                )
},server = F)




output$download_export_all_ui<-renderUI({
  validate(need(dim(parseFilePaths(volumes, input$file))[1]>0,message=F))
  return(downloadButton("download_export_all","Download"))
})




volumes <- c('Results'="collections/results",
             'Collections'="collections/collections",
             'Scripts'="collections/scripts",
             'Rest'="."
)
shinyFileChoose(input, 'file', roots=volumes, session=session)
output$filepaths <- renderPrint({
  validate(need(dim(parseFilePaths(volumes, input$file))[1]>0,message=F))
  parseFilePaths(volumes, input$file)})



output$download_export_coll_RData <- downloadHandler(
  filename = function(){
    paste0(input$export_collection,".RData")
  },
  content = function(file){
    file.copy(paste0("collections/collections/",input$export_collection,".RData"), file)
  }
)



#render parameters for downloading a collection
output$Export_Analysis_Parameter_DL<-renderUI({
  #browser()
  load(paste0("collections/collections/",input$export_collection,".RData"))
  ids<-as.numeric(as.character(info[[3]]$x....id..))
  number_of_buttons<-ceiling(length(ids)/input$download_batch_size)
  values$export_number_of_buttons<-number_of_buttons
  dl_button_ids<-paste(input$export_collection,"export_NR",1:number_of_buttons,sep="")
  actionbutton_list<-lapply(X =1:number_of_buttons,FUN = function(x){
    return(tagList(downloadButton(outputId = dl_button_ids[x],label = paste0("Token Part ",x),icon = icon("download"),class = "dl1"),
                   downloadButton(outputId = paste0(dl_button_ids[x],"meta"),label = paste0("Meta Part ",x),icon = icon("download"),class = "dl2"),
                   tags$br()
    ))
  })
  tagList(
    tags$div(paste(length(ids)," documents were found. They were split into ",number_of_buttons,"parts.")), 
    busyIndicator(text = "Retrieving documents from database",wait = 0),
    shinyBS::bsButton(inputId = "Export_Prepare_Documents",label = "Prepare Documents for Download (might take some minutes)",icon = icon("database"),style = "primary"),
    tags$br(),
    tags$br(),
    hidden(tags$div(id="export_csv",
                    do.call(tagList,actionbutton_list)
    )
    )
  )
})



#get documents from database
observeEvent(input$Export_Prepare_Documents,{
  load(paste0("collections/collections/",input$export_collection,".RData"))
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
  shinyjs::show(id = "export_csv")
})



#create download functionality for downloading token objects
observe({
  validate(
    need(!is.null(values$export_number_of_buttons),message=FALSE)
  )
  lapply(1:values$export_number_of_buttons,FUN = function(i){
    output[[paste(input$export_collection,"export_NR",i,sep="")]]<-downloadHandler(
      filename = function(con){
        paste("token_",input$export_collection,"_",i,".csv",sep="")
      },
      content = function(con) {
        if(dim(values$token_tmp)[1]==0){
          shinyWidgets::sendSweetAlert(session=session,title = "no documents found.",text = "Have you clicked 'Prepare Documents'?",type = "warning")
        }
        else{
          write.table(values$token_tmp[((floor((dim(values$token_tmp)[1]/values$export_number_of_buttons)*(i-1))+1):floor((dim(values$token_tmp)[1]/values$export_number_of_buttons)*(i))),], con,col.names = F,row.names = F,sep=",")
        }
      }
    )
  })
})


#create download functionality for downloading meta objects
observe({
  validate(
    need(!is.null(values$export_number_of_buttons),message=FALSE)
  )
  lapply(1:values$export_number_of_buttons,FUN = function(i){
    output[[paste(input$export_collection,"export_NR",i,"meta",sep="")]]<-downloadHandler(
      filename = function(con){
        paste("meta_",input$export_collection,"_",i,".csv",sep="")
      },
      content = function(con) {
        if(dim(values$meta_tmp)[1]==0){
          shinyWidgets::sendSweetAlert(session=session,title = "no documents found.",text = "Have you clicked 'Prepare Documents'?",type = "warning")
        }
        else{
          write.table(values$meta_tmp[((floor((dim(values$meta_tmp)[1]/values$export_number_of_buttons)*(i-1))+1):floor((dim(values$meta_tmp)[1]/values$export_number_of_buttons)*(i))),], con,col.names = F,row.names = F,sep=",")
        }
      }
    )
  })
})















output$download_export_all <- downloadHandler(
  filename = function(){
    if( dim(parseFilePaths(volumes, input$file))[1]>1){
      return(paste0("ilcm_download",".zip"))
    }
    else{
      return(as.character(parseFilePaths(volumes, input$file)$name))
    }
  },
  content = function(file){
    if( dim(parseFilePaths(volumes, input$file))[1]>1){
      #create the zip file
      zip(file,as.character(parseFilePaths(volumes, input$file)$datapath))
    }
    else{
      file.copy(as.character(parseFilePaths(volumes, input$file)$datapath), file)
    }
  }
)

