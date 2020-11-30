#' render export of results
#' depends on:
#'   input$export_results_analysis: export results of analysis
#'   
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

#' render export of result files
#' depends on:
#'   input$export_results_resultset: export of a result set
#'   input$export_results_analysis: export results of analysis
#'   input$export_results_resultset_class: export class of result set
#'   values$export_results_files: export result files
#'   values$export_results_table: result table for export
#'   
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



#' check if export button for a result is clicked
#' depends on:
#'   input$export_results_buttons: clicked export button?
#'   values$export_results_download_selected: export result for selected download
#'   
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

#' observe download process
#' depends on:
#'   values$export_results_table: result table for export
#'   values$export_results_download_selected: selected results for download
output$export_results_final_dl<-downloadHandler(
  filename = function(){
    as.character(values$export_results_table[values$export_results_download_selected,"filename"])
  },
  content = function(file){
    file.copy(values$export_results_files[values$export_results_download_selected], file)
  })

#' render extra options for export
#' depends on:
#'   input$export_results_resultset: result set for exported results
#'   input$export_results_analysis: result of certain analysis
#'   
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

#' export special topic theta results
#' depends on:
#'   input$export_results_resultset: result set for export
#'   
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

#' export special topic phi for result-export
#' depends on:
#'   input$export_results_resultset: result set for export
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



#' get annotations from database and render them in a datatable
#' depends on:
#'   input$export_update_annotations: update annotations for export
#'   values$host: host for export
#'   values$db_port: databse port for export
#'   
output$export_annotations<-DT::renderDataTable({
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



#' export all
#' depends on:
#'   input$file: selected file
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

#' render prints
#' depends on:
#'   input$file: selected file
output$filepaths <- renderPrint({
  validate(need(dim(parseFilePaths(volumes, input$file))[1]>0,message=F))
  parseFilePaths(volumes, input$file)})


#' check if download button
#' depends on:
#'   input$export_collection: selected collection for export
output$download_button_coll_ilcm <- downloadHandler(
  filename = function(){
    paste0(input$export_collection,".RData")
  },
  content = function(file){
    file.copy(paste0("collections/collections/",input$export_collection,".RData"), file)
  }
)




#' render parameters for downloading a collection
#' depends on:
#'   input$export_download_batch_size:specify batch size for export 
#'   input$export_coll_format: select collection format for export
output$Export_Analysis_Parameter_DL<-renderUI({
  validate(
    need(input$export_download_batch_size>0,"Please specify a batch size greater than 0"),
    need(input$export_coll_format%in%c("DataFrame"),message=F)
  )
  load(paste0("collections/collections/",input$export_collection,".RData"))
  ids<-as.numeric(as.character(info[[3]][,1]))
  number_of_buttons<-ceiling(length(ids)/input$export_download_batch_size)
  values$export_number_of_buttons<-number_of_buttons
  chosen_collection<-input$export_collection
  # replace punctuation in collection name if present
  chosen_collection<-gsub(pattern = "[:]",replacement = "",x = chosen_collection)
  values$export_chosen_collection<-chosen_collection
  dl_button_ids<-paste(chosen_collection,"export_NR",1:number_of_buttons,sep="")
  actionbutton_list_csv<-lapply(X =1:number_of_buttons,FUN = function(x){
    return(tagList(downloadButton(outputId = dl_button_ids[x],label = paste0("Token Part ",x),icon = icon("download"),class = "dl1"),
                   downloadButton(outputId = paste0(dl_button_ids[x],"meta"),label = paste0("Meta Part ",x),icon = icon("download"),class = "dl2"),
                   tags$br()
    ))
  })
  actionbutton_list_RData<-tagList(
    downloadButton(outputId = "download_export_RData_token",label = "Token",icon = icon("download"),class = "dl1"),
    downloadButton(outputId = "download_export_RData_meta",label = "Meta",icon = icon("download"),class = "dl2"),
    tags$br()
  )
  tagList(
    tags$div(paste(length(ids)," documents were found. They were split into ",number_of_buttons,"parts.")), 
    busyIndicator(text = "Retrieving documents from database",wait = 0),
    shinyBS::bsButton(inputId = "Export_Prepare_Documents",label = "Prepare Documents for Download (might take some minutes)",icon = icon("database"),style = "primary"),
    tags$br(),
    tags$br(),
    hidden(tags$div(id="export_csv",
                    tags$h4("CSV"),
                    do.call(tagList,actionbutton_list_csv),
                    tags$h4("RData"),
                    do.call(tagList,actionbutton_list_RData)
    )
    )
  )
})





#' get documents from database
#' depends on:
#'   input$Export_Prepare_Documents: prepare documents for export
#'   values$host: host for export
#'   values$db_port: database port
#'   values$export_token_tmp: temporary token for export
#'   values$export_meta_tmp: temporary meta data for export
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
  values$export_token_tmp<-token
  not_all_na<-which(!sapply(meta, function(x)all(is.na(x))))
  values$export_meta_tmp<-meta[,not_all_na]
  shinyWidgets::sendSweetAlert(session=session,title = "Data ready for Download",text=paste0(dim(meta)[1]," Documents could be retrieved from database"),type = "success")
  shinyjs::show(id = "export_csv")
})



#' create download functionality for downloading token objects
#' depends on:
#'   values$export_number_of_buttons: number of buttons for export
#'   values$export_chosen_collection: choosen collection for export
#'   values$export_token_tmp: temporary token for export
#'   values$export_meta_tmp: temporary meta data for export
#'   
observe({
  validate(
    need(!is.null(values$export_number_of_buttons),message=FALSE)
  )
  lapply(1:values$export_number_of_buttons,FUN = function(i){
    output[[paste(values$export_chosen_collection,"export_NR",i,sep="")]]<-downloadHandler(
      filename = function(con){
        paste("token_",values$export_chosen_collection,"_",i,".csv",sep="")
      },
      content = function(con) {
        if(dim(values$export_token_tmp)[1]==0){
          shinyWidgets::sendSweetAlert(session=session,title = "no documents found.",text = "Have you clicked 'Prepare Documents'?",type = "warning")
        }
        else{
          export_data<-values$export_token_tmp[which(values$export_token_tmp$id%in%((floor((dim(values$export_meta_tmp)[1]/values$export_number_of_buttons)*(i-1))+1):floor((dim(values$export_meta_tmp)[1]/values$export_number_of_buttons)*(i)))),]
          export_data<-apply(X = export_data,MARGIN = 2,FUN = function(x){stringr::str_replace_all(string = x,pattern = '"',replacement = "'")})
          write.table(export_data, con,col.names = F,row.names = F,sep=",",quote = T)   
        }
      }
    )
  })
})


#' create download functionality for downloading meta objects
#' depends on:
#'   values$export_number_of_buttons: export number of buttons
#'   values$export_chosen_collection: choosen collection for export
#'   values$export_meta_tmp: temporary meta data for export
#'   
observe({
  validate(
    need(!is.null(values$export_number_of_buttons),message=FALSE)
  )
  lapply(1:values$export_number_of_buttons,FUN = function(i){
    output[[paste(values$export_chosen_collection,"export_NR",i,"meta",sep="")]]<-downloadHandler(
      filename = function(con){
        paste("meta_",values$export_chosen_collection,"_",i,".csv",sep="")
      },
      content = function(con) {
        if(dim(values$export_meta_tmp)[1]==0){
          shinyWidgets::sendSweetAlert(session=session,title = "no documents found.",text = "Have you clicked 'Prepare Documents'?",type = "warning")
        }
        else{
          export_data<-values$export_meta_tmp[((floor((dim(values$export_meta_tmp)[1]/values$export_number_of_buttons)*(i-1))+1):floor((dim(values$export_meta_tmp)[1]/values$export_number_of_buttons)*(i))),]
          export_data<-apply(X = export_data,MARGIN = 2,FUN = function(x){stringr::str_replace_all(string = x,pattern = '"',replacement = "'")})
          write.table(export_data, con,col.names = T,row.names = F,sep=",",quote = T) 
        }
      }
    )
  })
})

#' download functionality for downloading collection meta objects as RData
#' depends on:
#'   input$export_collection: export collection
#'   -values$export_meta_tmp: temporary meta data for export
output$download_export_RData_meta <- downloadHandler(
  filename = function(){
    paste0(input$export_collection,"_meta.RData")
  },
  content = function(file){
    token<-values$export_meta_tmp
    save(token,file=file)
  }
)

#' download functionality for downloading collection token objects as RData
#' depends on:
#'   input$export_collection: export collection
#'   values$export_token_tmp: temporary token for export
output$download_export_RData_token <- downloadHandler(
  filename = function(){
    paste0(input$export_collection,"_token.RData")
  },
  content = function(file){
    meta<-values$export_token_tmp
    save(meta,file=file)
  }
)



#' update select input for collections (input$export_collection), when a new collection is created
#' depends on:
#'   values$coll_saved: saved collection
observe({
  values$coll_saved
  updateSelectInput(session = session,inputId = "export_collection", choices = stringr::str_remove(string = list.files("collections/collections/"),pattern = ".RData"))
})








#' start export of all data
#' depends on:
#'   input$file: file input
#'   
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
