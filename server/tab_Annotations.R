#' get annotations from database and render them in a datatable
#' depends on:
#'   input$update_annotations: update annotations
#'   values$host: selected host
#'   values$db_port: selected data base port
#'   input$project_selected: selected project
#'   input$annotation_filter: annotation filter by category
#'   values$annotations_all: all annotations
output$annotations<-renderDataTable({
  input$update_annotations
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
  rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
  data<-RMariaDB::dbGetQuery(mydb, paste("select * from Annotations",sep=""))
  RMariaDB::dbDisconnect(mydb)
  validate(
    need(length(data)>0,"no annotations found!")
  )
  #filter annotations by project
  data<-data[which(data[,10]==paste0((input$project_selected))),]
  validate(
    need(dim(data)[1]>0,"no annotations for this project found!")
  )
  #filetr annotations by category
  if(length(input$annotation_filter)>0){
    data<-data[which(data[,7]%in%input$annotation_filter),]
  }
  
  for(i in 1:dim(data)[1]){
    data[i,8]<-paste0('<b style="background-color:',data[i,8],';">',data[i,8],'</b>')
  }
  colnames(data) = str_wrap(colnames(data),width = 8)
  
  data<-data.frame(data, Delete = shinyInput(
    shinyBS::bsButton,
    dim(data)[1],
    'delete_button_annotation_',
    label = "Delete",
    onclick = 'Shiny.onInputChange(\"delete_annotation\",  this.id)',
    style="danger",
    icon=icon("delete")
  ))
  #open details window buttons 
  Open = shinyInput(
    shinyBS::bsButton,
    dim(data)[1],
    'open_document_view_button_annotations_results_',
    label = "",
    size="extra-small",
    style="info",
    icon=icon("search"),
    onclick = 'Shiny.onInputChange(\"open_document_view_button_annotations_results\",  this.id)'
  )
  data<-cbind(Open,data)
  
  colnames(data)<-c("Open","Anno ID","User","Dataset","ID","From","To","Category","Color","Date","Project","Collection","Global Doc ID","Text","Document Annotation","Delete")
  values$annotations_all<-data
  dt<-datatable(data=data,rownames = F,escape = F,selection = "none",class = "row-border compact")
  
})

#' download button for currently displayed annotations
#' depends on:
#'   values$annotations_all: all annotations
output$download_token<-downloadHandler(
  filename = function() {
    paste('data-', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    write.csv(values$annotations_all, con)
  }
)  


#' if delete annotation is clicked delete db entry
#' depends on:
#'   input$delete_annotation: delete annotation
#'   values$host: selected host
#'   values$db_port: selected data base port
#'   values$annotations_all: all annotations
observeEvent(input$delete_annotation, {
  selectedRow <-
    as.numeric(strsplit(input$delete_annotation, "_")[[1]][4])
  if(selectedRow>0){
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
    shinyjs::useShinyjs()
    isolate(shinyjs::runjs('Shiny.onInputChange(\"delete_annotation\",  "delete_button_annotation_0")'))
    write_to_MariaDB(mydb = mydb,query =paste0('Delete from Annotations where anno_id="',isolate(values$annotations_all[selectedRow,2]),'";') )
    RMariaDB::dbDisconnect(mydb)
    shinyjs::useShinyjs()
    shinyjs::click(id = "update_annotations")
  }
})




#' check wheather a document is selected in annotations datatable // if yes get data from db 
#' depends on:
#'   input$annotations_rows_selected: selected rows from annotations
#'   values$host: selected host
#'   values$db_port: selected database port
#'   values$Anno_selected: selected annotation
#'   values$annotations_all: all annotations
#'   values$Anno_id_global: global id of annotation
#'   values$Anno_dataset: annotation dataset
#'   values$Anno_token: annotation token
#'   values$Anno_meta: meta data of annotation
#'   values$Anno_scheme_changed: changed annotation scheme
#'   values$set_Anno_anno_scheme: set of annotation schemes
#'   input$project_selected: selected project
#'   values$Anno_new: new annotation
#'   values$Anno_annotations_show: show annotations
#'   values$Anno_Doc_reload: reload documents of annotation
#'   values$Anno_annos: annotations
observeEvent(input$open_document_view_button_annotations_results,{
  s = as.numeric(strsplit(input$open_document_view_button_annotations_results, "_")[[1]][7])
  if (length(s)) {
    if(s>0){
      isolate(shinyjs::runjs('Shiny.onInputChange(\"open_document_view_button_annotations_results\",  "open_document_view_button_annotations_results_0")'))
      mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
      values$Anno_selected<-as.integer(values$annotations_all[s[length(s)],"ID"])
      values$Anno_id_global<-as.integer(values$annotations_all[s[length(s)],"Global Doc ID"])
      
      values$Anno_dataset<-(values$annotations_all[s[length(s)],"Dataset"])
      rs <- dbSendQuery(mydb, 'set character set "utf8"')
      values$Anno_token<-RMariaDB::dbGetQuery(mydb, paste("select * from token where dataset='",isolate(values$annotations_all[s[length(s)],"Dataset"]),"' and id=",isolate(values$Anno_selected),";",sep=""))
      ind<-RMariaDB::dbGetQuery(mydb, paste("select id_doc,dataset,title,date,language,token,id,mde1,mde2,mde3,mde4,mde5,mde6,mde7,mde8,mde9 from documents where id=",isolate(values$Anno_id_global)," limit 1;",sep=""))
      
      values$Anno_Collection_chosen<-values$annotations_all[s[length(s)],"Collection"]
      
      #reduce to need metadata
      ava<-dbGetQuery(mydb, paste0("SELECT * FROM ilcm.metadata_names where dataset in('",paste(unique(ind[,"dataset"]),collapse="','"),"');"))
      RMariaDB::dbDisconnect(mydb)
      
      empty_metadata<-names(which(apply(ava,MARGIN = 2,function(x){all(is.na(x))})))
      if(length(empty_metadata)>0){
        ind<-ind[,-which(colnames(ind)%in%empty_metadata)]
      }
      ind_new<-data.frame()
      #split metadata for differing datasets/metadata names
      for(d in ind$dataset){
        ind_tmp<-ind[which(ind[,"dataset"]==d),] 
        ind_tmp<-ind_tmp[,which(apply(ind_tmp,2,function(x){!all(is.na(x))}))]
        if(dim(ind_tmp)[1]>0){
          colnames(ind_tmp)<-c("id_doc","dataset","title","date","language","token","id",(ava)[which(ava[,1]==d),2:length(colnames(ava))][!is.na(ava[which(ava[,1]==d),-1])])
          ind_new<-plyr::rbind.fill(ind_new,ind_tmp)
        }
      }
      
      meta<-colnames(ind_new)[which(!colnames(ind_new)%in%c("id_doc","dataset","title","date","language","token","id"))]
      if(length(meta)>0){
        ind_new<-ind_new[,c("id_doc","id","dataset","title","date","token","language",meta)]
      }
      ind_new$date<-substr(ind_new$date,1,10)
      values$Anno_meta<-ind_new
      
      
      
      RMariaDB::dbDisconnect(mydb)
      #browser()
      
      #updateSelectizeInput(session = session,inputId = "Anno_anno_scheme_selected",selected = input$project_selected)
      values$Anno_scheme_changed<-FALSE
      values$set_Anno_anno_scheme<-input$project_selected
      updateTabsetPanel(session = session,inputId = "category",selected = "Document View3")
      shinyjs::useShinyjs()
      shinyjs::runjs(" Shiny.onInputChange('Anno_anno_tag',null);
                     Shiny.onInputChange('Anno_anno_start',null);
                     Shiny.onInputChange('Anno_anno_end',null);")
      values$Anno_new<-NULL
      values$Anno_annotations_show<-matrix(c(0),0,13)
      values$Anno_Doc_reload<-runif(1,0,1)
      values$Anno_annos<-NULL
    }
  }
})





#########################################
# Active Learning Annotations           #
#########################################


output$annotations_active_learning <- DT::renderDataTable({
  input$update_annotations_active_learning
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
  rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
  data<-RMariaDB::dbGetQuery(mydb, paste("select * from annotations_classification",sep=""))
  RMariaDB::dbDisconnect(mydb)
  validate(
    need(length(data)>0,"no annotations found!")
  )
  #filter annotations by project
  data<-data[which(data[,"project"]==paste0((input$project_selected))),]
  validate(
    need(dim(data)[1]>0,"no annotations for this project found!")
  )
  #filer annotations by category
  if(length(input$annotation_filter)>0){
    data<-data[which(data[,"category"]%in%input$annotation_filter),]
  }
  
  validate(
    need(dim(data)[1]>0,"no annotations for this category found!")
  )
  # load annmotation scheme to get colour informations
  load(paste0("collections/annotation_schemes/",data$project[1],".RData"))
  names_overall<-names(unlist(anno))
  anno<-unlist(anno)
  name_tag<-names_overall[which(grepl(pattern = paste0(NULL,".name"),x = names_overall))]
  names<-anno[name_tag]
  color_tag<-names_overall[which(grepl(pattern = paste0(NULL,".color"),x = names_overall))]
  colors<-anno[color_tag]
  names(colors)<-names
  colors <- colors[match(data$category,names(colors))]
  for(i in 1:dim(data)[1]){
    data[i,"category"]<-paste0('<b style="background-color:',colors[i],';">',data[i,"category"],'</b>')
  }
  colnames(data) = str_wrap(colnames(data),width = 8)
  
  data<-data.frame(data, Delete = shinyInput(
    shinyBS::bsButton,
    dim(data)[1],
    'delete_button_annotation_active_learning_',
    label = "Delete",
    onclick = 'Shiny.onInputChange(\"delete_annotation_active_learning\",  this.id)',
    style="danger",
    icon=icon("delete")
  ))
  #open details window buttons 
  # Open = shinyInput(
  #   shinyBS::bsButton,
  #   dim(data)[1],
  #   'open_document_view_button_annotations_results_',
  #   label = "",
  #   size="extra-small",
  #   style="info",
  #   icon=icon("search"),
  #   onclick = 'Shiny.onInputChange(\"open_document_view_button_annotations_results\",  this.id)'
  # )
  #data<-cbind(Open,data)
  
  colnames(data)<-c("Dataset","Document ID","Sentence ID","Project","Category","Status","Timestamp","Document Annotation","Delete")
  values$annotations_all_active_learning<-data
  dt<-datatable(data=data,rownames = F,escape = F,selection = "none",class = "row-border compact")
  
})


#' if delete annotation is clicked delete db entry
#' depends on:
#'   input$delete_annotation: delete annotation
#'   values$host: selected host
#'   values$db_port: selected data base port
#'   values$annotations_all: all annotations
observeEvent(input$delete_annotation_active_learning, {
  selectedRow <-
    as.numeric(strsplit(input$delete_annotation_active_learning, "_")[[1]][6])
  if(selectedRow>0){
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
    shinyjs::useShinyjs()
    isolate(shinyjs::runjs('Shiny.onInputChange(\"delete_annotation_active_learning\",  "delete_button_annotation_active_learning_0")'))
    #browser()
    write_to_MariaDB(mydb = mydb,query =paste0('Delete from annotations_classification where dataset="',isolate(values$annotations_all_active_learning[selectedRow, "Dataset"]),
                                               '" and doc_id=',isolate(values$annotations_all_active_learning[selectedRow, "Document ID"]),
                                               " and project='",isolate(values$annotations_all_active_learning[selectedRow, "Project"]),
                                               "' and sid=",isolate(values$annotations_all_active_learning[selectedRow, "Sentence ID"]),
                                               " and category='",stringr::str_extract(string = isolate(values$annotations_all_active_learning[selectedRow, "Category"]),
                                                                                      pattern = "(?<=>).+(?=</b>)"),
                                               "';") )
    RMariaDB::dbDisconnect(mydb)
    shinyjs::useShinyjs()
    shinyjs::click(id = "update_annotations_active_learning")
  }
})
