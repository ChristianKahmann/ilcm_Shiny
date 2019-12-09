values$last_selected<-character(0)

output$classification_UI<-renderUI({
  return(tagList(
    tags$h4(paste("Active Learning for project: ",input$project_selected)),
    tabsetPanel(
      tabPanel(title = "one category for multiple examples",tagList(
        DT::dataTableOutput(outputId = "Class_classifications"),
        DT::dataTableOutput(outputId = "Class_evaluated_labels")
      )
      ),
      tabPanel(title =  "all categories on single example documents",
               uiOutput(outputId = "Class_all_categories_avail_UI"),
               uiOutput(outputId = "Class_all_categories_UI"))
    )
  )
  )
})

#####multiple categories######
output$Class_all_categories_avail_UI<-renderUI({
  choices<-list.files(paste0("collections/results/classification/activeLearning_documents/",input$project_selected))
  validate(
    need(length(choices)>0,"no training sets found for this project")
  )
  selectInput(inputId = "Class_all_categories_avail",label = "available training sets",choices = choices,multiple = F)
})


output$Class_all_categories_UI<-renderUI({
  return(tagList(
    column(2,
           uiOutput(outputId = "Class_all_categories_documents_UI"),
           withBusyIndicatorUI(
             bsButton(inputId = "Class_all_categories_save",label = "Save",icon = icon("save"),style = "primary")
           )
    ),
    column(7,
           tags$div(style = 'height: 60vh; overflow-y: auto;',
                    uiOutput(outputId = "Class_all_categories_document_UI")
           )
    ),
    column(3,
           tags$h4("Predictions"),
           uiOutput("Class_all_categories_treshold_UI"),
           tags$div(style = 'height: 50vh; overflow-y: auto;',
                    DT::dataTableOutput(outputId = "Class_all_categories_annotations")
           )
    )
  ))  
})

output$Class_all_categories_documents_UI<-renderUI({
  validate(
    need(length(input$Class_all_categories_avail)>0,message=F),
    need(file.exists(paste0("collections/results/classification/activeLearning_documents/",input$project_selected,"/",input$Class_all_categories_avail,"/examples.RData")),message=F)
  )
  values$Class_all_categories_reload
  load(paste0("collections/results/classification/activeLearning_documents/",input$project_selected,"/",input$Class_all_categories_avail,"/examples.RData"))
  load(paste0("collections/annotation_schemes/",project,".RData"))
  names<-names(unlist(anno))
  anno<-unlist(anno)
  name_color<-names[which(grepl(pattern = paste0(isolate(input$anno_tag),".color"),x = names))]
  color<-anno[name_color]
  name_tag<-names[which(grepl(pattern = paste0(isolate(input$anno_tag),".name"),x = names))]
  name<-anno[name_tag]
  values$Class_all_documents_annos<-cbind(name,color)
  values$Class_all_documents_labels<-labels
  values$Class_all_documents_texts<-texts
  choiceNames<-unique(paste(rep("Document:",dim(texts)[1]),texts[,"id_doc"],sep=""))
  choiceValues<-unique(paste(texts[,"dataset"],texts[,"id_doc"],sep="_"))
  already_evaluated<-sapply(choiceValues,FUN = function(x){
    dataset<-stringr::str_split(string = x,pattern = "_",simplify =T )[1]
    id<-stringr::str_split(string = x,pattern = "_",simplify =T )[2]
    text<-texts[intersect(which(texts[,1]==dataset),which(texts[,2]==id)),]
    return(any(text[,7:9]==TRUE))
  })
  #style already evaluated documents
  choiceN<-list()
  for(i in 1:length(choiceNames)){
    if(already_evaluated[i]){
      choiceN[[i]]<-tags$div(choiceNames[i],icon("check"))
    }
    else{
      choiceN[[i]]<-choiceNames[i]
    }
  }
  
  selected<-choiceValues[1]
  #browser()
  if(!is.null(isolate(values$last_selected))){
    if(isolate(values$last_selected)%in%choiceValues){
      selected<-isolate(values$last_selected)
    }
  }  
  
  
  res<-prettyRadioButtons(inputId = "Class_all_categories_documents",label = "available documents",choiceNames = choiceN,choiceValues = choiceValues,
                          plain=TRUE, animation="pulse",fill=T,width="100%",selected = selected)
  values$Class_all_categories_reload_documents<-runif(1,0,1)
  return(res)
})

observe({
  values$last_selected<-input$Class_all_categories_documents
})


output$Class_all_categories_document_UI<-renderUI({
  validate(
    need(length(isolate(input$Class_all_categories_avail))>0,message=F),
    need(!is.null(input$Class_all_categories_documents),message=F),
    need(file.exists(paste0("collections/results/classification/activeLearning_documents/",input$project_selected,"/",input$Class_all_categories_avail,"/examples.RData")),message=F)
  )
  
  #get chosen texts
  values$Class_all_categories_reload_documents
  dataset<-stringr::str_split(input$Class_all_categories_documents,pattern = "_",simplify = T)[1,1]
  id_doc<-stringr::str_split(input$Class_all_categories_documents,pattern = "_",simplify = T)[1,2]
  data<-isolate(values$Class_all_documents_texts[intersect(which(isolate(values$Class_all_documents_texts[,"id_doc"])==id_doc),which(isolate(values$Class_all_documents_texts[,"dataset"])==dataset)),])
  #browser()
  validate(
    need(
      dim(data)[1]>0,message=F)
  )
  title<-unique(data[,"title"])
  sentences<-data[,"text"]
  #clean whitespaces before , and .
  sentences<-stringr::str_replace_all(string = sentences,pattern = " \\.","\\.")
  sentences<-stringr::str_replace_all(string = sentences,pattern = " ,",",")
  sentences<-stringr::str_replace_all(string = sentences,pattern = " \\?","\\?")
  sentences<-stringr::str_replace_all(string = sentences,pattern = " \\!","\\!")
  sentences<-stringr::str_replace_all(string = sentences,pattern = "„ ","„")
  sentences<-stringr::str_replace_all(string = sentences,pattern = " “","“")
  #add highlights to selected span
  if(!is.null(values$highlight_span)){
    sentences[values$highlight_span[[1]]]<-paste0("<span style='color:",values$highlight_span[[3]],";font-weight:bold'>",sentences[values$highlight_span[[1]]],"</span>")
    #shinyjs::runjs(paste0('document.getElementById("Class',values$highlight_span[[1]],'").focus({preventScroll:false});'))
    shinyjs::runjs(paste0('document.getElementById("Class',values$highlight_span[[1]],'").scrollIntoView({behavior: "smooth", block: "center", inline: "nearest"});'))
  }
  
  a<-list()
  for(i in 1:length(sentences)){
    a[[i]]<-paste0("<span id='Class",i,"'","tabindex='2'",">",sentences[i],"</span>")
  }
  a<-do.call(rbind,a)
  a<-HTML(a)
  return(tagList(
    tags$h4(title),
    tags$p(a)
  ))
})


output$Class_all_categories_annotations<-DT::renderDataTable({
  validate(
    need(length(input$Class_all_categories_avail)>0,message=F),
    need(!is.null(isolate(input$Class_all_categories_documents)),message=F),
    need(!is.null(input$Class_all_categories_threshold),message=F)
  )
  threshold=input$Class_all_categories_threshold
  dataset<-stringr::str_split(input$Class_all_categories_documents,pattern = "_",simplify = T)[1,1]
  id_doc<-stringr::str_split(input$Class_all_categories_documents,pattern = "_",simplify = T)[1,2]
  data<-isolate(values$Class_all_documents_texts[intersect(which(isolate(values$Class_all_documents_texts[,"id_doc"])==id_doc),which(isolate(values$Class_all_documents_texts[,"dataset"])==dataset)),])
  validate(
    need(
      dim(data)[1]>0,message=F)
  ) 
  ids<-paste(dataset,id_doc,data[,3],sep="_")
  labels_predictions<-values$Class_all_documents_labels$predictions[ids]
  labels_probabilities<-values$Class_all_documents_labels$probabilities[ids,,drop=F]
  keep<-which(apply(labels_probabilities,1,max)>threshold)
  
  validate(
    need(length(keep)>0,"no prediction has a likelihood higher than the set threshold")
  )
  labels_predictions<-as.character(labels_predictions)[keep]
  labels_probabilities<-apply(labels_probabilities[keep,,drop=F],1,max)
  
  highlight<-data.frame(span_id=NULL,name=NULL,color=NULL,stringsAsFactors = F)
  if(!all(labels_predictions%in%values$Class_all_documents_annos[,1])){
    shinyWidgets::sendSweetAlert(session=session,title = "Wrong annotation set",text = paste0("It seems the annotations set has changed between annotating and classification.
                           Please make sure all annotated classes are still available in the annotation scheme.",
                                                                                              " Here the class: ",paste(setdiff(unique(labels_predictions),values$Class_all_documents_annos[,1]),collapse=" ")," is missing."),type = "warning")
    return(NULL)
  }
  for(i in 1:length(labels_predictions)){
    highlight<-rbind(highlight,data.frame(span_id=keep[i],name=labels_predictions[i],color=values$Class_all_documents_annos[which(values$Class_all_documents_annos[,1]==labels_predictions[i]),2]))
  }
  
  values$highlight_spans<-highlight
  #create yes/no checkbox
  checkboxes<-lapply(rownames(highlight),function(x){
    radio_id<-paste0("Class_correct_annotation_",x)
    condition<-paste0('input.',radio_id,'=="deny"')
    selected<-character(0)
    selected_deny<-data[highlight[x,1],10]
    if(data[highlight[x,1],10]%in%c("","FALSE","unknown")){
      selected_deny<-"NEG"
    }
    
    if(data[highlight[x,1],7]==TRUE){
      selected<-"approve"
    }
    if(data[highlight[x,1],8]==TRUE){
      selected<-"deny"
    }
    if(data[highlight[x,1],9]==TRUE){
      selected<-"ignore"
    }
    return(as.character(tagList(prettyRadioButtons(inputId = radio_id,label = "",choiceValues = c("approve","deny","ignore"),choiceNames = list(tags$span(style = "color:#00b241;font-weight: bold;", "approve"),tags$span(style = "color:#f2342e;font-weight: bold;", "deny"),
                                                                                                                                                tags$span(style = "color:##002f7c;font-weight: bold;", "ignore"))
                                                   ,selected=selected),
                                conditionalPanel(condition=condition,
                                                 shinyWidgets::prettyRadioButtons(inputId = paste0(radio_id,"_deny"),label = "other category?",choices = c("NEG",setdiff(values$Class_all_documents_annos[,1],highlight[x,2])),
                                                                                  status = "primary",shape = "curve",animation = "jelly",bigger = T,inline = T,plain = T,selected=selected_deny
                                                 )))
    )
    )
  })
  
  predictions<-lapply(lapply(paste(labels_predictions,round(labels_probabilities,digits = 4),sep=" "),tags$div),as.character)
  
  data_for_table<-data.frame(prediction=unlist(predictions),checkboxes=do.call(rbind,checkboxes),stringsAsFactors = F)
  rownames(data_for_table)<-keep
  values$Class_spans_to_highlight<-keep
  colnames(data_for_table)<-c("prediction","feedback")
  remove_existing_radio_buttons_classification_whole_document(ids = rownames(highlight))
  return(datatable(data = data.frame(data_for_table),style="bootstrap",class="table-bordered",escape = F,rownames = F,selection="none",options=list(pageLength = dim(data_for_table)[1],ordering=F,dom = 't',
                                                                                                                                                    rowCallback=JS('function(row, data) { 
$(row).mouseenter(function(){
 var hover_index = $(this)[0].rowIndex 
Shiny.onInputChange("hoverIndexJS", hover_index); 
}); 
$(row).mouseleave(function(){
 Shiny.onInputChange("hoverIndexJS", "none"); 
}); 
}'),preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                                                                                                                                    drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')), callback = JS("table.rows().every(function(i, tab, row) {
                    var $this = $(this.node());
                    $this.attr('id', this.data()[0]);
                    $this.addClass('shiny-input-radiogroup');
  });
                    Shiny.unbindAll(table.table().node());
                    Shiny.bindAll(table.table().node());")
  )
  )
},server = F)

#forward right hovered span with corresponding color to document view
observe({
  validate(
    need(!is.null(input$hoverIndexJS),message = F)
  )
  if(input$hoverIndexJS!="none"){
    values$highlight_span<-values$highlight_spans[input$hoverIndexJS,]
    print(input$hoverIndexJS)
  }
  else{
    values$highlight_span<-NULL
  }
})


#theshold input for sentence probabilities
output$Class_all_categories_treshold_UI<-renderUI({
  validate(
    need(length(input$Class_all_categories_avail)>0,message=F),
    need(!is.null(input$Class_all_categories_documents),message=F),
    need(file.exists(paste0("collections/results/classification/activeLearning_documents/",input$project_selected,"/",input$Class_all_categories_avail,"/examples.RData")),message=F)
  )
  number_of_classes<-length(isolate(levels(values$Class_all_documents_labels$predictions)))
  if(number_of_classes<=2){
    threshold<-0.51
  }
  else{
    threshold<-round(1/(number_of_classes-1),digits = 4)
  }
  numericInput(inputId = "Class_all_categories_threshold",label = "Threshold:",value =threshold,min = 0,max = 1,step = 0.05)
})




observe({
  validate(
    need(!is.null((values$highlight_spans)),message=FALSE)
  )
  
  x<-unlist(lapply(rownames(isolate(values$highlight_spans)),function(i){
    if(is.null(input[[paste0("Class_correct_annotation_",i)]])){
      return(" ")
    }
    else{
      return(input[[paste0("Class_correct_annotation_",i)]])
    }
  }))
  y<-unlist(lapply(rownames(isolate(values$highlight_spans)),function(i){
    if(is.null(input[[paste0("Class_correct_annotation_",i,"_deny")]])){
      return(" ")
    }
    else{
      return(input[[paste0("Class_correct_annotation_",i,"_deny")]])
    }
  }))
  
  texts<-isolate(values$Class_all_documents_texts)
  ids<-paste(texts[,1],texts[,2],texts[,3],sep="_")
  rn<-which(ids%in%rownames(isolate(values$highlight_spans)))
  
  approved<-rn[which(x=="approve")]
  denied<-rn[which(x=="deny")]
  ignored<-rn[which(x=="ignore")]
  other<-y[which(x=="deny")]
  #set to default
  print("change of set values")
  isolate(values$Class_all_documents_texts[rn,7:9]<-FALSE)
  isolate(values$Class_all_documents_texts[rn,10]<-FALSE)
  #adjsut to input values
  isolate(values$Class_all_documents_texts[approved,7]<-TRUE)
  isolate(values$Class_all_documents_texts[denied,8]<-TRUE)
  isolate(values$Class_all_documents_texts[denied,10]<-other)
  isolate(values$Class_all_documents_texts[ignored,9]<-TRUE)
})


observeEvent(input$Class_all_categories_save,{
  withBusyIndicatorServer("Class_all_categories_save", {
    browser()
    texts<-isolate(values$Class_all_documents_texts)
    labels<-isolate(values$Class_all_documents_labels)
    project<-input$project_selected
    save(texts,labels,project,file=paste0("collections/results/classification/activeLearning_documents/",input$project_selected,"/",input$Class_all_categories_avail,"/examples.RData"))
    data<-texts
    #save approved classifications
    approved<-data[which(data[,"approved"]==TRUE),1:3,drop=F]
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
    if(dim(approved)[1]>0){
      categories<-as.character(labels$predictions[which(data[,"approved"]==TRUE)])
      approved<-data.frame(approved,project=input$project_selected,class=categories,status="approved",stringsAsFactors = F)
      colnames(approved)<-c("dataset","doc_id","sid","project","category","status")
      class(approved$doc_id)<-"integer"
      class(approved$sid)<-"integer"
      known<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from annotations_classification where project='",input$project_selected,"' and category in ('",paste(categories,collapse="','"),"') and status='approved';"))[,1:6]
      approved<-dplyr::anti_join(x = approved,known)
      if(dim(approved)[1]>0){
        approved<-data.frame(approved,timestamp=Sys.time())
        colnames(approved)<-c("dataset","doc_id","sid","project","category","status","timestamp")
        vals<-""
        for(i in 1:dim(approved)[1]){
          vals<-paste0(vals,'("',approved[i,1],'",','"',approved[i,2],'"',',','"',approved[i,3],'","',approved[i,4],'","',approved[i,5],'",','"',approved[i,6],'"',',','"',approved[i,7],'", ','"FALSE"','),')
        }
        vals<-substr(vals,1,nchar(vals)-1)
        query<-paste('Insert Ignore into annotations_classification Values',vals,' ON DUPLICATE KEY UPDATE status="approved" , timestamp="',Sys.time(),'";',sep="")
        
        try({
          write_to_MariaDB(mydb,query)
        })
      }
    }
    not_approved<-data[which(data[,"approved"]==FALSE),c(1:3)]
    
    for(i in 1:dim(not_approved)[1]){
      delete_from_MariaDB(mydb = mydb,query = paste0('Delete from annotations_classification where dataset="',not_approved[i,1],'" and doc_id=',not_approved[i,2],' and sid=',not_approved[i,3],
                                                     ' and project="',input$project_selected,'" and category="',as.character(labels$predictions[i]),'" and status="approved";'))
    }
    #save ignored
    ignored<-data[which(data[,"ignored"]==TRUE),1:3]
    if(dim(ignored)[1]>0){
      categories<-as.character(labels$predictions[which(data[,"ignored"]==TRUE)])
      ignored<-data.frame(ignored,project=input$project_selected,class=categories,status="ignored",stringsAsFactors = F)
      colnames(ignored)<-c("dataset","doc_id","sid","project","category","status")
      class(ignored$doc_id)<-"integer"
      class(ignored$sid)<-"integer"
      known<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from annotations_classification where project='",input$project_selected,"' and category in ('",paste(categories,collapse="','"),"') and status='ignored';"))[,1:6]     
      ignored<-dplyr::anti_join(x = ignored,known)
      if(dim(ignored)[1]>0){
        ignored<-data.frame(ignored,timestamp=Sys.time())
        colnames(ignored)<-c("dataset","doc_id","sid","project","category","status","timestamp")
        vals<-""
        for(i in 1:dim(ignored)[1]){
          vals<-paste0(vals,'("',ignored[i,1],'",','"',ignored[i,2],'"',',','"',ignored[i,3],'","',ignored[i,4],'","',ignored[i,5],'",','"',ignored[i,6],'"',',','"',ignored[i,7],'", ','"FALSE"','),')
        }
        vals<-substr(vals,1,nchar(vals)-1)
        query<-paste('Insert Ignore into annotations_classification Values',vals,' ON DUPLICATE KEY UPDATE status="ignored", timestamp="',Sys.time(),'";',sep="")
        
        try({
          write_to_MariaDB(mydb,query)
        })
      }
    }
    not_ignored<-data[which(data[,"ignored"]==FALSE),1:3]
    
    for(i in 1:dim(not_ignored)[1]){
      delete_from_MariaDB(mydb = mydb,query = paste0('Delete from annotations_classification where dataset="',not_ignored[i,1],'" and doc_id=',not_ignored[i,2],' and sid=',not_ignored[i,3],
                                                     ' and project="',input$project_selected,'" and category="',as.character(labels$predictions[i]),'"and status="ignored";'))
    }
    #save denied
    denied<-data[which(data[,"denied"]==TRUE),1:3]
    if(dim(denied)[1]>0){
      categories<-as.character(labels$predictions[which(data[,"denied"]==TRUE)])
      denied<-data.frame(denied,project=input$project_selected,class=categories,status=paste("denied",data[which(data[,"denied"]==TRUE),10],sep="_"),stringsAsFactors = F)
      colnames(denied)<-c("dataset","doc_id","sid","project","category","status")
      class(denied$doc_id)<-"integer"
      class(denied$sid)<-"integer"
      known<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from annotations_classification where project='",input$project_selected,"' and category in ('",paste(categories,collapse="','"),"') and status='denied';"))[,1:6]
      denied<-dplyr::anti_join(x = denied,known)
      if(dim(denied)[1]>0){
        denied<-data.frame(denied,timestamp=Sys.time())
        colnames(denied)<-c("dataset","doc_id","sid","project","category","status","timestamp")
        vals<-""
        for(i in 1:dim(denied)[1]){
          vals<-paste0(vals,'("',denied[i,1],'",','"',denied[i,2],'"',',','"',denied[i,3],'","',denied[i,4],'","',denied[i,5],'",','"',denied[i,6],'"',',','"',denied[i,7],'", ','"FALSE"','),')
        }
        vals<-substr(vals,1,nchar(vals)-1)
        query<-paste('Replace into annotations_classification Values',vals,';',sep="")
        
        try({
          write_to_MariaDB(mydb,query)
        })
      }
    }
    not_denied<-data[which(data[,"denied"]==FALSE),1:3]
    
    for(i in 1:dim(not_denied)[1]){
      delete_from_MariaDB(mydb = mydb,query = paste0('Delete from annotations_classification where dataset="',not_denied[i,1],'" and doc_id=',not_denied[i,2],' and sid=',not_denied[i,3],
                                                     ' and project="',input$project_selected,'" and category="',as.character(labels$predictions[i]),'"and status LIKE "denied%";'))
    }
    RMariaDB::dbDisconnect(conn = mydb)
    shinyWidgets::sendSweetAlert(session=session,title = "Changes saved",type = "success")
    values$Class_all_categories_reload<-runif(1,0,1)
  })
})





























##############################################
#####one category######
##############################################
output$Class_classifications<-DT::renderDataTable({
  values$Class_update_classifications
  files<-list.files(path = paste0("collections/results/classification/activeLearning/",input$project_selected),full.names = T,include.dirs = F,recursive = T,pattern = "training_examples.RData")
  validate(
    need(length(files)>0,message="No classifications found for this project!")
  )
  class<-NULL
  for(i in 1:length(files)){
    load(files[i])
    evaluated<-paste0("<b style='color:green'>",length(which(data[,"approved"]==TRUE)),"</b>/","<b style='color:red'>",length(which(data[,"denied"]==TRUE)),"</b>/","<b style='color:orange'>",length(which(data[,"ignored"]==TRUE)),"</b>/","<b>",dim(data)[1],"</b>")
    information<-c(as.character(learning_meta$date),as.character(learning_meta$color),learning_meta$category,paste("collection:",learning_meta$collection),learning_meta$strategy,evaluated)
    class<-rbind(class,information)
    class[i,2]<-paste0('<b style="background-color:',class[i,2],';">',class[i,2],'</b>')
  }
  colnames(class)<-c("date","color","category","collection","strategy","evaluated")
  load(paste0("collections/annotation_schemes/",learning_meta$project,".RData"))
  names<-names(unlist(anno))
  anno<-unlist(anno)
  name_tag<-names[which(grepl(pattern = paste0(NULL,".name"),x = names))]
  values$class_anno_names<-as.character(anno[name_tag])
  values$class_anno_selected<-learning_meta$category
  table_data<-reactiveValues(data=data.frame(
    class,
    Log = shinyInput(
      shinyBS::bsButton,
      length(files),
      paste0('actions_log_',input$project_selected,"_"),
      label = "Log",
      style="primary",
      icon=icon("signal"),
      onclick = 'Shiny.onInputChange(\"classification_buttons\",  this.id)'
    ),  
    Features = shinyInput(
      shinyBS::bsButton,
      length(files),
      paste0('actions_features_',input$project_selected,"_"),
      label = "Features",
      style="warning",
      icon=icon("line-chart"),
      onclick = 'Shiny.onInputChange(\"classification_buttons\",  this.id)'
    ),
    Evaluate = shinyInput(
      shinyBS::bsButton,
      length(files),
      paste0('actions_eval_',input$project_selected,"_"),
      label = "Evaluate",
      style="info",
      icon=icon("edit"),
      onclick = 'Shiny.onInputChange(\"classification_buttons\",  this.id)'
    ),
    Delete = shinyInput(
      shinyBS::bsButton,
      length(files),
      paste0('actions_del_',input$project_selected,"_"),
      label = "Delete",
      style="danger",
      icon=icon("trash"),
      onclick = 'Shiny.onInputChange(\"classification_buttons\",  this.id)'
    ),
    Rerun =shinyInput(
      shinyBS::bsButton,
      length(files),
      paste0('actions_rerun_',input$project_selected,"_"),
      label = "Rerun",
      style="success",
      icon=icon("reply"),
      onclick = 'Shiny.onInputChange(\"classification_buttons_rerun\",  this.id)'
    ),
    stringsAsFactors = FALSE
  ))
  table<-datatable(data = table_data$data,options=list(dom="tp",order=list(0,"desc")),rownames = F,escape = F,selection="none",caption = "Classifications for selected project")
  return(table)
})

observeEvent(input$classification_buttons_rerun,{
  file_id<-as.numeric(stringr::str_split(string = input$classification_buttons_rerun,pattern = "_",simplify = "")[1,4])
  #get task_id
  ID<-get_task_id_counter()+1
  set_task_id_counter(ID)
  
  
  load(list.files(path = paste0("collections/results/classification/activeLearning/",input$project_selected),
                  full.names = T,include.dirs = F,recursive = T,pattern = "parameters.RData")[file_id])
  
  process_info<-list(ID,parameters$collection,"Classification Re-Run",as.character(Sys.time()))
  #save logfile path
  logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
  #create logfile
  write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
  #save data needed in script execution 
  save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
  #start script
  system(paste0('Rscript collections/scripts/Classification_Script.R',' &'))
  
  shinyWidgets::sendSweetAlert(session=session,title = "Process started",text = "The classification process was started again with the same parameter settings,
                         but only with tagged active learning data added.",type="success")
})




observe({
  validate(
    need(!is.null(input$project_selected),message=FALSE),
    need(input$classification_buttons!="not_reactive",message=FALSE),
    need(!is.null(input$classification_buttons),message=FALSE)
  )
  x<-input$classification_buttons
  values$class_eval_id_selected<-stringr::str_split(string = x,pattern = "_",simplify = T)[1,4]
  button_input<-str_split(string = x,pattern = "_",simplify = TRUE)[2:4]
  load(list.files(path = paste0("collections/results/classification/activeLearning/",input$project_selected),full.names = T,include.dirs = F,recursive = T,pattern = "training_examples.RData")[as.numeric(button_input[3])])
  data$other<-as.character(data$other)
  values$Class_eval_data<-data[1:10,]
  values$class_eval_data_index<-10
  values$Class_eval_data_complete<-data
  values$Class_eval_save_path<-list.files(path = paste0("collections/results/classification/activeLearning/",input$project_selected),full.names = T,include.dirs = F,recursive = T,pattern = "training_examples.RData")[as.numeric(button_input[3])]
  values$Class_eval_meta<-learning_meta
  values$Class_eval_result<-result
  #delete button was clicked
  if(button_input=="del"){
    values$Class_file_delete<-list.files(paste0("collections/results/classification/activeLearning/",input$project_selected),full.names = T)[as.numeric(button_input[3])]
    showModal(modalDialog(easyClose = T,
                          title = "Delete Classification",footer=NULL,
                          "Are you sure you want to delete this classification?",
                          shinyBS::bsButton(inputId = "class_delete","Delete")
    ))
  }
  #if features button was clicked
  if(button_input=="features"){
    load(list.files(path = paste0("collections/results/classification/activeLearning/",input$project_selected),full.names = T,include.dirs = F,recursive = T,pattern = "feature_matrix.RData")[as.numeric(button_input[3])])
    values$Class_eval_feature_matrix<-feature_matrix
    showModal(modalDialog(easyClose = T,size = "l",
                          title = "Feature Breakdown",footer=NULL,
                          sliderInput(inputId = "Class_eval_feature_breakdown_n",label = "number of features",min = 2,max = floor(dim(feature_matrix)[2]/2),value =min((dim(feature_matrix)[2]/2),10) ,step = 2),
                          plotly::plotlyOutput(outputId = "Class_eval_feature_breakdown_plot")
    ))
  }
  #evaluate button was clicked
  if(button_input=="eval"){
    if(values$Class_eval_meta$context_unit=="Document"){
      showModal(modalDialog(title = "Evaluate machine classified examples",easyClose = F,size = "l",
                            navbarPage(title = "Evaluate Classification",id = "Eval",
                                       tabPanel("Evaluate",
                                                tags$h2(learning_meta$category),
                                                tags$div(
                                                  "Load new examples:",
                                                  shinyBS::bsButton(inputId = "Class_eval_random","Random",style = "primary"),
                                                  shinyBS::bsButton(inputId = "Class_eval_highest_scored",label = "Next",style = "primary"),
                                                  withBusyIndicatorUI(
                                                    shinyBS::bsButton(inputId = "Class_eval_save",label = "Save",style = "success",icon=icon("save"))
                                                  ),
                                                  shinyBS::bsButton(inputId="Class_eval_close",label = "Dismiss",style = "danger",icon=icon("close")),
                                                  tags$br(),
                                                  tags$hr(),
                                                  uiOutput(outputId = "Class_eval_examples")
                                                )
                                       )
                            )
      ))
    }
    else{
      showModal(modalDialog(title = "Evaluate machine classified examples",easyClose = F,size = "l",
                            navbarPage(title = "Evaluate Classification",id = "Eval",
                                       tabPanel("Evaluate",
                                                tags$h2(learning_meta$category),
                                                tags$div(
                                                  "Load new examples:",
                                                  shinyBS::bsButton(inputId = "Class_eval_random","Random",style = "primary"),
                                                  shinyBS::bsButton(inputId = "Class_eval_highest_scored",label = "Next",style = "primary"),
                                                  withBusyIndicatorUI(
                                                    shinyBS::bsButton(inputId = "Class_eval_save",label = "Save",style = "success",icon=icon("save"))
                                                  ),
                                                  shinyBS::bsButton(inputId="Class_eval_close",label = "Dismiss",style = "danger",icon=icon("close")),
                                                  tags$br(),
                                                  tags$hr(),
                                                  uiOutput(outputId = "Class_eval_examples")%>%
                                                    withSpinner(type = 6)
                                                )
                                       ),
                                       tabPanel("Document",
                                                uiOutput(outputId = "Class_eval_document")
                                       )
                            )
      ))
    }
  }
  #log button was clicked
  if(button_input=="log"){
    showModal(modalDialog(title = paste0("Classification logs for category: ",learning_meta$category),
                          plotlyOutput(outputId ="Class_log_plot" ),
                          tags$div(paste0("best F-score: ",max(result)))
    ))
  }
  shinyjs::runjs('Shiny.onInputChange(\"classification_buttons\",  "not_reactive")')
})


#plot for feature breakdown for active learning examples
output$Class_eval_feature_breakdown_plot<-plotly::renderPlotly({
  validate(
    need(
      !is.null( values$Class_eval_feature_matrix),message=F
    )
  )
    pos<- values$Class_eval_feature_matrix[,order( values$Class_eval_feature_matrix,decreasing = T)][1:(input$Class_eval_feature_breakdown_n/2)]
    neg<- values$Class_eval_feature_matrix[,order( values$Class_eval_feature_matrix,decreasing = F)][1:(input$Class_eval_feature_breakdown_n/2)]
    neg<-sort(neg,decreasing = F)
    pos<-sort(pos,decreasing = F)
    yform <- list(categoryorder = "array",
                  categoryarray = c(names(neg),names(pos)),
                  title="features")
    
   p<-plotly::plot_ly()%>%
      plotly::add_bars(x=pos,y=factor(names(pos)),orientation="h",name="positive weights",marker=list(color="#1FDE78",line=list(color="#136639",width=1)))%>%
      plotly::add_bars(x = neg,y = factor(names(neg)),name="negative weights",marker=list(color="#D42750",line=list(color="#460F1C",width=1)))%>%
      plotly::layout(yaxis=yform,xaxis=list(title="SVM weights"))
   return(p)
})


output$Class_log_plot<-renderPlotly({
  result<-values$Class_eval_result
  plot_ly(x=factor(c("0.003", "0.01", "0.03", "0.1", "0.3", "1", "3" , "10", "30", "100"),levels=c("0.003", "0.01", "0.03", "0.1", "0.3", "1", "3" , "10", "30", "100")),y=result,type = "scatter",mode="lines")%>%
    layout(xaxis=list(title="C-Value"),yaxis=list(title="F-score",range=c(0,1)))
})


shiny::observeEvent(ignoreInit = T,input$Class_eval_close,{
  shiny::removeModal()
})


output$Class_eval_examples<-renderUI({
  if(values$Class_eval_meta$context_unit=="Document"){
    values_pairs<-paste(paste0("('",stringr::str_split(values$Class_eval_data$doc_id_global,pattern = "_",simplify = T)[,1],"' , ",stringr::str_split(values$Class_eval_data$doc_id_global,pattern = "_",simplify = T)[,2],")"),
                        collapse=", ")
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
    RMariaDB::dbBegin(conn = mydb)
    titles=RMariaDB::dbGetQuery(mydb,paste0("SELECT title FROM ilcm.documents where (dataset, id_doc) IN ( ",values_pairs," );"))
    RMariaDB::dbDisconnect(mydb)
    
    examples<-lapply(X = 1:dim(values$Class_eval_data)[1],FUN = function(x){
      radio_id<-paste0("Class_eval_radio_buttons_",isolate(values$class_eval_id_selected),"_",rownames(values$Class_eval_data)[x])
      condition<-paste0('input.',radio_id,'=="deny"')
      selected<-values$Class_eval_data[x,6]
      if(selected==" "){
        selected<-"NEG"
      }
      tagList(
        tags$h4(titles[x,1]),
        tags$b(values$Class_eval_data[x,2]),
        tags$br(),
        fluidRow(
          column(7,offset = 5,
                 
                 shinyWidgets::prettyRadioButtons(inputId = radio_id,label = "",choices = c("approve","deny","ignore"),selected = c("approve","deny","ignore")[which(values$Class_eval_data[x,3:5]==TRUE)],
                                                  status = "warning",shape = "square",animation = "rotate",bigger = T,inline = T),
                 conditionalPanel(condition = condition,
                                  shinyWidgets::prettyRadioButtons(inputId = paste0(radio_id,"_deny"),label = "other category?",choices = c("NEG",setdiff(values$class_anno_names,values$class_anno_selected)),
                                                                   selected = selected,
                                                                   status = "primary",shape = "curve",animation = "jelly",bigger = T,inline = T,plain = T)
                 )
          )
        ),
        tags$hr()
      )
    }
    )
  }
  else{
    examples<-lapply(X = 1:dim(values$Class_eval_data)[1],FUN = function(x){
      radio_id<-paste0("Class_eval_radio_buttons_",isolate(values$class_eval_id_selected),"_",rownames(values$Class_eval_data)[x])
      condition<-paste0('input.',radio_id,'=="deny"')
      selected<-values$Class_eval_data[x,6]
      if(selected==" "){
        selected<-"NEG"
      }
      tagList(
        tags$b(values$Class_eval_data[x,2]),
        tags$br(),
        fluidRow(
          column(4,
                 shinyBS::bsButton(inputId = paste0("Class_eval_link_document_",rownames(values$Class_eval_data)[x]),label = "See document",icon = icon("eye"),onclick = 'Shiny.onInputChange(\"Class_eval_doclink\",  this.id)')
          ),
          column(7,offset = 1,
                 
                 shinyWidgets::prettyRadioButtons(inputId = radio_id,label = "",choices = c("approve","deny","ignore"),selected = c("approve","deny","ignore")[which(values$Class_eval_data[x,3:5]==TRUE)],
                                                  status = "warning",shape = "square",animation = "rotate",bigger = T,inline = T),
                 conditionalPanel(condition = condition,
                                  shinyWidgets::prettyRadioButtons(inputId = paste0(radio_id,"_deny"),label = "other category?",choices = c("NEG",setdiff(values$class_anno_names,values$class_anno_selected)),
                                                                   selected = selected,
                                                                   status = "primary",shape = "curve",animation = "jelly",bigger = T,inline = T,plain = T)
                 )
          )
        ),
        tags$hr()
      )
    }
    )
  }
  do.call(tagList,examples)
})


observeEvent(input$Class_eval_random,{
  values$Class_eval_data<-values$Class_eval_data_complete[sample(x = 1:50,size = 10,replace = FALSE),]
})

observeEvent(input$Class_eval_highest_scored,{
  if(values$class_eval_data_index==dim(values$Class_eval_data_complete)[1]){
    values$class_eval_data_index<-0
  }
  idx<-(values$class_eval_data_index+1):(values$class_eval_data_index+10)
  values$Class_eval_data<-values$Class_eval_data_complete[idx,]
  values$class_eval_data_index<-values$class_eval_data_index+10
})


#change data object if sentences have been approved/denied/ignored
observe({
  validate(
    need(!is.null(values$Class_eval_data),message=FALSE)
  )
  
  x<-unlist(lapply(rownames(values$Class_eval_data),function(i){
    if(is.null(input[[paste0("Class_eval_radio_buttons_",isolate(values$class_eval_id_selected),"_",i)]])){
      return(" ")
    }
    else{
      return(input[[paste0("Class_eval_radio_buttons_",isolate(values$class_eval_id_selected),"_",i)]])
    }
  }))
  y<- unlist(lapply(rownames(values$Class_eval_data),function(i){
    if(is.null(input[[paste0("Class_eval_radio_buttons_",isolate(values$class_eval_id_selected),"_",i,"_deny")]])){
      return(" ")
    }
    else{
      return(input[[paste0("Class_eval_radio_buttons_",isolate(values$class_eval_id_selected),"_",i,"_deny")]])
    }
  }))
  approved<-rownames(values$Class_eval_data)[which(x=="approve")]
  denied<-rownames(values$Class_eval_data)[which(x=="deny")]
  ignored<-rownames(values$Class_eval_data)[which(x=="ignore")]
  other<-rownames(values$Class_eval_data)
  
  isolate(values$Class_eval_data_complete[rownames(values$Class_eval_data),3:5]<-FALSE)
  isolate(values$Class_eval_data_complete[approved,3]<-TRUE)
  isolate(values$Class_eval_data_complete[denied,4]<-TRUE)
  isolate(values$Class_eval_data_complete[ignored,5]<-TRUE)
  if(any(sapply(X = y,FUN = nchar)>1)){
    isolate(values$Class_eval_data_complete[other,6]<-y)
  }
})


observeEvent(input$class_delete,{
  shiny::removeModal()
  unlink(values$Class_file_delete,recursive = T)
  values$Class_update_classifications<-runif(1,0,1)
})



observeEvent(input$Class_eval_save,{
  withBusyIndicatorServer("Class_eval_save", {
    data<-isolate(values$Class_eval_data_complete)
    learning_meta<-isolate(values$Class_eval_meta)
    result<-isolate(values$Class_eval_result)
    save(data,learning_meta,result,file=values$Class_eval_save_path)
    #save approved classifications
    if(learning_meta$context_unit=="Document"){
      approved<-stringr::str_split(string = data[which(data[,"approved"]==TRUE),1],pattern = "_",simplify = T)
      approved<-cbind(approved,"0")
    }
    else{
      approved<-stringr::str_split(string = data[which(data[,"approved"]==TRUE),1],pattern = "_",simplify = T)
    }
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
    if(dim(approved)[1]>0){
      approved<-data.frame(approved,project=input$project_selected,class=learning_meta$category,status="approved",stringsAsFactors = F)
      colnames(approved)<-c("dataset","doc_id","sid","project","category","status")
      class(approved$doc_id)<-"integer"
      class(approved$sid)<-"integer"
      known<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from annotations_classification where project='",input$project_selected,"' and category='",learning_meta$category,"' and status='approved';"))[,1:6]
      approved<-dplyr::anti_join(x = approved,known)
      if(dim(approved)[1]>0){
        approved<-data.frame(approved,timestamp=Sys.time())
        colnames(approved)<-c("dataset","doc_id","sid","project","category","status","timestamp")
        vals<-""
        for(i in 1:dim(approved)[1]){
          if(learning_meta$context_unit=="Document"){
            vals<-paste0(vals,'("',approved[i,1],'",','"',approved[i,2],'"',',','"',approved[i,3],
                         '","',approved[i,4],'","',approved[i,5],'",','"',approved[i,6],'"',',','"',approved[i,7],'"',',"TRUE"','),')
          }
          else{
            vals<-paste0(vals,'("',approved[i,1],'",','"',approved[i,2],'"',',','"',approved[i,3],
                         '","',approved[i,4],'","',approved[i,5],'",','"',approved[i,6],'"',',','"',approved[i,7],'"',',"FALSE"','),')
          }
        }
        vals<-substr(vals,1,nchar(vals)-1)
        query<-paste('Insert Ignore into annotations_classification Values',vals,' ON DUPLICATE KEY UPDATE status="approved" , timestamp="',Sys.time(),'";',sep="")
        
        try({
          write_to_MariaDB(mydb,query)
        })
      }
    }
    if(learning_meta$context_unit=="Document"){
      not_approved<-stringr::str_split(string = data[which(data[,"approved"]==FALSE),1],pattern = "_",simplify = T)
      not_approved<-cbind(not_approved,"0")
    }
    else{
      not_approved<-stringr::str_split(string = data[which(data[,"approved"]==FALSE),1],pattern = "_",simplify = T)
    }
    
    if(dim(not_approved)[1]>0){
      for(i in 1:dim(not_approved)[1]){
        delete_from_MariaDB(mydb = mydb,query = paste0('Delete from annotations_classification where dataset="',not_approved[i,1],'" and doc_id=',not_approved[i,2],' and sid=',not_approved[i,3],
                                                       ' and project="',input$project_selected,'" and category="',learning_meta$category,'" and status="approved";'))
      }
    }
    #save ignored
    if(learning_meta$context_unit=="Document"){
      ignored<-stringr::str_split(string = data[which(data[,"ignored"]==TRUE),1],pattern = "_",simplify = T)
      ignored<-cbind(ignored,"0")
    }
    else{
      ignored<-stringr::str_split(string = data[which(data[,"ignored"]==TRUE),1],pattern = "_",simplify = T)
    }
    if(dim(ignored)[1]>0){
      ignored<-data.frame(ignored,project=input$project_selected,class=learning_meta$category,status="ignored",stringsAsFactors = F)
      colnames(ignored)<-c("dataset","doc_id","sid","project","category","status")
      class(ignored$doc_id)<-"integer"
      class(ignored$sid)<-"integer"
      known<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from annotations_classification where project='",input$project_selected,"' and category='",learning_meta$category,"' and status='ignored';"))[,1:6]
      ignored<-dplyr::anti_join(x = ignored,known)
      if(dim(ignored)[1]>0){
        ignored<-data.frame(ignored,timestamp=Sys.time())
        colnames(ignored)<-c("dataset","doc_id","sid","project","category","status","timestamp")
        vals<-""
        for(i in 1:dim(ignored)[1]){
          if(learning_meta$context_unit=="Document"){
            vals<-paste0(vals,'("',ignored[i,1],'",','"',ignored[i,2],'"',',','"',ignored[i,3],'","',ignored[i,4],
                         '","',ignored[i,5],'",','"',ignored[i,6],'"',',','"',ignored[i,7],'"',',"TRUE"','),')
          }
          else{
            vals<-paste0(vals,'("',ignored[i,1],'",','"',ignored[i,2],'"',',','"',ignored[i,3],'","',ignored[i,4],
                         '","',ignored[i,5],'",','"',ignored[i,6],'"',',','"',ignored[i,7],'"',',"FALSE"','),')
          }
        }
        vals<-substr(vals,1,nchar(vals)-1)
        query<-paste('Insert Ignore into annotations_classification Values',vals,' ON DUPLICATE KEY UPDATE status="ignored", timestamp="',Sys.time(),'";',sep="")
        
        try({
          write_to_MariaDB(mydb,query)
        })
      }
    }
    if(learning_meta$context_unit=="Document"){
      not_ignored<-stringr::str_split(string = data[which(data[,"ignored"]==FALSE),1],pattern = "_",simplify = T)
      not_ignored<-cbind(not_ignored,"0")
    }
    else{
      not_ignored<-stringr::str_split(string = data[which(data[,"ignored"]==FALSE),1],pattern = "_",simplify = T)
    }
    if(dim(not_ignored)[1]>0){
      for(i in 1:dim(not_ignored)[1]){
        delete_from_MariaDB(mydb = mydb,query = paste0('Delete from annotations_classification where dataset="',not_ignored[i,1],'" and doc_id=',not_ignored[i,2],' and sid=',not_ignored[i,3],
                                                       ' and project="',input$project_selected,'" and category="',learning_meta$category,'"and status="ignored";'))
      }
    }
    #save denied
    if(learning_meta$context_unit=="Document"){
      denied<-stringr::str_split(string = data[which(data[,"denied"]==TRUE),1],pattern = "_",simplify = T)
      denied<-cbind(denied,"0")
    }
    else{
      denied<-stringr::str_split(string = data[which(data[,"denied"]==TRUE),1],pattern = "_",simplify = T)
    }
    if(dim(denied)[1]>0){
      denied<-data.frame(denied,project=input$project_selected,class=learning_meta$category,status=paste("denied",data[which(data[,"denied"]==TRUE),6],sep="_"),stringsAsFactors = F)
      colnames(denied)<-c("dataset","doc_id","sid","project","category","status")
      class(denied$doc_id)<-"integer"
      class(denied$sid)<-"integer"
      known<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from annotations_classification where project='",input$project_selected,"' and category='",learning_meta$category,"' and status LIKE 'denied%';"))[,1:6]
      denied<-dplyr::anti_join(x = denied,known)
      if(dim(denied)[1]>0){
        denied<-data.frame(denied,timestamp=Sys.time())
        colnames(denied)<-c("dataset","doc_id","sid","project","category","status","timestamp")
        vals<-""
        for(i in 1:dim(denied)[1]){
          if(learning_meta$context_unit=="Document"){
            vals<-paste0(vals,'("',denied[i,1],'",','"',denied[i,2],'"',',','"',denied[i,3],'","',denied[i,4],
                         '","',denied[i,5],'",','"',denied[i,6],'"',',','"',denied[i,7],'"',',"TRUE"','),')
          }
          else{
            vals<-paste0(vals,'("',denied[i,1],'",','"',denied[i,2],'"',',','"',denied[i,3],'","',denied[i,4],
                         '","',denied[i,5],'",','"',denied[i,6],'"',',','"',denied[i,7],'"',',"FALSE"','),')
          }
        }
        vals<-substr(vals,1,nchar(vals)-1)
        query<-paste('Replace into annotations_classification Values',vals,';',sep="")
        
        try({
          write_to_MariaDB(mydb,query)
        })
      }
    }
    if(learning_meta$context_unit=="Document"){
      not_denied<-stringr::str_split(string = data[which(data[,"denied"]==FALSE),1],pattern = "_",simplify = T)
      not_denied<-cbind(not_denied,"0")
    }
    else{
      not_denied<-stringr::str_split(string = data[which(data[,"denied"]==FALSE),1],pattern = "_",simplify = T)
    }
    if(dim(not_denied)[1]>0){
      for(i in 1:dim(not_denied)[1]){
        delete_from_MariaDB(mydb = mydb,query = paste0('Delete from annotations_classification where dataset="',not_denied[i,1],'" and doc_id=',not_denied[i,2],' and sid=',not_denied[i,3],
                                                       ' and project="',input$project_selected,'" and category="',learning_meta$category,'"and status LIKE "denied%";'))
      }
    }
    
    
    
    RMariaDB::dbDisconnect(conn = mydb)
    shinyWidgets::sendSweetAlert(session=session,title = "Changes saved",type = "success")
    removeModal()
    values$Class_update_classifications<-runif(1,0,1)
  })
})


#document link
observe({
  validate(
    need(!is.null(values$Class_eval_data),message=FALSE)
  )
  
  doc_id<-(stringr::str_split(string = input$Class_eval_doclink,pattern = "_",simplify = T)[5:7])
  
  validate(
    need(!is.na(doc_id),message=FALSE)
  )
  
  values$Class_eval_doc_id<-doc_id
  updateTabsetPanel(session = session,inputId = "Eval",selected = "Document")
})

output$Class_eval_document<-renderUI({
  validate(
    need(!is.null(values$Class_eval_doc_id),"no document specified")
  )
  #identifier<-stringr::str_split(string = values$Class_eval_data_complete[as.character(values$Class_eval_doc_id),1],pattern = "_",simplify = T)
  identifier<-values$Class_eval_doc_id[1:3]
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host)
  token<-RMariaDB::dbGetQuery(mydb, paste("select * from token where dataset='",identifier[1],"' and id=",identifier[2],";",sep=""))
  meta<-RMariaDB::dbGetQuery(mydb, paste("select title,date from documents where dataset='",identifier[1],"' and id_doc=",identifier[2],";",sep=""))
  RMariaDB::dbDisconnect(mydb)
  words<-token[,5]
  words[which(token[,3]==as.numeric(identifier[3]))]<-paste0('<font style="color: ','#181EBD',';"' ,'>',words[which(token[,3]==as.numeric(identifier[3]))],'</font>')
  text<-paste(words,collapse = " ")
  
  tagList(
    shinyBS::bsButton(inputId = "Class_eval_go_back",label = "Go Back",style = "primary",icon = icon("undo")),
    tags$h4(paste(meta[1,1], "released on", meta[1,2])),
    tags$br(),    
    tags$div(HTML(text))
  )
}
)


observeEvent(input$Class_eval_go_back,{
  updateTabsetPanel(session = session,inputId = "Eval",selected = "Evaluate")
})









#######################################################################################################
#evaluated labels
######################################################################################################
output$Class_evaluated_labels<-DT::renderDataTable({
  values$Class_update_classifications
  files<-list.files(path = paste0("collections/results/classification/activeLearning/",input$project_selected),full.names = T,include.dirs = F,recursive = T,pattern = "training_examples.RData")
  validate(
    need(length(files)>0,message=FALSE)
  )
  class<-NULL
  for(i in 1:length(files)){
    load(files[i])
    context_unit<-"Sentence"
    try({
      if(!is.null(learning_meta$context_unit)){
        context_unit<-learning_meta$context_unit
      }
    })
    class<-rbind(class,cbind(cbind(learning_meta$color,cbind(learning_meta$category,data)),files[i],context_unit))
  }
  keep<-unlist(lapply(1:dim(class)[1],function(x){
    return(any(class[x,5:7]==TRUE))
  }))
  class<-data.frame(class[which(keep),,drop=FALSE],stringsAsFactors = F)
  validate(
    need(dim(class)[1]>0,message=FALSE)
  )
  #if redundant active learning settings were done,  select jsut distinct entries
  class_distinct<-distinct(.data = class,learning_meta.category,doc_id_global,.keep_all = T)
  values$Class_eval_class<-class
  values$Class_eval_class_distinct<-class_distinct
  
  values$Class_eval_context_unit<-as.character(unique(values$Class_eval_class$context_unit)[1])
  categories<-unique(class_distinct[,2])
  df<-NULL
  for(i in categories){
    subclass=class_distinct[which(class_distinct[,2]==i),]
    df<-rbind(df,data.frame(as.character(unique(subclass[,1])),
                            i,
                            as.character(shinyBS::bsButton(inputId = paste0("Class_eval_show_approve_",i),label = paste(length(which(subclass[,5]==TRUE))," found"),icon = icon("search-plus")
                                                           ,style = "info",onclick = 'Shiny.onInputChange(\"Class_eval_evaluated_show\",  this.id)')),
                            as.character(shinyBS::bsButton(inputId = paste0("Class_eval_show_deny_",i),label = paste(length(which(subclass[,6]==TRUE))," found"),icon = icon("search-plus")
                                                           ,style = "info",onclick = 'Shiny.onInputChange(\"Class_eval_evaluated_show\",  this.id)')),
                            as.character(shinyBS::bsButton(inputId = paste0("Class_eval_show_ignore_",i),label = paste(length(which(subclass[,7]==TRUE))," found"),icon = icon("search-plus")
                                                           ,style = "info",onclick = 'Shiny.onInputChange(\"Class_eval_evaluated_show\",  this.id)'))
                            
    ))
  }
  df[,1]<-paste0('<b style="background-color:',df[,1],';">',df[,1],'</b>')
  colnames(df)<-c("color","category","approved","denied","ignored")
  table<-datatable(df,rownames = F,selection = "none",escape = F,options=list(dom="tp"),caption = "Evaluated labels for selected project")
  
  return(table)
})



observe({
  validate(
    need(!is.null(input$project_selected),message=FALSE),
    need(input$Class_eval_evaluated_show!="not_reactive",message=FALSE),
    need(!is.null(input$Class_eval_evaluated_show),message=FALSE)
  )
  x<-input$Class_eval_evaluated_show
  button_input<-str_split(string = x,pattern = "_",simplify = TRUE)[4:5]
  x<-isolate(values$Class_eval_class_distinct)[which(as.character(isolate(values$Class_eval_class_distinct[,2]))==button_input[2]),]
  if(button_input[1]=="approve"){
    isolate(values$Class_eval2_data<-x[which(x[,5]==TRUE),])
  }
  if(button_input[1]=="deny"){
    isolate(values$Class_eval2_data<-x[which(x[,6]==TRUE),])
  }
  if(button_input[1]=="ignore"){
    isolate(values$Class_eval2_data<-x[which(x[,7]==TRUE),])
  }
  if(values$Class_eval_context_unit=="Document"){
    showModal(modalDialog(title = "Evaluate machine classified examples",easyClose = T,size = "l",
                          navbarPage(title = "Evaluate Classification",id = "Eval2",
                                     tabPanel("Evaluate",
                                              withBusyIndicatorUI(
                                                shinyBS::bsButton(inputId = "Class_eval2_save",label = "Save",style = "success",icon=icon("save"))
                                              ),
                                              tags$br(),
                                              tags$hr(),
                                              uiOutput(outputId = "Class_eval2_examples")
                                     )
                          )
    )
    )
  }
  else{
    showModal(modalDialog(title = "Evaluate machine classified examples",easyClose = T,size = "l",
                          navbarPage(title = "Evaluate Classification",id = "Eval2",
                                     tabPanel("Evaluate",
                                              withBusyIndicatorUI(
                                                shinyBS::bsButton(inputId = "Class_eval2_save",label = "Save",style = "success",icon=icon("save"))
                                              ),
                                              tags$br(),
                                              tags$hr(),
                                              uiOutput(outputId = "Class_eval2_examples")
                                     ),
                                     tabPanel("Document",
                                              uiOutput(outputId = "Class_eval2_document")
                                     )
                          )
    )
    )
  }
  shinyjs::runjs('Shiny.onInputChange(\"Class_eval_evaluated_show\",  "not_reactive")')
})


output$Class_eval2_examples<-renderUI({
  validate(
    need(dim(values$Class_eval2_data)[1]>0,"No evaluated labels found!")
  )
  if(values$Class_eval_context_unit=="Document"){
    values_pairs<-paste(paste0("('",stringr::str_split(values$Class_eval2_data$doc_id_global,pattern = "_",simplify = T)[,1],"' , ",stringr::str_split(values$Class_eval2_data$doc_id_global,pattern = "_",simplify = T)[,2],")"),
                        collapse=", ")
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
    RMariaDB::dbBegin(conn = mydb)
    titles=RMariaDB::dbGetQuery(mydb,paste0("SELECT title FROM ilcm.documents where (dataset, id_doc) IN ( ",values_pairs," );"))
    RMariaDB::dbDisconnect(mydb)
    examples<-lapply(X = 1:dim(values$Class_eval2_data)[1],FUN = function(x){
      radio_id<-paste0("Class_eval2_radio_buttons_",rownames(values$Class_eval2_data)[x])
      condition<-paste0('input.',radio_id,'=="deny"')
      tagList(
        tags$h4(titles[x,1]),
        tags$b(values$Class_eval2_data[x,4]),
        tags$br(),
        fluidRow(
          column(7,offset = 5,
                 shinyWidgets::prettyRadioButtons(inputId = radio_id,label = "",choices = c("approve","deny","ignore"),selected = c("approve","deny","ignore")[which(values$Class_eval2_data[x,5:7]==TRUE)],
                                                  status = "warning",shape = "square",animation = "rotate",bigger = T,inline = T),
                 conditionalPanel(condition = condition,
                                  shinyWidgets::prettyRadioButtons(inputId = paste0(radio_id,"_deny"),label = "other category?",choices = c("NEG",setdiff(values$class_anno_names,values$class_anno_selected)),
                                                                   selected = values$Class_eval2_data[x,8],
                                                                   status = "primary",shape = "curve",animation = "jelly",bigger = T,inline = T,plain = T)
                 )
          )
        ),
        tags$hr()
      )
    }
    )
  }
  else{
    examples<-lapply(X = 1:dim(values$Class_eval2_data)[1],FUN = function(x){
      radio_id<-paste0("Class_eval2_radio_buttons_",rownames(values$Class_eval2_data)[x])
      condition<-paste0('input.',radio_id,'=="deny"')
      tagList(
        tags$b(values$Class_eval2_data[x,4]),
        tags$br(),
        fluidRow(
          column(4,
                 shinyBS::bsButton(inputId = paste0("Class_eval2_link_document_",rownames(values$Class_eval2_data)[x]),label = "See document",icon = icon("eye"),onclick = 'Shiny.onInputChange(\"Class_eval2_doclink\",  this.id)')
          ),
          column(7,offset = 1,
                 shinyWidgets::prettyRadioButtons(inputId = radio_id,label = "",choices = c("approve","deny","ignore"),selected = c("approve","deny","ignore")[which(values$Class_eval2_data[x,5:7]==TRUE)],
                                                  status = "warning",shape = "square",animation = "rotate",bigger = T,inline = T),
                 conditionalPanel(condition = condition,
                                  shinyWidgets::prettyRadioButtons(inputId = paste0(radio_id,"_deny"),label = "other category?",choices = c("NEG",setdiff(values$class_anno_names,values$class_anno_selected)),
                                                                   selected = values$Class_eval2_data[x,8],
                                                                   status = "primary",shape = "curve",animation = "jelly",bigger = T,inline = T,plain = T)
                 )
          )
        ),
        tags$hr()
      )
    }
    )
  }
  do.call(tagList,examples)
})

observe({
  validate(
    need(!is.null(values$Class_eval2_data),message=FALSE),
    need(input$Class_eval2_doclink!="not_reactive",message=FALSE)
  )
  
  doc_id<-as.numeric(stringr::str_split(string = input$Class_eval2_doclink,pattern = "_",simplify = T)[5])
  
  validate(
    need(!is.na(doc_id),message=FALSE)
  )
  
  values$Class_eval2_doc_id<-doc_id
  updateTabsetPanel(session = session,inputId = "Eval2",selected = "Document")
  shinyjs::runjs('Shiny.onInputChange(\"Class_eval2_doclink\",  "not_reactive")')
  
})




observe({
  validate(
    need(!is.null(values$Class_eval2_data),message=FALSE)
  )
  
  x<-unlist(lapply(rownames(values$Class_eval2_data),function(i){
    if(is.null(input[[paste0("Class_eval2_radio_buttons_",i)]])){
      return(" ")
    }
    else{
      return(input[[paste0("Class_eval2_radio_buttons_",i)]])
    }
  }))
  
  y<- unlist(lapply(rownames(values$Class_eval2_data),function(i){
    if(is.null(input[[paste0("Class_eval2_radio_buttons_",i,"_deny")]])){
      return(" ")
    }
    else{
      return(input[[paste0("Class_eval2_radio_buttons_",i,"_deny")]])
    }
  }))
  approved<-rownames(values$Class_eval2_data)[which(x=="approve")]
  denied<-rownames(values$Class_eval2_data)[which(x=="deny")]
  ignored<-rownames(values$Class_eval2_data)[which(x=="ignore")]
  other<-rownames(values$Class_eval2_data)
  
  isolate(values$Class_eval_class_distinct[rownames(values$Class_eval2_data),5:7]<-FALSE)
  isolate(values$Class_eval_class_distinct[approved,5]<-TRUE)
  isolate(values$Class_eval_class_distinct[denied,6]<-TRUE)
  isolate(values$Class_eval_class_distinct[ignored,7]<-TRUE)
  if(any(sapply(X = y,FUN = nchar)>1)){
    isolate(values$Class_eval_class_distinct[other,8]<-y)
  }
})


output$Class_eval2_document<-renderUI({
  validate(
    need(!is.null(values$Class_eval2_doc_id),"no document specified")
  )
  identifier<-stringr::str_split(string = values$Class_eval_class_distinct[as.character(values$Class_eval2_doc_id),3],pattern = "_",simplify = T)
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
  token<-RMariaDB::dbGetQuery(mydb, paste("select * from token where dataset='",identifier[1,1],"' and id=",identifier[1,2],";",sep=""))
  meta<-RMariaDB::dbGetQuery(mydb, paste("select title,date from documents where dataset='",identifier[1,1],"' and id_doc=",identifier[1,2],";",sep=""))
  RMariaDB::dbDisconnect(mydb)
  words<-token[,5]
  words[which(token[,3]==as.numeric(identifier[1,3]))]<-paste0('<font style="color: ','#181EBD',';"' ,'>',words[which(token[,3]==as.numeric(identifier[1,3]))],'</font>')
  text<-paste(words,collapse = " ")
  
  tagList(
    shinyBS::bsButton(inputId = "Class_eval2_go_back",label = "Go Back",style = "primary",icon = icon("undo")),
    tags$h4(paste(meta[1,1], " released on", meta[1,2])),
    tags$br(),    
    tags$div(HTML(text))
  )
}
)




observeEvent(input$Class_eval2_go_back,{
  updateTabsetPanel(session = session,inputId = "Eval2",selected = "Evaluate")
})


observeEvent(input$Class_eval2_save,{
  withBusyIndicatorServer("Class_eval2_save", {
    data_class_distinct<-isolate(values$Class_eval_class_distinct)
    data_class<-values$Class_eval_class
    data_joint_class<-merge(data_class[,c("learning_meta.category","doc_id_global","files.i.")],data_class_distinct[,-9],by=c("learning_meta.category","doc_id_global"))[,colnames(data_class)]
    for(i in unique(data_joint_class[,9])){
      i<-as.character(i)
      subset<-data_joint_class[which(data_joint_class[,9]==i),]
      load(i)
      data[which(data[,1]%in%subset[,3]),3:6]<-subset[,5:8]
      save(data,learning_meta,result,file=i)
    }  
    if(is.null(learning_meta$context_unit)){
      learning_meta$context_unit<-"Sentence"
    }
    #save approved classifications
    
    data<-data_class_distinct[,c("doc_id_global" ,"token","approved","denied","ignored","other")]
    if(learning_meta$context_unit=="Document"){
      approved<-stringr::str_split(string = data[which(data[,"approved"]==TRUE),1],pattern = "_",simplify = T)
      approved<-cbind(approved,"0")
    }
    else{
      approved<-stringr::str_split(string = data[which(data[,"approved"]==TRUE),1],pattern = "_",simplify = T)
    }
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
    if(dim(approved)[1]>0){
      approved<-data.frame(approved,project=input$project_selected,class=learning_meta$category,status="approved",stringsAsFactors = F)
      colnames(approved)<-c("dataset","doc_id","sid","project","category","status")
      class(approved$doc_id)<-"integer"
      class(approved$sid)<-"integer"
      known<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from annotations_classification where project='",input$project_selected,"' and category='",learning_meta$category,"' and status='approved';"))[,1:6]
      approved<-dplyr::anti_join(x = approved,known)
      if(dim(approved)[1]>0){
        approved<-data.frame(approved,timestamp=Sys.time())
        colnames(approved)<-c("dataset","doc_id","sid","project","category","status","timestamp")
        vals<-""
        for(i in 1:dim(approved)[1]){
          if(learning_meta$context_unit=="Document"){
            vals<-paste0(vals,'("',approved[i,1],'",','"',approved[i,2],'"',',','"',approved[i,3],
                         '","',approved[i,4],'","',approved[i,5],'",','"',approved[i,6],'"',',','"',approved[i,7],'"',',"TRUE"','),')
          }
          else{
            vals<-paste0(vals,'("',approved[i,1],'",','"',approved[i,2],'"',',','"',approved[i,3],
                         '","',approved[i,4],'","',approved[i,5],'",','"',approved[i,6],'"',',','"',approved[i,7],'"',',"FALSE"','),')
          }
        }
        vals<-substr(vals,1,nchar(vals)-1)
        query<-paste('Insert Ignore into annotations_classification Values',vals,' ON DUPLICATE KEY UPDATE status="approved" , timestamp="',Sys.time(),'";',sep="")
        
        try({
          write_to_MariaDB(mydb,query)
        })
      }
    }
    if(learning_meta$context_unit=="Document"){
      not_approved<-stringr::str_split(string = data[which(data[,"approved"]==FALSE),1],pattern = "_",simplify = T)
      not_approved<-cbind(not_approved,"0")
    }
    else{
      not_approved<-stringr::str_split(string = data[which(data[,"approved"]==FALSE),1],pattern = "_",simplify = T)
    }
    
    if(dim(not_approved)[1]>0){
      for(i in 1:dim(not_approved)[1]){
        delete_from_MariaDB(mydb = mydb,query = paste0('Delete from annotations_classification where dataset="',not_approved[i,1],'" and doc_id=',not_approved[i,2],' and sid=',not_approved[i,3],
                                                       ' and project="',input$project_selected,'" and category="',learning_meta$category,'" and status="approved";'))
      }
    }
    #save ignored
    if(learning_meta$context_unit=="Document"){
      ignored<-stringr::str_split(string = data[which(data[,"ignored"]==TRUE),1],pattern = "_",simplify = T)
      ignored<-cbind(ignored,"0")
    }
    else{
      ignored<-stringr::str_split(string = data[which(data[,"ignored"]==TRUE),1],pattern = "_",simplify = T)
    }
    if(dim(ignored)[1]>0){
      ignored<-data.frame(ignored,project=input$project_selected,class=learning_meta$category,status="ignored",stringsAsFactors = F)
      colnames(ignored)<-c("dataset","doc_id","sid","project","category","status")
      class(ignored$doc_id)<-"integer"
      class(ignored$sid)<-"integer"
      known<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from annotations_classification where project='",input$project_selected,"' and category='",learning_meta$category,"' and status='ignored';"))[,1:6]
      ignored<-dplyr::anti_join(x = ignored,known)
      if(dim(ignored)[1]>0){
        ignored<-data.frame(ignored,timestamp=Sys.time())
        colnames(ignored)<-c("dataset","doc_id","sid","project","category","status","timestamp")
        vals<-""
        for(i in 1:dim(ignored)[1]){
          if(learning_meta$context_unit=="Document"){
            vals<-paste0(vals,'("',ignored[i,1],'",','"',ignored[i,2],'"',',','"',ignored[i,3],
                         '","',ignored[i,4],'","',ignored[i,5],'",','"',ignored[i,6],'"',',','"',ignored[i,7],'"',',"TRUE"','),')
          }
          else{
            vals<-paste0(vals,'("',ignored[i,1],'",','"',ignored[i,2],'"',',','"',ignored[i,3],
                         '","',ignored[i,4],'","',ignored[i,5],'",','"',ignored[i,6],'"',',','"',ignored[i,7],'"',',"FALSE"','),')
          }
        }
        vals<-substr(vals,1,nchar(vals)-1)
        query<-paste('Insert Ignore into annotations_classification Values',vals,' ON DUPLICATE KEY UPDATE status="ignored", timestamp="',Sys.time(),'";',sep="")
        
        try({
          write_to_MariaDB(mydb,query)
        })
      }
    }
    if(learning_meta$context_unit=="Document"){
      not_ignored<-stringr::str_split(string = data[which(data[,"ignored"]==FALSE),1],pattern = "_",simplify = T)
      not_ignored<-cbind(not_ignored,"0")
    }
    else{
      not_ignored<-stringr::str_split(string = data[which(data[,"ignored"]==FALSE),1],pattern = "_",simplify = T)
    }
    if(dim(not_ignored)[1]>0){
      for(i in 1:dim(not_ignored)[1]){
        delete_from_MariaDB(mydb = mydb,query = paste0('Delete from annotations_classification where dataset="',not_ignored[i,1],'" and doc_id=',not_ignored[i,2],' and sid=',not_ignored[i,3],
                                                       ' and project="',input$project_selected,'" and category="',learning_meta$category,'"and status="ignored";'))
      }
    }
    #save denied
    
    if(learning_meta$context_unit=="Document"){
      denied<-stringr::str_split(string = data[which(data[,"denied"]==TRUE),1],pattern = "_",simplify = T)
      denied<-cbind(denied,"0")
    }
    else{
      denied<-stringr::str_split(string = data[which(data[,"denied"]==TRUE),1],pattern = "_",simplify = T)
    }
    if(dim(denied)[1]>0){
      denied<-data.frame(denied,project=input$project_selected,class=learning_meta$category,status=paste("denied",data[which(data[,"denied"]==TRUE),6],sep="_"),stringsAsFactors = F)
      colnames(denied)<-c("dataset","doc_id","sid","project","category","status")
      class(denied$doc_id)<-"integer"
      class(denied$sid)<-"integer"
      known<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from annotations_classification where project='",input$project_selected,"' and category='",learning_meta$category,"' and status LIKE 'denied%';"))[,1:6]
      denied<-dplyr::anti_join(x = denied,known)
      if(dim(denied)[1]>0){
        denied<-data.frame(denied,timestamp=Sys.time())
        colnames(denied)<-c("dataset","doc_id","sid","project","category","status","timestamp")
        vals<-""
        for(i in 1:dim(denied)[1]){
          if(learning_meta$context_unit=="Document"){
            vals<-paste0(vals,'("',denied[i,1],'",','"',denied[i,2],'"',',','"',denied[i,3],
                         '","',denied[i,4],'","',denied[i,5],'",','"',denied[i,6],'"',',','"',denied[i,7],'"',',"TRUE"','),')
          }
          else{
            vals<-paste0(vals,'("',denied[i,1],'",','"',denied[i,2],'"',',','"',denied[i,3],
                         '","',denied[i,4],'","',denied[i,5],'",','"',denied[i,6],'"',',','"',denied[i,7],'"',',"FALSE"','),')
          }
        }
        vals<-substr(vals,1,nchar(vals)-1)
        query<-paste('Replace into annotations_classification Values',vals,';',sep="")
        
        try({
          write_to_MariaDB(mydb,query)
        })
      }
    }
    if(learning_meta$context_unit=="Document"){
      not_denied<-stringr::str_split(string = data[which(data[,"denied"]==FALSE),1],pattern = "_",simplify = T)
      not_denied<-cbind(not_denied,"0")
    }
    else{
      not_denied<-stringr::str_split(string = data[which(data[,"denied"]==FALSE),1],pattern = "_",simplify = T)
    }
    if(dim(not_denied)[1]>0){
      for(i in 1:dim(not_denied)[1]){
        delete_from_MariaDB(mydb = mydb,query = paste0('Delete from annotations_classification where dataset="',not_denied[i,1],'" and doc_id=',not_denied[i,2],' and sid=',not_denied[i,3],
                                                       ' and project="',input$project_selected,'" and category="',learning_meta$category,'"and status LIKE "denied%";'))
      }
    }
    RMariaDB::dbDisconnect(conn = mydb)
    shinyWidgets::sendSweetAlert(session=session,title = "Changes saved",type = "success")
    removeModal()
    values$Class_update_classifications<-runif(1,0,1)
  })
})

