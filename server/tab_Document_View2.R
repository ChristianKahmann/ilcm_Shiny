values$Doc_anno_loaded<-FALSE
#render title of document
output$Doc_DV_title<-renderText({
  title<- values$Doc_meta$title
  return(title)
})

#create reactive object, which stores the made annotations in the document
values$Doc_annotations_marked<-matrix(c(0),0,13)
values$Doc_annotations_show<-matrix(c(0),0,13)



#render the document
output$Doc_document<-renderUI({
  values$Doc_Doc_reload
  validate(
      need(
      !is.null(values$Doc_token),"no document specified"
    )
  )
  
  input$Doc_anno_id
  values$Doc_anno_deleted
  if(isolate(is.null(isolate(values$Doc_new)))){
    #get Annotations from db for this document
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
    rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
    annotations<-RMariaDB::dbGetQuery(mydb, paste("select * from Annotations where id='",(values$Doc_token)[1,2],"'",
                                                  " and trim(dataset)='",isolate(values$Doc_token)[1,1],"' and document_annotation='FALSE';",sep = ""))
    
    annotations_documentwide<-RMariaDB::dbGetQuery(mydb, paste("select * from Annotations where id='",(values$Doc_token)[1,2],"'",
                                                               " and trim(dataset)='",isolate(values$Doc_token)[1,1],"' and document_annotation='TRUE';",sep = ""))
    RMariaDB::dbDisconnect(mydb)
    annotations<-annotations[which(annotations[,"Anno_set"]==input$Doc_anno_scheme_selected),1:13]
    annotations_documentwide<-annotations_documentwide[which(annotations_documentwide[,"Anno_set"]==input$Doc_anno_scheme_selected),1:13]
    values$Doc_annos_documentwide<-data.frame(name=annotations_documentwide$Annotation,user=annotations_documentwide$User,color=annotations_documentwide$color,
                                          annotation_scheme=annotations_documentwide$Anno_set,id=annotations_documentwide$anno_id,stringsAsFactors = F)
      #no annotations made so far?
    if(dim(annotations)[1]==0){
      
      if(dim(annotations_documentwide)[1]>0){
        isolate(values$Doc_annotations_show<-rbind(isolate(values$Doc_annotations_show),as.matrix(annotations_documentwide)))
      }
      
      values$Doc_show<-paste(isolate(values$Doc_token)[,"word"],collapse = " ")
      if(is.null(isolate(values$Doc_annos))){
        isolate(values$Doc_annos<-matrix(c(0),0,6))
      }
    }
    #re-create annotation markup
    else{
      #save text
      marked_text<-(values$Doc_token)[,"word"]
      #load annotation sheme
      db_annotations<-annotations
      colnames(db_annotations)<-colnames(isolate(values$Doc_annotations_show))
      isolate(values$Doc_annotations_show<-rbind(isolate(values$Doc_annotations_show),as.matrix(db_annotations)))
      #add color tags to text
      if(is.null(isolate(values$Doc_annos))){
        isolate(values$Doc_annos<-matrix(c(0),0,6))
      }
      #get indices of words with annotations
      indis<-NULL
      names<-NULL
      user<-NULL
      color<-NULL
      scheme<-NULL
      ids<-NULL
      for(i in 1:dim(annotations)[1]){
        indis<-c(indis,(annotations[i,5]:annotations[i,6]))
        names<-c(names,rep(annotations[i,7],length(annotations[i,5]:annotations[i,6])))
        user<-c(user,rep(annotations[i,2],length(annotations[i,5]:annotations[i,6])))
        color<-c(color,rep(annotations[i,8],length(annotations[i,5]:annotations[i,6])))
        scheme<-c(scheme,rep(annotations[i,10],length(annotations[i,5]:annotations[i,6])))
        ids<-c(ids,rep(annotations[i,1],length(annotations[i,5]:annotations[i,6])))
      }
      annos<-matrix(c(0),length(indis),6)
      annos[,1]<-indis
      annos[,2]<-names
      annos[,3]<-user
      annos[,4]<-color
      annos[,5]<-scheme
      annos[,6]<-ids
      isolate(values$Doc_annos<-rbind(isolate(values$Doc_annos),annos))
      #paste words together
      values$Doc_show<-paste(marked_text,collapse=" ")
    }
  }
  #add made annotations to anno storage variables
  else{
    if(!is.null(isolate(input$Doc_anno_tag))){
      load(file = paste0("collections/annotation_schemes/",isolate(input$Doc_anno_scheme_selected),".RData"))
      names<-names(unlist(anno))
      anno<-unlist(anno)
      name_color<-names[which(grepl(pattern = paste0(isolate(input$Doc_anno_tag),".color"),x = names))]
      color<-anno[name_color]
      name_isDocumentAnnotation<-names[which(grepl(pattern = paste0(isolate(input$anno_tag),".isDocumentAnnotation"),x = names))]
      isDocumentAnnotation<-anno[name_isDocumentAnnotation]
      name_tag<-names[which(grepl(pattern = paste0(isolate(input$Doc_anno_tag),".name"),x = names))]
      name<-anno[name_tag]
      id<-uuid::UUIDgenerate(use.time = T)
      if(isDocumentAnnotation=="FALSE"){
      if(!is.null(isolate(input$Doc_anno_start))){
        for(k in isolate(input$Doc_anno_start):isolate(input$Doc_anno_end)){
          isolate(values$Doc_annos<-rbind(isolate(values$Doc_annos),c(k,name,values$user,color,input$Doc_anno_scheme_selected,id)))
        }
        indi<-isolate(input$Doc_anno_start):isolate(input$Doc_anno_end)
        new_values<-c(id,values$user,isolate(values$Doc_meta$dataset),isolate(values$Doc_selected),isolate(input$Doc_anno_start),isolate(input$Doc_anno_end),
                      name,color,as.character(Sys.time()),isolate(input$Doc_anno_scheme_selected),values$current_collection,as.numeric(isolate(values$Doc_meta)[1,"id"]),"FALSE")
        values$Doc_annotations_marked<-rbind(isolate(values$Doc_annotations_marked),new_values)
        #colnames(new_values)<-colnames(isolate(values$annotations_show))
        values$Doc_annotations_show<-rbind(isolate(values$Doc_annotations_show),new_values)
      }
      }
      else{
        isolate(values$Doc_annos_documentwide[nrow(values$Doc_annos_documentwide)+1,]<-c(name,values$user,color,input$Doc_anno_scheme_selected,id))
        isolate(colnames(values$Doc_annos_documentwide)<-c("name","user","color","annotation_scheme","id"))
        
        
        new_values<-c(id,values$user,isolate(values$Doc_meta$dataset),isolate(values$Doc_selected),1,dim(values$Doc_token)[1],
                      name,color,as.character(Sys.time()),isolate(input$Doc_anno_scheme_selected),values$current_collection,as.numeric(isolate(values$Doc_meta)[1,"id"]),"TRUE")
        values$Doc_annotations_marked<-rbind(isolate(values$Doc_annotations_marked),new_values)
        #colnames(new_values)<-colnames(isolate(values$annotations_show))
        values$Doc_annotations_show<-rbind(isolate(values$Doc_annotations_show),new_values)
      }
      shinyjs::useShinyjs()
      isolate(shinyjs::runjs('Shiny.onInputChange(\"Doc_anno_tag\",  null)'))
    }
  }
  #check weather Pos or Entity Tags are schosen
  if(input$Doc_DV_POS!="None" && !is.null(input$Doc_DV_POS)){
    values$Doc_mark_pos<-which(values$Doc_token[,"pos"]==input$Doc_DV_POS)
  }
  else{
    values$Doc_mark_pos<-matrix(c(0),0,1)
  }
  if(input$Doc_DV_Entity!="None" && !is.null(input$Doc_DV_Entity)){
    values$Doc_mark_ner<-which(stringr::str_replace_all(string = isolate(values$Doc_token[,"entity"]),pattern = "_[A-Z]$",replacement = "")==input$Doc_DV_Entity)
  }
  else{
    values$Doc_mark_ner<-matrix(c(0),0,1)
  }
  if(input$Doc_Doc_View_paragraph==T){
    values$Doc_mark_space<-which(values$Doc_token[,7]=="SPACE")
  }
  else{
    values$Doc_mark_space<-NULL
  }
  counter <- 1
  text<-lapply(isolate(values$Doc_show), function(x) {
    strings <- stringr::str_split(x, pattern = "\\s")[[1]]
    strings<-add_tags(strings,isolate(values$Doc_annos)[which(isolate(values$Doc_annos[,5])==input$Doc_anno_scheme_selected),,drop=F],isolate(values$Doc_mark_pos),isolate(values$Doc_mark_ner),isolate(values$Doc_mark_space),values$Doc_highlight_annos)
    a<-list()
    for(i in 1:length(strings)){
      a[[i]]<-paste0("<span span_nr='",i,"'>",strings[i],"</span>")
    }
    a<-do.call(rbind,a)
    a<-HTML(a)
    tags$p(a)
  })
  text<-tagList(text)
  isolate(values$Doc_new<-1)
  return(text)
  
})

#render metadata 
output$Doc_DV_metadata_UI<-renderUI({
  validate(
    need(!is.null(values$Doc_meta),message=F)
  )
  tag<-list()
  for(i in 1:length(values$Doc_meta)){
    tag[[i]]<-tags$div(
      tags$h5(tags$b(names(values$Doc_meta)[i])),
      tags$div(values$Doc_meta[i])
    )
  }
  return(tagList(tag))
})


#render select options for POS Tags
output$Doc_DV_POS<-renderUI({
  options<-c("None",unique(values$Doc_token[,"pos"]))
  radioButtons(inputId = "Doc_DV_POS",label = "POS-TAGS",choices = options,selected = "None")
})

#render select options for Entity Tags
output$Doc_DV_Entity<-renderUI({
  options<-c("None",unique(values$Doc_token[,"entity"]))
  options<-options[-which(nchar(options)<2)]
  options<-stringr::str_replace_all(string = options,pattern = "_[A-Z]$",replacement = "")
  options<-unique(options)
  radioButtons(inputId = "Doc_DV_Entity",label = "Entity-TAGS",choices = options,selected = "None")
})

observeEvent(eventExpr = input$Doc_anno_scheme_selected,handlerExpr = {
  values$Doc_new<-NULL
},ignoreInit = T,priority = 1)


###########################
#      Annotations        #
###########################

output$Doc_DV_Annotation_Schemes<-renderUI({
  values$newscheme
  validate(validate(need(
    length(list.files("collections/annotation_schemes/"))>0,message="No annotation schemes found. You can create schemes in the 'Categories' tab."
  )))
  selectizeInput(inputId = "Doc_anno_scheme_selected",label="Which Annotation Scheme?",choices=stringr::str_replace_all(list.files("collections/annotation_schemes/"),".RData","")) 
})


observeEvent(input$Doc_save_annotations,{
  if(dim(isolate(values$Doc_annotations_marked))[1]==0){
    shinyWidgets::sendSweetAlert(session = session,title = "no annotations found",type = "warning")
  }
  else{
    #save Annotations to DB
    vals<-""
    token<-isolate(values$Doc_token)
    token[,5]<-stringr::str_replace_all(string = token[,5],pattern = '"',replacement = "'")
    for(i in 1:dim(isolate(values$Doc_annotations_marked))[1]){
      if(values$Doc_annotations_marked[i,13]=="FALSE"){
        vals<-paste0(vals,'("',isolate(values$Doc_annotations_marked[i,1]),'",','"',isolate(values$Doc_annotations_marked[i,2]),'"',',','"',isolate(values$Doc_annotations_marked[i,3]),'",',isolate(values$Doc_annotations_marked[i,4]),',',isolate(values$Doc_annotations_marked[i,5]),',','"',isolate(values$Doc_annotations_marked[i,6]),'"',',','"',isolate(values$Doc_annotations_marked[i,7]),'"',',','"',isolate(values$Doc_annotations_marked[i,8]),'"',',','"',isolate(values$Doc_annotations_marked[i,9]),'"',',','"',isolate(values$Doc_annotations_marked[i,10]),'"',',','"',isolate(values$Doc_annotations_marked[i,11]),'"',' , ',isolate(values$Doc_annotations_marked[i,12]),' ,"',paste(token[isolate(values$Doc_annotations_marked)[i,5]:isolate(values$Doc_annotations_marked[i,6]),5],collapse=" "),'"',' , "',isolate(values$Doc_annotations_marked[i,13]),'"),')
      }
      else{
        vals<-paste0(vals,'("',isolate(values$Doc_annotations_marked[i,1]),'",','"',isolate(values$Doc_annotations_marked[i,2]),'"',',','"',isolate(values$Doc_annotations_marked[i,3]),'",',isolate(values$Doc_annotations_marked[i,4]),',',isolate(values$Doc_annotations_marked[i,5]),',','"',isolate(values$Doc_annotations_marked[i,6]),'"',',','"',isolate(values$Doc_annotations_marked[i,7]),'"',',','"',isolate(values$Doc_annotations_marked[i,8]),'"',',','"',isolate(values$Doc_annotations_marked[i,9]),'"',',','"',isolate(values$Doc_annotations_marked[i,10]),'"',',','"',isolate(values$Doc_annotations_marked[i,11]),'"',' , ',isolate(values$Doc_annotations_marked[i,12]),' ,"',paste0(substr(paste(token[isolate(values$Doc_annotations_marked)[i,5]:isolate(values$Doc_annotations_marked[i,6]),5],collapse=" "),1,100),"..."),'"',' , "',isolate(values$Doc_annotations_marked[i,13]),'"),')
      }
    }
    vals<-substr(vals,1,nchar(vals)-1)
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
    query<-paste('Insert Ignore into Annotations Values',vals,';',sep="")
    
    try({
      write_to_MariaDB(mydb,query)
    })
    RMariaDB::dbDisconnect(mydb)
    values$Doc_anno_loaded<-TRUE
    shinyWidgets::sendSweetAlert(type = "success",session = session,title = paste0( dim(values$Doc_made_annotations)[1]," Annotations saved"))
  }
})

output$Doc_Legend_Table<-renderDataTable({
  try({
    load(paste("collections/annotation_schemes/",input$Doc_anno_scheme_selected,sep=""))
    data<-t(anno[,])
    data<-datatable(data.frame(data[1,]),rownames = F,colnames=NULL,width = 20,height = 50,autoHideNavigation = T,options = list(dom='t'),selection = "single")
    data<-formatStyle(table = data ,backgroundColor = styleEqual(unique(anno[,1]),anno[,2]),fontWeight = "bold",columns = 1,color="white")
    return(data)  
    
  },silent = T)
  
})

output$Doc_Legend<-renderUI({
  dataTableOutput(outputId = "Doc_Legend_Table")
})

s_alt<-0
output$Doc_p2<-renderDataTable({ 
  s = input$Doc_Legend_Table_rows_selected
  
  if (length(s)) {
    load(paste("collections/annotation_schemes/",input$Doc_anno_scheme_selected,sep=""))
    
    values$Doc_anno_all<-anno
    
    values$Doc_anno<-values$Doc_anno_all[s,]
    values$Doc_new<-1
    proxy = dataTableProxy('Doc_Legend_Table')
    proxy %>% selectRows(NULL)
    values$Doc_Legend_new<-runif(1)
    return(NULL)
  }
  return(NULL)
  
})



output$annotationComponents2<-renderUI({
  validate(
    need(!is.null(input$Doc_anno_scheme_selected),message=FALSE)
  )
  load(paste0("collections/annotation_schemes/",input$Doc_anno_scheme_selected,".RData"))
  annotateTextComponent_div2(anno)
})




output$Doc_made_annotations<-renderUI({
  validate(
    need(dim(values$Doc_annotations_show)[1]>0,message=FALSE)
  )
  anno<-values$Doc_annotations_show[which(values$Doc_annotations_show[,10]==(input$Doc_anno_scheme_selected)),,drop=F]
  rownames(anno)<-rep("",dim(anno)[1])
  anno<-unique(anno)
  if(dim(anno)[1]>0){
    div_list<-lapply(X = 1:dim(anno)[1],FUN = function(x){
      user<-anno[x,2]
      class<-anno[x,7]
      color<-anno[x,8]
      schema<-anno[x,10]
      square_id<-paste0("D",as.character(anno[x,1]))
      shinyjs::onevent(event = "mouseenter",id = square_id,expr = {
        #print(paste("test rein",square_id))
        highlight_annos<-matrix(c(0),length(anno[x,5]:anno[x,6]),2)
        highlight_annos[,1]<-anno[x,5]:anno[x,6]
        highlight_annos[,2]<-color
        values$Doc_highlight_annos<- highlight_annos
        values$Doc_delete_Doc_box_id<-as.character(anno[x,1])
      }
      )
      shinyjs::onevent(event = "mouseleave",id = square_id,expr = {
        #print(paste("test raus",square_id))
        values$Doc_highlight_annos<-NULL
      }
      )
      shinyjqui::jqui_draggable(tags$div(id=square_id,class="anno-box",style=paste0('width:2em;height:2em;border:1px solid #ccc; margin:1em 1em 0.5em 0;display:inline-block;background: ',color),title=paste(class," (annotated by ",user,")")),options = list(revert=T))
    })
    do.call(tagList,div_list)
  }
  else{
    return(NULL)
  }
})

#check whether an annotation box was drop in the trash div, if this is
observe({
  validate(
    need(!is.null(input$Doc_delete_annotation_box),message=FALSE)
  )
  shinyWidgets::confirmSweetAlert(session = session,inputId = "Doc_confirm_delete_Doc_box",type = "warning",title = "Are you sure you want to delete this annotation",danger_mode = T)
})

#if user has confiremd to delete annotation, delete the annotation in the database and also the annotation matrices
observeEvent(input$Doc_confirm_delete_Doc_box,{
  if(isTRUE(input$Doc_confirm_delete_Doc_box)){
    values$Doc_annotations_marked<-isolate(values$Doc_annotations_marked[-which(isolate(values$Doc_annotations_marked[,1]==isolate(values$Doc_delete_Doc_box_id))),,drop=F])
    values$Doc_annotations_show<-isolate(values$Doc_annotations_show[-which(isolate(values$Doc_annotations_show[,1]==isolate(values$Doc_delete_Doc_box_id))),,drop=F])
    values$Doc_annos_documentwide<-isolate(values$Doc_annos_documentwide[-which(isolate(values$Doc_annos_documentwide$id==isolate(values$Doc_delete_anno_box_id))),,drop=F])
    values$Doc_annos<-isolate(values$Doc_annos[-which(isolate(values$Doc_annos[,6]==isolate(values$Doc_delete_Doc_box_id))),,drop=F])
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
    write_to_MariaDB(mydb = mydb,query =paste0('Delete from Annotations where Anno_id="',isolate(values$Doc_delete_Doc_box_id),'";') )
    RMariaDB::dbDisconnect(mydb)
    values$Doc_anno_deleted<-runif(1,0,1)
  }
}
)


output$Doc_DV_documentwide_annotations<-renderUI({
  validate(
    need(
      dim(values$Doc_annos_documentwide)[1]>0,message=F
    )
  )
  data<-values$Doc_annos_documentwide
  icon_tags<-lapply(1:dim(data)[1],FUN = function(x){
    style=paste0( "color: ",data$color[x],"; font-size:1.8em;")
    title=paste0(data$name[x]," (annotated by ",data$user[x],")")
    return( tags$i(
      class = "fa fa-certificate", 
      style = style,
      title=title
    ))
  })
  return(tagList(tags$h6("Document annotations:"),icon_tags))
  
})