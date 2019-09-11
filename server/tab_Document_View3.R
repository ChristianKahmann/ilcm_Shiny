
output$Anno_DV_title<-renderText({
  title<- values$Anno_meta[,"title"]
  return(title)
})

#create reactive object, which stores the made annotations in the document
values$Anno_annotations_marked<-matrix(c(0),0,12)
values$Anno_annotations_show<-matrix(c(0),0,12)



#render the document
output$Anno_document<-renderUI({
  values$Anno_token[1,1]
  validate(
    need(
      !is.null(values$Anno_token),"no document specified"
    )
  )
  input$Anno_anno_id
  values$Anno_anno_deleted
  #check if document was opened in this session before
  if(isolate(is.null(isolate(values$Anno_new)))){
    #get Annotations from db for this document
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
    rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
    annotations<-RMariaDB::dbGetQuery(mydb, paste("select * from Annotations where id='",(values$Anno_token)[1,"id"],"'",
                                                  " and trim(dataset)='",isolate(values$Anno_token)[1,"dataset"],"';",sep = ""))
    RMariaDB::dbDisconnect(mydb)
    annotations<-annotations[which(annotations[,"Anno_set"]==input$Anno_anno_scheme_selected),1:12]
    #no annotations made so far?
    if(dim(annotations)[1]==0){
      values$Anno_show<-paste(isolate(values$Anno_token)[,"word"],collapse = " ")
      if(is.null(isolate(values$Doc_annos))){
        isolate(values$Anno_annos<-matrix(c(0),0,6))
      }
    }
    #re-create annotation markup
    else{
      #save text
      marked_text<-isolate(values$Anno_token)[,"word"]
      #load annotation sheme
      db_annotations<-annotations
      colnames(db_annotations)<-colnames(isolate(values$Anno_annotations_show))
      isolate(values$Anno_annotations_show<-rbind(isolate(values$Anno_annotations_show),as.matrix(db_annotations)))
      #add color tags to text
      if(is.null(isolate(values$Anno_annos))){
        isolate(values$Anno_annos<-matrix(c(0),0,6))
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
      isolate(values$Anno_annos<-rbind(isolate(values$Anno_annos),annos))
      #paste words together
      values$Anno_show<-paste(marked_text,collapse=" ")
    }
  }
  #add made annotations to anno storage variables
  else{
    if(!is.null(isolate(input$Anno_anno_tag))){
      load(file = paste0("collections/annotation_schemes/",isolate(input$Anno_anno_scheme_selected),".RData"))
      names<-names(unlist(anno))
      anno<-unlist(anno)
      name_color<-names[which(grepl(pattern = paste0(isolate(input$Anno_anno_tag),".color"),x = names))]
      color<-anno[name_color]
      name_tag<-names[which(grepl(pattern = paste0(isolate(input$Anno_anno_tag),".name"),x = names))]
      name<-anno[name_tag]
      id<-uuid::UUIDgenerate(use.time = T)
      if(!is.null(isolate(input$Anno_anno_start))){
        for(k in isolate(input$Anno_anno_start):isolate(input$Anno_anno_end)){
          isolate(values$Anno_annos<-rbind(isolate(values$Anno_annos),c(k,name,values$user,color,input$Anno_anno_scheme_selected,id)))
        }
        indi<-isolate(input$Anno_anno_start):isolate(input$Anno_anno_end)
        new_values<-c(id,values$user,isolate(values$Anno_meta$dataset),isolate(values$Anno_selected),isolate(input$Anno_anno_start),isolate(input$Anno_anno_end),name,color,as.character(Sys.time()),isolate(input$Anno_anno_scheme_selected),"no_collection",as.numeric(isolate(values$Anno_meta)[1,"id"]))
        values$Anno_annotations_marked<-rbind(isolate(values$Anno_annotations_marked),new_values)
        #colnames(new_values)<-colnames(isolate(values$annotations_show))
        values$Anno_annotations_show<-rbind(isolate(values$Anno_annotations_show),new_values)
      }
      shinyjs::useShinyjs()
      isolate(shinyjs::runjs('Shiny.onInputChange(\"Anno_anno_tag\",  null)'))
    }
  }
  #check weather Pos or Entity Tags are schosen
  if(input$Anno_DV_POS!="None" && !is.null(input$Anno_DV_POS)){
    values$Anno_mark_pos<-which(values$Anno_token[,"pos"]==input$Anno_DV_POS)
  }
  else{
    values$Anno_mark_pos<-matrix(c(0),0,1)
  }
  if(input$Anno_DV_Entity!="None" && !is.null(input$Anno_DV_Entity)){
    values$Anno_mark_ner<-which(stringr::str_replace_all(string = isolate(values$Anno_token[,"entity"]),pattern = "_[A-Z]$",replacement = "")==input$Anno_DV_Entity)
  }
  else{
    values$Anno_mark_ner<-matrix(c(0),0,1)
  }
  if(input$Anno_Doc_View_paragraph==T){
    values$Anno_mark_space<-which(values$Anno_token[,7]=="SPACE")
  }
  else{
    values$Anno_mark_space<-NULL
  }
  counter <- 1
  text<-lapply(isolate(values$Anno_show), function(x) {
    strings <- stringr::str_split(x, pattern = "\\s")[[1]]
    strings<-add_tags(strings,isolate(values$Anno_annos)[which(isolate(values$Anno_annos[,5])==input$Anno_anno_scheme_selected),,drop=F],isolate(values$Anno_mark_pos),isolate(values$Anno_mark_ner),isolate(values$Anno_mark_space),values$Anno_highlight_annos)
    a<-list()
    for(i in 1:length(strings)){
      a[[i]]<-paste0("<span span_nr='",i,"'>",strings[i],"</span>")
    }
    a<-do.call(rbind,a)
    a<-HTML(a)
    tags$p(a)
  })
  text<-tagList(text)
  values$Anno_new<-1
  return(text)
  
})

#render metadata 
output$Anno_DV_metadata_UI<-renderUI({
  validate(
    need(!is.null(values$Anno_meta),message=F)
  )
  tag<-list()
  for(i in 1:length(values$Anno_meta)){
    tag[[i]]<-tags$div(
      tags$h5(tags$b(names(values$Anno_meta)[i])),
      tags$div(values$Anno_meta[i])
    )
  }
  return(tagList(tag))
})

#render select options for POS Tags
output$Anno_DV_POS<-renderUI({
  options<-c("None",unique(values$Anno_token[,"pos"]))
  radioButtons(inputId = "Anno_DV_POS",label = "POS-TAGS",choices = options,selected = "None")
})

#render select options for Entity Tags
output$Anno_DV_Entity<-renderUI({
  options<-c("None",unique(values$Anno_token[,"entity"]))
  options<-options[-which(nchar(options)<2)]
  options<-stringr::str_replace_all(string = options,pattern = "_[A-Z]$",replacement = "")
  options<-unique(options)
  radioButtons(inputId = "Anno_DV_Entity",label = "Entity-TAGS",choices = options,selected = "None")
})

observeEvent(input$Anno_anno_scheme_selected,{
  values$Anno_new<-NULL
})


###########################
#      Annotations        #
###########################


output$Anno_DV_Annotation_Schemes<-renderUI({
  values$newscheme
  selectizeInput(inputId = "Anno_anno_scheme_selected",label="Which Annotation Scheme?",choices=stringr::str_replace_all(list.files("collections/annotation_schemes/"),".RData","")) 
})



observeEvent(input$Anno_save_annotations,{
  if(dim(isolate(values$Anno_annotations_marked))[1]==0){
    shinyWidgets::sendSweetAlert(session = session,title = "no annotations found",type = "warning")
  }
  else{
    #save Annotations to DB
    vals<-""
    for(i in 1:dim(isolate(values$Anno_annotations_marked))[1]){
      vals<-paste0(vals,'("',isolate(values$Anno_annotations_marked[i,1]),'",','"',isolate(values$Anno_annotations_marked[i,2]),'"',',','"',isolate(values$Anno_annotations_marked[i,3]),'",',isolate(values$Anno_annotations_marked[i,4]),',',isolate(values$Anno_annotations_marked[i,5]),',','"',isolate(values$Anno_annotations_marked[i,6]),'"',',','"',isolate(values$Anno_annotations_marked[i,7]),'"',',','"',isolate(values$Anno_annotations_marked[i,8]),'"',',','"',isolate(values$Anno_annotations_marked[i,9]),'"',',','"',isolate(values$Anno_annotations_marked[i,10]),'"',',','"',isolate(values$Anno_annotations_marked[i,11]),'"',' , ',isolate(values$Anno_annotations_marked[i,12]),' ,"',paste(isolate(values$Anno_token)[isolate(values$Anno_annotations_marked)[i,5]:isolate(values$Anno_annotations_marked[i,6]),5],collapse=" "),'"),')
    }
    vals<-substr(vals,1,nchar(vals)-1)
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
    query<-paste('Insert Ignore into Annotations Values',vals,';',sep="")
    
    try({
      write_to_MariaDB(mydb,query)
    })
    RMariaDB::dbDisconnect(mydb)
    values$Anno_anno_loaded<-TRUE
    shinyWidgets::sendSweetAlert(type = "success",session = session,title = paste0( dim(values$Anno_made_annotations)[1]," Annotations saved"))
  }
})


output$annotationComponents3<-renderUI({
  validate(
    need(!is.null(input$Anno_anno_scheme_selected),message=FALSE)
  )
  load(paste0("collections/annotation_schemes/",input$Anno_anno_scheme_selected,".RData"))
  annotateTextComponent_div3(anno)
})



output$Anno_made_annotations<-renderUI({
  validate(
    need(dim(values$Anno_annotations_show)[1]>0,message=FALSE)
  )
  anno<-values$Anno_annotations_show[which(values$Anno_annotations_show[,10]==(input$Anno_anno_scheme_selected)),,drop=F]
  rownames(anno)<-rep("",dim(anno)[1])
  anno<-unique(anno)
  if(dim(anno)[1]>0){
    div_list<-lapply(X = 1:dim(anno)[1],FUN = function(x){
      user<-anno[x,2]
      class<-anno[x,7]
      color<-anno[x,8]
      schema<-anno[x,10]
      square_id<-paste0("A",as.character(anno[x,1]))
      shinyjs::onevent(event = "mouseenter",id = square_id,expr = {
        #print(paste("test rein",square_id))
        highlight_annos<-matrix(c(0),length(anno[x,5]:anno[x,6]),2)
        highlight_annos[,1]<-anno[x,5]:anno[x,6]
        highlight_annos[,2]<-color
        values$Anno_highlight_annos<- highlight_annos
        values$Anno_delete_anno_box_id<-as.character(anno[x,1])
      }
      )
      shinyjs::onevent(event = "mouseleave",id = square_id,expr = {
        #print(paste("test raus",square_id))
        values$Anno_highlight_annos<-NULL
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
    need(!is.null(input$Anno_delete_annotation_box),message=FALSE)
  )
  shinyWidgets::confirmSweetAlert(session = session,inputId = "Anno_confirm_delete_anno_box",type = "warning",title = "Are you sure you want to delete this annotation",danger_mode = T)
})

#if user has confiremd to delete annotation, delete the annotation in the database and also the annotation matrices
observeEvent(input$Anno_confirm_delete_anno_box,{
  if(isTRUE(input$Anno_confirm_delete_anno_box)){
    values$Anno_annotations_marked<-isolate(values$Anno_annotations_marked[-which(isolate(values$Anno_annotations_marked[,1]==isolate(values$Anno_delete_anno_box_id))),,drop=F])
    values$Anno_annotations_show<-isolate(values$Anno_annotations_show[-which(isolate(values$Anno_annotations_show[,1]==isolate(values$Anno_delete_anno_box_id))),,drop=F])
    values$Anno_annos<-isolate(values$Anno_annos[-which(isolate(values$Anno_annos[,6]==isolate(values$Anno_delete_anno_box_id))),,drop=F])
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
    write_to_MariaDB(mydb = mydb,query =paste0('Delete from Annotations where anno_id="',isolate(values$Anno_delete_anno_box_id),'";') )
    RMariaDB::dbDisconnect(mydb)
    values$Anno_anno_deleted<-runif(1,0,1)
  }
}
)

