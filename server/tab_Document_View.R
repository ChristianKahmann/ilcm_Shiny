values$anno_loaded<-FALSE
#render title of document
output$DV_title<-renderText({
  title<-(values$title)
  return((title))
})

#render metadata 
output$DV_metadata_UI<-renderUI({
  validate(
    need(!is.null(values$meta),message=F)
  )
  tag<-list()
  for(i in 1:length(values$meta)){
    tag[[i]]<-tags$div(
      tags$h5(tags$b(names(values$meta)[i])),
      tags$div(values$meta[i])
    )
  }
  return(tagList(tag))
})


#create reactive object, which stores the made annotations in the document
values$annotations_marked<-matrix(c(0),0,13)
values$annotations_show<-matrix(c(0),0,13)

#render the document
output$document<-renderUI({
  values$Doc_reload
  validate(
    need(
      !is.null(values$token),
      "no document specified"
    )
  )
  input$anno_id
  values$anno_deleted
  #check if document was opened in this session before
  if(isolate(is.null(isolate(values$new)))){
    #get Annotations from db for this document
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
    rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
    annotations<-RMariaDB::dbGetQuery(mydb, paste("select * from Annotations where id='",(values$token)[1,2],"'",
                                                  " and trim(dataset)='",isolate(values$token)[1,1],"' and document_annotation='FALSE';",sep = ""))
    
    annotations_documentwide<-RMariaDB::dbGetQuery(mydb, paste("select * from Annotations where id='",(values$token)[1,2],"'",
                                                               " and trim(dataset)='",isolate(values$token)[1,1],"' and document_annotation='TRUE';",sep = ""))
    RMariaDB::dbDisconnect(mydb)
    annotations<-annotations[which(annotations[,"Anno_set"]==input$anno_scheme_selected),1:13]
    annotations_documentwide<-annotations_documentwide[which(annotations_documentwide[,"Anno_set"]==input$anno_scheme_selected),1:13]
    values$annos_documentwide<-data.frame(name=annotations_documentwide$Annotation,user=annotations_documentwide$User,color=annotations_documentwide$color,
                                          annotation_scheme=annotations_documentwide$Anno_set,id=annotations_documentwide$anno_id,stringsAsFactors = F)

    #no annotations made so far?
    if(dim(annotations)[1]==0){
     
      if(dim(annotations_documentwide)[1]>0){
        isolate(values$annotations_show<-rbind(isolate(values$annotations_show),as.matrix(annotations_documentwide)))
      }
      values$show<-paste(isolate(values$token)[,"word"],collapse = " ")
      if(is.null(isolate(values$annos))){
        isolate(values$annos<-matrix(c(0),0,6))
      }
    }
    #re-create annotation markup
    else{
      #save text
      marked_text<-isolate(values$token)[,"word"]
      #load annotation sheme
      load(paste("collections/annotation_schemes/",annotations[1,10],".RData",sep=""))
      db_annotations<-annotations
      colnames(db_annotations)<-colnames(isolate(values$annotations_show))
      isolate(values$annotations_show<-rbind(isolate(values$annotations_show),as.matrix(db_annotations)))
      isolate(values$annotations_show<-rbind(isolate(values$annotations_show),as.matrix(annotations_documentwide)))
      
      #add color tags to text
      if(is.null(isolate(values$annos))){
        isolate(values$annos<-matrix(c(0),0,6))
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
      isolate(values$annos<-rbind(isolate(values$annos),annos))
      #paste words together
      values$show<-paste(marked_text,collapse=" ")
    }
  }
  #add made annotations to anno storage variables
  else{
    if(!is.null(isolate(input$anno_tag))){
      load(file = paste0("collections/annotation_schemes/",isolate(input$anno_scheme_selected),".RData"))
      names<-names(unlist(anno))
      anno<-unlist(anno)
      name_color<-names[which(grepl(pattern = paste0(isolate(input$anno_tag),".color"),x = names))]
      color<-anno[name_color]
      name_isDocumentAnnotation<-names[which(grepl(pattern = paste0(isolate(input$anno_tag),".isDocumentAnnotation"),x = names))]
      isDocumentAnnotation<-anno[name_isDocumentAnnotation]
      name_tag<-names[which(grepl(pattern = paste0(isolate(input$anno_tag),".name"),x = names))]
      name<-anno[name_tag]
      Anno_id<-uuid::UUIDgenerate(use.time = T)
      if(isDocumentAnnotation=="FALSE"){
        if(!is.null(isolate(input$anno_start))){
          for(k in isolate(input$anno_start):isolate(input$anno_end)){
            isolate(values$annos<-rbind(isolate(values$annos),c(k,name,values$user,color,input$anno_scheme_selected,Anno_id)))
          }
          indi<-isolate(input$anno_start):isolate(input$anno_end)
          new_values<-c(Anno_id,values$user,isolate(values$meta$dataset),isolate(values$selected),isolate(input$anno_start),isolate(input$anno_end),name,color,as.character(Sys.time()),isolate(input$anno_scheme_selected),"no_collection",as.numeric(isolate(values$meta)[1,"id"]),"FALSE")
          values$annotations_marked<-rbind(isolate(values$annotations_marked),new_values)
          #colnames(new_values)<-colnames(isolate(values$annotations_show))
          values$annotations_show<-rbind(isolate(values$annotations_show),new_values)
        }
      }
      else{
        isolate(values$annos_documentwide[nrow(values$annos_documentwide)+1,]<-c(name,values$user,color,input$anno_scheme_selected,Anno_id))
        isolate(colnames(values$annos_documentwide)<-c("name","user","color","annotation_scheme","id"))
        new_values<-c(Anno_id,values$user,isolate(values$meta$dataset),isolate(values$selected),1,dim(values$token)[1],name,color,as.character(Sys.time()),isolate(input$anno_scheme_selected),"no_collection",as.numeric(isolate(values$meta)[1,"id"]),"TRUE")
        values$annotations_marked<-rbind(isolate(values$annotations_marked),new_values)
        #colnames(new_values)<-colnames(isolate(values$annotations_show))
        values$annotations_show<-rbind(isolate(values$annotations_show),new_values)
      }
      shinyjs::useShinyjs()
      isolate(shinyjs::runjs('Shiny.onInputChange(\"anno_tag\",  null)'))
    }
  }
  #check weather Pos or Entity Tags are chosen
  if(input$DV_POS!="None" && !is.null(input$DV_POS)){
    values$mark_pos<-which(values$token[,"pos"]==input$DV_POS)
  }
  else{
    values$mark_pos<-matrix(c(0),0,1)
  }
  if(input$DV_Entity!="None" && !is.null(input$DV_Entity)){
    values$mark_ner<-which(stringr::str_replace_all(string = isolate(values$token[,"entity"]),pattern = "_[A-Z]$",replacement = "")==input$DV_Entity)
  }
  else{
    values$mark_ner<-matrix(c(0),0,1)
  }
  if(input$Doc_View_paragraph==T){
    values$mark_space<-which(values$token[,7]=="SPACE")
  }
  else{
    values$mark_space<-NULL
  }
  
  counter <- 1
  text<-lapply(isolate(values$show), function(x) {
    strings <- stringr::str_split(x, pattern = "\\s")[[1]]
    strings<-add_tags(strings,isolate(values$annos)[which(isolate(values$annos[,5])==input$anno_scheme_selected),,drop=F],isolate(values$mark_pos),isolate(values$mark_ner),isolate(values$mark_space),values$highlight_annos)
    a<-list()
    for(i in 1:length(strings)){
      a[[i]]<-paste0("<span span_nr='",i,"'>",strings[i],"</span>")
    }
    a<-do.call(rbind,a)
    a<-HTML(a)
    tags$p(a)
  })
  text<-tagList(text)
  isolate(values$new<-1)
  return(text)
  
})


#render select options for POS Tags
output$DV_POS<-renderUI({
  options<-setdiff(c("None",unique(values$token[,"pos"])),"SPACE")
  radioButtons(inputId = "DV_POS",label = "POS-TAGS",choices = options,selected = "None")
})

#render select options for Entity Tags
output$DV_Entity<-renderUI({
  options<-c("None",unique(values$token[,"entity"]))
  options<-options[-which(nchar(options)<2)]
  options<-stringr::str_replace_all(string = options,pattern = "_[A-Z]$",replacement = "")
  options<-unique(options)
  radioButtons(inputId = "DV_Entity",label = "Entity-TAGS",choices = options,selected = "None")
})

observeEvent(input$anno_scheme_selected,{
  values$new<-NULL
},priority = 1)



output$DV_documentwide_annotations<-renderUI({
  validate(
    need(
      dim(values$annos_documentwide)[1]>0,message=F
    )
  )
  data<-values$annos_documentwide
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