

output$DV_Annotation_Schemes<-renderUI({
  values$newscheme
  validate(validate(need(
    length(list.files("collections/annotation_schemes/"))>0,message="No annotation schemes found. You can create schemes in the 'Categories' tab."
  )))
  selectizeInput(inputId = "anno_scheme_selected",label="Which Annotation Scheme?",choices=stringr::str_replace_all(list.files("collections/annotation_schemes/"),".RData","")) 
})


observeEvent(input$save_annotations,{
  if(dim(isolate(values$annotations_marked))[1]==0){
    shinyWidgets::sendSweetAlert(session = session,title = "no annotations found",type = "warning")
  }
  else{
      #save Annotations to DB
    vals<-""
    token<-isolate(values$token)
    token[,5]<-stringr::str_replace_all(string = token[,5],pattern = '"',replacement = "'")
    for(i in 1:dim(isolate(values$annotations_marked))[1]){
      if(values$annotations_marked[i,13]=="FALSE"){
        vals<-paste0(vals,'("',isolate(values$annotations_marked[i,1]),'",','"',isolate(values$annotations_marked[i,2]),'"',',','"',isolate(values$annotations_marked[i,3]),'",',isolate(values$annotations_marked[i,4]),',',isolate(values$annotations_marked[i,5]),',','"',isolate(values$annotations_marked[i,6]),'"',',','"',isolate(values$annotations_marked[i,7]),'"',',','"',isolate(values$annotations_marked[i,8]),'"',',','"',isolate(values$annotations_marked[i,9]),'"',',','"',isolate(values$annotations_marked[i,10]),'"',',','"',isolate(values$annotations_marked[i,11]),'"',' , ',isolate(values$annotations_marked[i,12]),' ,"',paste(token[isolate(values$annotations_marked)[i,5]:isolate(values$annotations_marked[i,6]),5],collapse=" "),'"',' , "',isolate(values$annotations_marked[i,13]),'"),')
      }
      else{
        vals<-paste0(vals,'("',isolate(values$annotations_marked[i,1]),'",','"',isolate(values$annotations_marked[i,2]),'"',',','"',isolate(values$annotations_marked[i,3]),'",',isolate(values$annotations_marked[i,4]),',',isolate(values$annotations_marked[i,5]),',','"',isolate(values$annotations_marked[i,6]),'"',',','"',isolate(values$annotations_marked[i,7]),'"',',','"',isolate(values$annotations_marked[i,8]),'"',',','"',isolate(values$annotations_marked[i,9]),'"',',','"',isolate(values$annotations_marked[i,10]),'"',',','"',isolate(values$annotations_marked[i,11]),'"',' , ',isolate(values$annotations_marked[i,12]),' ,"',paste0(substr(paste(token[isolate(values$annotations_marked)[i,5]:isolate(values$annotations_marked[i,6]),5],collapse=" "),1,100),"..."),'"',' , "',isolate(values$annotations_marked[i,13]),'"),')
      }
    }
    vals<-substr(vals,1,nchar(vals)-1)
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
    query<-paste('Insert Ignore into Annotations Values',vals,';',sep="")
    try({
      write_to_MariaDB(mydb,query)
    })
    RMariaDB::dbDisconnect(mydb)
    values$anno_loaded<-TRUE
    shinyWidgets::sendSweetAlert(type = "success",session = session,title = paste0( dim(values$made_annotations)[1]," Annotations saved"))
  }
})


output$annotationComponents<-renderUI({
  validate(
    need(!is.null(input$anno_scheme_selected),message=FALSE)
  )
  load(paste0("collections/annotation_schemes/",input$anno_scheme_selected,".RData"))
  annotateTextComponent_div(anno)
})



output$made_annotations<-renderUI({
  validate(
    need(dim(values$annotations_show)[1]>0,message=FALSE)
  )
  anno<-values$annotations_show[which(values$annotations_show[,10]==(input$anno_scheme_selected)),,drop=F]
  rownames(anno)<-rep("",dim(anno)[1])
  anno<-unique(anno)
  if(dim(anno)[1]>0){
    div_list<-lapply(X = 1:dim(anno)[1],FUN = function(x){
      user<-anno[x,2]
      class<-anno[x,7]
      color<-anno[x,8]
      schema<-anno[x,10]
      square_id<-as.character(anno[x,1])
      shinyjs::onevent(event = "mouseenter",id = square_id,expr = {
        #print(paste("test rein",square_id))
        highlight_annos<-matrix(c(0),length(anno[x,5]:anno[x,6]),2)
        highlight_annos[,1]<-anno[x,5]:anno[x,6]
        highlight_annos[,2]<-color
        values$highlight_annos<- highlight_annos
        values$delete_anno_box_id<-square_id
      }
      )
      shinyjs::onevent(event = "mouseleave",id = square_id,expr = {
        #print(paste("test raus",square_id))
        values$highlight_annos<-NULL
      }
      )
      #shinyDND::dragUI(id=square_id,style=paste0('width:2em;height:2em;border:1px solid #ccc; margin:1em 1em 0.5em 0;display:inline-block;background: ',color))#,title=paste(class," (annotated by ",user,")"))
       shinyjqui::jqui_draggable(tags$div(id=square_id,class="anno-box",
                                          style=paste0('width:2em;height:2em;border:1px solid #ccc; margin:1em 1em 0.5em 0;display:inline-block;background: ',color)
                                          ,title=paste(class," (annotated by ",user,")")),options = list(revert=T))
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
    need(!is.null(input$delete_annotation_box),message=FALSE)
  )
  shinyWidgets::confirmSweetAlert(session = session,inputId = "confirm_delete_anno_box",type = "warning",title = "Are you sure you want to delete this annotation",danger_mode = T)
})


# 
# observe({
#   validate(
#     need(!is.null(input$trash),message=FALSE)
#   )
#   shinyWidgets::confirmSweetAlert(session = session,inputId = "confirm_delete_anno_box",type = "warning",title = "Are you sure you want to delete this annotation",danger_mode = T)
# })


#if user has confiremd to delete annotation, delete the annotation in the database and also the annotation matrices
observeEvent(input$confirm_delete_anno_box,{
  if(isTRUE(input$confirm_delete_anno_box)){
    values$annotations_marked<-isolate(values$annotations_marked[-which(isolate(values$annotations_marked[,1]==isolate(values$delete_anno_box_id))),,drop=F])
    values$annotations_show<-isolate(values$annotations_show[-which(isolate(values$annotations_show[,1]==isolate(values$delete_anno_box_id))),,drop=F])
    values$annos_documentwide<-isolate(values$annos_documentwide[-which(isolate(values$annos_documentwide$id==isolate(values$delete_anno_box_id))),,drop=F])
    values$annos<-isolate(values$annos[-which(isolate(values$annos[,6]==isolate(values$delete_anno_box_id))),,drop=F])
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
    write_to_MariaDB(mydb = mydb,query =paste0('Delete from Annotations where anno_id="',isolate(values$delete_anno_box_id),'";') )
    RMariaDB::dbDisconnect(mydb)
    values$anno_deleted<-runif(1,0,1)
  }
}
)
