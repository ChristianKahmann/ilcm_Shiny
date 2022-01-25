#' search for documents via sub collection name
#' depends on:
#'    values$Sub_search: sub collection search 
output$Doc_Search_Sub_Save_Name_UI<-renderUI({
  validate(
    need(values$Sub_search==T,message=F))
  textInput(inputId = "Sub_Collection_Name",label = "Sub-Collection name:",value = "")
})

#' save sub collection
#' depends on:
#'   values$Sub_search: search in subcollection
output$Doc_Search_Sub_Save_UI<-renderUI({
  validate(
    need(values$Sub_search==T,message=F))
  shinyBS::bsButton(inputId = "save_Sub_Collection",label ="save" ,icon=icon("save"),style = "info")
})


#' find selected rows from collection
#' depends on:
#'   input$collections_rows_selected: selected rows from collection
#'   values$Doc_url: document url
#'   values$Doc_collection_name: collection name from documents
#'   values$Doc_ids: document ids
#'   values$Doc_q: selected characters/words from document
#'   values$Doc_fq: time stamp from document
#'   values$Doc_del: delete document
#'   values$Doc_dataset: documents from dataset
#'   values$dataset_Sub: subcollection from dataset
#'   values$metadata_available_Sub: meta data fr available subcollection
#'   values$numFound_Sub: found numbers from subcollection
#'   values$current_collection: current collection
#'   values$Sub_search: subcollection search
observe({
  validate(
    need(length(input$collections_rows_selected)>0,"no Collection specified")
  )
  load(list.files("collections/collections/", full.names = T)[[input$collections_rows_selected]])
  values$Doc_url<-info[[4]]
  values$Doc_collection_name<-info[[5]]
  values$Doc_ids<-info[[3]]
  values$Doc_q<-info[[8]]
  values$Doc_fq<-paste0('(collections:"',info[[5]],'")')
  values$Doc_del<-info[[10]]
  #values$Doc_dataset<-stringr::str_remove_all(string = stringr::str_extract(string = info[[9]],pattern = "dataset_s:{1,20}"),pattern = "dataset_s:")
  values$Doc_dataset<-stringr::str_remove_all(stringr::str_remove(string = stringr::str_extract(string = info[[9]],pattern = "dataset_s:.*?($| AND)"),pattern = " AND"),pattern = "dataset_s:")
  values$dataset_Sub<-stringr::str_remove(string = stringr::str_extract(string = info[[9]],pattern = "dataset_s:.*?($| AND)"),pattern = " AND")
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
  rs<-dbGetQuery(mydb, paste0("SELECT * FROM ilcm.metadata_names where dataset in('",stringr::str_replace_all(string = values$dataset_Sub,pattern = "dataset_s:",replacement = ""),"');"))
  RMariaDB::dbDisconnect(mydb)
  if(dim(rs)[1]>0){
    values$metadata_available_Sub<-rs
  }
  
  values$numFound_Sub<-dim(info[[1]])[1]
  values$current_collection<-info[[5]]
  values$Sub_search<-F
})

values$Doc_custom<-F


values$Doc_reload_keep<-F

#' collection of documents
#' depends on:
#'   input$collections_rows_selected: selected rows from collection
#'   input$Doc_row_sel: selected documents from rows
#'   values$Doc_reload_keep:reload documents to keep them
#'   values$custom_inputtext_Sub: costomed input text from subcollection
#'   values$solr_url: url for solr
#'   input$sort: select sorting method
#'   values$Doc_q: selected character/word from document
#'   values$Doc_fq: time stamp from document
#'   values$Doc_del: delete documents
#'   values$host: selected host
#'   values$db_port: selected database port
#'   values$Documents_Results: result document
#'   values$Doc_custom: customed documents
#'   input$control: controle input
#'   values$control: controle values
#'   values$sort: sorting method
#'   input$sortupdateSliderInput: update slider input
#'   values$Doc_reload_keep: reload documents to keep them
#'   
output$collection_documents<-DT::renderDataTable({
  validate(
    need(length(input$collections_rows_selected)>0,"no Collection specified"),
    need(!is.null(input$Doc_row_sel),message=FALSE)
  )
  load(list.files("collections/collections/", full.names = T)[[input$collections_rows_selected]])
  ids<-info[[3]]
  order=list(0,"asc")
  values$Doc_reload_keep
  remove_existing_checkboxes_Doc(1:10)
  if(input$Doc_row_sel>=1){
    if(values$Doc_custom==TRUE){
      if(stringr::str_detect(pattern = "fl=",string = (values$custom_inputtext_Sub))){
        ind<-data.frame(do.call(rbind,solr_custom(url = isolate(values$custom_inputtext_Sub),start=((input$Doc_row_sel-1)*10))$response[[3]]))
      }
      else{
        ind<-data.frame(data.table::rbindlist(solr_custom(url = isolate(values$custom_inputtext_Sub),start=((input$Doc_row_sel-1)*10))$response[[3]],use.names = T))
      }
      co_names<-intersect(c("id","id_doc_i","score","dataset_s","section_ss","title_txt","author_txt","date_dt","token_i"),colnames(ind))
      ind<-ind[,co_names]
      colnames(ind)<-stringr::str_replace_all(string = colnames(ind),pattern = "_[a-z]+$",replacement = "")
    }
    else{
      all_fields<-data.frame(t(rep("",17)))
      colnames(all_fields)<-c("id_doc_i","dataset_s","title_txt","date_dt","language_s","token_i","id","score","mde1_ss","mde2_ss","mde3_ss","mde4_ss","mde5_ss","mde6_ss","mde7_ss","mde8_ss","mde9_ss")
      ind<-(solr::solr_search(base = values$solr_url,sort = isolate(input$sort),q = (values$Doc_q),
                              fl="id_doc_i,dataset_s,title_txt,date_dt,language_s,token_i,id,score,mde1_ss,mde2_ss,mde3_ss,mde4_ss,mde5_ss,mde6_ss,mde7_ss,mde8_ss,mde9_ss",
                              fq=(values$Doc_fq),rows="10",start = (input$Doc_row_sel-1)*10))
      ind<-plyr::rbind.fill(all_fields,data.frame(ind))[-1,]
      
      #get highlights from solr for keyword and context
      hl<-highlight(base = (values$solr_url),start = ((input$Doc_row_sel-1)*10),q = (values$Doc_q),fq=(values$Doc_fq),rows="10",hl.fl="body_txt",fl="id",raw=F,sort = isolate(input$sort))
      #make search term appear red in keyword and context
      hl<-lapply(X = hl,FUN = function(i){i<-stringr::str_replace_all(string = i,pattern = '<em>','<span style="color:red">');stringr::str_replace_all(string = i,pattern = '</em>',"</span>")})
      if(!dim(ind)[1]>0){
        shinyWidgets::confirmSweetAlert(session = session,title = "No documents found.",text =  "For the selected colllection no documents were found. Maybe solr is not finished yet with marking the documents with their collection tag.
                                     Try to reselect the collection a little later. If this does not help you can add the collection tag to solr once more.",inputId="Documents_reupload_SolrTag",
                                        closeOnClickOutside=T,btn_labels=c("Wait","Re-upload collection tag to solr"), type = "warning")
      }
      validate(
        need(dim(ind)[2]>1,message=F),
        need(dim(ind)[1]>0,message=F))
      colnames(ind)<-stringr::str_replace_all(string = colnames(ind),pattern = "_[a-z]+$",replacement = "")
    }
    #bind keyword and context to result set
    if(length(ind)>0){
      ind<-cbind(ind,rep(0,dim(ind)[1]))
    }
    if(length(ind>0)){
      try(expr = {
        for(i in 1:dim(ind)[1]){
          ind[i,dim(ind)[2]]<-hl[[as.character(ind[i,"id"])]]
        }
      },silent = T)
      colnames(ind)[dim(ind)[2]]<-"keyword and context"
    }
  }
  del<-which(ind[,"id"]%in%values$Doc_del)
  if(length(del)>0){
    ind<-ind[-del,,drop=F]
  }
  #reduce to needed metadata
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
  ava<-dbGetQuery(mydb, paste0("SELECT * FROM ilcm.metadata_names where dataset in('",paste(unique(ind[,"dataset"]),collapse="','"),"');"))
  RMariaDB::dbDisconnect(mydb)
  
  empty_metadata<-names(which(apply(ava,MARGIN = 2,function(x){all(is.na(x))})))
  if(length(empty_metadata)>0){
    ind<-ind[,-which(colnames(ind)%in%empty_metadata)]
  }
  ind_new<-data.frame()
  #split metadata for differing datasets/metadata names
  for(d in unique(ind[,"dataset"])){
    ind_tmp<-ind[which(ind[,"dataset"]==d),] 
    keep_columns<-which(apply(ind_tmp,2,function(x){!all(is.na(x))}))
    ind_tmp<-ind_tmp[,keep_columns]
    if(dim(ind_tmp)[1]>0){
      colnames(ind_tmp)<-c("id_doc","dataset","title","date","language","token","id","score",(ava)[which(ava[,1]==d),2:length(colnames(ava))][!is.na(ava[which(ava[,1]==d),-1])],"keyword and context")[keep_columns]
      ind_new<-plyr::rbind.fill(ind_new,ind_tmp)
    }
  }
  
  meta<-colnames(ind_new)[which(!colnames(ind_new)%in%c("id_doc","dataset","title","date","language","token","id","score","keyword and context"))]
  if(length(meta)>0){
    ind_new<-ind_new[,c("id_doc","id","dataset","score","title","date","token","language",meta,"keyword and context")]
  }
  
  ind<-ind_new
  
  
  data<-data.frame(ind)
  colnames(data)<-colnames(ind)
  #just use Date information, no daytime
  data$date<-substr(data$date,1,10)
  values$Documents_Results<-data
  #make titles appear bold
  data$title<-paste0("<b>",data$title,"</b>")
  remove_existing_checkboxes(1:10)
  #open details window buttons 
  Open = shinyInput(
    shinyBS::bsButton,
    dim(data)[1],
    'open_document_view_button_documents_results_',
    label = "",
    size="extra-small",
    style="info",
    icon=icon("search"),
    onclick = 'Shiny.onInputChange(\"open_document_view_button_documents_results\",  this.id)'
  )
  data<-cbind(Open,data)
  tabledata<-data.frame(data,keep=shinyInput_checkbox_Doc(checkboxInput,dim(data)[1],"Doccbox_",values=!(data[,"id"]%in%isolate(values$Doc_delete_documents)),label=NULL))
  
  #set options for datatable, check if output is already sorted by solr
  if(values$Doc_custom==TRUE){
    data<-datatable(tabledata
                    ,selection = "none",rownames = FALSE,escape = F,class = "row-border compact",options = list(
                      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
                      dom="t"
                    )
    )
  }
  else{
    if(!is.null(isolate(input$sort))){
      if(nchar(isolate(input$sort))>0){
        data<-datatable(tabledata
                        ,selection = "none",rownames = FALSE,escape = F,class = "row-border compact", extensions =  "Buttons",options = list(
                          preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                          drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
                          dom="Bt",buttons = I('colvis'),
                          order=list((which(colnames(data)==stringr::str_replace_all(string = stringr::str_replace_all(isolate(input$sort)," .+",""),pattern = "_[a-z]+$",replacement = ""))-1),str_split(isolate(input$sort),pattern = " ")[[1]][2])
                        ),
                        callback = JS(
                          '$(".sorting").on("click",function() {
                          i = this.innerHTML;
                          o = this.outerHTML;
                          console.log(i);
                          console.log(o);
                          if (o.includes("column ascending")){
                          var sort = " asc"
                          }
                          if (o.includes("column descending")){
                          var sort = " desc"
                          }
                          switch(i){
                          case "id_doc":
                          var name = "id_doc_i"
                          break;
                          case "date":
                          var name = "date_dt"
                          break;
                          case "token":
                          var name = "token_i"
                          break;
                          case "score":
                          var name = "score"
                          break;
                          case "id":
                          var name = "id"
                          break;
                          case "dataset":
                          var name = "dataset_s"
                          break;
                          }
                          Shiny.onInputChange("sort", name.concat(sort));
                          Shiny.onInputChange("control",Math.random());
      });
                          
                          $(".sorting_desc").on("click",function() {
                          i = this.innerHTML;
                          o = this.outerHTML;
                          var sort = " asc"
                          
                          switch(i){
                          case "id_doc":
                          var name = "id_doc_i"
                          break;
                          case "date":
                          var name = "date_dt"
                          break;
                          case "token":
                          var name = "token_i"
                          break;
                          case "score":
                          var name = "score"
                          break;
                          case "id":
                          var name = "id"
                          break;
                          case "dataset":
                          var name = "dataset_s"
                          break;  
                          }
                          Shiny.onInputChange("sort", name.concat(sort));
                          Shiny.onInputChange("control",Math.random());
                          });
                          
                          $(".sorting_asc").on("click",function() {
                          i = this.innerHTML;
                          o = this.outerHTML;
                          var sort = " desc"
                          switch(i){
                          case "id_doc":
                          var name = "id_doc_i"
                          break;
                          case "date":
                          var name = "date_dt"
                          break;
                          case "token":
                          var name = "token_i"
                          break;
                          case "score":
                          var name = "score"
                          break;
                          case "id":
                          var name = "id"
                          break;
                          case "dataset":
                          var name = "dataset_s"
                          break;
                          }
                          Shiny.onInputChange("sort", name.concat(sort));
                          Shiny.onInputChange("control",Math.random());
                          });'
                        )
        )
      }
    }
    else{
      data<-datatable(tabledata,selection = "none",rownames = FALSE,class = "row-border compact",escape = F,extensions = "Buttons",options = list(
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
        dom="Bt",buttons = I('colvis'),
        order=list(3,"desc")
      ),
      callback = JS(
        '$(".sorting").on("click",function() {
        i = this.innerHTML;
        o = this.outerHTML;
        if (o.includes("column ascending")){
        var sort = " desc"
        }
        if (o.includes("column descending")){
        var sort = " asc"
        }
        switch(i){
        case "id_doc":
        var name = "id_doc_i"
        break;
        case "date":
        var name = "date_dt"
        break;
        case "token":
        var name = "token_i"
        break;
        case "score":
        var name = "score"
        break;
        case "id":
        var name = "id"
        break;
        case "dataset":
        var name = "dataset_s"
        break;
        }
        Shiny.onInputChange("sort", name.concat(sort));
        Shiny.onInputChange("control",Math.random());
    });
        
        $(".sorting_desc").on("click",function() {
        i = this.innerHTML;
        o = this.outerHTML;
        if (o.includes("column ascending")){
        var sort = " desc"
        }
        if (o.includes("column descending")){
        var sort = " asc"
        }
        switch(i){
        case "id_doc":
        var name = "id_doc_i"
        break;
        case "date":
        var name = "date_dt"
        break;
        case "token":
        var name = "token_i"
        break;
        case "score":
        var name = "score"
        break;
        case "id":
        var name = "id"
        break;
        case "dataset":
        var name = "dataset_s"
        break;
        }
        Shiny.onInputChange("sort", name.concat(sort));
        Shiny.onInputChange("control",Math.random());
        });'
      )
      )
    }
  }
  shinyjs::enable(id = "Doccbox_1")
  #datatable object with javascript script, that created the solr sort argument, when a table header is clicked
  if(!is.null((input$control))){
    if(isolate(values$control!=input$control)&isolate(values$sort!=input$sort)){
      values$sort<-(input$sort)
      values$control<-isolate(input$control)
      updateSliderInput(inputId = "Doc_row_sel",session = session,value=1)
    }
  }
  values$Doc_reload_keep<-FALSE
  return(data)
},server = F
)

#' document row
#' depends on:
#'   input$collections_rows_selected: selected rows from collection
#'   values$numFound_Sub: found numbers in sub collection
#'   input$Documents_reupload_SolrTag: if user wants to re upload collection tag to solr and show progress
#'   values$Doc_collection_name: collection name of document
#'   values$update_solr_url: update url of solr
#'   values$update_solr_port: update port of solr
#'   values$Doc_ids: document ids 
output$Documents_row<-renderUI({
  validate(
    need(length(input$collections_rows_selected)>0,message=FALSE)
  )
  load(list.files("collections/collections/", full.names = T)[[input$collections_rows_selected]])  
  sliderInput(inputId = "Doc_row_sel",label = NULL,min = 1,max = ceiling(values$numFound_Sub/10),value = 1,step = 1,width = "100%")
})


#if user wants to re upload collection tag to solr do that, and show progress
observeEvent(ignoreNULL = T,input$Documents_reupload_SolrTag,{
  if(input$Documents_reupload_SolrTag){
    n=4
    withProgress(message = paste0('Re-uploading collection tag: ',values$Doc_collection_name," to solr"), value = 0, {
      
      incProgress(1/n, detail = "Connecting to solr")
      host<-values$update_solr_url
      port<-values$update_solr_port
      conn<-solrium::SolrClient$new(host = host,port = port,path="search")
      
      incProgress(1/n, detail = "Creating update statement")
      body<-create_body_solr_update_add(ids = values$Doc_ids[,1],field_name = "collections",values = rep(values$Doc_collection_name,length(values$Doc_ids[,1])))
      
      
      incProgress(1/n, detail = "Uploading Collection Tags")
      rm(solr_update_working)
      try({
        conn$update_atomic_json(name = "iLCM",body = body)->solr_update_working
      })
      if(!exists("solr_update_working")){
        conn$update_atomic_json(name = "iLCM",body = body)
      }
      
      incProgress(1/n, detail = "Comit changes")
      solrium::commit(conn = conn,name="iLCM")
      
    }
    )
    #deselect and then reselect chosen colelction row to show updated results
    selected_Row<-input$collections_rows_selected
    proxy_collections %>% selectRows(NULL)
    proxy_collections %>% selectRows(selected_Row)
  }
})



#' check wheather a document is selected in Search_results datatable // if yes get data from db and which to document view
#' depends on:
#'   input$collection_documents_rows_selected: selected row from document collection
#'   values$Documents_Results: document results
#'   values$host: used host
#'   values$db_port: used database port
#'   values$Doc_token: document token
#'   values$Doc_new: new document
#'   values$Doc_Doc_reload: reload document after deleted
#'   values$Doc_annotations_show: show document annotations
#'   values$Doc_annos: document annotations
observeEvent(input$open_document_view_button_documents_results,{
  s = as.numeric(strsplit(input$open_document_view_button_documents_results, "_")[[1]][7])
  if (length(s)) {
    if(s>0){
      isolate(shinyjs::runjs('Shiny.onInputChange(\"open_document_view_button_documents_results\",  "open_document_view_button_documents_results_0")'))
      values$Doc_selected<-as.integer(values$Documents_Results[s[length(s)],"id_doc"])
      values$collection_dataset<-values$Documents_Results[s[length(s)],"dataset"]
      proxy = dataTableProxy('collection_documents')
      mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
      rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
      values$Doc_token<-RMariaDB::dbGetQuery(mydb, paste("select * from token where dataset='",values$collection_dataset,"' and id=",isolate(values$Doc_selected),";",sep=""))
      RMariaDB::dbDisconnect(mydb)
      values$Doc_meta<-isolate(values$Documents_Results[s[length(s)],1:(dim(isolate(values$Documents_Results))[2]-1)])
      proxy %>% selectRows(NULL)
      
      updateTabsetPanel(session = session,inputId = "coll",selected = "Document View2")
      shinyjs::useShinyjs()
      shinyjs::runjs(" Shiny.onInputChange('Doc_anno_tag',null);
                     Shiny.onInputChange('Doc_anno_start',null);
                     Shiny.onInputChange('Doc_anno_end',null);")
      values$Doc_new<-NULL
      values$Doc_Doc_reload<-runif(1,0,1)
      values$Doc_annotations_show<-matrix(c(0),0,13)
      values$Doc_annos<-NULL
    }
  }
})


#' check which documents should be excluded from collection
#' depends on:
#'   input$Doccbox_1: put documents in virtuelle box
#'   values$Documents_Results: document result
#'   values$Doc_delete_documents: documents to delete
observe({
  a<-lapply(X = 1:10,FUN=function(x){return(input[[paste0("Doccbox_",x)]])})
  validate(
    need(!is.null(input$Doccbox_1),
         message=FALSE)
  )
  a<-do.call(rbind,a)
  docs_del<-isolate(values$Documents_Results[which(a==F),"id"])
  
  isolate(values$Doc_delete_documents<-setdiff(values$Doc_delete_documents,values$Documents_Results[,"id"]))
  
  isolate(values$Doc_delete_documents<-c(isolate(values$Doc_delete_documents),isolate(values$Documents_Results[which(a==F),"id"])))
  
})

#' reset the list for documents marked for deletion when collection changed
#' depends on:
#'   input$collections_rows_selected: selected rows from collection
#'   values$Doc_delete_documents: deleted documents
observeEvent(input$collections_rows_selected,{
  values$Doc_delete_documents<-NULL
})


#' reset the list for documents marked for deletion when refresh button for keep is pressed and trigger a reload of the search results table
#' depends on:
#'   input$Doc_Search_results_reset_delete:  reset the list for documents marked for deletion
#'   values$Doc_delete_documents: list of documents
#'   values$Doc_reload_keep: reload documents to keep them
observeEvent(input$Doc_Search_results_reset_delete,{
  values$Doc_delete_documents<-NULL
  values$Doc_reload_keep<-TRUE
})

#' delete documentions after delete button was pressed
#' depends on:
#'   values$Doc_delete_documents: list of documents to delete
output$delete_documents_from_colelction_button_ui<-renderUI({
  if(length(values$Doc_delete_documents)>0){
    return(actionButton(inputId = "delete_documents_from_colelction_button",label = "delete documents",styleclass = "danger",icon = icon("trash")))
  }
  else{
    return(NULL)
  }
})

#' observe is delete button was pressed
#' depends on:
#'   input$delete_documents_from_colelction_button: button to delete selected documents
#'   values$Doc_delete_documents: documents to delete
observeEvent(input$delete_documents_from_colelction_button,{
  shinyWidgets::confirmSweetAlert(session = session,inputId = "confirm_delete_docs",type = "warning",title = paste0("Are you sure you want to delete ",length(values$Doc_delete_documents)," documents from the collection?"),danger_mode = T)
})

#' confirm to delete documents after button was pressed
#' depends on:
#'   input$confirm_delete_docs: confirm documents that should be deleted
#'   input$collections_rows_selected: selcted document rows from collection
#'   values$Doc_delete_documents: list of documents to delete
#'   values$Doc_reload_keep: reload documents so thex wont get deleted
#'   values$numFound_Sub: found number of document from stack of subcollections
observeEvent(input$confirm_delete_docs,{
  if(isTRUE(input$confirm_delete_docs)){
    load(list.files("collections/collections/", full.names = T)[[input$collections_rows_selected]])
    toDel<-which(as.character(info[[3]][,1])%in%values$Doc_delete_documents)
    ids_old<-info[[3]][,1]
    info[[1]]<-info[[1]][-toDel,,drop=F]
    info[[2]]<-info[[2]][-toDel,,drop=F]
    info[[3]]<-info[[3]][-toDel,,drop=F]
    info[[6]]<-info[[6]][-toDel,,drop=F]
    info[[7]]<-info[[7]][-toDel,,drop=F]
    info[[4]]<-paste0(info[[4]]," ; documentes with ids:",paste(values$Doc_delete_documents,collapse = ", ")," deleted by the user: ",values$user)
    info[[8]]<-info[[8]]
    info[[9]]<-info[[9]]
    info[[10]]<-c(info[[10]],values$Doc_delete_documents)
    save(info,file=list.files("collections/collections/", full.names = T)[[input$collections_rows_selected]])
    host<-values$update_solr_url
    port<-values$update_solr_port
    try({future::future(expr = {
      body<-create_body_solr_update_remove(ids = ids_old,field_name = "collections",values = rep(info[[5]],length(ids_old)))
      conn<-solrium::SolrClient$new(host = host,port = port,path="search")
      try(silent = T,{
        rm(solr_update_working)
        conn$update_atomic_json(name = "iLCM",body = body)->solr_update_working
      })
      if(!exists("solr_update_working")){
        conn$update_atomic_json(name = "iLCM",body = body)
      }
      body<-create_body_solr_update_add(ids = info[[3]][,1],field_name = "collections",values = rep(info[[5]],length(info[[3]][,1])))
      try(silent = T,{
        rm(solr_update_working)
        conn$update_atomic_json(name = "iLCM",body = body)->solr_update_working
      })
      if(!exists("solr_update_working")){
        conn$update_atomic_json(name = "iLCM",body = body)
      }
      solrium::commit(conn = conn,name="iLCM")
    }) %...>% future:::ClusterRegistry("stop")
    })
    delete_collection_from_db(info[[5]])
    save_collection_to_db(info)
    values$Doc_reload_keep<-TRUE
    values$Doc_delete_documents<-NULL
    values$numFound_Sub<-nrow(info[[1]])
    Sys.sleep(1.5)
  }
})


#' create modal for searching in sub corpora
#' depends on:
#'   input$Doc_Search_Sub: search for documents in subcollection
observeEvent(input$Doc_Search_Sub,{
  showModal(modalDialog(
    div(style = 'overflow-x:hidden;',
        box(width=NULL,collapsible = T,
            navbarPage(title="",theme=shinytheme(navbarstyle),id = "navbar_search_Sub",
                       source(file.path("ui","tab_Simple_Sub.R"),local = T)$value,
                       source(file.path("ui","tab_Detailed_Sub.R"),local = T)$value,
                       source(file.path("ui","tab_Custom_Sub.R"),local = T)$value
            )
        )
    )
  )
  )
})

#' found number of documents
#' depends on:
#'   values$Documents_Results: relsulting documents for search
#'   values$numFound_Sub: found number of documents in stack from subcollection
#'   values$host: selected host
#'   values$db_port: selected port to database
output$Doc_Num_Found<-renderUI({
  validate(
    need(dim(values$Documents_Results)[1]>0,message=F)
  )
  validate(
    need(values$numFound_Sub>0,
         message=FALSE)
  )
  return(paste0(values$numFound_Sub," Documents were found!"))
})


#' save sub-collection
#' depends on:
#'   input$save_Sub_Collection: save sub-collection
#'   input$Sub_Collection_Name: name of sub-collection
#'   values$host: selected host
#'   values$db_port: selected data base port
#'   values$Doc_custom: customed documents
#'   values$custom_inputtext_Sub: customed input text from subcollection
#'   values$numFound_Sub: found number of documents in stack of subcollection
#'   values$solr_url: url for solr
#'   values$Doc_q: selected character/word from document
#'   values$Doc_fq: time stamp from document
#'   values$Doc_delete_documents: delete documents
#'   values$update_solr_url: update solr url
#'   values$update_solr_port: update solr port
#'   values$Doc_solr_query: solr query of documents
#'   values$coll_saved: saved collection
#'   values$num_collections: number of collections
observeEvent(input$save_Sub_Collection,{
  #check if collection name is still avaiable
  if(stringr::str_detect(string = input$Sub_Collection_Name,pattern = "_")){
    shinyWidgets::sendSweetAlert(type = "warning",session = session,title = "'_' used in collection name",text = "Please don't use '_' in the collection name.")
  }
  else{
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
    coll_names_in_db<-RMariaDB::dbGetQuery(mydb, 'Select distinct name from Collections')
    if(isolate(input$Sub_Collection_Name)%in%coll_names_in_db[,1]){
      shinyWidgets::sendSweetAlert(type = "warning",session = session,title = "Collection Name is already used. Please use another one.")
    }
    else{
      try({
        if(values$Doc_custom==TRUE){
          url<-isolate(values$custom_inputtext_Sub)
          if(stringr::str_detect(string = url,pattern = "fl=")){
            url<-stringr::str_replace_all(string = url,pattern = "fl=[a-z_,]+",replacement = "fl=id_doc_i,dataset_s,date_dt,id")
          }
          else{
            url<-paste(url,"&fl=id_doc_i,dataset_s,date_dt,id,score")
          }
          x<-data.frame(data.table::rbindlist(solr_custom(url = paste0(url,"&rows=",values$numFound_Sub),start=0)$response[[3]],use.names = T))
        }
        else{
          x<-(solr_search(base = values$solr_url,q = values$Doc_q,fl="id_doc_i , dataset_s ,date_dt,id,score",fq=values$Doc_fq,rows=isolate(values$numFound_Sub),start = 0))
        }
        if(length(isolate(values$Doc_delete_documents))>0){
          x<-x[-which(x[,"id"]%in%isolate(values$Doc_delete_documents)),]
        }
        host<-values$update_solr_url
        port<-values$update_solr_port
        try({future::future(expr = {
          body<-create_body_solr_update_add(ids = x[,"id"],field_name = "collections",values = rep(input$Sub_Collection_Name,length(x[,"id"])))
          conn<-solrium::SolrClient$new(host =host,port = port,path="search")
          try(silent = T,{
            rm(solr_update_working)
            conn$update_atomic_json(name = "iLCM",body = body)->solr_update_working
          })
          if(!exists("solr_update_working")){
            conn$update_atomic_json(name = "iLCM",body = body)
          }
          solrium::commit(conn = conn,name="iLCM")
        }) %...>% future:::ClusterRegistry("stop")
        })
        x<-x[order(x[,"id_doc_i"]),]
        indices<-data.frame(x[,"id_doc_i"])
        dataset<-data.frame(x[,"dataset_s"])
        ids<-data.frame(x[,"id"])
        score<-data.frame(x[,"score"])
        url<-isolate(values$Doc_solr_query)
        if(length(isolate(values$Doc_delete_documents))>0){
          url<-paste0(url," \n documents with id: ",paste0(isolate(values$Doc_delete_documents),collapse=", ")," deleted by user:",values$user ,sep="")
        }
        name<-isolate(input$Sub_Collection_Name)
        dates<-data.frame(substr(x[,"date_dt"],1,10))
        q<-values$Doc_q
        fq<-values$Doc_fq
        del<-values$Doc_delete_documents
        info<-list(indices,dataset,ids,url,name,dates,score,q,fq,del)
        if(length(indices)>=1){
          save(info,file=paste("collections/collections/",isolate(input$Sub_Collection_Name),".RData",sep = ""))
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
})
