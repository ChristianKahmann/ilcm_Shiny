values$control<-0
values$sort<-""
values$reload_keep<-FALSE

#get metadata fields for selected corpora
observeEvent(input$dataset,{
  values$metadata_available<-NULL
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
  rs<-dbGetQuery(mydb, paste0("SELECT * FROM ilcm.metadata_names where dataset in('",paste(input$dataset,collapse="','"),"');"))
  RMariaDB::dbDisconnect(mydb)
  if(dim(rs)[1]>0){
    values$metadata_available<-rs
  }
})


#render output table for search results
output$search_results_datatable<-DT::renderDataTable({
  validate(
    need(values$numFound>0,
         "no Documents found"),
    need(input$SR_row_sel>0,
         message=FALSE)
  )
  #check wheather reload function is triggered
  values$reload_keep
  remove_existing_checkboxes(1:10)
  if(input$SR_row_sel>=1){
    if(values$custom==TRUE){
      if(stringr::str_detect(pattern = "fl=",string = (values$custom_inputtext))){
        ind<-data.frame(do.call(rbind,solr_custom(url = isolate(values$custom_inputtext),start=((input$SR_row_sel-1)*10))$response[[3]]))
      }
      else{
        ind<-data.frame(data.table::rbindlist(solr_custom(url = isolate(values$custom_inputtext),start=((input$SR_row_sel-1)*10))$response[[3]],use.names = T))
      }
      #co_names<-intersect(c("id","id_doc_i","score","dataset_s","section_ss","title_txt","author_txt","date_dt","token_i"),colnames(ind))
      #ind<-ind[,co_names]
      #colnames(ind)<-stringr::str_replace_all(string = colnames(ind),pattern = "_[a-z]+$",replacement = "")
      all_fields<-data.frame(t(rep("",17)))
      colnames(all_fields)<-c("id_doc_i","dataset_s","title_txt","date_dt","language_s","token_i","id","score","mde1_ss","mde2_ss","mde3_ss","mde4_ss","mde5_ss","mde6_ss","mde7_ss","mde8_ss","mde9_ss")
      ind<-plyr::rbind.fill(all_fields,data.frame(ind))[-1,]
    }
    else{
      all_fields<-data.frame(t(rep("",17)))
      colnames(all_fields)<-c("id_doc_i","dataset_s","title_txt","date_dt","language_s","token_i","id","score","mde1_ss","mde2_ss","mde3_ss","mde4_ss","mde5_ss","mde6_ss","mde7_ss","mde8_ss","mde9_ss")
      ind<-(solr::solr_search(base = values$url,sort = isolate(input$sort),q = (values$q),
                              fl="id_doc_i,dataset_s,title_txt,date_dt,language_s,token_i,id,score,mde1_ss,mde2_ss,mde3_ss,mde4_ss,mde5_ss,mde6_ss,mde7_ss,mde8_ss,mde9_ss",
                              fq=(values$fq),rows="10",start = (input$SR_row_sel-1)*10))
      ind<-plyr::rbind.fill(all_fields,data.frame(ind))[-1,]
      #get highlights from solr for keyword and context
      hl<-highlight(base = (values$url),start = ((input$SR_row_sel-1)*10),q = (values$q),fq=(values$fq),rows="10",hl.fl="body_txt",fl="id",raw=F,sort = isolate(input$sort))
      #make search term appear red in keyword and context
      hl<-lapply(X = hl,FUN = function(i){i<-stringr::str_replace_all(string = i,pattern = '<em>','<span style="color:red">');stringr::str_replace_all(string = i,pattern = '</em>',"</span>")})
      #ind<-ind[,c("id","id_doc_i","score","dataset_s","section_ss","title_txt","author_txt","date_dt","token_i")]
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
  #reduce to need metadata
  ava<-isolate(values$metadata_available)
  empty_metadata<-names(which(apply(ava,MARGIN = 2,function(x){all(is.na(x))})))
  if(length(empty_metadata)>0){
    ind<-ind[,-which(colnames(ind)%in%empty_metadata)]
  }
  ind_new<-data.frame()
  #split metadata for differing datasets/metadata names
  for(d in isolate(input$dataset)){
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
    ind_new<-ind_new[,intersect(colnames(ind_new),c("id_doc","id","dataset","score","title","date","token","language",meta,"keyword and context"))]
    }
  
  ind<-ind_new
  
  data<-data.frame(ind)
  colnames(data)<-colnames(ind)
  #just use Date information, no daytime
  data$date<-substr(data$date,1,10)
  values$Search_Results<-data
  #make titles appear bold
  data$title<-paste0("<b>",data$title,"</b>")
  
  #set options for datatable, check if output is already sorted by solr
  if(values$custom==TRUE){
    remove_existing_checkboxes(1:10)
    data<-datatable(data.frame(data,keep=shinyInput_checkbox(checkboxInput,dim(data)[1],"cbox_",values=!(data[,2]%in%isolate(values$delete_documents)),label=NULL))
                    ,selection = "single",rownames = FALSE,escape = F,class = "row-border compact",options = list(
                      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
                      dom="t",
                      columnDefs=list(list(className="no_select",targets=(dim(data)[2])),list(orderable=F,targets=(0:(dim(data)[2]))))
                    ),
                    callback = JS(
                      '
                      table.on("click", "td.no_select", function(e) {
                      e.stopPropagation()
                      });
                      '
                    )
    )
  }
  else{
    if(!is.null(isolate(input$sort))){
      if(nchar(isolate(input$sort))>0){
        remove_existing_checkboxes(1:10)
        data<-datatable(data.frame(data,keep=shinyInput_checkbox(checkboxInput,dim(data)[1],"cbox_",values=!(data[,2]%in%isolate(values$delete_documents)),label=NULL))
                        ,selection = "single",rownames = FALSE,escape = F,class = "row-border compact",options = list(
                          preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                          drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
                          dom="t",
                          columnDefs=list(list(className="no_select",targets=(dim(data)[2])),list(orderable=F,targets=c(4,7:(dim(data)[2])))),
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
  });
        table.on("click", "td.no_select", function(e) {
        e.stopPropagation()
        });
        '
                        )
        )
      }
    }
    else{
      data<-datatable(data.frame(data,keep=shinyInput_checkbox(checkboxInput,dim(data)[1],"cbox_",values=!(data[,2]%in%isolate(values$delete_documents)),label=NULL)),selection = "single",rownames = FALSE,class = "row-border compact",escape = F,options = list(
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
        dom="t",
        order=list(3,"desc"),
        columnDefs=list(list(className="no_select",targets=(dim(data)[2])),list(orderable=F,targets=c(4,7:(dim(data)[2]))))
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
  });

      table.on("click", "td.no_select", function(e) {
      e.stopPropagation()
      });
      '
      )
      )
    }
  }
  shinyjs::enable(id = "cbox_1")
  #datatable object with javascript script, that created the solr sort argument, when a table header is clicked
  if(!is.null((input$control))){
    if(isolate(values$control!=input$control)&isolate(values$sort!=input$sort)){
      values$sort<-(input$sort)
      values$control<-isolate(input$control)
      updateSliderInput(inputId = "SR_row_sel",session = session,value=1)
    }
  }
  values$reload_keep<-FALSE
  return(data)
},server = F
)

values$numFound<-0

#slider for paging
output$SR_row<-renderUI({
  if(values$numFound>0){
    return(tagList(
      sliderInput(inputId = "SR_row_sel",label = "Page:",min = 1,max = ceiling(values$numFound/10),value = 0,step = 1)
    ))  
  }
})

#check wheather a document is selected in Search_results datatable // if yes get data from db and switch to document view
observe({
  s = input$search_results_datatable_rows_selected
  if (length(s)) {
    
    proxy = dataTableProxy('search_results_datatable')
    if(!"dataset"%in%colnames(values$Search_Results)){
      shinyWidgets::sendSweetAlert(session=session,title = "No dataset",text = "Please specify custom query to return the dataset_s value")
    }
    else{
      if(!"id_doc"%in%colnames(values$Search_Results)){
        shinyWidgets::sendSweetAlert(session=session,title = "No document id",text = "Please specify custom query to return the id_doc_i value")
      }
      else{
        dataset<-values$Search_Results[s[length(s)],"dataset"]
        values$selected<-as.integer(values$Search_Results[s[length(s)],"id_doc"])
        mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
        rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8mb4"')
        values$token<-RMariaDB::dbGetQuery(mydb, paste("select * from token where dataset='",dataset,"' and id=",isolate(values$selected),";",sep=""))
        RMariaDB::dbDisconnect(mydb)
        values$title<-as.character(isolate(values$Search_Results[s[length(s)],"title"]))
        values$meta<-(isolate(values$Search_Results[s[length(s)],1:(dim(isolate(values$Search_Results))[2]-1)]))
        proxy %>% selectRows(NULL)
        
        updateTabsetPanel(session = session,inputId = "expl",selected = "Document View")
        shinyjs::useShinyjs()
        shinyjs::runjs(" Shiny.onInputChange('anno_tag',null);
                     Shiny.onInputChange('anno_start',null);
                     Shiny.onInputChange('anno_end',null);")
        values$new<-NULL
        values$Doc_reload<-runif(1,0,1)
        values$annotations_show<-matrix(c(0),0,13)
        values$annos<-NULL
      }
    }
  }
})

#render an outputline telling the user how many results were found for the current search
output$SR_Num_Found<-renderText({
  validate(
    need(values$numFound>0,
         message=FALSE)
  )
  return(paste0(values$numFound," Documents were found!"))
})


#check which documents should be excluded from collection
observe({
  a<-lapply(X = 1:10,FUN=function(x){return(input[[paste0("cbox_",x)]])})
  validate(
    need(!is.null(input$cbox_1),
         message=FALSE)
  )
  a<-do.call(rbind,a)
  docs_del<-isolate(values$Search_Results[which(a==F),2])
  isolate(values$delete_documents<-setdiff(values$delete_documents,values$Search_Results[,2]))
  isolate(values$delete_documents<-c(isolate(values$delete_documents),isolate(values$Search_Results[which(a==F),2])))
})

#reset the list for documents marked for deletion when corpus is changed
observeEvent(input$dataset,{
  values$delete_documents<-NULL
})


#reset the list for documents marked for deletion when refresh button for keep is pressed and trigger a reload of the search results table
observeEvent(input$Search_results_reset_delete,{
  values$delete_documents<-NULL
  values$reload_keep<-TRUE
})


