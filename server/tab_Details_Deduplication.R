#' reset user input
#' depends on:
#'   input$Det_DD_reset_user_input: confirmation to reset
#'   values$DD_whitelist: customed whitelist
#'   values$DD_blacklist customed blacklist
observeEvent(ignoreInit = T,input$Det_DD_reset_user_input,{
  values$DD_whitelist<-NULL
  values$DD_blacklist<-NULL
})


#' details on deduplication
#' depends on:
#'   values$Det_DD_results: deduplication results
#'   input$Det_DD_strategy: deduplication strategy
#'   input$Det_DD_threshold: deduplication threshold
#'   values$invalidate_deduplication_visulisation: invalidate visualisation of deduplication
#'   values$Det_DD_node_degree: node degree for deduplication
#'   values$Det_DD_meta: meta data from deduplication
#'   values$Det_DD_data_display: display data from deduplication 
#'   values$Det_DD_current_table: show current table for deduplication
#'   values$blacklist: customed blacklist
#'   values$whitelist: customed whitelist 
observe({
  validate(
    need(!is.null(values$Det_DD_results),message=F),
    need(!is.null(input$Det_DD_strategy),message=F),
    need(!is.null(input$Det_DD_threshold),message=F)
  )
  input$Det_DD_strategy
  input$Det_DD_threshold
  values$invalidate_deduplication_visulisation
  data<-data.frame(a=values$Det_DD_results[,1],token_a=0,date_a=0,b=values$Det_DD_results[,2],token_b=0,date_b=0,similarity=values$Det_DD_results[,3],keep_a=0,keep_b=0,stringsAsFactors = F)
  data<-data[which(data$similarity>input$Det_DD_threshold),]
  data<-data[order(data$similarity,decreasing = T),]
  if(dim(data)[1]>0)
  {
    x<-as.data.frame(table(c(data[,"a"],data[,"b"])))
    values$Det_DD_node_degree<-x
    data<-merge(data,y = x,by.x = "a",by.y = "Var1")
    data<-merge(data,y = x,by.x = "b",by.y = "Var1")
    data<-data[,c("a","token_a","date_a","b","token_b","date_b","similarity","keep_a","keep_b","Freq.x","Freq.y")]
    final_remove<-matrix(c(0),dim(data)[1],2)
    
    ############
    if(input$Det_DD_strategy=="longest"){
      try({
        relevant_data<-cbind(values$Det_DD_meta[data[,1],"token"],values$Det_DD_meta[data[,4],"token"])
        remove<-apply(relevant_data,MARGIN = 1,FUN = which.max)
        final_remove<-matrix(c(0),dim(data)[1],2)
        final_remove[which(remove==1),1]<-1
        final_remove[which(remove==2),2]<-1
      })
    } 
    if(input$Det_DD_strategy=="shortest"){
      try({
        relevant_data<-cbind(values$Det_DD_meta[data[,1],"token"],values$Det_DD_meta[data[,4],"token"])
        remove<-apply(relevant_data,MARGIN = 1,FUN = which.min)
        final_remove<-matrix(c(0),dim(data)[1],2)
        final_remove[which(remove==1),1]<-1
        final_remove[which(remove==2),2]<-1
      })
    }
    if(input$Det_DD_strategy=="latest"){
      try({
        relevant_data<-cbind(values$Det_DD_meta[data[,1],"date"],values$Det_DD_meta[data[,4],"date"])
        remove<-apply(relevant_data,MARGIN = 1,FUN = which.max)
        final_remove<-matrix(c(0),dim(data)[1],2)
        final_remove[which(remove==1),1]<-1
        final_remove[which(remove==2),2]<-1
      })
    } 
    if(input$Det_DD_strategy=="earliest"){
      try({
        relevant_data<-cbind(values$Det_DD_meta[data[,1],"date"],values$Det_DD_meta[data[,4],"date"])
        remove<-apply(relevant_data,MARGIN = 1,FUN = which.min)
        final_remove<-matrix(c(0),dim(data)[1],2)
        final_remove[which(remove==1),1]<-1
        final_remove[which(remove==2),2]<-1
      })
    }
    if(input$Det_DD_strategy=="maximum node degree"){
      try({
        relevant_data<-cbind(values$Det_DD_meta[data[,1],"Freq.x"],values$Det_DD_meta[data[,4],"Freq.y"])
        remove<-apply(relevant_data,MARGIN = 1,FUN = which.max)
        final_remove<-matrix(c(0),dim(data)[1],2)
        final_remove[which(remove==1),1]<-1
        final_remove[which(remove==2),2]<-1
      })
      if(length(remove)==0){
        remove<-sample(x = c(1,2),size = 1)
      }
    }
    if(input$Det_DD_strategy=="random"){
      remove<-sample(x = c(1,2),size = dim(data)[1],replace = T)
      final_remove<-matrix(c(0),dim(data)[1],2)
      final_remove[which(remove==1),1]<-1
      final_remove[which(remove==2),2]<-1
    }
    
    data[,8:9]<-final_remove
    data[,2]<-values$Det_DD_meta[data[,1],"token"]
    data[,3]<-values$Det_DD_meta[data[,1],"date"]
    data[,5]<-values$Det_DD_meta[data[,4],"token"]
    data[,6]<-values$Det_DD_meta[data[,4],"date"]
    
    ##sort data by according to chosen strategy
    rownames(data)<-1:dim(data)[1]
    if(input$Det_DD_strategy=="latest"){
      sort<-apply(X = data,MARGIN = 1,FUN = function(x){max(x[c(3,6)])})
      data<-data[order(sort,decreasing = T),]
    }
    if(input$Det_DD_strategy=="earliest"){
      sort<-apply(X = data,MARGIN = 1,FUN = function(x){min(x[c(3,6)])})
      data<-data[order(sort,decreasing = F),]
    }
    if(input$Det_DD_strategy=="longest"){
      sort<-as.numeric(apply(X = data,MARGIN = 1,FUN = function(x){max(x[c(2,5)])}))
      data<-data[order(sort,decreasing = T),]
    }
    if(input$Det_DD_strategy=="shortest"){
      sort<-as.numeric(apply(X = data,MARGIN = 1,FUN = function(x){min(x[c(2,5)])}))
      data<-data[order(sort,decreasing = F),]
    }
    if(input$Det_DD_strategy=="maximum node degree"){
      sort<-as.numeric(apply(X = data,MARGIN = 1,FUN = function(x){max(x[c(10,11)])}))
      data<-data[order(sort,decreasing = T),]
    }
    if(input$Det_DD_strategy=="random"){
      sort<-sample(x = 1:dim(data)[1],size = dim(data)[1],replace = T)
      data<-data[order(sort,decreasing = T),]
    }
    values$Det_DD_data_display<-data
  }
  else{
    values$Det_DD_data_display<-data
    values$Det_DD_current_table<-data
    values$blacklist<-NULL
    values$whitelist<-NULL
  }
})

#' display data 
#' depends on:
#'   values$Det_DD_data_display: data to display
#'   values$DD_whitelist: customed whitelist
#'   values$DD_blacklist: customed blacklist
#'   input$Det_DD_strategy: deduplications strategy
#'   values$Det_DD_current_table: current table for defuplication
observe({
  validate(
    need(dim(values$Det_DD_data_display)[1]>0,message=F)
  )
  d_tmp<-values$Det_DD_data_display
  documents<-unique(union(d_tmp[,"a"],d_tmp[,"b"]))
  #user decisions
  user_whitelist<-(values$DD_whitelist)
  user_blacklist<-(values$DD_blacklist)
  whitelist<-user_whitelist
  blacklist<-user_blacklist
  if(length(user_whitelist)>0){
    blacklist<-unique(c(blacklist,setdiff(union(d_tmp[which(d_tmp[,c("a")]%in%as.numeric(whitelist)),"b"],d_tmp[which(d_tmp[,c("b")]%in%as.numeric(whitelist)),"a"]),union(blacklist,whitelist))))
  }
  
  if(length(blacklist)>0){
    d_tmp<-d_tmp[-union(which(d_tmp[,"a"]%in%blacklist),which(d_tmp[,"b"]%in%blacklist)),,drop=F]
  }
  count=0
  if(dim(d_tmp)[1]>0){
    repeat({
      count=count+1
      top_pair<-d_tmp[1,,drop=F]
      id_del<-decide_which_document_to_delete(pair=top_pair,strategy = input$Det_DD_strategy)
      blacklist<-c(blacklist,id_del)
      d_tmp<-d_tmp[-union(which(d_tmp[,"a"]%in%blacklist),which(d_tmp[,"b"]%in%blacklist)),,drop=F]
      if(dim(d_tmp)[1]==0){
        break
      }
    })
  }
  
  whitelist<-setdiff(documents,blacklist)
  
  values$Det_DD_current_table<-values$Det_DD_data_display
  values$blacklist<-blacklist
  values$whitelist<-whitelist
})


#' show data table from deduplication with blacklist information
#' depends on:
#'   values$blacklist: values of selected blacklist
#'   values$DD_blacklist: customed blacklist
#'   values$Det_DD_meta: deduplication meta data
output$Det_DD_Table_Black<-DT::renderDataTable({
  validate(
    need(length(values$blacklist)>0,message="No duplicates found.")
  )
  ###chaining deduplication defaults
  data_user<-cbind(values$DD_blacklist,values$Det_DD_meta[as.numeric(values$DD_blacklist),c("token","date")])
  if(dim(data_user)[1]>0){
    data_user<-cbind(data_user,rep("set by user",dim(data_user)[1]))
    colnames(data_user)<-c("id","number of token","date","removal reason")
  }
  
  data<-cbind(setdiff(values$blacklist,values$DD_blacklist),values$Det_DD_meta[as.numeric(setdiff(values$blacklist,values$DD_blacklist)),c("token","date")])
  data<-cbind(data,rep("default strategy",dim(data)[1]))
  colnames(data)<-c("id","number of token","date","removal reason")
  
  data<-rbind(data_user,data)
  
  Diff = shinyInput_big(
    shinyBS::bsButton,
    dim(data)[1],
    'show_diff_',
    label = "",
    style="primary",
    icon=icon("exchange"),
    onclick = 'Shiny.onInputChange(\"show_diff\",  this.id)'
  )
  Keep = shinyInput_big(
    shinyBS::bsButton,
    dim(data)[1],
    'DDkeep_',
    label = "",
    style="success",
    icon=icon("save"),
    onclick = 'Shiny.onInputChange(\"DD_Keep_Black\",  this.id)'
  )
  Remove = shinyInput_big(
    shinyBS::bsButton,
    dim(data)[1],
    'DDremove_',
    label = "",
    style="danger",
    icon=icon("trash"),
    onclick = 'Shiny.onInputChange(\"DD_Remove_Black\",  this.id)'
  )
  data<-cbind(data,Diff,Keep,Remove)
  datatable(data =  data,rownames = F,escape = F,selection = "none",caption = "Documents to be removed",options = list(dom="tp",pageLength=9)
  )
})



#' show data table from deduplication with whitelist information
#' depends on:
#'   values$whitelist: values of whitelist
#'   values$DD_whitelist: customed whitelist
#'   values$Det_DD_meta: meta data from deduplication
output$Det_DD_Table_White<-DT::renderDataTable({
  validate(
    need(length(values$whitelist)>0,message="No duplicates found")
  )
  ###chaining deduplication defaults
  data_user<-cbind(values$DD_whitelist,values$Det_DD_meta[as.numeric(values$DD_whitelist),c("token","date")])
  if(dim(data_user)[1]>0){
    data_user<-cbind(data_user,rep("set by user",dim(data_user)[1]))
    colnames(data_user)<-c("id","number of token","date","removal reason")
  }
  
  
  data<-cbind(setdiff(values$whitelist,values$DD_whitelist),values$Det_DD_meta[as.numeric(setdiff(values$whitelist,values$DD_whitelist)),c("token","date")])
  data<-cbind(data,rep("default strategy",dim(data)[1]))
  colnames(data)<-c("id","number of token","date","removal reason")
  
  data<-rbind(data_user,data)
  
  Diff = shinyInput_big(
    shinyBS::bsButton,
    dim(data)[1],
    'show_diff_white_',
    label = "",
    style="primary",
    icon=icon("exchange"),
    onclick = 'Shiny.onInputChange(\"show_diff_White\",  this.id)'
  )
  Keep = shinyInput_big(
    shinyBS::bsButton,
    dim(data)[1],
    'DDkeep_white_',
    label = "",
    style="success",
    icon=icon("save"),
    onclick = 'Shiny.onInputChange(\"DD_Keep_White\",  this.id)'
  )
  Remove = shinyInput_big(
    shinyBS::bsButton,
    dim(data)[1],
    'DDremove_white_',
    label = "",
    style="danger",
    icon=icon("trash"),
    onclick = 'Shiny.onInputChange(\"DD_Remove_White\",  this.id)'
  )
  data<-cbind(data,Diff,Keep,Remove)
  datatable(data =  data,rownames = F,escape = F,selection = "none",caption = "Documents to be kept",options = list(dom="tp",pageLength=9)
  )
})




#' compare documents and find differences with blacklist
#' depends on:
#'   input$show_diff: differences in result
#'   values$blacklist: values of blacklist
#'   values$Det_DD_current_table: current table of deduplication results
#'   values$documents_for_diff: selected documents to show differences between them 
observeEvent(input$show_diff,ignoreInit = T,{
  id<-stringr::str_split(string =input$show_diff,pattern = "_",simplify = T )[1,3]
  validate(
    need(id!="0",message=F)
  )
  id<-values$blacklist[as.numeric(id)]
  choices<-unique(union(values$Det_DD_current_table[which(values$Det_DD_current_table[,"a"]==id),"b"],values$Det_DD_current_table[which(values$Det_DD_current_table[,"b"]==id),"a"]))
  isolate(shinyjs::runjs('Shiny.onInputChange(\"show_diff\",  "show_diff_0")'))
  values$documents_for_diff<-id
  showModal(modalDialog(easyClose = T,size = "l",
                        selectInput(inputId = "DD_table_modal_diff_select",label = "Compare with:",choices = choices,multiple = F),
                        tags$div(style = 'height: 68vh; overflow-y: auto;',diffr::diffrOutput(outputId = "Det_DD_diffr"))
                        
                        
  ))
})

#' show document differences
#' depends on:
#'   values$Det_DD_meta: meta data of deduplication
#'   values$documents_for_diff: selected documents for comparision
#'   input$DD_table_modal_diff_select: selected tables for modal differences
output$Det_DD_diffr<-diffr::renderDiffr({
  file1 = tempfile()
  writeLines(values$Det_DD_meta[as.numeric(values$documents_for_diff),"body"], con = file1)
  file2 = tempfile()
  writeLines(values$Det_DD_meta[as.numeric(input$DD_table_modal_diff_select),"body"], con = file2)
  diffr::diffr(file1,file2, before = paste0("Document: ",values$documents_for_diff), after = paste0("Document: ",input$DD_table_modal_diff_select))
})


#' keep blacklist
#' depends on:
#'   input$DD_Keep_Black: initiate keeping blacklist
#'   values$DD_whitelist: customed whitelist
#'   values$DD_blacklist: customed blacklist
observeEvent(input$DD_Keep_Black,ignoreInit = T,{
  id<-as.numeric(values$blacklist[as.numeric(stringr::str_split(string = input$DD_Keep_Black,pattern = "_",simplify = T)[1,2])])
  validate(
    need(id!=0,message=F)
  )
  values$DD_whitelist<-c(values$DD_whitelist,id)
  values$DD_blacklist<-setdiff(values$DD_blacklist,id)
  values$DD_recalc<-runif(1,0,1)
  isolate(shinyjs::runjs('Shiny.onInputChange(\"DD_Keep_Black\",  "DDkeep_0")'))
})


#' remove blacklist
#' depends on:
#'   input$DD_Remove_Black: initiate removing of blacklist
#'   values$blacklist: values from blacklist
#'   values$DD_blacklist: customed blacklist
#'   values$DD_whitelist: custome whitelist
observeEvent(input$DD_Remove_Black,ignoreInit = T,{
  id<-as.numeric(values$blacklist[as.numeric(stringr::str_split(string = input$DD_Remove_Black,pattern = "_",simplify = T)[1,2])])
  validate(
    need(id!=0,message=F)
  )
  values$DD_blacklist<-c(values$DD_blacklist,id)
  values$DD_whitelist<-setdiff(values$DD_whitelist,id)
  values$DD_recalc<-runif(1,0,1)
  isolate(shinyjs::runjs('Shiny.onInputChange(\"DD_Remove_Black\",  "DDremove_0")'))
})





#' compare documents and find differences with whitelist
#' depends on:
#'   input$show_diff_White: initiate calculation
#'   values$whitelist: values of whitelist
#'   values$Det_DD_current_table: current table of deduplication results
#'   values$documents_for_diff_white: selected documents for difference calculation
observeEvent(input$show_diff_White,ignoreInit = T,{
  id<-stringr::str_split(string =input$show_diff_White,pattern = "_",simplify = T )[1,4]
  validate(
    need(id!="0",message=F)
  )
  id<-values$whitelist[as.numeric(id)]
  choices<-unique(union(values$Det_DD_current_table[which(values$Det_DD_current_table[,"a"]==id),"b"],values$Det_DD_current_table[which(values$Det_DD_current_table[,"b"]==id),"a"]))
  isolate(shinyjs::runjs('Shiny.onInputChange(\"show_diff_White\",  "show_diff_White_0")'))
  values$documents_for_diff_white<-id
  showModal(modalDialog(easyClose = T,size = "l",
                        selectInput(inputId = "DD_table_modal_diff_select_white",label = "Compare with:",choices = choices,multiple = F),
                        tags$div(style = 'height: 68vh; overflow-y: auto;',diffr::diffrOutput(outputId = "Det_DD_diffr_white"))
                        
                        
  ))
})

#' show difference calculation
#' depends on:
#'   values$Det_DD_meta: meta data
#'   values$documents_for_diff_white: selected documents for calculation
#'   input$DD_table_modal_diff_select_white: show table of  modals for selected documents
output$Det_DD_diffr_white<-diffr::renderDiffr({
  file1 = tempfile()
  writeLines(values$Det_DD_meta[as.numeric(values$documents_for_diff_white),"body"], con = file1)
  file2 = tempfile()
  writeLines(values$Det_DD_meta[as.numeric(input$DD_table_modal_diff_select_white),"body"], con = file2)
  diffr::diffr(file1,file2, before = paste0("Document: ",values$documents_for_diff_white), after = paste0("Document: ",input$DD_table_modal_diff_select_white))
})


#' keep whitelist 
#' depends on:
#'   input$DD_Keep_White: initiate keeping whitelist
#'   values$whitelist: values of whitelist
#'   values$DD_whitelist: customed whitelist
#'   values$DD_blacklist: customed blacklist
#'   values$DD_recalc: initiate recalculation
observeEvent(input$DD_Keep_White,ignoreInit = T,{
  id<-as.numeric(values$whitelist[as.numeric(stringr::str_split(string = input$DD_Keep_White,pattern = "_",simplify = T)[1,3])])
  validate(
    need(id!=0,message=F)
  )
  values$DD_whitelist<-c(values$DD_whitelist,id)
  values$DD_blacklist<-setdiff(values$DD_blacklist,id)
  values$DD_recalc<-runif(1,0,1)
  isolate(shinyjs::runjs('Shiny.onInputChange(\"DD_Keep_White\",  "DDkeep_white_0")'))
})

#' remove whitelist
#' depends on:
#'   input$DD_Remove_White: initiate removing whitelist
#'   values$whitelist: values of whitelist
#'   values$DD_blacklist: customed blacklist
#'   values$DD_whitelist: customed whitelist
#'   values$DD_recalc: start recalculation
observeEvent(input$DD_Remove_White,ignoreInit = T,{
  id<-as.numeric(values$whitelist[as.numeric(stringr::str_split(string = input$DD_Remove_White,pattern = "_",simplify = T)[1,3])])
  validate(
    need(id!=0,message=F)
  )
  values$DD_blacklist<-c(values$DD_blacklist,id)
  values$DD_whitelist<-setdiff(values$DD_whitelist,id)
  values$DD_recalc<-runif(1,0,1)
  isolate(shinyjs::runjs('Shiny.onInputChange(\"DD_Remove_White\",  "DDremove_white_0")'))
})




##########################graph#########################



#' show deduplication network
#' depends on:
#'   values$Det_DD_current_table: render table with current deduplication results
#'   values$blacklist: values of blacklist
#'   values$whitelist: values of whitelist
#'   values$DD_whitelist: customed whitelist
#'   values$DD_blacklist: customed blacklist
#'   input$Det_DD_use_igraph_layout: chosen igraph layout
#'   values$Det_DD_meta: meta data from deduplication
output$Det_DD_Network<-visNetwork::renderVisNetwork({
  validate(
    need(!is.null(values$Det_DD_current_table),message="Calculating...")
  )
  validate(
    need(dim(values$Det_DD_current_table)[1]>0,message="No duplicates found.")
  )
  t<-values$Det_DD_current_table
  
  blacklist<-cbind(values$blacklist,"#DB4A54")
  whitelist<-cbind(values$whitelist,"#5DD39E")
  
  groups<-cbind(unique(union(t[,"a"],t[,"b"])),rep(0,length(unique(union(t[,"a"],t[,"b"])))))
  groups[which(groups[,1]%in%values$whitelist),2]<-"Keep by default strategy"
  groups[which(groups[,1]%in%values$blacklist),2]<-"Remove by default strategy"
  groups[which(groups[,1]%in%values$DD_whitelist),2]<-"Keep by user setting"
  groups[which(groups[,1]%in%values$DD_blacklist),2]<-"Remove by user setting"
  
  #browser()
  whitelist_user<-NULL
  blacklist_user<-NULL
  if(length(which(values$whitelist%in%values$DD_whitelist))>0){
    whitelist<-whitelist[-which(values$whitelist%in%values$DD_whitelist),]
    whitelist_user<-cbind(values$DD_whitelist,"#3B42A5")
  }
  if(length(which(values$blacklist%in%values$DD_blacklist))>0){
    blacklist<-blacklist[-which(values$blacklist%in%values$DD_blacklist),]
    blacklist_user<-cbind(values$DD_blacklist,"orange")
  }
  colors<-rbind(blacklist,whitelist,whitelist_user,blacklist_user)
  
  nodes<-data.frame(
    id=unique(union(t[,"a"],t[,"b"])),
    label=paste("Document",unique(union(t[,"a"],t[,"b"]))),
    group=groups[,2]
  )
  colnames(colors)<-c("id","color")
  #nodes<-merge(x = nodes,y = colors,by.x = "id",by.y = "id")
  edges<-data.frame(
    from=t[,"a"],
    to=t[,"b"],
    length=(t[,"similarity"]*10),
    value=t[,"similarity"],
    title=t[,"similarity"],
    color=fifer::number.to.colors(value = t[,"similarity"],colors = c("gold","orange1","orangered1","red2"))
  )
  
  # get document titles from db
  titles <- values$Det_DD_meta[,"title"]
  nodes$label<-titles[as.numeric(nodes$id)]
  if(input$Det_DD_use_igraph_layout==TRUE){
    network<-visNetwork::visNetwork(nodes = nodes,edges = edges) %>%
      visNetwork::visIgraphLayout(randomSeed = 1)%>%
      visNetwork::visGroups(groupname = "Keep by default strategy", shape="dot",color = "#07d94b") %>%
      visNetwork::visGroups(groupname = "Remove by default strategy", shape="dot",color = "#f54745") %>%
      visNetwork::visGroups(groupname = "Keep by user setting", shape="dot",size=28,color = "#017015",borderWidth=2.5) %>%
      visNetwork::visGroups(groupname = "Remove by user setting", shape="dot",size=28,color = "#b30200",borderWidth=2.5) %>%
      visNetwork::visEdges(scaling=list(min=2,max=8))%>%
      visNetwork::visLegend(position = "right")%>%
      visNetwork::visEvents(type = "on",doubleClick = "function(properties) {
                 Shiny.onInputChange(\"DD_graph_node_selected\",  properties.nodes)            }")
  }
  else{
    network<-visNetwork::visNetwork(nodes = nodes,edges = edges) %>%
      visNetwork::visPhysics(stabilization = FALSE)%>%
      visNetwork::visEdges(smooth = FALSE)%>%
      visNetwork::visGroups(groupname = "Keep by default strategy", shape="dot",color = "#07d94b") %>%
      visNetwork::visGroups(groupname = "Remove by default strategy", shape="dot",color = "#f54745") %>%
      visNetwork::visGroups(groupname = "Keep by user setting", shape="dot",size=28,color = "#017015",borderWidth=2.5) %>%
      visNetwork::visGroups(groupname = "Remove by user setting", shape="dot",size=28,color = "#b30200",borderWidth=2.5) %>%
      visNetwork::visLegend(position = "right")%>%
      visNetwork::visEvents(type = "on",doubleClick = "function(properties) {
                 Shiny.onInputChange(\"DD_graph_node_selected\",  properties.nodes)
            }"
      )
  }
})

#' observe if a node from graph is selected
#' depends on:
#'   input$DD_graph_node_selected: selected node from graph
#'   values$Det_DD_current_table: current table for deduplication
#'   values$Det_DD_meta: meta data from deduplication
observeEvent(input$DD_graph_node_selected,ignoreNULL = T,{
  validate(
    need(input$DD_graph_node_selected!=0,message=F)
  )
  choices<-unique(union(values$Det_DD_current_table[which(values$Det_DD_current_table[,"a"]==input$DD_graph_node_selected),"b"],values$Det_DD_current_table[which(values$Det_DD_current_table[,"b"]==input$DD_graph_node_selected),"a"]))
  showModal(
    modalDialog(title = paste("Document: ",input$DD_graph_node_selected),size = "l",easyClose = F,
                navbarPage(title = "",id = "DD_graph_modal",
                           tabPanel("Document Setting",
                                    tags$h3("Metadata"),
                                    tags$h4("Title:"),
                                    tags$span(values$Det_DD_meta[as.numeric(input$DD_graph_node_selected),"title"]),
                                    tags$h4("Release date:"),
                                    tags$span(values$Det_DD_meta[as.numeric(input$DD_graph_node_selected),"date"]),
                                    tags$h4("Number of token:"),
                                    tags$span(values$Det_DD_meta[as.numeric(input$DD_graph_node_selected),"token"]),
                                    tags$h4("Node degree:"),
                                    tags$span(values$Det_DD_node_degree[which(values$Det_DD_node_degree[,1]==input$DD_graph_node_selected),2]),
                                    tags$br(),
                                    tags$hr(),
                                    fluidRow(
                                      shinyBS::bsButton(inputId = "Det_DD_graph_keep",label = "keep node",icon = icon("save"),style = "success"),
                                      shinyBS::bsButton(inputId = "Det_DD_graph_remove",label = "remove node",icon = icon("trash"),style = "danger")
                                    )
                           ),
                           tabPanel("Diff View",
                                    selectInput(inputId = "DD_graph_modal_diff_select",label = "Compare with:",choices = choices,multiple = F),
                                    tags$div(style = 'height: 58vh; overflow-y: auto;',diffr::diffrOutput(outputId = "Det_DD_diffr_graph"))
                           )
                ),footer=actionButton("deduplication_dissmiss_modal",label="Dismiss")
    )
  )
  
})

#' observe dissmissing of modals
#' depends on:
#'   input$deduplication_dissmiss_modal: dissmiss modals of deduplication
observeEvent(ignoreNULL = T,input$deduplication_dissmiss_modal,{
  removeModal()
  isolate(shinyjs::runjs('Shiny.onInputChange(\"DD_graph_node_selected\",  0)'))
})


#' deduplication for differences calculation in graph
#' depends on:
#'   values$Det_DD_meta: meta data from deduplication
#'   input$DD_graph_node_selected: selected node from deduplication graph
#'   input$DD_graph_modal_diff_select: selected modals from graph differences
output$Det_DD_diffr_graph<-diffr::renderDiffr({
  file1 = tempfile()
  writeLines(values$Det_DD_meta[as.numeric(input$DD_graph_node_selected),"body"], con = file1)
  file2 = tempfile()
  writeLines(values$Det_DD_meta[as.numeric(input$DD_graph_modal_diff_select),"body"], con = file2)
  diffr::diffr(file1,file2, before = paste0("Document: ", input$DD_graph_node_selected), after = paste0("Document: ",input$DD_graph_modal_diff_select))
})

#' observe the keeping of elements in graph
#' depends on:
#'   input$Det_DD_graph_keep: elements from keeping functions
#'   input$DD_graph_node_selected: selected node in graph
#'   values$DD_whitelist: customed whitelist
#'   values$DD_blacklist: customed blacklist
#'   values$DD_recalc: start recalculation
observeEvent(input$Det_DD_graph_keep,ignoreInit = T,{
  id<-input$DD_graph_node_selected
  validate(
    need(id!=0,message=F)
  )
  #values$Det_DD_data_display[which(values$Det_DD_data_display[,"a"]==id),"keep_a"]<-9
  #values$Det_DD_data_display[which(values$Det_DD_data_display[,"b"]==id),"keep_b"]<-9
  values$DD_whitelist<-c(values$DD_whitelist,input$DD_graph_node_selected)
  values$DD_blacklist<-setdiff(values$DD_blacklist,id)
  isolate(shinyjs::runjs('Shiny.onInputChange(\"DD_graph_node_selected\",  0)'))
  print("whitelist hinzu")
  values$DD_recalc<-runif(1,0,1)
})

#' observe remove-action in graph
#' depends on:
#'   input$Det_DD_graph_remove: initiate removing of element in graph
#'   input$DD_graph_node_selected: selected nodes
#'   values$DD_blacklist: customed blacklist
#'   values$DD_whitelist: customed whitelist 
#'   values$DD_recalc: start recalculation
observeEvent(input$Det_DD_graph_remove,ignoreInit = T,{
  id<-input$DD_graph_node_selected
  validate(
    need(id!=0,message=F)
  )
  #values$Det_DD_data_display[which(values$Det_DD_data_display[,"a"]==id),"keep_a"]<-9
  #values$Det_DD_data_display[which(values$Det_DD_data_display[,"b"]==id),"keep_b"]<-9
  values$DD_blacklist<-c(values$DD_blacklist,id)
  values$DD_whitelist<-setdiff(values$DD_whitelist,id)
  isolate(shinyjs::runjs('Shiny.onInputChange(\"DD_graph_node_selected\",  0)'))
  print("blacklist hinzu")
  values$DD_recalc<-runif(1,0,1)
})

#' obsere help-icon is selected
#' depends on:
#'   input$DD_help: help icon selected
observeEvent(input$DD_help,{
  showModal(
    modalDialog(title = "How to use graph:",
                tags$div("You can double click a node to get further information or change it's settings!")
    )
  )
  
})

#' observe if saving of collaction is selected
#' depends on:
#'   input$Det_DD_save_collection: initiate saving of collection
#'   values$blacklist: values from blacklist
#'   values$update_solr_url: update solr url
#'   values$update_solr_port: update port from solr
#'   values$coll_saved: collection saved successfully
#'   values$num_collections: number from collection
observeEvent(ignoreInit = T,input$Det_DD_save_collection,{
  if(length(values$blacklist)>0){
    final_remove<-as.numeric(values$blacklist)
    info<-values$Det_DD_info
    info[[1]]<-info[[1]][-as.numeric(final_remove),1,drop=F]
    info[[2]]<-info[[2]][-as.numeric(final_remove),1,drop=F]
    info[[3]]<-info[[3]][-as.numeric(final_remove),1,drop=F]
    info[[4]]<-paste0(info[[4]]," | ",length(final_remove)," documents removed in document deduplication with interaction by User:",values$user, " | Documents removed:(",paste(final_remove,collapse=", ") ,")" )
    info[[5]]<-paste0(info[[5]], " deduplicated")
    if(file.exists(paste("collections/collections/",info[[5]],".RData",sep = ""))){
      info[[5]]<-paste0(info[[5]], " ",Sys.time())
    }
    info[[6]]<-info[[6]][-as.numeric(final_remove),1,drop=F]
    info[[7]]<-info[[7]][-as.numeric(final_remove),1,drop=F]
    
    host<-values$update_solr_url
    port<-values$update_solr_port
    try({future::future(expr = {
      body<-create_body_solr_update_add(ids = info[[3]][,1],field_name = "collections",values = rep(info[[5]],length(info[[3]][,1])))
      conn<-solrium::SolrClient$new(host = host,port = port,path="search")
      try(silent = T,{
        rm(solr_update_working)
        conn$update_atomic_json(name = "iLCM",body = body)->solr_update_working
      })
      if(!exists("solr_update_working")){
        conn$update_atomic_json(name = "iLCM",body = body)
      }
      solrium::commit(conn = conn,name="iLCM")
    }) %...>% future:::ClusterRegistry(action = "stop")
    })
    
    save(info,file=paste("collections/collections/",info[[5]],".RData",sep = ""))
    save_collection_to_db(info)
    values$coll_saved<-runif(1,min = 0,max = 1)
    values$num_collections<-length(list.files("collections/collections/"))
    shinyWidgets::sendSweetAlert(session=session,title = "Collection created",text = paste0("new collection with name:",paste0(info[[5]])," has been created"),type = "success")
  }
  else{
    shinyWidgets::sendSweetAlert(session=session,title = "No new collection created",text = "There was no duplicate found.",type = "warning")
  }
})

#' handle download of deduplication
#' depends on:
#'    values$Det_DD_meta: meta data from deduplication
#'    values$blacklist: values from blacklist
output$Det_DD_download_clean<-downloadHandler(
  filename = function() {
    paste('duplicate_free_collection', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    export_data<-as.matrix(values$Det_DD_meta[-as.numeric(values$blacklist),])
    export_data<-apply(X = export_data,MARGIN = 2,FUN = function(x){stringr::str_replace_all(string = x,pattern = '"',replacement = "'")})
    write.table(export_data, con,col.names = F,row.names = F,sep=",",quote = T)
  }
)

#' download duplicates
#' depends on:
#'   values$Det_DD_meta:  deduplication meta data
#'   values$blacklist: values from blacklist
output$Det_DD_download_duplicates<-downloadHandler(
  filename = function() {
    paste('duplicates', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    values$Det_DD_results->results
    meta<-values$Det_DD_meta
    blacklist<-values$blacklist
    whitelist<-values$whitelist
    all_na<-which(apply(X = meta,MARGIN = 2,FUN = function(x){
      all(is.na(x))
    }))
    if(length(all_na)>0){
      meta<-meta[,-all_na]
    }
    results<-results[which(results$score>input$Det_DD_threshold),]
    results<-cbind(1:nrow(results),results)
    duplicates<-matrix(c(""),nrow = (nrow(results)*2),ncol = (4+ncol(meta)))
    count=0
    for(i in 1:nrow(results)){
      count=count+1
      duplicate<-results[i,]
      information1<-c(duplicate[1,1],duplicate[1,4],duplicate[1,2]%in%whitelist,duplicate[1,2]%in%blacklist)
      information1<-c(information1,unlist(meta[as.numeric(duplicate[1,2]),,drop=T]))
      duplicates[count,]<-information1
      count=count+1
      information2<-c(duplicate[1,1],duplicate[1,4],duplicate[1,3]%in%whitelist,duplicate[1,3]%in%blacklist)
      information2<-c(information2,unlist(meta[as.numeric(duplicate[1,3]),,drop=T]))
      duplicates[count,]<-information2
    }
    colnames(duplicates)<-c(c("Duplicate ID","Similarity","Keep","Delete"),colnames(meta))

    #export_data<-as.matrix(values$Det_DD_meta[as.numeric(values$blacklist),])
    export_data<-apply(X = duplicates,MARGIN = 2,FUN = function(x){stringr::str_replace_all(string = x,pattern = '"',replacement = "'")})
    write.table(export_data, con,col.names = T,row.names = F,sep=",",quote = T)
  }
)