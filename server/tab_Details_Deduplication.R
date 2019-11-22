observeEvent(ignoreInit = T,input$Det_DD_reset_user_input,{
  values$DD_whitelist<-NULL
  values$DD_blacklist<-NULL
})



observe({
  validate(
    need(!is.null(values$Det_DD_results),message=F),
    need(!is.null(input$Det_DD_strategy),message=F),
    need(!is.null(input$Det_DD_threshold),message=F)
  )
  input$Det_DD_strategy
  input$Det_DD_threshold
  
  
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
    #browser()
    for(i in 1:dim(data)[1]){
      if(input$Det_DD_strategy=="longest"){
        try({
          remove<-which.max(c(values$Det_DD_meta[data[i,1],"token"],values$Det_DD_meta[data[i,4],"token"]))
        })
        if(length(remove)==0){
          remove<-sample(x = c(1,2),size = 1)
        }
      }  
      if(input$Det_DD_strategy=="shortest"){
        try({
          remove<-which.min(c(values$Det_DD_meta[data[i,1],"token"],values$Det_DD_meta[data[i,4],"token"]))
        })
        if(length(remove)==0){
          remove<-sample(x = c(1,2),size = 1)
        }
      }
      if(input$Det_DD_strategy=="latest"){
        try({
          remove<-which.max(c(values$Det_DD_meta[data[i,1],"date"],values$Det_DD_meta[data[i,4],"date"]))
        })
        if(length(remove)==0){
          remove<-sample(x = c(1,2),size = 1)
        }
      }
      if(input$Det_DD_strategy=="earliest"){
        try({
          remove<-which.min(c(values$Det_DD_meta[data[i,1],"date"],values$Det_DD_meta[data[i,4],"date"]))
        })
        if(length(remove)==0){
          remove<-sample(x = c(1,2),size = 1)
        }
      }
      if(input$Det_DD_strategy=="maximum node degree"){
        #browser()
        try({
          remove<-which.max(c(data[i,"Freq.x"],data[i,"Freq.y"]))
        })
        if(length(remove)==0){
          remove<-sample(x = c(1,2),size = 1)
        }
      }
      if(input$Det_DD_strategy=="random"){
        remove<-sample(x = c(1,2),size = 1)
      }
      final_remove[i,remove]<-1
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
    #browser()
    values$Det_DD_data_display<-data
  }
  else{
    values$Det_DD_data_display<-data
    values$Det_DD_current_table<-data
    values$blacklist<-NULL
    values$whitelist<-NULL
  }
})


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
  if(dim(d_tmp)[1]>0){
      repeat({
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
  
  # #set by user
  # values$DD_whitelist<-unique(c(union(d_tmp[which(d_tmp[,"keep_a"]==9),"a"],d_tmp[which(d_tmp[,"keep_b"]==9),"b"]),isolate(values$DD_whitelist)))
  # values$DD_blacklist<-setdiff(unique(c(union(d_tmp[which(d_tmp[,"keep_a"]==8),"a"],d_tmp[which(d_tmp[,"keep_b"]==8),"b"]),isolate(values$DD_blacklist))),values$DD_whitelist)
  # 
  # blacklist<-setdiff(unique(c(values$DD_blacklist,union(d_tmp[which(d_tmp[,c("a")]%in%as.numeric(isolate(values$DD_whitelist))),"b"],d_tmp[which(d_tmp[,c("b")]%in%as.numeric(isolate(values$DD_whitelist))),"a"]))),values$DD_whitelist)
  # whitelist<-unique(c(values$DD_whitelist,setdiff(union(d_tmp[which(d_tmp[,c("a")]%in%as.numeric(blacklist)),"b"],d_tmp[which(d_tmp[,c("b")]%in%as.numeric(blacklist)),"a"]),union(values$DD_whitelist,blacklist))[1]))
  # whitelist<-whitelist[!is.na(whitelist)]
  # 
  # repeat({
  #   repeat({
  #     lenB<-length(blacklist)
  #     blacklist<-unique(c(blacklist,setdiff(union(d_tmp[which(d_tmp[,c("a")]%in%as.numeric(whitelist)),"b"],d_tmp[which(d_tmp[,c("b")]%in%as.numeric(whitelist)),"a"]),union(blacklist,whitelist))))
  #     blacklist<-blacklist[!is.na(blacklist)]
  #     if(length(blacklist)==lenB ){break}
  #   })
  #   lenW<-length(whitelist)
  #   whitelist<-unique(c(whitelist,setdiff(union(d_tmp[which(d_tmp[,c("a")]%in%as.numeric(blacklist)),"b"],d_tmp[which(d_tmp[,c("b")]%in%as.numeric(blacklist)),"a"]),union(whitelist,blacklist))[1]))
  #   whitelist<-whitelist[!is.na(whitelist)]
  #   if(length(whitelist)==lenW ){break}
  # })
  # 
  # repeat({
  #   seen<-unique(union(c(which(d_tmp[,"a"]%in%whitelist),which(d_tmp[,"b"]%in%whitelist)),c(which(d_tmp[,"a"]%in%blacklist),which(d_tmp[,"b"]%in%blacklist))))
  #   if(length(seen)==0){
  #     d_rest<-d_tmp
  #   }
  #   else{
  #     d_rest<-d_tmp[-seen,]
  #   }
  #   if(dim(d_rest)[1]==0){break}
  #   if(d_rest[1,"keep_a"]==1){
  #     whitelist<-unique(c(whitelist,d_rest[1,"a"]))
  #   }
  #   else{
  #     whitelist<-unique(c(whitelist,d_rest[1,"b"]))
  #   }
  #   
  #   #browser()
  #   repeat({
  #     repeat({
  #       lenB<-length(blacklist)
  #       blacklist<-unique(c(blacklist,setdiff(union(d_tmp[which(d_tmp[,c("a")]%in%as.numeric(whitelist)),"b"],d_tmp[which(d_tmp[,c("b")]%in%as.numeric(whitelist)),"a"]),union(blacklist,whitelist))))
  #       blacklist<-blacklist[!is.na(blacklist)]
  #       if(length(blacklist)==lenB ){break}
  #     })
  #     lenW<-length(whitelist)
  #     whitelist<-unique(c(whitelist,setdiff(union(d_tmp[which(d_tmp[,c("a")]%in%as.numeric(blacklist)),"b"],d_tmp[which(d_tmp[,c("b")]%in%as.numeric(blacklist)),"a"]),union(whitelist,blacklist))[1]))
  #     whitelist<-whitelist[!is.na(whitelist)]
  #     if(length(whitelist)==lenW ){break}
  #   })
  # })
  
  
  values$Det_DD_current_table<-values$Det_DD_data_display
  values$blacklist<-blacklist
  values$whitelist<-whitelist
})



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


output$Det_DD_diffr<-diffr::renderDiffr({
  file1 = tempfile()
  writeLines(values$Det_DD_meta[as.numeric(values$documents_for_diff),"body"], con = file1)
  file2 = tempfile()
  writeLines(values$Det_DD_meta[as.numeric(input$DD_table_modal_diff_select),"body"], con = file2)
  diffr::diffr(file1,file2, before = paste0("Document: ",values$documents_for_diff), after = paste0("Document: ",input$DD_table_modal_diff_select))
})



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


output$Det_DD_diffr_white<-diffr::renderDiffr({
  file1 = tempfile()
  writeLines(values$Det_DD_meta[as.numeric(values$documents_for_diff_white),"body"], con = file1)
  file2 = tempfile()
  writeLines(values$Det_DD_meta[as.numeric(input$DD_table_modal_diff_select_white),"body"], con = file2)
  diffr::diffr(file1,file2, before = paste0("Document: ",values$documents_for_diff_white), after = paste0("Document: ",input$DD_table_modal_diff_select_white))
})



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




output$Det_DD_Network<-visNetwork::renderVisNetwork({
  validate(
    need(!is.null(values$Det_DD_current_table),message=F),
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
                 Shiny.onInputChange(\"DD_graph_node_selected\",  properties.nodes)
            }"
      )
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

observeEvent(ignoreNULL = T,input$deduplication_dissmiss_modal,{
  removeModal()
  isolate(shinyjs::runjs('Shiny.onInputChange(\"DD_graph_node_selected\",  0)'))
})



output$Det_DD_diffr_graph<-diffr::renderDiffr({
  file1 = tempfile()
  writeLines(values$Det_DD_meta[as.numeric(input$DD_graph_node_selected),"body"], con = file1)
  file2 = tempfile()
  writeLines(values$Det_DD_meta[as.numeric(input$DD_graph_modal_diff_select),"body"], con = file2)
  diffr::diffr(file1,file2, before = paste0("Document: ", input$DD_graph_node_selected), after = paste0("Document: ",input$DD_graph_modal_diff_select))
})


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


observeEvent(input$DD_help,{
  showModal(
    modalDialog(title = "How to use graph:",
                tags$div("You can double click a node to get further information or change it's settings!")
    )
  )
  
})

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
    shinyWidgets::sendSweetAlert(session=session,title = "Collection created",text = paste0("new collection with name:",paste0(info[[5]])," has been created"),type = "success")
  }
  else{
    shinyWidgets::sendSweetAlert(session=session,title = "No new collection created",text = "There was no duplicate found.",type = "warning")
  }
})

