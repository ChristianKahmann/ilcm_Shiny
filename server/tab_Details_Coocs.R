
observe({
  values$Details_Data_CO
  validate(need(!is.null(input$Det_CO_Measure),message=F))
  if(input$Det_CO_Measure=="Dice"){
    load(paste0(values$Details_Data_CO,"/dice.RData"))
    values$coocs_matrix<-coocs_matrix_dice
  }
  if(input$Det_CO_Measure=="Mutual information"){
    load(paste0(values$Details_Data_CO,"/mi.RData"))
    values$coocs_matrix<-coocs_matrix_mi
  }
  if(input$Det_CO_Measure=="Log-likelihood"){
    load(paste0(values$Details_Data_CO,"/log.RData"))
    values$coocs_matrix<-coocs_matrix_log
  }
  if(input$Det_CO_Measure=="Count"){
    load(paste0(values$Details_Data_CO,"/count.RData"))
    values$coocs_matrix<-coocs_matrix_count
  }
})


#create/recreate coocs_network when update button is clicked
observeEvent(input$Det_CO_Update,{
  #render cooc-network using NetworkD3 library
  #load result matrix and input parameters
  word<-isolate(input$Det_CO_Word)
  if(nchar(word)<1){
    shinyWidgets::sendSweetAlert(session=session,title = "no Input specified",type = "warning")
  }
  else{
    depth<-isolate(input$Det_CO_Depth)
    charge<-isolate(input$Det_CO_Charge)
    data<-values$coocs_matrix
    coocs<-data[word,which(data[word,]>0)]
    coocs<-coocs[order(coocs,decreasing = T)][1:min(isolate(input$Det_CO_RootLinks),length(coocs))]
    validate(need(length(coocs)>0,"no co-occurring word found"))
    nodes<-matrix(c(0),0,3)
    links<-matrix(c(0),0,3)
    colnames(nodes)<-c("name","group","size")
    colnames(links)<-c("source","target","value")
    nodes<-rbind(nodes,c(word,1,2))
    if(length(coocs)==1){
      if(is.na(coocs)){
        shinyWidgets::sendSweetAlert(session=session,title = "No co-occurrence found",text = "For your specified word, no co-occurring word could be found",type = "warning")
        return(NULL)
      }
      cooc<-data[word,which(data[word,]>0),drop=F]
      nodes<-rbind(nodes,c(colnames(cooc),2,1))
      links<-rbind(links,c(0,1,(as.double(cooc))))
    }
    else{
      #create first nodes and links
      for (i in 1:length(coocs))
      {
        cooc<-coocs[i]
        nodes<-rbind(nodes,c(names(cooc),2,1))
        links<-rbind(links,c(0,(which(names(coocs)==names(cooc))),(as.double(cooc))))
      }
      length_alt<-1
      length<-dim(nodes)[1]
      #add nodes and edges depending on depth
      if(depth>1){
        for(l in 2:depth){
          for (j in (length_alt+1):length)
          {
            name<-nodes[j,1]
            coocs_iter<-data[name,which(data[name,]>0)]
            coocs_iter<-coocs_iter[-which(names(coocs_iter)%in%nodes[,1])]
            coocs_iter<-coocs_iter[order(coocs_iter,decreasing = T)]
            if(length(coocs_iter)>1)
            {
              for (k in 1:min(length(coocs_iter),isolate(input$Det_CO_LeavesLinks)))
              {
                cooc<-coocs_iter[k]
                nodes<-rbind(nodes,c(names(cooc),(l+1),1))
                links<-rbind(links,c((j-1),(which(nodes[,1]==names(cooc))-1),(as.double(cooc))))
              }
            }
          }
          length_alt<-length
          length<-dim(nodes)[1]
        }
      }
    }
    #add intra edges between existing nodes
    cut<-dim(links)[1]
    count1<-0
    count2<-0
    for(start in nodes[,1]){
      targets<-nodes[which(as.numeric(nodes[,2])>=as.numeric(nodes[which(nodes[,1]==start),2])),1]
      targets<-targets[-which(targets==start)]
      new_links<-matrix(c(rep(count1,length(targets)), (which(nodes[,1]%in%targets)-1),data[start,targets]),ncol=3)
      links<-rbind(links,new_links)
      count1<-count1+1
    }
    links<-unique(links)
    links<-links[which(as.numeric(links[,3])>0),,drop=F]
    #make it undirected // reduce edge pairs a,b ; b,a to a,b
    #browser()
    edges<-data.frame(links[,1:2,drop=F])
    rownames(edges)<-1:dim(edges)[1]
    rownames(links)<-1:dim(edges)[1]
    edges<-t(apply(edges,1, function(x) sort(x)))
    edges<-as.data.frame(edges)
    edges<-unique(edges,Margin=1)
    links<-links[as.numeric(rownames(edges)),,drop=F]
    #delete edges if more than max number are avaiable
    if(dim(links)[1]>isolate(input$Det_CO_Max_Edges)){
      if(cut<isolate(input$Det_CO_Max_Edges)){
        links_to_cut<-links[(cut+1):dim(links)[1],]
        links_to_cut<-links_to_cut[order(links_to_cut[,3],decreasing = T),]
        links_to_cut<-links_to_cut[1:(isolate(input$Det_CO_Max_Edges)-cut),]
        links<-rbind(links[1:cut,],links_to_cut)
      }
      if(cut>isolate(input$Det_CO_Max_Edges)){
        links<-links[1:cut,]
      }
      
    }
    links<-data.frame(links)
    nodes<-data.frame(nodes)
    class(nodes$group)<-"integer"
    class(nodes$size)<-"integer"
    class(links$value)<-"numeric"
    class(links$source)<-"integer"
    class(links$target)<-"integer"
    values$cooc_nodes<-nodes
    values$cooc_edges<-links
    
    output$cooc_heatmap<-renderPlotly({
      word<-isolate(input$Det_CO_Word)
      depth<-isolate(input$Det_CO_Depth)
      data<-values$coocs_matrix
      coocs<-data[word,which(data[word,]>0)]
      coocs<-coocs[order(coocs,decreasing = T)][1:min(isolate(input$Det_CO_RootLinks),length(coocs))]
      validate(need(length(coocs)>0,"no co-occurring word found"))
      nodes<-matrix(c(0),0,3)
      links<-matrix(c(0),0,3)
      colnames(nodes)<-c("name","group","size")
      colnames(links)<-c("source","target","value")
      nodes<-rbind(nodes,c(word,1,2))
      #browser()
      if(length(coocs)==1){
        cooc<-data[word,which(data[word,]>0),drop=F]
        nodes<-rbind(nodes,c(colnames(cooc),2,1))
        links<-rbind(links,c(0,1,(as.double(cooc))))
      }
      else{
        #create first nodes and links
        for (i in 1:length(coocs))
        {
          cooc<-coocs[i]
          nodes<-rbind(nodes,c(names(cooc),2,1))
          links<-rbind(links,c(0,(which(names(coocs)==names(cooc))),(as.double(cooc))))
        }
        length_alt<-1
        length<-dim(nodes)[1]
        #add nodes and edges depending on depth
        if(depth>1){
          for(l in 2:depth){
            for (j in (length_alt+1):length)
            {
              name<-nodes[j,1]
              coocs_iter<-data[name,which(data[name,]>0)]
              coocs_iter<-coocs_iter[-which(names(coocs_iter)%in%nodes[,1])]
              coocs_iter<-coocs_iter[order(coocs_iter,decreasing = T)]
              if(length(coocs_iter)>1)
              {
                for (k in 1:min(length(coocs_iter),isolate(input$Det_CO_LeavesLinks)))
                {
                  cooc<-coocs_iter[k]
                  nodes<-rbind(nodes,c(names(cooc),(l+1),1))
                  links<-rbind(links,c((j-1),(which(nodes[,1]==names(cooc))-1),(as.double(cooc))))
                }
              }
            }
            length_alt<-length
            length<-dim(nodes)[1]
          }
        }
      }
      
      coocs_matrix<-data[nodes[,1],nodes[,1]]
      values$Det_CO_dl_coocs<-coocs_matrix
      if(dim(coocs_matrix)[1]>0){
        hm<-plot_ly(x=rownames(coocs_matrix),y=rownames(coocs_matrix),z=matrix(coocs_matrix,ncol=dim(coocs_matrix)[1]),type="heatmap",colorscale="Greys")
        hm<-layout(p=hm,margin = list(l=100,b=100))
        return(hm)
      }
    })
  }
})

#output$cooc_net<-renderForceNetwork({
#  validate(
#    need(dim(values$cooc_nodes)[1]>0,message = "please press update Plot")
#  )
#  nodes<-values$cooc_nodes
#  links<-values$cooc_edges
#  links[,3]<-links[,3]+1
#  #call coocsChart function with given nodes, edges and charge parameter
#  return(coocsChart(nodes,links,(input$Det_CO_Charge)))
#})

output$visNetwork_cooc_net<-visNetwork::renderVisNetwork({
  validate(
    need(dim(values$cooc_nodes)[1]>0,message = "please press update Plot"),
    need(input$Det_CO_Threshold>=0,message="Please set the Threshold >= 0!"),
    need(input$Det_CO_Threshold<1,message="Please set the Threshold < 1!")
  )
  nodes<-values$cooc_nodes
  edges<-values$cooc_edges
  
  
  
  edges_for_betweenness<-edges
  edges_for_betweenness[,3]<-1
  edges<-edges[which(edges[,3]>(input$Det_CO_Threshold)),]
  
  validate(
    need(dim(edges)[1]>0,message="no edge left")
  )
  
  g <- igraph::graph_from_data_frame(edges_for_betweenness, directed = FALSE, vertices = NULL)
  names(igraph::edge_attr(g))[which(names(igraph::edge_attr(g)) == "value")] <- "weight"
  b <- igraph::betweenness(g, v = igraph::V(g), directed = F, #weights = NULL,
                           nobigint = TRUE, normalized = FALSE)
  b<-b[order(as.numeric(names(b)),decreasing = F)]
  c<-igraph::closeness(g)
  c<-c[order(as.numeric(names(c)),decreasing = F)]
  d<-igraph::degree(g)
  d<-d[order(as.numeric(names(d)),decreasing = F)]
  #browser()
  edges[,1:2]<-edges[,1:2]+1
  edges<-cbind(edges,fifer::number.to.colors(value = log(edges[,3]+1),colors = c("gold","orange1","orangered1","red2")))
  edges<-cbind(edges,(edges[,3]))
  edges[,3]<-(edges[,3])
  edges<-cbind(edges,rep("forestgreen",dim(edges)[1]))
  edges<-cbind(edges,rep(input$Det_CO_smooth,dim(edges)[1]))
  colnames(edges)<-c("from","to","value","color","title","highlight","smooth")
  nodes<-cbind(1:dim(nodes)[1],nodes)
  nodes<-cbind(nodes,rep(T,dim(nodes)[1]))
  if(input$Det_CO_node_scaling=="degree"){
    nodes[,4]<-d
  }
  if(input$Det_CO_node_scaling=="betweenness"){
    nodes[,4]<-b
  }
  if(input$Det_CO_node_scaling=="centrality"){
    nodes[,4]<-c
  }
  #nodes[,4]<-sqrt(b)
  #nodes[,4]<-c
  nodes<-cbind(nodes,apply(nodes,MARGIN = 1,FUN = function(x){paste0(x[2],": ",x[4])}))
  values$name_for_cooc_knk<-nodes[,"name"]
  colnames(nodes)<-c("id","label","group","value","shadow","title")
  visNetwork::visNetwork(nodes = nodes,edges = edges)%>%
    visNetwork::visNodes(font = list(color="black",size=20,background="white"))%>%
    visNetwork::visEdges(scaling=list(min=2,max=8))%>%
    visNetwork::visPhysics(barnesHut = list(gravitationalConstant=input$Det_CO_gravity))%>%
    visNetwork::visLayout(randomSeed = 1,hierarchical = input$Det_CO_Layout)%>%
    visNetwork::visEvents(click = "function(nodes){
                          Shiny.onInputChange('cooc_word', nodes.nodes[0]);
                          ;}"
    )
})

#set clicked word in cooc-graph as input word in knk
# output$coocs_knk_word_ui<-renderUI({
#   if(!is.null(input$cooc_word)){
#     word<-isolate(values$name_for_cooc_knk)[as.numeric(input$cooc_word)]
#     ui_res<-selectizeInput(inputId = "coocs_knk_word",label = "Word",choices = values$coocs_terms,selected = word)
#   }
#   else{
#     ui_res<-selectInput(inputId = "coocs_knk_word",label = "Word",choices = rownames(values$coocs_matrix),selected=character(0))
#   }
#   return(ui_res)
# })
# 
# 
# observeEvent(input$coocs_knk_calc,{
#   output$cooc_kwic_table<-DT::renderDataTable({
#     if(!is.null(input$coocs_knk_word)){
#       result<-kwic2(Tokens = isolate(values$coocs_token),target = isolate(input$coocs_knk_word),k = isolate(input$coocs_knk_k),n=isolate(input$coocs_knk_n))
#       result[,2]<-paste0('<span style="color:red">',result[,2],'</span>')
#       return(datatable(result,escape = F))
#     }
#   })
# })
# 

output$Det_CO_download_coocs<-downloadHandler(
  filename = function() {
    paste('co-occurence_matrix-', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-as.matrix(values$Det_CO_dl_coocs)
    write.csv(data, con)
  }
) 


output$cooc_examples_table<-DT::renderDataTable({
  validate(
    need(length(input$coocs_examples_words)>0,"Please choose at least one word")
  )
  dtm<-values$cooc_examples_dtm[,input$coocs_examples_words,drop=F]
  avail<-rownames(dtm)[which(rowSums(dtm)==length(input$coocs_examples_words))]
  if(length(avail)==0){
    return(NULL)
  }
  unit<-"documents"
  k=input$coocs_examples_k
  colors<-randomcoloR::randomColor(count = length(input$coocs_examples_words),luminosity = "dark")
  
  if(stringr::str_detect(string = rownames(dtm)[1],pattern = "[0-9]_[0-9]")){
    unit<-"sentences"
  }
  number_of_examples<-length(avail)
  if(input$coocs_examples_all==FALSE){
    number_of_examples<-min(input$coocs_examples_n,length(avail))
  }
  text<-matrix(c(0),number_of_examples,1)
  for(i in 1:number_of_examples){
    tokens<-values$coocs_token[which(values$coocs_token[,1]==avail[i]),]
    id_targets<-union(which(tolower(tokens[,"token"])%in%input$coocs_examples_words),which(tolower(tokens[,"lemma"])%in%input$coocs_examples_words))[1]
    example<-paste(tokens[max(1,(min(id_targets)-k)):min(dim(tokens)[1],(max(id_targets)+k)),"token"],collapse=" ")
    text[i,1]<-example
  }
  document_ids<-avail[1:number_of_examples]
  values$coocs_examples_document_ids<-document_ids
  values$coocs_examples_texts<-text
  for(i in 1:length(input$coocs_examples_words)){
    word<-input$coocs_examples_words[i]
    text[,1]<-stringr::str_replace_all(string = tolower(text[,1]),pattern = paste(" ",word," ",sep=""),replacement = paste0('<b style="color:',colors[i],'"> ',word,' </b>'))
  }
  
  data<-data.frame(Text=text,
                   SeeDocument = shinyInput(
                     shinyBS::bsButton,
                     length(document_ids),
                     'coocs_kwic_show_doc_button_',
                     label = "",
                     style="primary",
                     icon=icon("search"),
                     onclick = 'Shiny.onInputChange(\"coocs_kwic_document\",  this.id)'
                   ),stringsAsFactors = FALSE)
  

  return(datatable(data = data,escape=F,selection="none"))
})


#kwic show documents 
observeEvent(input$coocs_kwic_document,{
  selected_row<-as.numeric(stringr::str_split(string = input$coocs_kwic_document,pattern = "_",simplify = T)[1,6])
  validate(
    need(selected_row>0,message=F)
  )
  dataset<- stringr::str_split(string=values$coocs_examples_document_ids[selected_row],pattern = "_",simplify = T)[1,1]
  doc_id<- stringr::str_split(string=values$coocs_examples_document_ids[selected_row],pattern = "_",simplify = T)[1,2]
  token<-get_token_from_db(dataset = dataset,doc_ids = doc_id,host=values$host,port=values$port)
  
  text<-paste(token[,"word"],collapse=" ")
  showModal(
    modalDialog(title = paste0("Document: ",dataset,"_",doc_id),easyClose = T,
      tags$div(text)
  ))
  isolate(shinyjs::runjs('Shiny.onInputChange(\"coocs_kwic_document\",  "coocs_kwic_show_doc_button_0")'))
})







#link downloadbutton for the exampel texts in  co-occurrences  restuls tab
output$Det_CO_download_examples<-downloadHandler(
  filename = function() {
    paste('examples-', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-as.matrix(values$coocs_examples_texts)
    write.csv(data, con)
  }
)  

#link downloadbutton for the top co-occurrecnes in coocs results tab
output$coocs_download_dice<-downloadHandler(
  filename = function() {
    paste('results-dice_', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-as.matrix(values$coocs_top_dl_dice)
    write.csv(data, con)
  }
)  

output$coocs_download_mi<-downloadHandler(
  filename = function() {
    paste('results-mi_', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-as.matrix(values$coocs_top_dl_mi)
    write.csv(data, con)
  }
)  

output$coocs_download_log<-downloadHandler(
  filename = function() {
    paste('results-log_', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-as.matrix(values$coocs_top_dl_log)
    write.csv(data, con)
  }
)  


output$coocs_examples_stats<-renderUI({
  validate(
    need(length(input$coocs_examples_words)>0,message=F)
  )
  dtm<-values$cooc_examples_dtm[,input$coocs_examples_words,drop=F]
  avail<-which(rowSums(dtm)==length(input$coocs_examples_words))
  unit<-"documents"
  if(stringr::str_detect(string = rownames(dtm)[1],pattern = ".")){
    unit<-"sentences"
  }
  l<-length(avail)
  if(l==0){
    return(tags$h4("the chosen words don't occcur together in any document!"))
  }
  else{
    return(tags$h4(paste0("the chosen words occcur together in ",l," ",unit)))
  }
  
})


observeEvent(input$tabBox_coocs,{
  if(input$tabBox_coocs=="Top-Co-occurrences"){
    if(values$coocs_load_top==FALSE){
      load(paste0(values$Details_Data_CO,"/dice.RData"))
      values$coocs_dice<-coocs_matrix_dice
      load(paste0(values$Details_Data_CO,"/mi.RData"))
      values$coocs_mi<-coocs_matrix_mi
      load(paste0(values$Details_Data_CO,"/log.RData"))
      values$coocs_log<-coocs_matrix_log
      load(paste0(values$Details_Data_CO,"/count.RData"))
      values$coocs_count<-coocs_matrix_count
      print("data load for top coocs")
      values$coocs_load_top<-TRUE
    }
  }
  if(input$tabBox_coocs=="Kwic"){
    if(values$coocs_load_examples==FALSE){
      load(paste0(values$Details_Data_CO,"/dtm.RData"))
      values$cooc_examples_dtm<-tmca.util::make_binary(dtm = dtm)
      
      values$coocs_load_examples<-TRUE
    }
  }
})

output$coocs_top_dice_table<-DT::renderDataTable({
  validate(need(
    values$coocs_load_top==TRUE,message=F)
  )
  matrix<-values$coocs_dice
  if(input$coocs_top_word1!=""){
    matrix<-matrix[input$coocs_top_word1,,drop=F]
  }
  if(input$coocs_top_word2!=""){
    matrix<-matrix[,input$coocs_top_word2,drop=F]
    if(input$coocs_top_word1==""){
      matrix<-t(matrix)
    }
  }
  y <- matrix[which(matrix>0)]
  keep<-order(y,decreasing=T)[1:min(length(y),(2*input$coocs_top_max))]
  x <- which(matrix>0, arr.ind = T)
  x<-x[keep,,drop=F]
  #remove dublicates
  x<-t(apply(x,1, function(i) sort(i)))
  x<-as.data.frame(x)
  x<-unique(x,Margin=1)
  data<-matrix(c(0),dim(x)[1],3)
  data[,1]<-rownames(matrix)[x[,1]]
  data[,2]<-colnames(matrix)[x[,2]]
  data[,3]<-matrix[as.matrix(x)]
  data<-data[order(data[,3],decreasing=T),,drop=F]
  data[,3]<-round(as.numeric(data[,3]),digits = 4)
  values$coocs_top_dl_dice<-data
  datatable(data = data)
})

output$coocs_top_mi_table<-DT::renderDataTable({
  validate(need(
    values$coocs_load_top==TRUE,message=F)
  )
  matrix<-values$coocs_mi
  if(input$coocs_top_word1!=""){
    matrix<-matrix[input$coocs_top_word1,,drop=F]
  }
  if(input$coocs_top_word2!=""){
    matrix<-matrix[,input$coocs_top_word2,drop=F]
    if(input$coocs_top_word1==""){
      matrix<-t(matrix)
    }
  }
  y <- matrix[which(matrix>0)]
  keep<-order(y,decreasing=T)[1:min(length(y),(2*input$coocs_top_max))]
  x <- which(matrix>0, arr.ind = T)
  x<-x[keep,,drop=F]
  x<-t(apply(x,1, function(i) sort(i)))
  x<-as.data.frame(x)
  x<-unique(x,Margin=1)
  data<-matrix(c(0),dim(x)[1],3)
  data[,1]<-rownames(matrix)[x[,1]]
  data[,2]<-colnames(matrix)[x[,2]]
  data[,3]<-matrix[as.matrix(x)]
  data<-data[order(data[,3],decreasing=T),,drop=F]
  data[,3]<-round(as.numeric(data[,3]),digits = 4)
  values$coocs_top_dl_mi<-data
  datatable(data = data)
})

output$coocs_top_log_table<-DT::renderDataTable({
  validate(need(
    values$coocs_load_top==TRUE,message=F)
  )
  matrix<-values$coocs_log
  if(input$coocs_top_word1!=""){
    matrix<-matrix[input$coocs_top_word1,,drop=F]
  }
  if(input$coocs_top_word2!=""){
    matrix<-matrix[,input$coocs_top_word2,drop=F]
    if(input$coocs_top_word1==""){
      matrix<-t(matrix)
    }
  }
  y <- matrix[which(matrix>0)]
  keep<-order(y,decreasing=T)[1:min(length(y),(2*input$coocs_top_max))]
  x <- which(matrix>0, arr.ind = T)
  x<-x[keep,,drop=F]
  x<-t(apply(x,1, function(i) sort(i)))
  x<-as.data.frame(x)
  x<-unique(x,Margin=1)
  data<-matrix(c(0),dim(x)[1],3)
  data[,1]<-rownames(matrix)[x[,1]]
  data[,2]<-colnames(matrix)[x[,2]]
  data[,3]<-matrix[as.matrix(x)]
  data<-data[order(as.numeric(data[,3]),decreasing=T),,drop=F]
  data[,3]<-round(as.numeric(data[,3]),digits = 4)
  values$coocs_top_dl_log<-data
  datatable(data = data)
})

