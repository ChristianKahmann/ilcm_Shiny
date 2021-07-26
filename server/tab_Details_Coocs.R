#' details for cooccurrence analysis
#' depends on:
#'    values$Details_Data_CO: cooccurrence analysis detailed data
#'    input$Det_CO_Measure: selected measurement for cooccurrence analysis
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


#' create/recreate coocs_network when update button is clicked
#' depends on:
#'   input$Det_CO_Update: update cooccurrence analysis
#'   input$Det_CO_Word: selected words for cooccurrence analysis
#'   input$Det_CO_Depth: cooccurrence analysis depth
#'   input$Det_CO_Charge: cooccurrence analysis charge
#'   values$coocs_matrix: matrix from cooccurrence analysis
#'   input$Det_CO_RootLinks: root links from cooccurrence analysis
#'   input$Det_CO_LeavesLinks: leaves links for cooccurrence analysis
#'   input$Det_CO_Max_Edges: maximal number if edges from cooccurrence tree
#'   values$cooc_nodes: all nodes from cooccurrence tree
#'   values$cooc_edges: all edges from cooccurrence tree
#'   values$Det_CO_dl_coocs: cooccurrence analysis details
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


#' visualize network for coccurrence analysis
#' depends on:
#'   values$cooc_nodes: nodes for cooccurrence
#'   input$Det_CO_Threshold: threshold of cooccurrence
#'   values$cooc_edges: edges of cooccurrence
#'   input$Det_CO_smooth: smooth values for visualization
#'   input$Det_CO_node_scaling: selected scaling for nodes
#'   values$name_for_cooc_knk: names for cooccurrences knk 
#'   input$Det_CO_use_igraph_layout: use an igraph layout
#'   input$Det_CO_gravity: selected cooccurrence gravity
#'   input$Det_CO_Layout: selected cooccurrence layout
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
  if(input$Det_CO_use_igraph_layout==TRUE){
    return(
      visNetwork::visNetwork(nodes = nodes,edges = edges)%>%
        visNetwork::visIgraphLayout(randomSeed = 1)%>%
        visNetwork::visNodes(font = list(color="black",size=20,background="white"))%>%
        visNetwork::visEdges(scaling=list(min=2,max=8))%>%
        visNetwork::visPhysics(barnesHut = list(gravitationalConstant=input$Det_CO_gravity))%>%
        visNetwork::visLayout(randomSeed = 1,hierarchical = input$Det_CO_Layout)%>%
        visNetwork::visEvents(click = "function(nodes){
                          Shiny.onInputChange('cooc_word', nodes.nodes[0]);
                          ;}"
        )
    )
  }
  else{
    return(
      visNetwork::visNetwork(nodes = nodes,edges = edges)%>%
        visNetwork::visNodes(font = list(color="black",size=20,background="white"))%>%
        visNetwork::visEdges(scaling=list(min=2,max=8))%>%
        visNetwork::visPhysics(barnesHut = list(gravitationalConstant=input$Det_CO_gravity))%>%
        visNetwork::visLayout(randomSeed = 1,hierarchical = input$Det_CO_Layout)%>%
        visNetwork::visEvents(click = "function(nodes){
                          Shiny.onInputChange('cooc_word', nodes.nodes[0]);
                          ;}"
        )
    )
  }
})


#' download cooccurrence matrix
#' depends on:
#'   values$Det_CO_dl_coocs: cooccurrence datails
output$Det_CO_download_coocs<-downloadHandler(
  filename = function() {
    paste('co-occurence_matrix-', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-as.matrix(values$Det_CO_dl_coocs)
    write.csv(data, con)
  }
) 

#' create data table showing real texts containing specified words
#' allows for qualitative analysis of quantitative results
#' depends on:
#'   input$coocs_examples_words: selected example words
#'   input$coocs_examples_all: all selected examples
#'   values$cooc_examples_dtm: render example for document term matrix
#'   input$coocs_examples_k: examples parameter k
#'   input$coocs_examples_n: examples parameter n
#'   values$coocs_examples_document_ids: document ids
#'   values$coocs_examples_texts: text examples
#'   values$host: selected host
#'   values$port: selected port
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
  text_orig<-matrix(c(0),number_of_examples,1)
  # make sure words are splitted in case n grams were used
  words <- input$coocs_examples_words
  words <- unlist(flatten(stringr::str_split(string = words,pattern = "_",simplify = F)))
  
  for(i in 1:number_of_examples){
    tokens<-values$coocs_token[which(values$coocs_token[,1]==avail[i]),]
    tokens_orig<-tokens
    id_targets<-NULL
    id_targets_all<-list()
    for(j in 1:length(words)){
      id_targets<-c(id_targets,union(which(tolower(tokens[,"token"])%in%words[j]),which(tolower(tokens[,"lemma"])%in%words[j])))
      id_targets_all[[j]]<-union(which(tolower(tokens[,"token"])%in%words[j]),which(tolower(tokens[,"lemma"])%in%words[j]))
      if(length(id_targets)>2){
        id_targets<-c(min(id_targets),max(id_targets))
      }
    }
    for(h in 1:length(id_targets_all)){
      for(l in 1:length(id_targets_all[[h]])){
        tokens[id_targets_all[[h]][l],"token"]<-paste0(' <b style="color:',colors[h],'">',tokens[id_targets_all[[h]][l],"token"],'</b> ')
      }
    }
    example<-paste(tokens[max(1,(min(id_targets)-k)):min(dim(tokens)[1],(max(id_targets)+k)),"token"],collapse=" ")
    example_orig<-paste(tokens_orig[max(1,(min(id_targets)-k)):min(dim(tokens_orig)[1],(max(id_targets)+k)),"token"],collapse=" ")
    text[i,1]<-example
    text_orig[i,1]<-example_orig
  }
  document_ids<-avail[1:number_of_examples]
  values$coocs_examples_document_ids<-document_ids
  values$coocs_examples_texts<-text
  values$coocs_examples_texts_orig<-text_orig
  #for(i in 1:length(input$coocs_examples_words)){
  #  word<-input$coocs_examples_words[i]
  #  text[,1]<-stringr::str_replace_all(string = (text[,1]),pattern = stringr::regex(paste(" ",word," ",sep=""),ignore_case = T),replacement = paste0(' <b style="color:',colors[i],'">',word,'</b> '))
  #}
  
  #add document titles
  document_titles<-get_metadata_from_db(dataset = stringr::str_split(string = document_ids,pattern = "_",simplify = T)[,1],
                                        doc_ids = stringr::str_split(string = document_ids,pattern = "_",simplify = T)[,2],
                                        host = values$host, port = values$port)
  data<-data.frame(Text=text,
                   Title = document_titles$title,
                   SeeDocument = shinyInput(
                     shinyBS::bsButton,
                     length(document_ids),
                     'coocs_kwic_show_doc_button_',
                     label = "",
                     style="primary",
                     icon=icon("search"),
                     onclick = 'Shiny.onInputChange(\"coocs_kwic_document\",  this.id)'
                   ),stringsAsFactors = FALSE)
  colnames(data)<-c("Text","Document Title", "See Document?")
  
  return(datatable(data = data,escape=F,selection="none"))
})


#' kwic show documents 
#' depends on:
#'   input$coocs_kwic_document: kwic documents
#'   values$coocs_examples_document_ids: examples for document ids
#'   values$host: selected host
#'   values$port: selected port
#'   input$coocs_examples_words: example words for calculation
observeEvent(input$coocs_kwic_document,{
  selected_row<-as.numeric(stringr::str_split(string = input$coocs_kwic_document,pattern = "_",simplify = T)[1,6])
  validate(
    need(selected_row>0,message=F)
  )
  dataset<- stringr::str_split(string=values$coocs_examples_document_ids[selected_row],pattern = "_",simplify = T)[1,1]
  doc_id<- stringr::str_split(string=values$coocs_examples_document_ids[selected_row],pattern = "_",simplify = T)[1,2]
  token<-get_token_from_db(dataset = dataset,doc_ids = doc_id,host=values$host,port=values$port)
  id_targets<-union(which(tolower(token[,"word"])%in%input$coocs_examples_words),which(tolower(token[,"lemma"])%in%input$coocs_examples_words))
  
  token[id_targets,"word"]<-paste0(' <b style="color:',"black",'">',token[id_targets,"word"],'</b> ')
  ## edit: if skipgram selected show skigram windows back and front
  text<-paste(token[,"word"],collapse=" ")
  showModal(
    modalDialog(title = paste0("Document: ",dataset,"_",doc_id),easyClose = T,
                tags$div(HTML(text))
    ))
  isolate(shinyjs::runjs('Shiny.onInputChange(\"coocs_kwic_document\",  "coocs_kwic_show_doc_button_0")'))
})







#' link downloadbutton for the example texts in  co-occurrences restuls tab
#' depends on:
#'   values$coocs_examples_texts: example texts for cooccurrence analysis
output$Det_CO_download_examples<-downloadHandler(
  filename = function() {
    paste('examples-', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-as.matrix(values$coocs_examples_texts)
    write.csv(data, con)
  }
)  

#' link downloadbutton for the top co-occurrecnes in coocs results tab
#' depends on:
#'   values$coocs_top_dl_dice: top dl for cooccurrence analysis with dice measurement
output$coocs_download_dice<-downloadHandler(
  filename = function() {
    paste('results-dice_', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-as.matrix(values$coocs_top_dl_dice)
    write.csv(data, con)
  }
)  

#' link downloadbutton for the top co-occurrecnes in coocs results tab
#' depends on: 
#'   values$coocs_top_dl_mi: top dl for cooccurrence analysis with mutual information measurement
output$coocs_download_mi<-downloadHandler(
  filename = function() {
    paste('results-mi_', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-as.matrix(values$coocs_top_dl_mi)
    write.csv(data, con)
  }
)  
#' link downloadbutton for the top co-occurrecnes in coocs results tab
#' depends on: 
#'   values$coocs_top_dl_log: top dl for cooccurrence analysis with log likelihood measurement
output$coocs_download_log<-downloadHandler(
  filename = function() {
    paste('results-log_', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-as.matrix(values$coocs_top_dl_log)
    write.csv(data, con)
  }
)  

#' example stats for cooccurrence analysis
#' depends on:
#'   input$coocs_examples_words: example words
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


#' buttom for top cooccurrence presentation
#' depends on:
#'   input$tabBox_coocs: values from tab box (clicked or not)
#'   values$coocs_load_top: load top values
#'   values$Details_Data_CO: show detailes data for cooccurrence analysis
#'   values$coocs_dice: select dice measurement
#'   values$coocs_mi: select mutual information measurement
#'   values$coocs_log: select log likelihood measurement
#'   values$coocs_count: select counts as measurement
#'   values$cooc_examples_dtm: create document term matrix for examples
#'   values$coocs_load_examples: load examples
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

#' show top result for cooccurrence with dice measurement
#' depends on:
#'   values$coocs_load_top: load top values
#'   values$coocs_dice: selected dice measurement
#'   input$coocs_top_word1: top results for word 1
#'   input$coocs_top_word2: top results for word 2
#'   input$coocs_top_max: maximal number of top values
#'   values$coocs_top_dl_dice: detailes result data
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
  datatable(data = data,colnames = c('Term 1', 'Term 2','Value'))
})

#' show top result for cooccurrence with mutual information measurement
#' depends on:
#'   values$coocs_load_top: load top values
#'   values$coocs_mi: selected mutual information measurement
#'   input$coocs_top_word1: top results for word 1
#'   input$coocs_top_word2: top results for word 2
#'   input$coocs_top_max: maximal number of top values
#'   values$coocs_top_dl_mi: detailes result data
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
  datatable(data = data,colnames = c('Term 1', 'Term 2','Value'))
})

#' show top result for cooccurrence with log likelihood measurement
#' depends on:
#'   values$coocs_load_top: load top values
#'   values$coocs_log: selected log likelihood measurement
#'   input$coocs_top_word1: top results for word 1
#'   input$coocs_top_word2: top results for word 2
#'   input$coocs_top_max: maximal number of top values
#'   values$coocs_top_dl_log: detailes result data
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
  datatable(data = data,colnames = c('Term 1', 'Term 2','Value'))
})
