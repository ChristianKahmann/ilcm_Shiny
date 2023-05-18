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
  load(paste0(values$Details_Data_CO,"/parameters.RData"))
  validate(
    need(parameters$baseform_reduction!="stemming","KWIC does not work rightnow with stemmed texts. You can either use Lemmatization or no baseform reduction to use KWIC.")
  )
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
  ######################
  text<-values$co_meta$body[which(values$co_meta$id_doc==paste0(dataset,"_",doc_id))]
  # find matching spacy model
  avail_models <- stringr::str_remove_all(string = stringr::str_split(
    stringr::str_replace_all(string = system(command = "python -m spacy info",intern = T)[8],pattern = "Pipelines[ ]+",replacement = "")
    ,pattern = ", ",simplify = T),pattern = " ")
  
  detected_lang <- cld2::detect_language(text = text)
  avail_models <- avail_models[grepl(pattern = detected_lang,x = avail_models)]
  avail_models <- avail_models[grepl(pattern = "sm|md",x = avail_models)]
  if(length(avail_models==0)){
    model = "en_core_web_sm"
  }
  else{
    model = avail_models[1]
    model = stringr::str_remove(string = model,pattern = "\\(.+\\)")
  }
  spacyr::spacy_initialize(model = model)
  #when using splits 
  if(nchar(text)>1000000){
    token <- NULL
    for(i in 1:ceiling(nchar(text)/1000000)){
      text_split <- substr(text,(((i-1)*1000000)+1),(i*1000000))
      token_split <-spacyr::spacy_parse(iconv(paste(text_split,collapse="\n"), "UTF-8", "UTF-8",sub=''),pos = T,tag = F,lemma = T,entity = F,dependency = F)
      token <- rbind(token,token_split)
    }
  }
  else{
    token<-spacyr::spacy_parse(iconv(paste(text,collapse="\n"), "UTF-8", "UTF-8",sub=''),pos = T,tag = F,lemma = T,entity = F,dependency = F)
  }
  token<-cbind(rep("unknown",nrow(token)),token)
  colnames(token)[5]<-"word"
  colnames(token)[1]<-"dataset"
  colnames(token)[2]<-"id"
  colnames(token)[3]<-"sid"
  colnames(token)[4]<-"tid"
  # remove idx column from token
  #token<-token[,-ncol(token)]
  #####################
  
  #token<-get_token_from_db(dataset = dataset,doc_ids = doc_id,host=values$host,port=values$port)
  id_targets<-union(which(tolower(token[,"word"])%in%input$coocs_examples_words),which(tolower(token[,"lemma"])%in%input$coocs_examples_words))
  
  token[id_targets,"word"]<-paste0(' <b style="color:',"black",'">',token[id_targets,"word"],'</b> ')
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




######################################################################################################
#                                      shortest paths                                                #
######################################################################################################

output$Det_CO_shortest_paths_UI<-renderUI({
  validate(
    need(!is.null(input$Det_CO_shortest_path_treshold),message=FALSE)
  )
  shiny::withProgress(message = "Calculating shortest paths...",value = 0,min = 0,max = 1,expr = {
    shiny::incProgress(amount = 0,message = "Loading cooccurrence inforamtion...")
    load(paste0(values$Details_Data_CO,"/dice.RData"))
    shiny::incProgress(amount = 0.1,message = "Searching shortest paths...")
    
    data<-coocs_matrix_dice
    values$Det_CO_shortest_paths_cooc_matrix<-data
    data<-Matrix::triu(data)
    library(igraph)
    my_list=list()
    
    
    t<-summary(data)
    t<-t[which(t$x>=input$Det_CO_shortest_path_treshold),]
    e<-aggregate(t$j,by=list(t$i),FUN=function(x){
      c(x)
    })
    
    t5<-Sys.time()
    my_list4<- parallel::mclapply(1:nrow(data),mc.preschedule = T,mc.cleanup = T,mc.cores = (parallel::detectCores()-1),FUN = function(j){
      if(j%in%e$Group.1){
        return(e$x[[which(e$Group.1==j)]])
      }
      else{
        return(integer(0))
      }
    })
    t6<-Sys.time()-t5
    
    g<-graph_from_adj_list(my_list4)
    g<-as.undirected(graph = g,mode = "collapse")
    E(g)
    V(g)$name <- as_ids(V(g))
    V(g)$label <- colnames(data)
    values$Det_CO_shortest_path_g<-g
  })
  
  return(visNetwork::visNetworkOutput(outputId = "Det_CO_shortest_paths_network",height = "75vh"))
})


output$Det_CO_shortest_paths_network<-visNetwork::renderVisNetwork({
  validate(
    need(!is.null(values$Det_CO_shortest_path_g),message=F),
    need(!is.null(input$Det_CO_shortest_paths_word1),message=FALSE),
    need(!is.null(input$Det_CO_shortest_paths_word2),message=FALSE),
    need(input$Det_CO_shortest_paths_word1!="",message="Please specify word 1"),
    need(input$Det_CO_shortest_paths_word2!="",message="Please specify word 2"),
    need(input$Det_CO_shortest_paths_word1!=input$Det_CO_shortest_paths_word2,message="Please specify two seperate words!")
  )
  
  values$Det_CO_shortest_path_g->g
  
  from<-which(V(g)$label==input$Det_CO_shortest_paths_word1)
  to<-which(V(g)$label==input$Det_CO_shortest_paths_word2)
  
  #paths<-all_simple_paths(graph = g,from = from,to = to,mode = "out")
  #shortest.paths(graph = g,v = from,to = to)
  #distance_table(graph = g,directed = F)->dt
  
  nodes_of_interest <- c(from,to)
  selnodes <- V(g)[name %in% nodes_of_interest]
  selegoV <- ego(g, order=input$Det_CO_shortest_path_max_length_of_paths_shown, nodes = selnodes, mode = "all", mindist = 0)
  selegoG <- induced_subgraph(g,unlist(selegoV))
  from_new<-which(names(V(selegoG))==as.character(from))
  to_new<-which(names(V(selegoG))==as.character(to))
  
  shortest_path<-get.all.shortest.paths(graph =selegoG,from = from_new,to = to_new )
  validate(
    need(length(shortest_path$res)>0,message="No path between the 2 selected words was found! Specify different words or change the other parameters.")
  )
  
  print("calculating all paths...")
  if(length(shortest_path$res)>10){
    all_paths<-shortest_path$res
  }
  else{
    all_paths<-all_simple_paths(graph = selegoG, from = from_new,to_new,cutoff = max(input$Det_CO_shortest_path_max_length_of_paths_shown,length(shortest_path$res[[1]])))
  }
  all_nodes_nec<-as.integer(unique(unlist(lapply(all_paths,FUN = function(x){
    names(x)
  }))))
  
  selnodes <- V(g)[name %in% all_nodes_nec]
  selegoV <- ego(g, order=1, nodes = selnodes, mode = "all", mindist = 0)
  selegoG <- induced_subgraph(g,unlist(selegoV))
  #from_new<-which(names(V(selegoG))==as.character(from))
  #to_new<-which(names(V(selegoG))==as.character(to))
  
  nodes<-visNetwork::toVisNetworkData(igraph = selegoG)$nodes
  nodes$label<- V(selegoG)$label
  # delete unnec. nodes
  nec_nodes<-unique(unlist(lapply(all_paths,function(x){
    names(x)
  })))
  
  nodes<-nodes[which(nodes$id%in%as.numeric(nec_nodes)),]
  edges<-visNetwork::toVisNetworkData(igraph = selegoG)$edges
  
  edges<-edges[intersect(which(edges$from%in%nodes$id),which(edges$to%in%nodes$id)),]
  # All Paths
  all_paths<-all_paths[ order(unlist(lapply(all_paths,FUN = function(x){
    length(names(x))
  })),decreasing = F)]
  
  nec_edges<-lapply(X = 1:length(all_paths),FUN = function(x){
    get_relevant_edge_ids_for_paths(edges=edges,path = all_paths[[x]])
  })
  
  for(i in 1:length(nec_edges)){
    l<-length(unique(unlist(nec_edges[1:i])))
    if(l>input$Det_CO_shortest_path_max_number_edges){
      nec_edges<-unique(unlist(nec_edges[1:(i-1)]))
      break;
    }
    if(i==length(nec_edges)){
      nec_edges<-unique(unlist(nec_edges))
    }
  }
  
  
  edges<-edges[nec_edges,]
  
  nec_nodes<-unique(c(edges[,1],edges[,2]))
  nodes<-nodes[as.character(nec_nodes),]
  edges$width<-rep(4,nrow(edges))
  edges$color<-"#fff291"
  edges$shadow=FALSE
  edges$smooth<-TRUE
  # add edgle labels using coocs significance
  #browser()
  edges$title<-round(values$Det_CO_shortest_paths_cooc_matrix[cbind(edges[,1],edges[,2])],digits=4)
  edges$value<-round(values$Det_CO_shortest_paths_cooc_matrix[cbind(edges[,1],edges[,2])],digits=4)
  # Start Nodes
  nodes$group<-rep("all",nrow(nodes))
  nodes$group[which(nodes$id%in%c(from,to))]<-"start"
  nodes$size<-rep(10,nrow(nodes))
  nodes$size[which(nodes$id%in%c(from,to))]<-50
  nodes$font<-rep("14px arial black",nrow(nodes))
  nodes$font[which(nodes$id%in%c(from,to))]<-"34px arial red"
  # Shortest Path
  nodes$size[which(nodes$id%in%setdiff(as.numeric(names(shortest_path$res[[1]])),c(from,to)))]<-25
  nodes$group[which(nodes$id%in%setdiff(as.numeric(names(shortest_path$res[[1]])),c(from,to)))]<-"shortest_path"
  
  
  
  all_paths<-all_paths[ order(unlist(lapply(all_paths,FUN = function(x){
    length(names(x))
  })),decreasing = T)]
  
  all_paths_lengths<-unlist(lapply(all_paths,function(x){
    return(length(x))
  }))
  
  min_length<-min(all_paths_lengths)
  rel_path_edge<-get_relevant_edge_ids_for_paths(path = all_paths[[which(all_paths_lengths==min_length)]],edges = edges)
  edges$color[rel_path_edge]<-"#F6412D"
  edges$shadow[rel_path_edge]=TRUE
  ledges <- data.frame(color =  "#F6412D",
                       label = c(min_length))
  if(length(unique(all_paths_lengths))>1){
    rel_paths<-which(all_paths_lengths==(1+min_length))
    rel_path_edge<-unique(unlist(lapply(rel_paths,FUN = function(x){
      get_relevant_edge_ids_for_paths(path = all_paths[[x]],edges = edges)
    })
    )
    )
    edges$color[rel_path_edge]<-"#FF9800"
    edges$shadow[rel_path_edge]=TRUE
    ledges <- rbind(ledges,c("#FF9800",(min_length+1)))
  }
  
  
  
  validate(
    need(nrow(edges)<400,"The current settings results in more than 400 edges. Please adjust the settings of the graph.")
  )
  
  
  
  graph<-visNetwork::visNetwork(nodes = nodes,edges = edges)%>%
    visIgraphLayout(layout = "layout_in_circle")%>%
    visNetwork::visNodes(font = list(color="black",size=20,background="white",face="Helvetica"))%>%
    visNetwork::visEdges(scaling=list(min=5,max=20))%>%
    visLegend(addEdges = ledges,  useGroups = FALSE)
  return(graph)
})
