#' render detailed document term matrix for LDAvis
#' depends on:
#'    input$coll: selected collection
#'    input$Det_DTM_LDAvis_n: selected n for LDAvis claculation for detailed document term matrix 
#'    values$dtm_results: document term matrix results
output$Det_DTM_LDAvis<-LDAvis::renderVis({
  validate(
    need(!is.null(input$coll),message=F),
    need(!is.null(input$Det_DTM_LDAvis_n),message = F),
    need(!is.null(values$dtm_results),message = F)
  )
  if(input$coll=="Results"){
    return(NULL)
  }
  else{
    current_result<-values$dtm_results[[as.numeric(input$Det_DTM_LDAvis_n)]]
    phi<-current_result[[2]]
    theta<-current_result[[1]]
    doc.length<-current_result[[3]]
    vocab<-current_result[[5]]
    term.frequency<-current_result[[4]]
    tm<-LDAvis::createJSON(phi=phi,theta = theta,doc.length = doc.length,vocab = vocab,term.frequency = term.frequency, 
                           reorder.topics = F)#mds.method = svd_tsne )
    return(tm)
  }
})


#' render detailed document term matrix with dynamic table
#' depends on:
#'   input$Det_DTM_topic_dynamic_topic: dynamic topic for detailed document term matrix (topic calculation)
#'   values$dtm_results:document term matrix results
#'   input$Det_DTM_topic_dynamic_lambda: dynamic lambda for detailed document term matrix
#'   input$Det_DTM_topic_dynamic_number_of_words: dynamic number of words for detailed document term matrix (topic calculation)
#'   values$dtm_results_additional: additional document term matrix results
output$Det_DTM_dynamic_table<-DT::renderDataTable({
  validate(
    need(!is.null(input$Det_DTM_topic_dynamic_topic),message=F)
  )
  top_relevant_words_per_time_stamp<-lapply(X = values$dtm_results,FUN = function(x){
    relevance <- calculate_topic_relevance(lambda = input$Det_DTM_topic_dynamic_lambda,phi = x[[2]],theta = x[[1]],doc.length = x[[3]])[,input$Det_DTM_topic_dynamic_topic]
    names(relevance)<-values$dtm_results[[1]][[5]]
    names(sort(relevance,decreasing = T))[1:input$Det_DTM_topic_dynamic_number_of_words]
  })
  data<-do.call(cbind,top_relevant_words_per_time_stamp)
  colnames(data)<-values$dtm_results_additional$time_slice_names
  rownames(data)<-1:nrow(data)
  datatable(data,class = "row-border cell-border stripe",selection="none")
})


#' render detailed document term matrix with importance plot
#' depends on:
#'   values$dtm_results: document term matrix results
#'   values$dtm_results_additional: additional document term matrix results
#'   values$Det_DTM_topic_importances: topic importatnce from detailed document term matrix
output$Det_DTM_importance_plot<-renderPlotly({
  topic_importances<-lapply(1:length(values$dtm_results),FUN = function(x){
    theta<-values$dtm_results[[x]][[1]][which(values$dtm_results_additional$doc_belongings_to_time_slices==x),,drop=F]
    colSums(theta)/sum(Matrix::colSums(theta))
  })
  topic_importances<-do.call(rbind,topic_importances)
  values$Det_DTM_topic_importances<-topic_importances
  
  getPalette = colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))
  colors<-getPalette(ncol(topic_importances))
  
  p<-plot_ly(x=values$dtm_results_additional$time_slice_names,y=topic_importances[,1],type="bar",name=paste0("Topic: ",1 ),marker=list(color = colors[1]))
  for(i in 2:ncol(topic_importances)){
    p<-add_trace(p=p, y=topic_importances[,i],name=paste0("Topic: ",i),marker=list(color=colors[i]))  
  }
  p <- layout(p,barmode="stack",legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1))
  p
})

#' render important scatter plot with document term matrix
#' depends on:
#'   values$Det_DTM_topic_importances: important topics from detailed document term matrix
#'   values$dtm_results_additional: additional results from document term matrix
output$Det_DTM_importance_scatter_plot<-renderPlotly({
  validate(
    need(!is.null(values$Det_DTM_topic_importances),message=F)
  )
  values$Det_DTM_topic_importances->topic_importances
  getPalette = colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))
  colors<-getPalette(ncol(topic_importances))
  
  p<-plot_ly(x=values$dtm_results_additional$time_slice_names,y=topic_importances[,1],type="scatter",mode="markers+lines",name=paste0("Topic: ",1 ))
  for(i in 2:ncol(topic_importances)){
    p<-add_trace(p=p, y=topic_importances[,i],name=paste0("Topic: ",i))  
  }
  p <- layout(p,legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1))
  p
})



#' render dynamic wordcloud from detailed documnt term matrix
#' depends on:
#'   values$dtm_results_additional: additional results from document term matrix
#'   values$dtm_results: document term matrix results
output$Det_DTM_dynamic_wordcloud_UI<-renderUI({
  tagList(fluidRow(
    column(4,offset = 2,
           selectInput(inputId = "Det_DTM_dynamic_wordcloud_time_1",label = "Select Time Stamp #1",choices=setNames(nm = values$dtm_results_additional$time_slice_names,
                                                                                                            object = 1:length(values$dtm_results)))
            ),
    column(4,
           selectInput(inputId = "Det_DTM_dynamic_wordcloud_time_2",label = "Select Time Stamp #2",choices=setNames(nm = values$dtm_results_additional$time_slice_names,
                                                                                                            object = 1:length(values$dtm_results)))
           )
  ),
  tags$hr(),
  fluidRow(style="margin-left:0px;margin-right:0px;padding-right:0px;",
    column(3,
           DT::dataTableOutput(outputId = "Det_DTM_dynamic_wordcloud_table")
           ),
    column(4,offset=1,
           tags$h4("Less important"),
           wordcloud2Output(outputId = "Det_DTM_dynamic_wordcloud_plot_new")
           ),
    column(4,
           tags$h4("More important"),
           wordcloud2Output(outputId = "Det_DTM_dynamic_wordcloud_plot_gone")
    )
  )
  )
})


#' observe dynamic topic modeling visualization
#' depends on:
#'   input$Det_DTM_dynamic_wordcloud_time_1: first time sequence - dynamic wordcloud representation from detailed topic modelling
#'   input$Det_DTM_dynamic_wordcloud_time_2: second time sequence - dynamic wordcloud representation from detailed topic modelling
#'   values$dtm_results: document term matrix results
#'   input$Det_DTM_topic_dynamic_topic: dynamic topics from detailed document term matrix
#'   values$Det_DTM_dynamic_wordcloud_data: dynamic wordcloud data from detailed document term matrix
observe({
  validate(
    need(!is.null(input$Det_DTM_dynamic_wordcloud_time_1),message=F),
    need(!is.null(input$Det_DTM_dynamic_wordcloud_time_2),message=F),
    need(input$Det_DTM_dynamic_wordcloud_time_1!=input$Det_DTM_dynamic_wordcloud_time_2,message=F)
  )
  
  phi1<-values$dtm_results[[as.numeric(input$Det_DTM_dynamic_wordcloud_time_1)]][[2]][input$Det_DTM_topic_dynamic_topic,] 
  phi2<-values$dtm_results[[as.numeric(input$Det_DTM_dynamic_wordcloud_time_2)]][[2]][input$Det_DTM_topic_dynamic_topic,] 
  names(phi1)<-values$dtm_results[[1]][[5]]
  names(phi2)<-values$dtm_results[[1]][[5]]
  data<-cbind(phi1,phi2,phi2-phi1)
  rownames(data)<-names(phi1)  

  values$Det_DTM_dynamic_wordcloud_data<-data
})


#' render dynamic wordcloud table from detailed document term matrix
#' depends on:
#'   values$Det_DTM_dynamic_wordcloud_data: dynamic wordcloud data from detailed document term matrix
output$Det_DTM_dynamic_wordcloud_table<-DT::renderDataTable({
  validate(
    need(!is.null(values$Det_DTM_dynamic_wordcloud_data),message="Time Stamp 1 and 2 need to be different")
  )
  data<-data.frame(words=rownames(values$Det_DTM_dynamic_wordcloud_data),counts=values$Det_DTM_dynamic_wordcloud_data[,3])
  colnames(data)<-c("word","change")
  data[,2]<-round(data[,2],digits = 5)
  data<-data[order(data[,2],decreasing=T),]
  datatable(data=data,rownames = F)  
})

#' plot dynamic wordcloud from detailed document term matrix
#' depends on:
#'   values$Det_DTM_dynamic_wordcloud_data: dynamic wordcloud data
output$Det_DTM_dynamic_wordcloud_plot_gone<-renderWordcloud2({
  validate(
    need(!is.null(values$Det_DTM_dynamic_wordcloud_data),message=F)
  )
  colfuncrb <- colorRampPalette(c("gold", "springgreen"))
  #colfuncbb <- colorRampPalette(c("deeppink", "gold"))
  
  d<-data.frame(words=rownames(values$Det_DTM_dynamic_wordcloud_data),counts=values$Det_DTM_dynamic_wordcloud_data[,3])
  d[,2]<-round(d[,2],digits = 6)

  d[,2]<-as.numeric(as.character(d[,2]))
  d[,1]<-(as.character(d[,1]))
  
  d<-d[order(d[,2]),]
  #ind1<-which(d[,2]<0)
  ind2<-which(d[,2]>0)
  d<-d[ind2,]
  colors<-c((colfuncrb(length(ind2))))
  d<-cbind(d,colors)
  
  d[,2]<-abs(d[,2])
  #colors<-colors[order(d[,2],decreasing = T)]
  d<-d[order(d[,2],decreasing = T),]
  d[,2]<-d[,2]/max(d[,2])  
  d[,2]<-d[,2]*20
  wordcloud2(d[,1:2],size=0.5,color = d$colors,backgroundColor = "black",fontFamily = "Helvetica",minRotation = -pi/2,maxRotation = -pi/2)
})

#' plot new dynamic wordclud from detailed document term matrix
#' depends on:
#'   values$Det_DTM_dynamic_wordcloud_data: dynamic wordcloud data
output$Det_DTM_dynamic_wordcloud_plot_new<-renderWordcloud2({
  validate(
    need(!is.null(values$Det_DTM_dynamic_wordcloud_data),message=F)
  )
  #colfuncrb <- colorRampPalette(c("gold", "springgreen"))
  colfuncbb <- colorRampPalette(c("deeppink", "gold"))
  
  d<-data.frame(words=rownames(values$Det_DTM_dynamic_wordcloud_data),counts=values$Det_DTM_dynamic_wordcloud_data[,3])
  d[,2]<-round(d[,2],digits = 6)

  d[,2]<-as.numeric(as.character(d[,2]))
  d[,1]<-(as.character(d[,1]))
  
  d<-d[order(d[,2]),]
  ind1<-which(d[,2]<0)
  #ind2<-which(d[,2]>0)
  d<-d[ind1,]
  colors<-c((colfuncbb(length(ind1))))
  d<-cbind(d,colors)
  
  d[,2]<-abs(d[,2])
  #colors<-colors[order(d[,2],decreasing = T)]
  d<-d[order(d[,2],decreasing = T),]
  d[,2]<-d[,2]/max(d[,2])  
  d[,2]<-d[,2]*20
  wordcloud2(d[,1:2],size=0.5,color = d$colors,backgroundColor = "black",fontFamily = "Helvetica",minRotation = -pi/2,maxRotation = -pi/2)
})

#' render importance word plot from detailed document term matrix 
#' depends on:
#'   input$Det_DTM_word_importance_Words: importance words 
#'   values$dtm_results: document term matrix results
#'   values$dtm_results_additional: additional results from document term matrix
output$Det_DTM_word_importance_plot<-renderPlotly({
  validate(
    need(length(input$Det_DTM_word_importance_Words)>0,message = "Specify at least one word and topic!")
  )
  results<-values$dtm_results
  vocab_idx<-which(results[[1]][[5]]%in%input$Det_DTM_word_importance_Words)
  importance_values<-lapply(results,FUN = function(x){
    x[[2]][input$Det_DTM_word_importance_topic,vocab_idx]

  })
  importance_values<-do.call(cbind,importance_values)
  rownames(importance_values)<-input$Det_DTM_word_importance_Words
  p<-plot_ly(x=values$dtm_results_additional$time_slice_names,y=importance_values[1,],type="scatter",mode="lines+markers",name=rownames(importance_values)[1])
  if(nrow(importance_values)>1){
    for(i in 2:nrow(importance_values)){
      p<-add_trace(p,y=importance_values[i,],name=rownames(importance_values)[i])
    }
  }
  p
})




#' plot for topic validation tab showing the document with the words highlighted, which are relevant in the chosen topic
#' depends on:
#'   input$Det_DTM_validation_document: document from topic model with validation
#'   values$host: used host
#'   values$port: used port
#'   values$Details_Data_DTM: details of topic model data
#'   values$DTM_phi: topic model phi
#'   input$Det_DTM_validation_relevance_measure: relebance measurement for detailed topic model validation
#'   values$DTM_rel_counts: topic model relative counts
#'   values$Det_DTM_validation_topic: validation of topics from detailed topic model
#'   input$Det_DTM_validation_lambda: lambda validation from detailed topic model 
#'   values$DTM_theta: topic model theta
#'   values$DTM_doc.length: topic model document length
#'   input$Det_DTM_validation_minmax_gobal: global minimum and maximum for detailed topic model validation
#'   input$Det_DTM_validation_color_least_important: detailed topic model validation colour for least important values
#'   input$Det_DTM_validation_color_most_important: detailed topic model validation colour for most important values
#'   input$Det_DTM_validation_color_use_pie_colors: validation color to use in pie chart for detailed topic models 
output$DTM_validation_UI<-renderUI({
  return(
    tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$br(),
                      plotly::plotlyOutput("Det_DTM_validation_document_topic_pie"),
                      tags$h4("Most relevant words for chosen topic"),
                      wordcloud2Output(outputId = "Det_DTM_validation_wordcloud")
               ),
               column(8,
                      tags$br(),
                      uiOutput("Det_DTM_Validation_Document")
               )
      )
      
    ) 
  )
})

#' output to validate the topic modeling by checking for topics in selected document
#' depends on:
#'   input$Det_DTM_validation_document: selected document for validation
#'   values$host: selected host for database connection
#'   values$port: selected port for database connection
#'   values$Details_Data_DTM: details on data of dynamic topic modeling
#'   values$dtm_results_additional: additional results from dynamic topic modelling
#'   values$dtm_results: all results from dynamic topic modeling
#'   input$Det_DTM_validation_time: selected time stamp for validation
#'   input$Det_DTM_validation_relevance_measure: selected relevance measurement for validation
#'   values$DTM_rel_counts: relative counts for current document 
#'   input$Det_DTM_validation_topic: selected topic for validation 
#'   input$Det_DTM_validation_lambda: lambda parameter for validatio
#'   input$Det_DTM_validation_minmax_gobal: selected global minimum and maximum for validation
#'   input$Det_DTM_validation_color_use_pie_colors: selected colors for the piechart of topics
#'   input$Det_DTM_validation_color_least_important: color for least important words in document
#'   input$Det_DTM_validation_color_most_important: color for the most importan word in document
output$Det_DTM_Validation_Document<-renderUI({
  validate(
    need(
      !is.null(input$Det_DTM_validation_document),message=FALSE
    ),
    need(
      input$Det_DTM_validation_document!="",message="please choose a document"
    )
  )
  identifier<-stringr::str_split(string = input$Det_DTM_validation_document,pattern = "_",simplify = T)
  dataset<-identifier[1]
  doc_id<-identifier[2]
  token<-get_token_from_db(dataset = dataset,doc_ids = doc_id,sentence_ids = NULL,host=values$host,port=values$port)
  # remove idx column from token
  token<-token[,-ncol(token)]
  
  load(paste0(values$Details_Data_DTM,"/parameters.RData"))
  space_ids<-which(token[,"pos"]=="SPACE")
  if(length(space_ids)>0){
    token<-token[-space_ids,]
  }
  
  if(parameters$baseform_reduction=="none"){
    features<-tolower(token[,"word"])  
  }
  if(parameters$baseform_reduction=="lemma"){
    features<-tolower(token[,"lemma"])  
  }
  if(parameters$baseform_reduction=="stemming"){
    features<-tolower(quanteda::tokens_wordstem(quanteda::tokens(paste(token[,"word"],collapse=" ")),lang)$text1)
  }
  token<-cbind(token,features)
  token<-cbind(1:dim(token)[1],token)
  relevant_documents<-which(values$dtm_results_additional$doc_belongings_to_time_slices==as.numeric(input$Det_DTM_validation_time))
  theta<-values$dtm_results[[as.numeric(input$Det_DTM_validation_time)]][[1]]
  theta<-theta[relevant_documents,,drop=F]
  phi<-values$dtm_results[[as.numeric(input$Det_DTM_validation_time)]][[2]]
  doc.length<-values$dtm_results[[as.numeric(input$Det_DTM_validation_time)]][[3]]
  doc.length<-doc.length[relevant_documents,drop=F]
  vocab<-values$dtm_results[[as.numeric(input$Det_DTM_validation_time)]][[5]]
  values$DTM_rel_counts <- round((colSums(theta * doc.length))*phi,digits = 2)
  
  if(input$Det_DTM_validation_relevance_measure=="estimated relative word frequency per topic"){
    data<-values$DTM_rel_counts
    data<-do.call(cbind,lapply(X = 1:dim(data)[2],FUN = function(x){
      data[,x]/sum(data[,x])
    })
    )
    data<-round(data,digits = 2)
    colnames(data)<-vocab
    data<-data[input$Det_DTM_validation_topic,intersect(unique(features),colnames(data))]
    data<-data.frame(features=names(data),weight=data)
    min=0
    max=1
    
  }
  if(input$Det_DTM_validation_relevance_measure=="relevance score"){
    relevance<-calculate_topic_relevance(lambda=input$Det_DTM_validation_lambda,phi=phi,theta=theta,doc.length=doc.length)
    rownames(relevance)<-vocab
    #normalize relevance
    #relevance<-relevance-apply(relevance,1, FUN=min)
    #relevance<-t(t(as.matrix(relevance))/rowSums(relevance))
    #relevance<-relevance/max(relevance)
    data<-relevance[,input$Det_DTM_validation_topic]
    data<-data.frame(features=names(data),weight=data)
    if(input$Det_DTM_validation_minmax_gobal=="over all topics"){
      min=min(relevance)
      max=max(relevance)
    }
    if(input$Det_DTM_validation_minmax_gobal=="inside chosen topic"){
      min=min(relevance[,input$Det_DTM_validation_topic])
      max=max(relevance[,input$Det_DTM_validation_topic])
    }
    if(input$Det_DTM_validation_minmax_gobal=="inside chosen document"){
      max<-max(data[intersect(unique(features),rownames(data)),"weight"])
      min<-min(data[intersect(unique(features),rownames(data)),"weight"])
    }
  }
  if(input$Det_DTM_validation_relevance_measure=="word probability"){
    data<-phi[input$Det_DTM_validation_topic,]
    names(data)<-vocab
    data<-data.frame(features=names(data),weight=data)
    if(input$Det_DTM_validation_minmax_gobal=="over all topics"){
      max<-max(phi)
      min<-min(phi)
    }
    if(input$Det_DTM_validation_minmax_gobal=="inside chosen topic"){
      max<-max(data$weight)
      min<-min(data$weight)
    }
    if(input$Det_DTM_validation_minmax_gobal=="inside chosen document"){
      max<-max(data[intersect(unique(features),rownames(data)),"weight"])
      min<-min(data[intersect(unique(features),rownames(data)),"weight"])
    }
  }
  
  
  m<-merge(x = token,y=data,by="features",all.x=TRUE)
  m<-m[order(m[,2]),]
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))
  colors<-getPalette(dim(phi)[1])
  #colors<-colors[order(values$DTM_theta[input$Det_DTM_validation_document,],decreasing = F)]
  color<-colors[input$Det_DTM_validation_topic]
  if(input$Det_DTM_validation_color_use_pie_colors==TRUE){
    rbPal_pos <- colorRampPalette(c('floralwhite',color))
  }
  else{
    rbPal_pos <- colorRampPalette(c(input$Det_DTM_validation_color_least_important,input$Det_DTM_validation_color_most_important))
  }
  m<-cbind(m,rep("",dim(m)[1]))
  if(length(intersect(which(!is.na(m$weight)),which(m$weight!=0)))>0){
    m[intersect(which(!is.na(m$weight)),which(m$weight!=0)),12]<-  rbPal_pos(100)[as.numeric(cut(c(max,min,m$weight[intersect(which(!is.na(m$weight)),which(m$weight!=0))]),breaks = 100))[-c(1,2)]] #Alternative#seq(0,to = max(data$weight),length.out = 100) #original m$weight[intersect(which(!is.na(m$weight)),which(m$weight>0))]
  }
  
  strings<-apply(m,MARGIN = 1,FUN = function(x){
    if(is.na(x[11])){
      return(x[7])
    }
    else{
      return( paste0('<font style="background-color:',x[12],';"','title="feature: ',x[1],' with weight: ',round(as.numeric(x[11]),digits = 5),'">',x[7],'</font>'))
    }
  })
  
  document<-list()
  for(i in 1:dim(m)[1]){
    document[[i]]<-paste0("<span span_nr='",i,"'>",strings[i],"</span>")
  }
  document<-do.call(rbind,document)
  document<-HTML(document)
  tags$p(document)
  
  
})




#' wordcloud showing the relevant words for the chosen topic
#' depends on:
#'  input$Det_DTM_validation_topic: choosen topic from detailed topic models for validation
output$Det_DTM_validation_wordcloud <- wordcloud2::renderWordcloud2({
  validate(
    need(
      !is.null(input$Det_DTM_validation_topic),message=FALSE
    )
  )
  # @values$DTM_relevance calculated with lamda= 0.3
  relevant_documents<-which(values$dtm_results_additional$doc_belongings_to_time_slices==as.numeric(input$Det_DTM_validation_time))
  theta<-values$dtm_results[[as.numeric(input$Det_DTM_validation_time)]][[1]]
  theta<-theta[relevant_documents,,drop=F]
  phi<-values$dtm_results[[as.numeric(input$Det_DTM_validation_time)]][[2]]
  doc.length<-values$dtm_results[[as.numeric(input$Det_DTM_validation_time)]][[3]]
  doc.length<-doc.length[relevant_documents,drop=F]
  vocab<-values$dtm_results[[as.numeric(input$Det_DTM_validation_time)]][[5]]
  
  relevance<-calculate_topic_relevance(lambda=input$Det_DTM_validation_lambda,phi=phi,theta=theta,doc.length=doc.length)
  rownames(relevance)<-vocab
  data <- relevance[,input$Det_DTM_validation_topic]
  data <- data[order(data,decreasing=T)][1:50]
  data <- data.frame(cbind(names(data),data),stringsAsFactors = FALSE)
  class(data$data) <- "numeric"
  # normalize weights for wordcloud
  data$data <- data$data-min(data$data)
  data$data <- data$data/max(data$data)
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1,minRotation = -pi/2,maxRotation = -pi/2)
})



#' pie chart showing the topic distribution of a selected topic 
#' depends on:
#'   input$Det_DTM_validation_document: chosen document using doc_id as identifier
#'   values$DTM_theta: topic model theta
output$Det_DTM_validation_document_topic_pie<-plotly::renderPlotly({
  validate(
    need(
      !is.null(input$Det_DTM_validation_document),message=FALSE
    ),
    need(
      input$Det_DTM_validation_document!="",message=F
    )
  )
  relevant_documents<-which(values$dtm_results_additional$doc_belongings_to_time_slices==as.numeric(input$Det_DTM_validation_time))
  theta<-values$dtm_results[[as.numeric(input$Det_DTM_validation_time)]][[1]]
  #theta<-theta[relevant_documents,]
  phi<-values$dtm_results[[as.numeric(input$Det_DTM_validation_time)]][[2]]
  doc.length<-values$dtm_results[[as.numeric(input$Det_DTM_validation_time)]][[3]]
  doc.length<-doc.length[relevant_documents]
  vocab<-values$dtm_results[[as.numeric(input$Det_DTM_validation_time)]][[5]]
  
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))
  colors<-getPalette(dim(phi)[1])
  #browser()
  data<-theta[which(values$dtm_meta$id_doc==input$Det_DTM_validation_document),]
  names(data)<-1:length(data)
  data<-data.frame(class=paste("Topic: ",names(data)),likelihood=data)

  p <- plot_ly(data, labels = ~factor(class), values = ~likelihood, textposition = 'inside',source="DTM_validation_pie",marker = list(colors = colors),
               textinfo = 'label+percent') %>%
    plotly::add_pie(hole = 0.6) %>%
    plotly::layout(title = paste('Distribution of topics for chosen document'),legend=T,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   legend = list(x = 0.1, y = 0.9)
    )
  return(p)
})

click_pie_DTM_validation<-reactive({
  currentEventData<-event_data(event = "plotly_click", source = "DTM_validation_pie",session = session)
})

#' change selected topic if user click on a topic in the pie chart
observe({
  validate(
    need(!is.null(click_pie_DTM_validation()),message = F)
  )
  updateSliderInput(session = session,inputId = "Det_DTM_validation_topic",value = as.numeric(click_pie_DTM_validation()$pointNumber+1) )
})