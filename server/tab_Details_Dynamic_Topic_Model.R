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



output$Det_DTM_word_importance_plot<-renderPlotly({
  validate(
    need(length(input$Det_DTM_word_importance_Words)>0,message = "Specify at least on word and topic!")
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


