output$vs_similar_table<-DT::renderDataTable({
  validate(
    need(!is.null(input$Det_VS_words),message = FALSE),
    need(!is.null(input$Det_VS_n),message = FALSE),
    need(length(input$Det_VS_words)>0,"no words specified")
  )
  data<-values$vs_model %>% 
    closest_to(values$vs_model[[input$Det_VS_words]],input$Det_VS_n)
  colnames(data)<-c("word","distance")
  datatable(data = data)
})




output$vs_similar_plotly_tsne<-renderPlotly({
  validate(
    need(!is.null(input$Det_VS_words),message = FALSE),
    need(!is.null(input$Det_VS_n),message = FALSE),
    need(length(input$Det_VS_words)>0,"no words specified")
  )
  data<-values$vs_model %>% 
    closest_to(values$vs_model[[input$Det_VS_words]],input$Det_VS_n)
  data = values$vs_model[[data$word,average=F]]
  
  #browser()
  if(is.null(values$vs_tsne)){
    m<-tsne::tsne(as.matrix(as.matrix(data)),max_iter = 250)
    title="A two dimensional reduction of the vector space model using tsne"
  }
  else{
    m<-values$vs_tsne[rownames(data),]
    title="A two dimensional reduction of the vector space model using pretrained tsne"
  }
  chosen<-which(rownames(data)%in%input$Det_VS_words)
  t_chosen <- list(
    family = "sans serif",
    size = 14,
    color = toRGB("black"))
  t_others <- list(
    family = "sans serif",
    size = 12,
    color = toRGB("grey50"))
  
  p <- plot_ly(type = 'scatter', mode = 'text+markers', x = as.numeric(m[chosen,1]), y = as.numeric(m[chosen,2]),textfont = t_chosen, textposition = "top right",text=rownames(data)[chosen],name=" chosen words",
               marker = list(
                 color = 'rgb(239, 75, 67)',
                 size = 11,
                 line = list(
                   color = 'rgb(28, 2, 1)',
                   width = 1
                 )
               ))
  p<-add_trace(p, x = m[-chosen,1],y = m[-chosen,2],text=rownames(data)[-chosen],name="similar words",textfont = t_others,
               marker = list(
                 color = 'rgb(98, 59, 226)',
                 size = 8,
                 line = list(
                   color = 'rgb(98, 59, 226)',
                   width = 0.5
                 ))
  )
  p<-layout(p,xaxis=list(zeroline=F),yaxis=list(zeroline=F),title=title)
  
  return(p)
})

output$vs_similar_plotly_pca<-renderPlotly({
  validate(
    need(!is.null(input$Det_VS_words),message = FALSE),
    need(!is.null(input$Det_VS_n),message = FALSE),
    need(length(input$Det_VS_words)>0,"no words specified")
  )
  data<-values$vs_model %>% 
    closest_to(values$vs_model[[input$Det_VS_words]],input$Det_VS_n)
  data = values$vs_model[[data$word,average=F]]
  if(is.null(values$vs_pca)){
    m<-stats::predict(stats::prcomp(as.matrix(data)))[,1:2]
    title="A two dimensional reduction of the vector space model using pca"
  }
  else{
    m<-values$vs_pca[rownames(data),]
    title="A two dimensional reduction of the vector space model using pretrained pca"
  }
  chosen<-which(rownames(data)%in%input$Det_VS_words)
  t_chosen <- list(
    family = "sans serif",
    size = 14,
    color = toRGB("black"))
  t_others <- list(
    family = "sans serif",
    size = 12,
    color = toRGB("grey50"))
  
  p <- plot_ly(type = 'scatter', mode = 'text+markers', x = m[chosen,1], y = m[chosen,2],textfont = t_chosen, textposition = "top right",text=rownames(data)[chosen],name=" chosen words",
               marker = list(
                 color = 'rgb(239, 75, 67)',
                 size = 11,
                 line = list(
                   color = 'rgb(28, 2, 1)',
                   width = 1
                 )
               ))
  p<-add_trace(p, x = m[-chosen,1],y = m[-chosen,2],text=rownames(data)[-chosen],name="similar words",textfont = t_others,
               marker = list(
                 color = 'rgb(98, 59, 226)',
                 size = 8,
                 line = list(
                   color = 'rgb(98, 59, 226)',
                   width = 0.5
                 ))
               )
    p<-layout(p,xaxis=list(zeroline=F),yaxis=list(zeroline=F),title=title)
  
  return(p)
})




#######################linear Substructures###############

output$vs_linS_plotly_pca<-renderPlotly({
  validate(
    need(length(input$Det_VS_linS_words1)>1,"Please choose at least two words")
  )
  t_1 <- list(
    family = "sans serif",
    size = 16,
    color = toRGB("black"))
  
  data<-values$vs_model[c(input$Det_VS_linS_words1,input$Det_VS_linS_words2,input$Det_VS_linS_words3,input$Det_VS_linS_words4,input$Det_VS_linS_words5),]
  if(is.null(values$vs_pca)){
    m<-stats::predict(stats::prcomp(as.matrix(data)))[,1:2]
    title="A two dimensional reduction of the vector space model using pca"
  }
  else{
    m<-values$vs_pca[rownames(data),]
    title="A two dimensional reduction of the vector space model using pretrained pca"
  }
  p <- plot_ly(type = 'scatter', mode = 'text+line+markers',x=m[input$Det_VS_linS_words1,1],y=m[input$Det_VS_linS_words1,2],textfont = t_1, textposition = "top right",text=input$Det_VS_linS_words1,name="pair #1")#%>%
  if(length(input$Det_VS_linS_words2)>0){
    p<-add_trace(p,x=m[input$Det_VS_linS_words2,1],y=m[input$Det_VS_linS_words2,2],textfont = t_1, textposition = "top right",text=input$Det_VS_linS_words2,name="pair #2")
  }
  if(length(input$Det_VS_linS_words3)>0){
    p<-add_trace(p,x=m[input$Det_VS_linS_words3,1],y=m[input$Det_VS_linS_words3,2],textfont = t_1, textposition = "top right",text=input$Det_VS_linS_words3,name="pair #3")
  }
  if(length(input$Det_VS_linS_words4)>0){
    p<-add_trace(p,x=m[input$Det_VS_linS_words4,1],y=m[input$Det_VS_linS_words4,2],textfont = t_1, textposition = "top right",text=input$Det_VS_linS_words4,name="pair #3")
  }
  if(length(input$Det_VS_linS_words5)>0){
    p<-add_trace(p,x=m[input$Det_VS_linS_words5,1],y=m[input$Det_VS_linS_words5,2],textfont = t_1, textposition = "top right",text=input$Det_VS_linS_words5,name="pair #3")
  }
  
  p<-layout(p,xaxis=list(zeroline=F),yaxis=list(zeroline=F),title=title)
  
  
})



output$vs_linS_plotly_tsne<-renderPlotly({
  validate(
    need(length(input$Det_VS_linS_words1)>1,"Please choose at least two words")
  )
  t_1 <- list(
    family = "sans serif",
    size = 16,
    color = toRGB("black"))
  
  data<-values$vs_model[c(input$Det_VS_linS_words1,input$Det_VS_linS_words2,input$Det_VS_linS_words3,input$Det_VS_linS_words4,input$Det_VS_linS_words5),]
  if(is.null(values$vs_tsne)){
    m<-tsne::tsne(as.matrix(as.matrix(data)),max_iter = 250)
    rownames(m)<-c(input$Det_VS_linS_words1,input$Det_VS_linS_words2,input$Det_VS_linS_words3,input$Det_VS_linS_words4,input$Det_VS_linS_words5)
    title="A two dimensional reduction of the vector space model using tsne"
  }
  else{
    m<-values$vs_tsne[rownames(data),]
    title="A two dimensional reduction of the vector space model using pretrained tsne"
  }
  p <- plot_ly(type = 'scatter', mode = 'text+line+markers',x=m[input$Det_VS_linS_words1,1],y=m[input$Det_VS_linS_words1,2],textfont = t_1, textposition = "top right",text=input$Det_VS_linS_words1,name="pair #1")#%>%
  if(length(input$Det_VS_linS_words2)>0){
    p<-add_trace(p,x=m[input$Det_VS_linS_words2,1],y=m[input$Det_VS_linS_words2,2],textfont = t_1, textposition = "top right",text=input$Det_VS_linS_words2,name="pair #2")
  }
  if(length(input$Det_VS_linS_words3)>0){
    p<-add_trace(p,x=m[input$Det_VS_linS_words3,1],y=m[input$Det_VS_linS_words3,2],textfont = t_1, textposition = "top right",text=input$Det_VS_linS_words3,name="pair #3")
  }
  if(length(input$Det_VS_linS_words4)>0){
    p<-add_trace(p,x=m[input$Det_VS_linS_words4,1],y=m[input$Det_VS_linS_words4,2],textfont = t_1, textposition = "top right",text=input$Det_VS_linS_words4,name="pair #4")
  }
  if(length(input$Det_VS_linS_words5)>0){
    p<-add_trace(p,x=m[input$Det_VS_linS_words5,1],y=m[input$Det_VS_linS_words5,2],textfont = t_1, textposition = "top right",text=input$Det_VS_linS_words5,name="pair #5")
  }
  
  p<-layout(p,xaxis=list(zeroline=F),yaxis=list(zeroline=F),title=title)
  
  
})


################arithmetics###############

output$vs_arithmetics<-DT::renderDataTable({
  validate(
    need(length(input$Det_VS_arith_words_pos)>0,"Please specify at least one word")
  )

  if(length(input$Det_VS_arith_words_pos)==1){
    vector_pos<-values$vs_model[input$Det_VS_arith_words_pos,]
  }
  else{
    vector_pos<-base::colSums(x=values$vs_model[input$Det_VS_arith_words_pos,])
  }
  if(length(input$Det_VS_arith_words_neg)>0){
    if(length(input$Det_VS_arith_words_neg)==1){
      vector_neg<-values$vs_model[input$Det_VS_arith_words_neg,]
    }
    else{
      vector_neg<-base::colSums(x=values$vs_model[input$Det_VS_arith_words_neg,])
      vector_neg<-matrix(vector_neg,nrow=1,byrow = T)
    }
  }
  else{
    vector_neg<-matrix(c(0),0,dim(values$vs_model)[2])
  }
  if(dim(vector_neg)[1]==1){
    vector<-vector_pos-vector_neg
  }
  else{
    vector<-vector_pos
  }
  data<-values$vs_model %>% 
    closest_to(vector = matrix(vector,nrow=1,byrow = T),input$Det_VS_arith_n)
  colnames(data)<-c("word","cosine similarity")
  datatable(data = data)
})
