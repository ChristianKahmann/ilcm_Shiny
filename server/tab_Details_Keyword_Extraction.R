observe({
  validate(
    need(
      !is.null(values$Det_KE_stats),message=F
    ),
    need(
      !is.null(input$Det_KE_min_freq),message=F
    ),
    need(
      !is.null(input$Det_KE_ngram),message=F
    )
  )
  data<-values$Det_KE_stats
  if(values$Det_KE_method=="RAKE"){
    data<-data[order(data$rake,decreasing=T),]
  }
  if(values$Det_KE_method=="PMI Collocation"){
    data<-data[order(data$pmi,decreasing=T),c(1,2,3,4,5,6,7,10,9,8)]
  }
  if(values$Det_KE_method=="Phrase Sequence"){
    data<-data[order(data$freq,decreasing=T),]
  }
  if(values$Det_KE_method=="Textrank"){
    data<-data[order(data$freq,decreasing=T),]
    data<-data[,c("keyword","freq","ngram")]
  }
  data<-data[intersect(which(data$freq>=input$Det_KE_min_freq[1]),which(data$freq<=input$Det_KE_min_freq[2])),]
  data<-data[intersect(which(data$ngram>=input$Det_KE_ngram[1]),which(data$ngram<=input$Det_KE_ngram[2])),]
  values$Det_KE_data<-data
})



output$Det_KE_table<-DT::renderDataTable({
  datatable(data = values$Det_KE_data,rownames = F,selection="none",extensions = c('Buttons','Responsive'),class = "row-border compact",options=list(dom='Bfrtip',
                                                                                                       buttons = c('copy', 'csv', 'excel', 'print'),
                                                                                                       pageLength=15
  )
  )
},server=F)



output$Det_KE_plot<-plotly::renderPlotly({
  validate(
    need(
      !is.null(values$Det_KE_data),message=F
    )
  )
  p<-plotly::plot_ly(height = (0.68*as.numeric(input$dimension[2])),width=(0.65*as.numeric(input$dimension[1])),
                     y=factor(values$Det_KE_data$keyword[1:input$Det_KE_n],levels = values$Det_KE_data$keyword[1:input$Det_KE_n]),
                     x=values$Det_KE_data[1:input$Det_KE_n,ncol(values$Det_KE_data)],orientation="h",type="bar")
  p<-layout(p=p,autosize = F, yaxis=list( 
                              title="Keywords",
                             automargin = TRUE
                             ),
            xaxis=list(title=colnames(values$Det_KE_data)[ncol(values$Det_KE_data)])
            )
  
})