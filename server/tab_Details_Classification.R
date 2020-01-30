
output$Det_Class_classifier_performance<-renderUI({
  load(paste0(values$Details_Data_CL,"/result.RData"))
  return(tagList(
    plot_ly(x=factor(c("0.003", "0.01", "0.03", "0.1", "0.3", "1", "3" , "10", "30", "100"),levels=c("0.003", "0.01", "0.03", "0.1", "0.3", "1", "3" , "10", "30", "100")),y=result,type = "scatter",mode="lines")%>%
      layout(xaxis=list(title="C-Value"),yaxis=list(title="F-score",range=c(0,1))),
    tags$br(),
    tags$b(paste0("best F-score: ",max(result))),
    tags$br(),
    tags$hr(),
    fluidRow(style="margin-left:0px;margin-right:0px",
             column(4,
                    box(title = "Micro",status = "info",solidHeader = T,width = 12,
                        DT::dataTableOutput(outputId = "Det_CL_eval_micro"))
             ),
             column(8,
                    box(title = "Macro",status = "info",solidHeader = T,width = 12,
                        DT::dataTableOutput(outputId = "Det_CL_eval_macro")
                    )
             )
    )
  ))
})

output$Det_CL_eval_micro<-DT::renderDataTable({
  validate(
    need(!is.null(values$Det_CL_results_complete),message="F")
  )
  
  data<-as.data.frame(t(round(as.numeric(values$Det_CL_results_complete[[as.numeric(input$Det_CL_c)]][[as.numeric(input$Det_CL_fold)]]$micro),digits = 4)))
  colnames(data)<-c("Precision","Recall","F-Score")
  datatable(data = data,rownames = F,options = list(dom="t"))
})


output$Det_CL_eval_macro<-DT::renderDataTable({
  validate(
    need(!is.null(values$Det_CL_results_complete),message="F")
  )
  data<-round(values$Det_CL_results_complete[[as.numeric(input$Det_CL_c)]][[as.numeric(input$Det_CL_fold)]]$macro,digits=4)
  datatable(data=data,options=list(dom="tp"))
})


output$Det_Class_date_distribution<-renderPlotly({
  validate(
    need(!is.null(input$Det_CL_Time),message=F)
  )
  load(paste0(values$Details_Data_CL,"/result.RData"))
  max<-max(unlist(lapply(dates,max)))
  min<-min(unlist(lapply(dates,min)))
  data<-list()
  for(i in 1:length(dates)){
    missing_dates<-as.character(seq.Date(from = as.Date(min),to = as.Date(max),by = "day"))
    found_dates<-dates[[i]]
    if(input$Det_CL_Time=="Day"){
      found_dates<-substr(found_dates,1,10)
      missing_dates<-substr(as.character(missing_dates),1,10)
    }
    if(input$Det_CL_Time=="Month"){
      found_dates<-substr(found_dates,1,7)
      missing_dates<-substr(as.character(missing_dates),1,7)
    }
    if(input$Det_CL_Time=="Year"){
      found_dates<-substr(found_dates,1,4)
      missing_dates<-substr(as.character(missing_dates),1,4)
    }
    missing_dates<-unique(missing_dates)
    missing_dates<-missing_dates[-which(missing_dates%in%found_dates)]
    table<-table(found_dates)
    table<-cbind(names(table),table)
    table<-rbind(table,cbind(missing_dates,rep(0,length(missing_dates))))
    table<-table[order(table[,1],decreasing = F),]
    table<-matrix(table,ncol = 2)
    data[[i]]<-table
  }
  values$Class_timeseries_data<-do.call(cbind,data)
  p<-plot_ly(x=(data[[1]][,1]),y=as.numeric(data[[1]][,2]),type = "scatter",mode="lines+markers",name=names(dates)[1])
  if(length(data)>1){
    for(k in 2:length(data)){
      p<-add_trace(p,x=(data[[k]][,1]),y=as.numeric(data[[k]][,2]),mode="lines+markers",name=names(dates)[k])
    }
  }
  p<-layout(p,paper_bgcolor='rgb(255,255,255)',legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1), plot_bgcolor='rgb(229,229,229)',xaxis=list(autotick=T,showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = FALSE,side="bottom")
            ,margin=list(b=100),yaxis=list(rangemode = "tozero",title="Frequency",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T))
  
  return(p)
  
})


#render a piechart showing the distribution of found examples > threshold
output$Det_Class_pie<-plotly::renderPlotly({
  load(paste0(values$Details_Data_CL,"/result.RData"))
  counts<-as.data.frame(table(predictions))
  
  p <- plot_ly(counts, labels = ~predictions, values = ~Freq, textposition = 'inside',
               textinfo = 'label+percent') %>%
    plotly::add_pie(hole = 0.6) %>%
    plotly::layout(title = paste('Distribution of found classifications'),legend=T,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  return(p)
})



output$Det_CL_download_texts<-downloadHandler(
  filename = function() {
    paste('examples-', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    load(paste0(values$Details_Data_CL,"/texts.RData"))
    write.csv(original_text, con)
  }
)  

output$Det_CL_download_timeseries<-downloadHandler(
  filename = function() {
    paste('classification_timeseries-', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-values$Class_timeseries_data
    write.csv(data, con)
  }
)  


output$Det_CL_download_feature_matrix<-downloadHandler(
  filename = function() {
    paste('feature_matrix-', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-as.matrix(values$Det_CL_feature_matrix)
    write.csv(data, con)
  }
)



output$Det_CL_feature_UI<-renderUI({
  validate(
    need(
      !is.null(values$Det_CL_feature_matrix),message=F
    ),
    need(!is.null(input$Det_CL_feature_class),message=F
    )
  )
  #browser()
  data<-values$Det_CL_feature_matrix[input$Det_CL_feature_class,]
  data_pos<-sort(data,decreasing = T)[1:(input$Det_CL_number_of_features/2)]
  data_neg<-sort(data,decreasing = F)[1:(input$Det_CL_number_of_features/2)]
  data<-c(data_pos,data_neg)
  data<-data.frame(weight=data,frequency=values$Det_CL_word_counts[names(data)])
  
  t <- list(
    family = "sans serif",
    size = 12,
    color = toRGB("black"))
  if(input$Det_CL_feature_show_labels==T){
    p<-plotly::plot_ly(x=data$frequency,y=data$weight,color=data$weight,colors=c("firebrick","limegreen"),text=rownames(data),marker=list(size=12))%>%
      plotly::add_markers()%>%
      plotly::add_text(textfont = t, textposition = "top right")%>%
      plotly::layout(yaxis=list(zeroline=FALSE,title="SVM Weights"),xaxis=list(zeroline=FALSE,title="Frequency",type="log"),showlegend = FALSE,title="most discriminative features for and against chosen category")
  }
  else{
    p<-plotly::plot_ly(x=data$frequency,y=data$weight,color=data$weight,colors=c("firebrick","limegreen"),text=rownames(data),marker=list(size=12))%>%
      plotly::add_markers()%>%
      plotly::layout(yaxis=list(zeroline=FALSE,title="SVM Weights"),xaxis=list(zeroline=FALSE,title="Frequency",type="log"),showlegend = FALSE,title="most discriminative features for and against chosen category")
  }
  return(
    tagList(
      p,
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(6,
                      box(title = "Pro features",status = "info",solidHeader = T,width = 12,
                          DT::dataTableOutput(outputId = "Det_CL_feature_table_pro")
                      )
               ),
               column(6,
                      box(title = "Contra features",status = "info",solidHeader = T,width = 12,
                          DT::dataTableOutput(outputId = "Det_CL_feature_table_contra")
                      )
               )
      )
      
    )
  )
})

output$Det_CL_feature_table_pro<-DT::renderDataTable({
  data<-values$Det_CL_feature_matrix[input$Det_CL_feature_class,]
  data_pos<-sort(data,decreasing = T)[1:(input$Det_CL_number_of_features/2)]
  DT::datatable(data = data.frame(weight=data_pos,frequency=values$Det_CL_word_counts[names(data_pos)]),options = list(dom="tp"))
})

output$Det_CL_feature_table_contra<-DT::renderDataTable({
  data<-values$Det_CL_feature_matrix[input$Det_CL_feature_class,]
  data_neg<-sort(data,decreasing = F)[1:(input$Det_CL_number_of_features/2)]
  DT::datatable(data = data.frame(weight=data_neg,frequency=values$Det_CL_word_counts[names(data_neg)]),options = list(dom="tp"))
})


output$Det_CL_validation_document_UI<-renderUI({
  validate(
    need(
      !is.null(input$Det_CL_feature_class2),message=FALSE
    )
  )
  load(paste0(values$Details_Data_CL,"/result.RData"))
  values$Det_CL_probabilities<-probabilities
  documents_relevant<-names(predictions[which(predictions==input$Det_CL_feature_class2)])
  context_unit<-"Documents"
  if(length(stringr::str_extract_all(string = documents_relevant[1],pattern = "_"))==2){
    context_unit="Sentences"
  }
  return(selectInput(inputId = "Det_CL_validation_document",label =context_unit,choices = documents_relevant,multiple = F))
})

output$Det_CL_validation<-renderUI({
  validate(
    need(
      !is.null(input$Det_CL_validation_document),message=FALSE
    )
  )
  identifier<-stringr::str_split(string = input$Det_CL_validation_document,pattern = "_",simplify = T)
  dataset<-identifier[1]
  doc_id<-identifier[2]
  sentence_id<-NULL
  if(length(identifier)==3){
    sentence_id<-identifier[3]
  }
  token<-get_token_from_db(dataset = dataset,doc_ids = doc_id,sentence_ids = sentence_id,host=values$host,port=values$port)
  load(paste0(values$Details_Data_CL,"/parameters.RData"))
  space_ids<-which(token[,"pos"]=="SPACE")
  if(length(space_ids)>0){
    token<-token[-space_ids,]
  }
  
  if(parameters$baseform_reduction=="none"){
    features<-tolower(token[,"word"])  
  }
  if(parameters$baseform_reduction=="lemma"){
    features<-tolowertoken[,"lemma"]  
  }
  if(parameters$baseform_reduction=="stemming"){
    features<-tolower(quanteda::tokens_wordstem(quanteda::tokens(paste(token[,"word"],collapse=" ")),lang)$text1)
  }
  token<-cbind(token,features)
  token<-cbind(1:dim(token)[1],token)
  data<-values$Det_CL_feature_matrix[input$Det_CL_feature_class2,]
  data<-data.frame(features=names(data),weight=data)
  m<-merge(x = token,y=data,by="features",all.x=TRUE)
  m<-m[order(m[,2]),]
  
  rbPal_pos <- colorRampPalette(c('white','green'))
  rbPal_neg <- colorRampPalette(c('red','white'))
  m<-cbind(m,rep("",dim(m)[1]))

  if(length(intersect(which(!is.na(m$weight)),which(m$weight>0)))>0){
    m[intersect(which(!is.na(m$weight)),which(m$weight>0)),12]<-  rbPal_pos(100)[as.numeric(cut(m$weight[intersect(which(!is.na(m$weight)),which(m$weight>0))],breaks = 100))]
  }
  if(length(intersect(which(!is.na(m$weight)),which(m$weight<0)))>0){
    m[intersect(which(!is.na(m$weight)),which(m$weight<0)),12]<-  rbPal_neg(100)[as.numeric(cut(m$weight[intersect(which(!is.na(m$weight)),which(m$weight<0))],breaks = 100))]       
  }
  strings<-apply(m,MARGIN = 1,FUN = function(x){
    if(is.na(x[12])){
      return(x[7])
    }
    else{
      return( paste0('<font style="background-color:',x[12],';"','title="feature: ',x[1],' with weight: ',x[11],'">',x[7],'</font>'))
    }
    
  })
  
  a<-list()
  for(i in 1:dim(m)[1]){
    a[[i]]<-paste0("<span span_nr='",i,"'>",strings[i],"</span>")
  }
  a<-do.call(rbind,a)
  a<-HTML(a)

  return(
    tagList(
      tags$br(),
      tags$h4(paste("Probability:", values$Det_CL_probabilities[input$Det_CL_validation_document],sep="")),
      tags$br(),
      tags$p(a)
    ) 
  )
  
  
})
