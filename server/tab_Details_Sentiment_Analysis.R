
#' render Sentiments dates aggreagted by date
#' depends on:
#'   values$Det_Senti_meta: meta data for sentiment analysis
#'   input$Det_SA_Time: selected time interval for sentiment analysis 
#'   values$Det_SA_DL_res: sentiment analysis detailed results
#'   input$Det_SA_Lines: show marker lines between data
output$Det_Senti_date<-renderPlotly({
  validate(
    need(!is.null(values$Det_Senti_meta),message = F),
    need(!is.null(input$Det_SA_Time),message = F)
  )
  meta<-values$Det_Senti_meta
  if(input$Det_SA_Time=="Month"){
    dates<-substring(as.character(meta[,"date"]),1,7)
  }
  if(input$Det_SA_Time=="Year"){
    dates<-substring(as.character(meta[,"date"]),1,4)
  }
  if(input$Det_SA_Time=="Week"){
    dates<-strftime(as.character(meta[,"date"]),format = "%G-%V")
  }
  if(input$Det_SA_Time=="Day"){
    dates<-substring(as.character(meta[,"date"]),1,10)
  }
  unique_dates<-unique(dates)
  unique_dates<-sort(unique_dates)
  results<-matrix(c(0),length(unique_dates),2)
  
  for(i in 1:length(unique_dates)){
    idx<-which(dates==unique_dates[i])
    results_for_date<-list()
    for(j in 1:length(idx)){
      results_for_date[[j]]<-meta[idx[j],"scores"]
    }
    results[i,1]<-mean(na.omit(unlist(results_for_date)))
    results[i,2]<-length(idx)
    #print(i)
  }
  if(input$Det_SA_Lines==T){
    mode="markers+lines"
  }
  else{
    mode="markers"
  }
  
  p<-plot_ly(x=unique_dates,y=results[,1],type="scatter",mode=mode,name="avg. sentiment scores",marker = list(
    color = 'aquamarine',
    size = 14,
    line = list(
      color = 'darkblue',
      width = 2
    )))
  p<-plotly::add_trace(p = p,x=unique_dates,y=results[,2],mode=mode,name="number of documents found",yaxis="y2",marker = list(
    color = 'coral',
    size = 14,
    line = list(
      color = 'darkred',
      width = 2
    )
  ))
  p<-layout(p = p,legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1),margin=list(r=50,b=150),yaxis2=list(rangemode="tozero",tickfont = list(color = "red"),overlaying = "y",side = "right",title = "number of documents found"),
            yaxis=list(rangemode = "tozero",title="Sentiment Score",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T),
            xaxis=list(title="date"))
  rownames(results)<-unique_dates
  values$Det_SA_DL_res<-results
  return(p)
})


#' render sentiments aggreagted by  document length
#' depends on:
#'   values$Det_Senti_meta: detailed sentiment analysis meta data
#'   input$Det_SA_max_breaks: sentiment analysis maximum of breaks
#'   input$Det_SA_Lines: sentiment analysis lines 
output$Det_Senti_length<-renderPlotly({
  validate(
    need(!is.null(values$Det_Senti_meta),message = F)
  )
  meta<-values$Det_Senti_meta
  
  doc_lengths<-meta[,"token"]
  
  breaks<-exp(seq(log(min(doc_lengths)),log(max(doc_lengths)),l=min(input$Det_SA_max_breaks,length(doc_lengths))))
  bins<-.bincode(x = doc_lengths,breaks = breaks,right = T,include.lowest = T)
  
  unique_bins<-1:length(breaks)
  results<-matrix(c(0),length(breaks),2)
  
  for(i in 1:length(unique_bins)){
    idx<-which(bins==unique_bins[i])
    if(length(idx)>0){
      results_for_bin<-list()
      for(j in 1:length(idx)){
        results_for_bin[[j]]<-meta[idx[j],"scores"]
      }
      results[i,1]<-mean(na.omit(unlist(results_for_bin)))
      results[i,2]<-length(idx)
    }
    else{
      results[i,1]<-NA
      results[i,2]<-length(idx) 
    }
  }
  if(input$Det_SA_Lines==T){
    mode="markers+lines"
  }
  else{
    mode="markers"
  }
  p<-plot_ly(x=breaks,y=results[,1],type="scatter",mode=mode,name="avg. sentiment scores",marker = list(
    color = 'aquamarine',
    size = 14,
    line = list(
      color = 'darkblue',
      width = 2
    )))
  p<-plotly::add_trace(p = p,x=breaks,y=results[,2],mode=mode,type="scatter",name="number of documents found",yaxis="y2",marker = list(
    color = 'coral',
    size = 14,
    line = list(
      color = 'darkred',
      width = 2
    )
  ))
  p<-layout(p = p,barmode="group",legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1),margin=list(r=50,b=150),yaxis2=list(rangemode="tozero",tickfont = list(color = "red"),overlaying = "y",side = "right",title = "number of documents found"),
            yaxis=list(rangemode = "tozero",title="Sentiment Score",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T),
            xaxis=list(type="log",title="number of words per document"))
  rownames(results)<-breaks
  values$Det_SA_DL_res<-results
  return(p)
})

#' download csv containing sentiment data by date 
#' depends on:
#'   values$Det_SA_DL_res: detailes sentiment analysis results
output$Det_SA_download_timeseries<-downloadHandler(
  filename = function() {
    paste('Sentiment_Analysis-', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-values$Det_SA_DL_res
    write.csv(data, con)
  }
) 



#' placeholder for other sentiment plots aggragated by available metadata
#' depends on:
#'   values$Det_Senti_meta: check which metadata are avaiable
#'   values$host: selected host
#'   values$db_port: chosen database port
#'   values$senti_meta_reduced: reduced meta data for sentiment analysis
output$Det_Senti_tab_Panels_mde_UI<-renderUI({
  #check which metadata are avaiable
  meta<-values$Det_Senti_meta
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
  rs<-dbGetQuery(mydb, paste0("SELECT * FROM ilcm.metadata_names where dataset in('",paste(unique(meta[,"dataset"]),collapse="','"),"');"))
  RMariaDB::dbDisconnect(mydb)
  if(dim(rs)[1]>0){
    available<-rs
  }
  validate(
    need(exists("available"),"no extra metadata found"),
    need(!is.null(available),message=F)
  )
  not_empty_metadata<-names(which(apply(available,MARGIN = 2,function(x){!all(is.na(x))})))
  if(length(not_empty_metadata)>0){
    meta<-meta[,which(colnames(meta)%in%not_empty_metadata)]
  }
  colnames(meta)<-available[not_empty_metadata]
  more_than_one_value<-names(which(apply(meta,MARGIN = 2,FUN=function(x){
    length(unique(x))>1
  })
  )
  )
  meta<-meta[,more_than_one_value,drop=F]
  if(dim(meta)[2]==0){
    return(tags$h4("No metadata was found to visualize sentiment counts for"))
  }
  else{
    values$senti_meta_reduced<-meta
    metadata_list<-lapply(1:dim(meta)[2],function(x){
      title<-colnames(meta)[x]
      plot_id<-paste0("senti_plot_",x)
      return(tabPanel(title = title,
                      plotlyOutput(outputId = plot_id)
      ))
    })
    
  }
  return(
    do.call(tabsetPanel,metadata_list)
  )
})

#' plot sentiment anaylsis
#' depends on:
#'   values$senti_meta_reduced: reduced meta data from sentiment analysis
#'   input$Det_SA_min: minimum for sentiment analysis
#'   values$Det_Senti_meta: sentiment analysis meta data
#'   input$Det_SA_Lines: show marker lines from sentiment analysis
#'   values$Det_SA_DL_res: sentiment analysis detailes results
output$senti_plot_1<-plotly::renderPlotly({
  validate(
    need(!is.null(values$senti_meta_reduced),message=F),
    need(dim(values$senti_meta_reduced)[2]>0,message=F)
  )
  meta<-values$senti_meta_reduced
  sections<-meta[,1]
  table_sections<-table(sections)
  unique_sections<-names(table_sections[which(table_sections>=input$Det_SA_min)])
  validate(
    need(length(unique_sections)>0,message="no entry had more occurrences than the set minimal number")
  )
  results<-matrix(c(0),length(unique_sections),2)
  
  for(i in 1:length(unique_sections)){
    idx<-which(sections==unique_sections[i])
    if(length(idx)>0){
      results_for_bin<-list()
      for(j in 1:length(idx)){
        results_for_bin[[j]]<-values$Det_Senti_meta[idx[j],"scores"]
      }
      results[i,1]<-mean(na.omit(unlist(results_for_bin)))
      results[i,2]<-length(idx)
    }
    else{
      results[i,1]<-NA
      results[i,2]<-length(idx)
    }
  }
  if(input$Det_SA_Lines==T){
    mode="markers+lines"
  }
  else{
    mode="markers"
  }
  p<-plot_ly(x=unique_sections,y=results[,1],type="scatter",mode=mode,name="avg. sentiment scores",marker = list(
    color = 'aquamarine',
    size = 14,
    line = list(
      color = 'darkblue',
      width = 2
    )))
  p<-plotly::add_trace(p = p,x=unique_sections,y=results[,2],mode=mode,type="scatter",name="number of documents found",yaxis="y2",marker = list(
    color = 'coral',
    size = 14,
    line = list(
      color = 'darkred',
      width = 2
    )
  ))
  p<-layout(p = p,barmode="group",legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1),margin=list(r=50,b=150),yaxis2=list(rangemode="tozero",tickfont = list(color = "red"),overlaying = "y",side = "right",title = "number of documents found"),
            yaxis=list(rangemode = "tozero",title="Sentiment Score",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T),
            xaxis=list(title=colnames(meta)[1]))
  rownames(results)<-unique_sections
  values$Det_SA_DL_res<-results
  return(p)
})


#' plot sentiment anaylsis
#' depends on:
#'    values$senti_meta_reduced:
#'    input$Det_SA_min: minimum for sentiment analysis
#'    values$Det_Senti_meta: meta data for sentiment analysis
#'    input$Det_SA_Lines: show marker lines for sentiment analyisis
#'    values$Det_SA_DL_res: selected results for sentiment analysis
output$senti_plot_2<-plotly::renderPlotly({
  validate(
    need(!is.null(values$senti_meta_reduced),message=F),
    need(dim(values$senti_meta_reduced)[2]>1,message=F)
  )
  meta<-values$senti_meta_reduced
  sections<-meta[,2]
  table_sections<-table(sections)
  
  unique_sections<-names(table_sections[which(table_sections>=input$Det_SA_min)])
  validate(
    need(length(unique_sections)>0,message="no entry had more occurrences than the set minimal number")
  )
  results<-matrix(c(0),length(unique_sections),2)
  
  for(i in 1:length(unique_sections)){
    idx<-which(sections==unique_sections[i])
    if(length(idx)>0){
      results_for_bin<-list()
      for(j in 1:length(idx)){
        results_for_bin[[j]]<-values$Det_Senti_meta[idx[j],"scores"]
      }
      results[i,1]<-mean(na.omit(unlist(results_for_bin)))
      results[i,2]<-length(idx)
    }
    else{
      results[i,1]<-NA
      results[i,2]<-length(idx)
    }
  }
  if(input$Det_SA_Lines==T){
    mode="markers+lines"
  }
  else{
    mode="markers"
  }
  p<-plot_ly(x=unique_sections,y=results[,1],type="scatter",mode=mode,name="avg. sentiment scores",marker = list(
    color = 'aquamarine',
    size = 14,
    line = list(
      color = 'darkblue',
      width = 2
    )))
  p<-plotly::add_trace(p = p,x=unique_sections,y=results[,2],mode=mode,type="scatter",name="number of documents found",yaxis="y2",marker = list(
    color = 'coral',
    size = 14,
    line = list(
      color = 'darkred',
      width = 2
    )
  ))
  p<-layout(p = p,barmode="group",legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1),margin=list(r=50,b=150),yaxis2=list(rangemode="tozero",tickfont = list(color = "red"),overlaying = "y",side = "right",title = "number of documents found"),
            yaxis=list(rangemode = "tozero",title="Sentiment Score",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T),
            xaxis=list(title=colnames(meta)[2]))
  rownames(results)<-unique_sections
  values$Det_SA_DL_res<-results
  return(p)
})



#' render plot for sentiment analysis (plot 3)
#' depends on:
#'   values$senti_meta_reduced: reduced meta data from sentiment analysis
#'   input$Det_SA_min: minimum for sentiment analysis
#'   values$Det_Senti_meta: sentiment analysis meta data
#'   input$Det_SA_Lines: show marker lines from sentiment analysis
#'   values$Det_SA_DL_res: sentiment analysis detailes results  
output$senti_plot_3<-plotly::renderPlotly({
  validate(
    need(!is.null(values$senti_meta_reduced),message=F),
    need(dim(values$senti_meta_reduced)[2]>2,message=F)
  )
  meta<-values$senti_meta_reduced
  sections<-meta[,3]
  table_sections<-table(sections)
  
  unique_sections<-names(table_sections[which(table_sections>=input$Det_SA_min)])
  validate(
    need(length(unique_sections)>0,message="no entry had more occurrences than the set minimal number")
  )
  results<-matrix(c(0),length(unique_sections),2)
  
  for(i in 1:length(unique_sections)){
    idx<-which(sections==unique_sections[i])
    if(length(idx)>0){
      results_for_bin<-list()
      for(j in 1:length(idx)){
        results_for_bin[[j]]<-values$Det_Senti_meta[idx[j],"scores"]
      }
      results[i,1]<-mean(na.omit(unlist(results_for_bin)))
      results[i,2]<-length(idx)
    }
    else{
      results[i,1]<-NA
      results[i,2]<-length(idx)
    }
  }
  if(input$Det_SA_Lines==T){
    mode="markers+lines"
  }
  else{
    mode="markers"
  }
  p<-plot_ly(x=unique_sections,y=results[,1],type="scatter",mode=mode,name="avg. sentiment scores",marker = list(
    color = 'aquamarine',
    size = 14,
    line = list(
      color = 'darkblue',
      width = 2
    )))
  p<-plotly::add_trace(p = p,x=unique_sections,y=results[,2],mode=mode,type="scatter",name="number of documents found",yaxis="y2",marker = list(
    color = 'coral',
    size = 14,
    line = list(
      color = 'darkred',
      width = 2
    )
  ))
  p<-layout(p = p,barmode="group",legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1),margin=list(r=50,b=150),yaxis2=list(rangemode="tozero",tickfont = list(color = "red"),overlaying = "y",side = "right",title = "number of documents found"),
            yaxis=list(rangemode = "tozero",title="Sentiment Score",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T),
            xaxis=list(title=colnames(meta)[3]))
  rownames(results)<-unique_sections
  values$Det_SA_DL_res<-results
  return(p)
})



#' plot for sentiment analysis (4)
#' depends on:
#'   values$senti_meta_reduced: reduced meta data from sentiment analysis
#'   input$Det_SA_min: minimum for sentiment analysis
#'   values$Det_Senti_meta: sentiment analysis meta data
#'   input$Det_SA_Lines: show marker lines from sentiment analysis
#'   values$Det_SA_DL_res: sentiment analysis detailes results
output$senti_plot_4<-plotly::renderPlotly({
  validate(
    need(!is.null(values$senti_meta_reduced),message=F),
    need(dim(values$senti_meta_reduced)[2]>3,message=F)
  )
  meta<-values$senti_meta_reduced
  sections<-meta[,4]
  table_sections<-table(sections)
  
  unique_sections<-names(table_sections[which(table_sections>=input$Det_SA_min)])
  validate(
    need(length(unique_sections)>0,message="no entry had more occurrences than the set minimal number")
  )
  results<-matrix(c(0),length(unique_sections),2)
  
  for(i in 1:length(unique_sections)){
    idx<-which(sections==unique_sections[i])
    if(length(idx)>0){
      results_for_bin<-list()
      for(j in 1:length(idx)){
        results_for_bin[[j]]<-values$Det_Senti_meta[idx[j],"scores"]
      }
      results[i,1]<-mean(na.omit(unlist(results_for_bin)))
      results[i,2]<-length(idx)
    }
    else{
      results[i,1]<-NA
      results[i,2]<-length(idx)
    }
  }
  if(input$Det_SA_Lines==T){
    mode="markers+lines"
  }
  else{
    mode="markers"
  }
  p<-plot_ly(x=unique_sections,y=results[,1],type="scatter",mode=mode,name="avg. sentiment scores",marker = list(
    color = 'aquamarine',
    size = 14,
    line = list(
      color = 'darkblue',
      width = 2
    )))
  p<-plotly::add_trace(p = p,x=unique_sections,y=results[,2],mode=mode,type="scatter",name="number of documents found",yaxis="y2",marker = list(
    color = 'coral',
    size = 14,
    line = list(
      color = 'darkred',
      width = 2
    )
  ))
  p<-layout(p = p,barmode="group",legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1),margin=list(r=50,b=150),yaxis2=list(rangemode="tozero",tickfont = list(color = "red"),overlaying = "y",side = "right",title = "number of documents found"),
            yaxis=list(rangemode = "tozero",title="Sentiment Score",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T),
            xaxis=list(title=colnames(meta)[4]))
  rownames(results)<-unique_sections
  values$Det_SA_DL_res<-results
  return(p)
})



#' render plot sentiment analysis 5
#' depends on:
#'   values$senti_meta_reduced: reduced meta data from sentiment analysis
#'   input$Det_SA_min: minimum for sentiment analysis
#'   values$Det_Senti_meta: sentiment analysis meta data
#'   input$Det_SA_Lines: show marker lines from sentiment analysis
#'   values$Det_SA_DL_res: sentiment analysis detailes results
output$senti_plot_5<-plotly::renderPlotly({
  validate(
    need(!is.null(values$senti_meta_reduced),message=F),
    need(dim(values$senti_meta_reduced)[2]>4,message=F)
  )
  meta<-values$senti_meta_reduced
  sections<-meta[,5]
  table_sections<-table(sections)
  
  unique_sections<-names(table_sections[which(table_sections>=input$Det_SA_min)])
  validate(
    need(length(unique_sections)>0,message="no entry had more occurrences than the set minimal number")
  )
  results<-matrix(c(0),length(unique_sections),2)
  
  for(i in 1:length(unique_sections)){
    idx<-which(sections==unique_sections[i])
    if(length(idx)>0){
      results_for_bin<-list()
      for(j in 1:length(idx)){
        results_for_bin[[j]]<-values$Det_Senti_meta[idx[j],"scores"]
      }
      results[i,1]<-mean(na.omit(unlist(results_for_bin)))
      results[i,2]<-length(idx)
    }
    else{
      results[i,1]<-NA
      results[i,2]<-length(idx)
    }
  }
  if(input$Det_SA_Lines==T){
    mode="markers+lines"
  }
  else{
    mode="markers"
  }
  p<-plot_ly(x=unique_sections,y=results[,1],type="scatter",mode=mode,name="avg. sentiment scores",marker = list(
    color = 'aquamarine',
    size = 14,
    line = list(
      color = 'darkblue',
      width = 2
    )))
  p<-plotly::add_trace(p = p,x=unique_sections,y=results[,2],mode=mode,type="scatter",name="number of documents found",yaxis="y2",marker = list(
    color = 'coral',
    size = 14,
    line = list(
      color = 'darkred',
      width = 2
    )
  ))
  p<-layout(p = p,barmode="group",legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1),margin=list(r=50,b=150),yaxis2=list(rangemode="tozero",tickfont = list(color = "red"),overlaying = "y",side = "right",title = "number of documents found"),
            yaxis=list(rangemode = "tozero",title="Sentiment Score",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T),
            xaxis=list(title=colnames(meta)[5]))
  rownames(results)<-unique_sections
  values$Det_SA_DL_res<-results
  return(p)
})


#' render plot for sentiment analysis (6)
#' depends on:
#'   values$senti_meta_reduced: reduced meta data from sentiment analysis
#'   input$Det_SA_min: minimum for sentiment analysis
#'   values$Det_Senti_meta: sentiment analysis meta data
#'   input$Det_SA_Lines: show marker lines from sentiment analysis
#'   values$Det_SA_DL_res: sentiment analysis detailes results
output$senti_plot_6<-plotly::renderPlotly({
  validate(
    need(!is.null(values$senti_meta_reduced),message=F),
    need(dim(values$senti_meta_reduced)[2]>5,message=F)
  )
  meta<-values$senti_meta_reduced
  sections<-meta[,6]
  table_sections<-table(sections)
  
  unique_sections<-names(table_sections[which(table_sections>=input$Det_SA_min)])
  validate(
    need(length(unique_sections)>0,message="no entry had more occurrences than the set minimal number")
  )
  results<-matrix(c(0),length(unique_sections),2)
  
  for(i in 1:length(unique_sections)){
    idx<-which(sections==unique_sections[i])
    if(length(idx)>0){
      results_for_bin<-list()
      for(j in 1:length(idx)){
        results_for_bin[[j]]<-values$Det_Senti_meta[idx[j],"scores"]
      }
      results[i,1]<-mean(na.omit(unlist(results_for_bin)))
      results[i,2]<-length(idx)
    }
    else{
      results[i,1]<-NA
      results[i,2]<-length(idx)
    }
  }
  if(input$Det_SA_Lines==T){
    mode="markers+lines"
  }
  else{
    mode="markers"
  }
  p<-plot_ly(x=unique_sections,y=results[,1],type="scatter",mode=mode,name="avg. sentiment scores",marker = list(
    color = 'aquamarine',
    size = 14,
    line = list(
      color = 'darkblue',
      width = 2
    )))
  p<-plotly::add_trace(p = p,x=unique_sections,y=results[,2],mode=mode,type="scatter",name="number of documents found",yaxis="y2",marker = list(
    color = 'coral',
    size = 14,
    line = list(
      color = 'darkred',
      width = 2
    )
  ))
  p<-layout(p = p,barmode="group",legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1),margin=list(r=50,b=150),yaxis2=list(rangemode="tozero",tickfont = list(color = "red"),overlaying = "y",side = "right",title = "number of documents found"),
            yaxis=list(rangemode = "tozero",title="Sentiment Score",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T),
            xaxis=list(title=colnames(meta)[6]))
  rownames(results)<-unique_sections
  values$Det_SA_DL_res<-results
  return(p)
})


#' render plot sentiment analysis (7)
#'  depends on:
#'   values$senti_meta_reduced: reduced meta data from sentiment analysis
#'   input$Det_SA_min: minimum for sentiment analysis
#'   values$Det_Senti_meta: sentiment analysis meta data
#'   input$Det_SA_Lines: show marker lines from sentiment analysis
#'   values$Det_SA_DL_res: sentiment analysis detailes results
output$senti_plot_7<-plotly::renderPlotly({
  validate(
    need(!is.null(values$senti_meta_reduced),message=F),
    need(dim(values$senti_meta_reduced)[2]>6,message=F)
  )
  meta<-values$senti_meta_reduced
  sections<-meta[,7]
  table_sections<-table(sections)
  
  unique_sections<-names(table_sections[which(table_sections>=input$Det_SA_min)])
  validate(
    need(length(unique_sections)>0,message="no entry had more occurrences than the set minimal number")
  )
  results<-matrix(c(0),length(unique_sections),2)
  
  for(i in 1:length(unique_sections)){
    idx<-which(sections==unique_sections[i])
    if(length(idx)>0){
      results_for_bin<-list()
      for(j in 1:length(idx)){
        results_for_bin[[j]]<-values$Det_Senti_meta[idx[j],"scores"]
      }
      results[i,1]<-mean(na.omit(unlist(results_for_bin)))
      results[i,2]<-length(idx)
    }
    else{
      results[i,1]<-NA
      results[i,2]<-length(idx)
    }
  }
  if(input$Det_SA_Lines==T){
    mode="markers+lines"
  }
  else{
    mode="markers"
  }
  p<-plot_ly(x=unique_sections,y=results[,1],type="scatter",mode=mode,name="avg. sentiment scores",marker = list(
    color = 'aquamarine',
    size = 14,
    line = list(
      color = 'darkblue',
      width = 2
    )))
  p<-plotly::add_trace(p = p,x=unique_sections,y=results[,2],mode=mode,type="scatter",name="number of documents found",yaxis="y2",marker = list(
    color = 'coral',
    size = 14,
    line = list(
      color = 'darkred',
      width = 2
    )
  ))
  p<-layout(p = p,barmode="group",legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1),margin=list(r=50,b=150),yaxis2=list(rangemode="tozero",tickfont = list(color = "red"),overlaying = "y",side = "right",title = "number of documents found"),
            yaxis=list(rangemode = "tozero",title="Sentiment Score",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T),
            xaxis=list(title=colnames(meta)[7]))
  rownames(results)<-unique_sections
  values$Det_SA_DL_res<-results
  return(p)
})


#' render plot sentiment analysis (8)
#'  depends on:
#'   values$senti_meta_reduced: reduced meta data from sentiment analysis
#'   input$Det_SA_min: minimum for sentiment analysis
#'   values$Det_Senti_meta: sentiment analysis meta data
#'   input$Det_SA_Lines: show marker lines from sentiment analysis
#'   values$Det_SA_DL_res: sentiment analysis detailes results
output$senti_plot_8<-plotly::renderPlotly({
  validate(
    need(!is.null(values$senti_meta_reduced),message=F),
    need(dim(values$senti_meta_reduced)[2]>7,message=F)
  )
  meta<-values$senti_meta_reduced
  sections<-meta[,8]
  table_sections<-table(sections)
  
  unique_sections<-names(table_sections[which(table_sections>=input$Det_SA_min)])  
  validate(
    need(length(unique_sections)>0,message="no entry had more occurrences than the set minimal number")
  )
  results<-matrix(c(0),length(unique_sections),2)
  
  for(i in 1:length(unique_sections)){
    idx<-which(sections==unique_sections[i])
    if(length(idx)>0){
      results_for_bin<-list()
      for(j in 1:length(idx)){
        results_for_bin[[j]]<-values$Det_Senti_meta[idx[j],"scores"]
      }
      results[i,1]<-mean(na.omit(unlist(results_for_bin)))
      results[i,2]<-length(idx)
    }
    else{
      results[i,1]<-NA
      results[i,2]<-length(idx)
    }
  }
  if(input$Det_SA_Lines==T){
    mode="markers+lines"
  }
  else{
    mode="markers"
  }
  p<-plot_ly(x=unique_sections,y=results[,1],type="scatter",mode=mode,name="avg. sentiment scores",marker = list(
    color = 'aquamarine',
    size = 14,
    line = list(
      color = 'darkblue',
      width = 2
    )))
  p<-plotly::add_trace(p = p,x=unique_sections,y=results[,2],mode=mode,type="scatter",name="number of documents found",yaxis="y2",marker = list(
    color = 'coral',
    size = 14,
    line = list(
      color = 'darkred',
      width = 2
    )
  ))
  p<-layout(p = p,barmode="group",legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1),margin=list(r=50,b=150),yaxis2=list(rangemode="tozero",tickfont = list(color = "red"),overlaying = "y",side = "right",title = "number of documents found"),
            yaxis=list(rangemode = "tozero",title="Sentiment Score",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T),
            xaxis=list(title=colnames(meta)[8]))
  rownames(results)<-unique_sections
  values$Det_SA_DL_res<-results
  return(p)
})


#' render plot for sentiment analysis (9)
#'  depends on:
#'   values$senti_meta_reduced: reduced meta data from sentiment analysis
#'   input$Det_SA_min: minimum for sentiment analysis
#'   values$Det_Senti_meta: sentiment analysis meta data
#'   input$Det_SA_Lines: show marker lines from sentiment analysis
#'   values$Det_SA_DL_res: sentiment analysis detailes results 
output$senti_plot_9<-plotly::renderPlotly({
  validate(
    need(!is.null(values$senti_meta_reduced),message=F),
    need(dim(values$senti_meta_reduced)[2]>8,message=F)
  )
  meta<-values$senti_meta_reduced
  sections<-meta[,9]
  table_sections<-table(sections)
  
  unique_sections<-names(table_sections[which(table_sections>=input$Det_SA_min)])
  validate(
    need(length(unique_sections)>0,message="no entry had more occurrences than the set minimal number")
  )
  results<-matrix(c(0),length(unique_sections),2)
  
  for(i in 1:length(unique_sections)){
    idx<-which(sections==unique_sections[i])
    if(length(idx)>0){
      results_for_bin<-list()
      for(j in 1:length(idx)){
        results_for_bin[[j]]<-values$Det_Senti_meta[idx[j],"scores"]
      }
      results[i,1]<-mean(na.omit(unlist(results_for_bin)))
      results[i,2]<-length(idx)
    }
    else{
      results[i,1]<-NA
      results[i,2]<-length(idx)
    }
  }
  if(input$Det_SA_Lines==T){
    mode="markers+lines"
  }
  else{
    mode="markers"
  }
  p<-plot_ly(x=unique_sections,y=results[,1],type="scatter",mode=mode,name="avg. sentiment scores",marker = list(
    color = 'aquamarine',
    size = 14,
    line = list(
      color = 'darkblue',
      width = 2
    )))
  p<-plotly::add_trace(p = p,x=unique_sections,y=results[,2],mode=mode,type="scatter",name="number of documents found",yaxis="y2",marker = list(
    color = 'coral',
    size = 14,
    line = list(
      color = 'darkred',
      width = 2
    )
  ))
  p<-layout(p = p,barmode="group",legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1),margin=list(r=50,b=150),yaxis2=list(rangemode="tozero",tickfont = list(color = "red"),overlaying = "y",side = "right",title = "number of documents found"),
            yaxis=list(rangemode = "tozero",title="Sentiment Score",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T),
            xaxis=list(title=colnames(meta)[9]))
  rownames(results)<-unique_sections
  values$Det_SA_DL_res<-results
  return(p)
})



#' validation tab for sentiments showing the original documents with highlights sentiment scores
#' depends on:
#'   input$Det_SA_validation_document: detailed sentiment analysis for validation of documents
#'   values$Det_Senti_meta: sentiment analysis meta data
#'   values$host: selected host
#'   values$port: selected port
#'   values$Details_Data_SA: details of sentiment analysis data
output$Det_Senti_validation_UI<-renderUI({
  validate(
    need(
      !is.null(input$Det_SA_validation_document),message=FALSE
    ),
    need(
      input$Det_SA_validation_document!="",message="please choose a document"
    )
  )
  
  identifier<-stringr::str_split(string = input$Det_SA_validation_document,pattern = "_",simplify = T)
  dataset<-identifier[1]
  doc_id<-identifier[2]
  #browser()
  title_score<-values$Det_Senti_meta[intersect(which(values$Det_Senti_meta[,"dataset"]==dataset),which(values$Det_Senti_meta[,"id_doc"]==doc_id)),c("title","scores")]
  token<-get_token_from_db(dataset = dataset,doc_ids = doc_id,sentence_ids = NULL,host=values$host,port=values$port)
  # remove idx column from token
  token<-token[,-ncol(token)]
  
  validate(
    need(nrow(token)>0,message="Could not find doucments in the database")
  )

  load(paste0(values$Details_Data_SA,"/parameters.RData"))
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
  
  # load sentiment weights   
  sentiments<-read.csv(file = paste0("collections/sentiments/",parameters$Sentiment_Dictionary))
  colnames(sentiments)<-c("id","features","weight")
  
  
  #browser()
  m<-merge(x = token,y=sentiments,by = "features",all.x=TRUE)
  m<-m[order(m[,2]),]
  #getPalette = colorRampPalette(brewer.pal(12, "Paired"))
  #colors<-getPalette(10)
  #colors<-colors[order(values$tm_theta[input$Det_TM_validation_document,],decreasing = F)]
  m<-cbind(m,rep("",dim(m)[1]))
  rbPal_pos <- colorRampPalette(c("red","white","chartreuse1"))
  
  m[intersect(which(!is.na(m$weight)),which(m$weight!=0)),13]<-  rbPal_pos(100)[as.numeric(cut(c(max(as.numeric(sentiments[,"weight"])),
                                                                                                 min(as.numeric(sentiments[,"weight"])),
                                                                                                 m$weight[intersect(which(!is.na(m$weight)),which(m$weight!=0))]),breaks = 100))[-c(1,2)]] #Alternative#seq(0,to = max(data$weight),length.out = 100) #original m$weight[intersect(which(!is.na(m$weight)),which(m$weight>0))]
  
  
  strings<-apply(m,MARGIN = 1,FUN = function(x){
    if(is.na(x[12])){
      return(x[7])
    }
    else{
      return( paste0('<font style="font-weight: bold; background-color:',x[13],';"','title="feature: ',x[1],' with weight: ',round(as.numeric(x[12]),digits = 5),'">',x[7],'</font>'))
    }
  })
  
  document<-list()
  for(i in 1:dim(m)[1]){
    document[[i]]<-paste0("<span span_nr='",i,"'>",strings[i],"</span>")
  }
  document<-do.call(rbind,document)
  document<-HTML(document)
  
  return(tagList(
    tags$h4(title_score[1]),
    tags$h5(HTML(paste0("Document Sentiment Score: <b>",title_score[2],"</b>"))),
    tags$hr(),
    document
  ))
})



#' table showing the most "positive" documents
#' depends on:
#'   values$Det_Senti_meta: sentiment analysis meta data 
output$Det_Senti_validation_table_positive<-DT::renderDataTable({
  data<-values$Det_Senti_meta
  data<-data[order(data$score,decreasing=T),c("title","scores")]
  datatable(data = data,rownames=F,selection = "none")
})


#' table showing the most "negative" documents
#' depends on:
#'   values$Det_Senti_meta: sentiment analysis meta data
output$Det_Senti_validation_table_negative<-DT::renderDataTable({
  data<-values$Det_Senti_meta
  data<-data[order(data$score,decreasing=F),c("title","scores")]
  datatable(data = data,rownames=F,selection = "none")
})