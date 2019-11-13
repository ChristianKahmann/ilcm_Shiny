
##############Sentiments###############

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
    dates<-strftime(as.character(meta[,"date"]),format = "%Y-%V")
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



# output$Det_Senti_author<-renderPlotly({
#   meta<-values$Det_Senti_meta
#   authors<-meta[,"author"]
#   table_authors<-table(authors)
#   
#   unique_authors<-names(table_authors[which(table_authors>input$Det_SA_min_Author)])
#   validate(
#     need(length(unique_authors)>0,message=paste0("No author left with at least ",input$Det_SA_min_Author, " occurrences"))
#   )
#   results<-matrix(c(0),length(unique_authors),2)
#   
#   for(i in 1:length(unique_authors)){
#     idx<-which(authors==unique_authors[i])
#     if(length(idx)>0){
#       results_for_bin<-list()
#       for(j in 1:length(idx)){
#         results_for_bin[[j]]<-meta[idx[j],14]
#       }
#       results[i,1]<-mean(na.omit(unlist(results_for_bin)))
#       results[i,2]<-length(idx)
#     }
#     else{
#       results[i,1]<-NA
#       results[i,2]<-length(idx) 
#     }
#   }
#   if(input$Det_SA_Lines==T){
#     mode="markers+lines"
#   }
#   else{
#     mode="markers"
#   }
#   p<-plot_ly(x=unique_authors,y=results[,1],type="scatter",mode=mode,name="avg. sentiment scores",marker = list(
#     color = 'aquamarine',
#     size = 14,
#     line = list(
#       color = 'darkblue',
#       width = 2
#     )))
#   p<-plotly::add_trace(p = p,x=unique_authors,y=results[,2],mode=mode,type="scatter",name="number of documents found",yaxis="y2",marker = list(
#     color = 'coral',
#     size = 14,
#     line = list(
#       color = 'darkred',
#       width = 2
#     )
#   ))
#   p<-layout(p = p,barmode="group",legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1),margin=list(r=50,b=150),yaxis2=list(rangemode="tozero",tickfont = list(color = "red"),overlaying = "y",side = "right",title = "number of documents found"),
#             yaxis=list(rangemode = "tozero",title="Sentiment Score",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T),
#             xaxis=list(title="authors"))
#   rownames(results)<-unique_authors
#   values$Det_SA_DL_res<-results
#   return(p)
# })
# 
# 
# output$Det_Senti_section<-renderPlotly({
#   meta<-values$Det_Senti_meta
#   sections<-meta[,"section"]
#   table_sections<-table(sections)
# 
#   unique_sections<-names(table_sections[which(table_sections>input$Det_SA_min_Section)])
#   results<-matrix(c(0),length(unique_sections),2)
# 
#   for(i in 1:length(unique_sections)){
#     idx<-which(sections==unique_sections[i])
#     if(length(idx)>0){
#       results_for_bin<-list()
#       for(j in 1:length(idx)){
#         results_for_bin[[j]]<-meta[idx[j],14]
#       }
#       results[i,1]<-mean(na.omit(unlist(results_for_bin)))
#       results[i,2]<-length(idx)
#     }
#     else{
#       results[i,1]<-NA
#       results[i,2]<-length(idx)
#     }
#   }
#   if(input$Det_SA_Lines==T){
#     mode="markers+lines"
#   }
#   else{
#     mode="markers"
#   }
#   p<-plot_ly(x=unique_sections,y=results[,1],type="scatter",mode=mode,name="avg. sentiment scores",marker = list(
#     color = 'aquamarine',
#     size = 14,
#     line = list(
#       color = 'darkblue',
#       width = 2
#     )))
#   p<-plotly::add_trace(p = p,x=unique_sections,y=results[,2],mode=mode,type="scatter",name="number of documents found",yaxis="y2",marker = list(
#     color = 'coral',
#     size = 14,
#     line = list(
#       color = 'darkred',
#       width = 2
#     )
#   ))
#   p<-layout(p = p,barmode="group",legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1),margin=list(r=50,b=150),yaxis2=list(rangemode="tozero",tickfont = list(color = "red"),overlaying = "y",side = "right",title = "number of documents found"),
#             yaxis=list(rangemode = "tozero",title="Sentiment Score",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T),
#             xaxis=list(title="sections"))
#   rownames(results)<-unique_sections
#   values$Det_SA_DL_res<-results
#   return(p)
# 
# })


output$Det_SA_download_timeseries<-downloadHandler(
  filename = function() {
    paste('Sentiment_Analysis-', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-values$Det_SA_DL_res
    write.csv(data, con)
  }
) 



output$tab_Panels_senti_mde<-renderUI({
  #browser()
  #check which metadata are avaiable
  meta<-values$Det_Senti_meta
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
  rs<-dbGetQuery(mydb, paste0("SELECT * FROM ilcm.metadata_names where dataset in('",paste(unique(meta[,"dataset"]),collapse="','"),"');"))
  RMariaDB::dbDisconnect(mydb)
  if(dim(rs)[1]>0){
    available<-rs
  }
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