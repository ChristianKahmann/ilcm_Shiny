
output$Det_Class_classifier_performance<-renderUI({
  load(paste0(values$Details_Data_CL,"/result.RData"))
  return(tagList(
    plot_ly(x=c("0.003", "0.01", "0.03", "0.1", "0.3", "1", "3" , "10", "30", "100"),y=result,type = "scatter",mode="lines")%>%
      layout(xaxis=list(title="C-Value"),yaxis=list(title="F-score",range=c(0,1))),
    tags$br(),
    tags$b(paste0("best F-score: ",max(result)))
  ))
})

output$Det_Class_date_distribution<-renderPlotly({
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

output$Det_CO_download_coocs<-downloadHandler(
  filename = function() {
    paste('co-occurence_matrix-', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-as.matrix(values$Det_CO_dl_coocs)
    write.csv(data, con)
  }
) 
