#object to save TS results in
values$TS_memory<-list()
#reactive value used to inform timeseries plot about delete or reset events
values$TS_delete<-0
values$TS_delete_help<-0
#counter for the number of solr searches
values$search<-0
#render time series plot
output$TS_plot<-renderPlotly({
  if(!is.null((values$url))){
    print(values$solr_query)
    #check if delete or refresh was clicked
    print(isolate(values$TS_delete))
    print(isolate(values$TS_delete_help))
    print(values$search)
    if(values$TS_delete==isolate(values$TS_delete_help)){
      #get dates for current solr search
      if(isolate(values$numFound)>0){
        data<-facet_date(base = isolate(values$url),q = isolate(values$q),facet.field="date_dt",fq=isolate(values$fq_init),fl="facet",facet.limit = -1,facet.range.gap = "%2B1MONTH")
        date<-unlist(data$facet_fields$date_dt$X1)
        counts<-unlist(data$facet_fields$date_dt$X2)
        if(length(counts)>0){
          tmp_dates<-list()
          for(i in 1:length(counts)){
            #dates<-c(dates,rep(date[i],counts[i]))
            tmp_dates[[i]]<-rep(date[i],counts[i])
          }
        }
        dates<-unlist(tmp_dates)
        dates<-data.frame(dates)
        #create missing dates for plot
        missing_dates<-seq.Date(from = min(as.Date(dates[,1])),to = max(as.Date(dates[,1])),by = "day")
        
        #save data in a list, to display the results of the 10 searches
        if(length(isolate(values$TS_memory))>0){
          if(isolate(values$q)!=isolate(values$TS_memory)[[1]][[3]]){
            for( i in min(9,length(isolate(values$TS_memory))):1){
              isolate(values$TS_memory[[i+1]]<-values$TS_memory[[i]])
            }
          }
        }
        isolate(values$TS_memory[[1]]<-list(dates,missing_dates,isolate(values$q)))
      }
    }
    else{
      if(isolate(values$TS_delete)==1){
        values$TS_delete_help<-1
      }
      else{
        values$TS_delete_help<-0
      }
      
    }
    #object to store the plot data
    if(length(isolate(values$TS_memory))>0){
      results<-list()
      for(j in 1:length(isolate(values$TS_memory))){
        dates<-isolate(values$TS_memory)[[j]][[1]]
        missing_dates<-isolate(values$TS_memory)[[j]][[2]]
        if(input$TS_timeintervall=="day"){
          dates[,1]<-substr(dates[,1],1,10)
          missing_dates<-substr(as.character(missing_dates),1,10)
        }
        if(input$TS_timeintervall=="month"){
          dates[,1]<-substr(dates[,1],1,7)
          missing_dates<-substr(as.character(missing_dates),1,7)
        }
        if(input$TS_timeintervall=="year"){
          dates[,1]<-substr(dates[,1],1,4)
          missing_dates<-substr(as.character(missing_dates),1,4)
        }
        missing_dates<-missing_dates[-which(missing_dates%in%dates[,1])]
        missing_dates<-as.matrix(table(missing_dates))
        missing_dates[,1]<-0
        missing_dates<-cbind(rownames(missing_dates),missing_dates[,1])
        dates<-as.matrix(table(dates))
        dates<-cbind(rownames(dates),dates[,1])
        dates<-rbind(dates,missing_dates)
        #order data by date
        dates<-dates[order(dates[,1]),]
        dates<-matrix(dates,ncol = 2)
        results[[j]]<-dates
      }
      print("dates ready")
      #get abs number of documents from solr facets
      if(input$TS_rel_abs=="relative"){
        fq<-stringr::str_extract(string = isolate(values$fq),pattern = "(dataset_s:[A-Za-z0-9]+)")
        glob_data<-facet_date(base = isolate(values$url),q ="*:*",facet.field="date_dt",fq=fq,fl="facet",facet.limit = -1,facet.range.gap = "%2B1MONTH")
        date<-unlist(glob_data$facet_fields$date_dt$X1)
        counts<-unlist(glob_data$facet_fields$date_dt$X2)
        if(length(counts)>0){
          tmp_dates<-list()
          for(i in 1:length(counts)){
            #dates<-c(dates,rep(date[i],counts[i]))
            tmp_dates[[i]]<-rep(date[i],counts[i])
          }
        }
        dates<-unlist(tmp_dates)
        dates<-data.frame(dates)
        missing_dates<-seq.Date(from = min(as.Date(dates[,1])),to = max(as.Date(dates[,1])),by = "day")
        if(input$TS_timeintervall=="day"){
          dates[,1]<-substr(dates[,1],1,10)
          missing_dates<-substr(as.character(missing_dates),1,10)
        }
        if(input$TS_timeintervall=="month"){
          dates[,1]<-substr(dates[,1],1,7)
          missing_dates<-substr(as.character(missing_dates),1,7)
        }
        if(input$TS_timeintervall=="year"){
          dates[,1]<-substr(dates[,1],1,4)
          missing_dates<-substr(as.character(missing_dates),1,4)
        }
        missing_dates<-missing_dates[-which(missing_dates%in%dates[,1])]
        missing_dates<-as.matrix(table(missing_dates))
        missing_dates[,1]<-0
        missing_dates<-cbind(rownames(missing_dates),missing_dates[,1])
        dates<-as.matrix(table(dates))
        dates<-cbind(rownames(dates),dates[,1])
        dates<-rbind(dates,missing_dates)
        #order data by date
        dates<-dates[order(dates[,1]),]
        dates<-matrix(dates,ncol = 2)
        #divide counts for query by glob counts
        for(l in 1:length(results)){
          ids<-which(results[[l]][,1]%in%dates[,1])
          ids2<-which(dates[,1]%in%results[[l]][,1])
          results[[l]][ids,2]<-as.numeric(results[[l]][ids,2])/as.numeric(dates[ids2,2])
        }
        
      }
      values$share_results<-results[[1]] 
      #create time series scatter plot
      #//scattergl evtl besser für ältere Rechner
      p<-plot_ly(source="TI",x=(results[[1]][,1]),y=as.numeric(results[[1]][,2]),type = "scatter",mode="lines+markers",name=stringr::str_replace(string=isolate(values$TS_memory[[1]][[3]]),pattern = "\\* AND",""))
      if(length(results)>1){
        for(k in 2:length(results)){
          p<-add_trace(p,x=(results[[k]][,1]),y=as.numeric(results[[k]][,2]),mode="lines+markers",name=stringr::str_replace(string=isolate(values$TS_memory[[k]][[3]]),pattern = "\\* AND",""))
        }
      }
      layout(p,paper_bgcolor='rgb(255,255,255)',legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1), plot_bgcolor='rgb(229,229,229)',xaxis=list(autotick=T,showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = FALSE,side="bottom")
             ,margin=list(b=100),yaxis=list(rangemode = "tozero",title="Frequency",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T))
    }
    else{
      return(plotly_empty())
    }
  }
  else{
    return(plotly_empty())
  }
})
observeEvent(event_data("plotly_click",source = "TI"),{
  #get date clicked on in timeseries plot
  date<-event_data("plotly_click",source = "TI")[1,3]
  
  #get solr results for subsetting depending on clicked date
  if(grepl(pattern = "date",x = isolate(values$fq)))
  {
    if(grepl(pattern = "date_dt",x = isolate(values$fq)))
    {
      if(grepl(pattern = "TO",x = isolate(values$fq))){
        if(input$TS_timeintervall=="year"){
          fq<-stringr::str_replace(string = isolate(values$fq),pattern = "(date_dt).{47}",replacement = "")
          fq<-paste0('date_dt:','[',date,'-01-01T00:00:00Z TO ',date,'-12-31T00:00:00Z]',fq)
        }
        if(input$TS_timeintervall=="month"){
          days_in_month<-numberOfDays(as.Date(paste0(date,"-01")))
          fq<-stringr::str_replace(string = isolate(values$fq),pattern = "(date_dt).{47}",replacement = "")
          fq<-paste0('date_dt:','[',date,'-01T00:00:00Z TO ',date,'-',days_in_month,'T00:00:00Z]',fq)
        }
        if(input$TS_timeintervall=="day"){
          fq<-stringr::str_replace(string = isolate(values$fq),pattern = "(date_dt).{47}",replacement = "")
          fq<-paste0('date_dt=','"',date,'"',fq)
        }
      }
      else{
        if(input$TS_timeintervall=="year"){
          fq<-stringr::str_replace(string = isolate(values$fq),pattern = "(date_dt).{13}",replacement = "")
          fq<-paste0('date_dt:','[',date,'-01-01T00:00:00Z TO ',date,'-12-31T00:00:00Z]',fq)
        }
        if(input$TS_timeintervall=="month"){
          days_in_month<-numberOfDays(as.Date(paste0(date,"-01")))
          fq<-stringr::str_replace(string = isolate(values$fq),pattern = "(date_dt).{13}",replacement = "")
          fq<-paste0('date_dt:','[',date,'-01T00:00:00Z TO ',date,'-',days_in_month,'T00:00:00Z]',fq)
        }
        if(input$TS_timeintervall=="day"){
          fq<-stringr::str_replace(string = isolate(values$fq),pattern = "(date_dt).{13}",replacement = "")
          fq<-paste0('date_dt=','"',date,'"',fq)
        }
      }
      values$fq<-fq
    }
    else{
      fq<-stringr::str_replace(string = isolate(values$fq),pattern = '(date_dt=")[0-9\\-]{4,10}',replacement = "")
      fq<-paste0(fq,'date_dt=','"',date,'"')
      values$fq<-fq
    }
  }
  else{
    if(input$TS_timeintervall=="year"){
      values$fq<-paste0('date_dt:','[',date,'-01-01T00:00:00Z TO ',date,'-12-31T00:00:00Z]', ' AND ',as.character(isolate(values$fq)))
      
    }
    if(input$TS_timeintervall=="month"){
      days_in_month<-numberOfDays(as.Date(paste0(date,"-01")))
      values$fq<-paste0('date_dt:','[',date,'-01T00:00:00Z TO ',date,'-',days_in_month,'T00:00:00Z]', ' AND ',as.character(isolate(values$fq)))
      
    }
    if(input$TS_timeintervall=="day"){
      values$fq<-paste0('date_dt=','"',date,'" AND ',as.character(isolate(values$fq)))
    }
  }
  #set parameters for getting Search Results for clicked time
  values$numFound<-as.integer(str_replace_all(string = str_extract(string =as.character(solr::solr_search(base = isolate(values$url),q = isolate(values$q),fl="id",fq=isolate(values$fq),rows="1",raw=T)),pattern = "numFound\\\":[0-9]+,"),pattern = "\\D",replacement = ""))
  updateTextInput(session = session,inputId = "SR_row_sel",value = 1)
  values$start=1
  #switch tab to Search Results
  #showModal(modalDialog(
  #  title = "Important message",
  #  "This is an important message!"
  #updateTabsetPanel(session = session,inputId = "expl",selected = "Search Results")
  #)
  #)
  updateTabsetPanel(session = session,inputId = "expl",selected = "Search Results")
})


#render select button for memorized Times Series
output$TS_memory<-renderUI({
  if(length(values$TS_memory)>0){
    mem<-values$TS_memory[[1]][[3]]
    if(length(values$TS_memory)>1){
      for(i in 2:length(values$TS_memory)){
        mem<-c(mem,values$TS_memory[[i]][[3]])
      }
    }
  }
  else{
    mem<-NULL
  }
  mem<-stringr::str_replace(string = mem,pattern = "\\* AND ","")
  values$mem<-mem
  #values$TS_delete<-0
  selectInput(inputId = "TS_Select_Memory",label = NULL,choices = mem)
})

#check if delete was clicked, and then remove corresponding memorized time series
observeEvent(input$TS_delete_memory,{
  values$TS_memory[[which(values$mem==input$TS_Select_Memory)]]<-NULL
  print("delete")
  if(values$TS_delete_help==0){
    values$TS_delete<-1
  }
  else{
    values$TS_delete<-0
  }
})

#check if reset was clicked, and the reset the plot 
observeEvent(input$TS_reset,{
  values$TS_memory<-list()
  values$numFound<-0
  print("reset")
  if(values$TS_delete_help==0){
    values$TS_delete<-1
  }
  else{
    values$TS_delete<-0
  }
})


#create download link for time series memory
output$TS_download_memory<-downloadHandler(
  filename=function(){
    paste0(values$mem,".csv")
  },
  content=function(con){
    write.csv(table(values$TS_memory[[which(values$mem==input$TS_Select_Memory)]][[1]]),con)
  }
)


output$TS_calender<-renderUI({
    values$solr_query
    if(isolate(values$numFound)>0){
      data<-facet_date(base = isolate(values$url),q = isolate(values$q),facet.field="date_dt",fq=isolate(values$fq_init),fl="facet",facet.limit = -1,facet.range.gap = "%2B1MONTH")
      date<-unlist(data$facet_fields$date_dt$X1)
      counts<-unlist(data$facet_fields$date_dt$X2)
      if(length(counts)>0){
        tmp_dates<-list()
        for(i in 1:length(counts)){
          #dates<-c(dates,rep(date[i],counts[i]))
          tmp_dates[[i]]<-rep(date[i],counts[i])
        }
      }
      dates<-unlist(tmp_dates)
      dates<-data.frame(dates)
      #create missing dates for plot
      missing_dates<-seq.Date(from = min(as.Date(dates[,1])),to = max(as.Date(dates[,1])),by = "day")
      dates[,1]<-substr(dates[,1],1,10)
      missing_dates<-substr(as.character(missing_dates),1,10)
      missing_dates<-missing_dates[-which(missing_dates%in%dates[,1])]
      missing_dates<-as.matrix(table(missing_dates))
      missing_dates[,1]<-0
      missing_dates<-cbind(rownames(missing_dates),missing_dates[,1])
      dates<-as.matrix(table(dates))
      dates<-cbind(rownames(dates),dates[,1])
      dates<-rbind(dates,missing_dates)
      #order data by date
      dates<-dates[order(dates[,1]),]
      dates<-matrix(dates,ncol = 2)
      dates<-data.frame(date=dates[,1],pts=as.numeric(dates[,2]))
      print(Sys.time())
      maps<-calendarChart(dates)
      print(Sys.time())
      return(maps)
    }
})

#get documents for clicked calender heatmap
observe({
  print(input$heatmap_date)
})

