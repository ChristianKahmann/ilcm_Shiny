
#render FE words to export checkbox
output$FE_words_to_export<-renderUI({
  validate(
    need(length(input$Det_FE_Word)>0,message = FALSE)
  )
  checkboxGroupInput(inputId = "FE_CB_words_to_export",label ="TERMS",choices = input$Det_FE_Word,selected = NULL,inline = T)
})



#render FE plot
output$FE_plot<-renderPlotly({
  validate(
    need(length(input$Det_FE_Word)>0 | isTRUE(input$Det_FE_use_regexp),"choose at least one word or use regexp")
  )
  if(input$Det_FE_REL_ABS=="relative"){
    if(input$Det_FE_Term_Doc=="Word"){
      if(input$Det_FE_Time=="Day"){
        freq_matrix<-values$FE_rel_freqs_day
      }
      if(input$Det_FE_Time=="Week"){
        freq_matrix<-values$FE_rel_freqs_week
      }
      if(input$Det_FE_Time=="Month"){
        freq_matrix<-values$FE_rel_freqs_month
      }
      if(input$Det_FE_Time=="Year"){
        freq_matrix<-values$FE_rel_freqs_year
      }
    }
    else{
      validate(
        need(isFALSE(input$Det_FE_use_regexp),message = "no plots for documents level using reg exp")
      )
      if(input$Det_FE_Time=="Day"){
        freq_matrix<-values$FE_rel_doc_freqs_day
      }
      if(input$Det_FE_Time=="Week"){
        freq_matrix<-values$FE_rel_doc_freqs_week
      }
      if(input$Det_FE_Time=="Month"){
        freq_matrix<-values$FE_rel_doc_freqs_month
      }
      if(input$Det_FE_Time=="Year"){
        freq_matrix<-values$FE_rel_doc_freqs_year
      }
    }
  }
  else{
    if(input$Det_FE_Term_Doc=="Word"){
      if(input$Det_FE_Time=="Day"){
        freq_matrix<-values$FE_freqs_day
      }
      if(input$Det_FE_Time=="Week"){
        freq_matrix<-values$FE_freqs_week
      }
      if(input$Det_FE_Time=="Month"){
        freq_matrix<-values$FE_freqs_month
      }
      if(input$Det_FE_Time=="Year"){
        freq_matrix<-values$FE_freqs_year
      }
    }
    else{
      validate(
        need(isFALSE(input$Det_FE_use_regexp),message = "no plots for documents level using reg exp")
      )
      if(input$Det_FE_Time=="Day"){
        freq_matrix<-values$FE_doc_freqs_day
      }
      if(input$Det_FE_Time=="Week"){
        freq_matrix<-values$FE_doc_freqs_week
      }
      if(input$Det_FE_Time=="Month"){
        freq_matrix<-values$FE_doc_freqs_month
      }
      if(input$Det_FE_Time=="Year"){
        freq_matrix<-values$FE_doc_freqs_year
      }
    }
  }
  
  orig_dates<-rownames(freq_matrix)
  if(input$Det_FE_Time=="Month"){
    rownames(freq_matrix)<-paste0(rownames(freq_matrix),"-01")
  }
  if(input$Det_FE_Time=="Year"){
    rownames(freq_matrix)<-paste0(rownames(freq_matrix),"-01-01")
  }
  if(input$Det_FE_Time=="Week"){
    rownames(freq_matrix)<-as.character(as.Date(rownames(freq_matrix),format="%Y-%V"))
  }
  missing_dates<-as.character(seq.Date(from = min(as.Date(rownames(freq_matrix))),to = max(as.Date(rownames(freq_matrix))),by = tolower(input$Det_FE_Time)))
  if(input$Det_FE_Time=="Month"){
    missing_dates<-unique(substr(missing_dates,1,7))
  }
  if(input$Det_FE_Time=="Year"){
    missing_dates<-unique(substr(missing_dates,1,4))
  }
  if(input$Det_FE_Time=="Week"){
    missing_dates<-unique(strftime(as.character(missing_dates),format = "%Y-%V"))
  }
  missing_dates<-setdiff(missing_dates,orig_dates)
  freq_matrix<-rbind(freq_matrix,Matrix(c(0),length(missing_dates),dim(freq_matrix)[2]))
  rownames(freq_matrix)<-c(orig_dates,missing_dates)
  freq_matrix<-freq_matrix[order(rownames(freq_matrix)),,drop=F]
  if(input$Det_FE_use_regexp==FALSE){
    words<-input$Det_FE_Word
    
    values$freq_matrix<-freq_matrix
    
    p<-plot_ly()
    p<-plot_ly(x=rownames(freq_matrix),y=as.numeric(freq_matrix[,words[1]]),type = "scatter",mode="lines+markers",name=words[1])
    if(length(words)>1){
      for(k in 2:length(words)){
        p<-add_trace(p,x=rownames(freq_matrix),y=as.numeric(freq_matrix[,words[k]]),mode="lines+markers",name=words[k])
      }
    }
    p<-layout(p,paper_bgcolor='rgb(255,255,255)',legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1), plot_bgcolor='rgb(229,229,229)',xaxis=list(autotick=T,showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = FALSE,side="bottom")
              ,margin=list(b=80),yaxis=list(rangemode = "tozero",title="Frequency",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T))
    
    return(p)
  }
  else{
    validate(
      need(input$Det_FE_calc==TRUE,"set calculate on TRUE, when you are ready")
    )
    #find words matching to regexp
    vocab<-stringr::str_detect(string = colnames(freq_matrix),pattern = regex(input$Det_FE_regexp))
    values$Det_FE_regexp_words<-colnames(freq_matrix)[vocab]
    freq_matrix<-freq_matrix[,vocab,drop=F]
    freq_matrix<-as.matrix(rowSums(freq_matrix),drop=F)
    colnames(freq_matrix)<-input$Det_FE_regexp
    values$freq_matrix<-freq_matrix
    #browser()
    p<-plot_ly()
    p<-plot_ly(x=rownames(freq_matrix),y=as.numeric(freq_matrix[,1]),type = "scatter",mode="lines+markers",name=input$Det_FE_regexp)
    p<-layout(p,paper_bgcolor='rgb(255,255,255)',legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1), plot_bgcolor='rgb(229,229,229)',xaxis=list(autotick=T,showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = FALSE,side="bottom")
              ,margin=list(b=80),yaxis=list(rangemode = "tozero",title="Frequency",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T))
    
    return(p)
  }
})

#link downloadbutton in FE details to the according time series data
output$download_FE_frequencies<-downloadHandler(
  filename = function() {
    paste('Time_Series-', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    if(input$Det_FE_use_regexp==FALSE){
      if(length(input$FE_CB_words_to_export)==0){
        shinyWidgets::sendSweetAlert(session=session,title = "No words for export chosen.",text = "Please select the words you want to extract the time series data for in the corresponding checkboxes.",
                                     type="warning"  )
        break;
      }
      data<-as.matrix(values$freq_matrix[,input$FE_CB_words_to_export,drop=F])
      colnames(data)<-input$FE_CB_words_to_export
    }
    else{
      data<-as.matrix(values$freq_matrix[,1,drop=F])
      colnames(data)<-input$Det_FE_regexp
    }
    write.csv(data, con)
  }
)  

output$Det_FE_regexp_words<-renderUI({
  validate(
    need(!is.null(values$Det_FE_regexp_words),message=F)
  )
  return(tags$h6(paste("found words matching regexp:",paste(values$Det_FE_regexp_words,collapse=", "))))
})


###############################################
#       most frequent words                   #
###############################################


# get available points in time depending on chosen timeintervall and present it as a select input
output$Det_FE_most_frequent_words_points_in_time_UI<-renderUI({
  validate(
    need(!is.null(input$Det_FE_most_frequent_words_timeintervall),message=F)
  )
  timeintervall<-input$Det_FE_most_frequent_words_timeintervall
  if(timeintervall=="Day"){
    choices<-rownames(values$FE_freqs_day)
  }
  if(timeintervall=="Week"){
    choices<-rownames(values$FE_freqs_week)
  }
  if(timeintervall=="Month"){
    choices<-rownames(values$FE_freqs_month)
  }
  if(timeintervall=="Year"){
    choices<-rownames(values$FE_freqs_year)
  }
  return(selectizeInput(inputId="Det_FE_most_frequent_words_points_in_time",label="Points in time",choices=choices,multiple=T))
})


# renderTable and Wordcloud showing the most frequent words at chosen point(s) in time
output$Det_FE_most_frequent_words_UI<-renderUI({
  input$Det_FE_most_frequent_words_use_whole_time
  input$Det_FE_most_frequent_words_timeintervall
  input$Det_FE_most_frequent_words_points_in_time
  validate(
    need(!is.null(input$Det_FE_most_frequent_words_use_whole_time),message=F)
  )
  if(input$Det_FE_most_frequent_words_use_whole_time==TRUE){
    counts<-colSums(values$FE_freqs_day)
    counts<-data.frame(names=names(counts),counts=counts)
  }
  else{
    validate(
      need(length(input$Det_FE_most_frequent_words_points_in_time)>0,message="Please select atleast one point in time.")
    )
    if(input$Det_FE_most_frequent_words_timeintervall=="Day"){
      counts<-colSums(values$FE_freqs_day[input$Det_FE_most_frequent_words_points_in_time,,drop=F])
      counts<-data.frame(names=names(counts),counts=counts)
    }
    if(input$Det_FE_most_frequent_words_timeintervall=="Week"){
      counts<-colSums(values$FE_freqs_week[input$Det_FE_most_frequent_words_points_in_time,,drop=F])
      counts<-data.frame(names=names(counts),counts=counts)
    }
    if(input$Det_FE_most_frequent_words_timeintervall=="Month"){
      counts<-colSums(values$FE_freqs_month[input$Det_FE_most_frequent_words_points_in_time,,drop=F])
      counts<-data.frame(names=names(counts),counts=counts)
    }
    if(input$Det_FE_most_frequent_words_timeintervall=="Year"){
      counts<-colSums(values$FE_freqs_year[input$Det_FE_most_frequent_words_points_in_time,,drop=F])
      counts<-data.frame(names=names(counts),counts=counts)
    }
  }
  values$Det_FE_most_frequent_words_counts<-counts
  return(
    tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(6,
                      box(solidHeader = T,width = 12,title = "Table",status = "primary",
                        DT::dataTableOutput(outputId = "Det_FE_most_frequent_words_table")
                      )
               ),
               column(6,
                      box(solidHeader = T,width = 12,title = "Wordcloud",status = "primary",
                        wordcloud2::wordcloud2Output(outputId = "Det_FE_most_frequent_words_wc")
                      )
               )
      )
    )
  )
})

# datatable showing the counts of words per time point(s)
output$Det_FE_most_frequent_words_table<-DT::renderDataTable({
  validate(
    need(!is.null(values$Det_FE_most_frequent_words_counts),message=F)
  )
  data<- values$Det_FE_most_frequent_words_counts
  data<-data[order(data$counts,decreasing=T),]
  datatable(data=data,rownames = F,selection = "none")
})



# wordcloud showing the counts of words per time point(s)
output$Det_FE_most_frequent_words_wc<-wordcloud2::renderWordcloud2({
  validate(
    need(!is.null(values$Det_FE_most_frequent_words_counts),message=F)
  )
  data<- values$Det_FE_most_frequent_words_counts
  data<-data[order(data$counts,decreasing=T),]
  wordcloud2(data = data,size=1,fontFamily = "Helvetica",color = "random-light",minSize = 0.2,backgroundColor = "black",minRotation = -pi/2,maxRotation = -pi/2)
  
})

