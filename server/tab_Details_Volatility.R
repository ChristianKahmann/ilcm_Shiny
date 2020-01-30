

db1<<-1
db2<<-1
values$open<-F

observeEvent(input$Det_VA_Update,{
  #load data once and make it avaiable for all visulisations
  output$VA_plot<-renderPlotly({
 validate(
   need(nchar(input$Det_VA_Select_Word)>0,message="Please choose a word.")
 )
    termss<<-rownames(values$va_freq)
    un_dates<-values$va_un_dates[order(values$va_un_dates,decreasing = F)]
    label<<-un_dates
    p1<-plot_ly(source = as.character(input$Det_VA_Select_Word),x=label,y=( values$va_voldata[isolate({input$Det_VA_Select_Word}),]),type="scatter",mode="lines",name=paste0("volatility(",isolate(input$Det_VA_Select_Word),")"))
    p1<-add_trace(p=p1,y = (values$va_freq[isolate({input$Det_VA_Select_Word}),]), name = paste0("frequency(",isolate(input$Det_VA_Select_Word),")"), mode = 'lines',yaxis="y2")
    layout(p1,margin=list(r=50,b=150),legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1),yaxis2=list(rangemode="tozero",tickfont = list(color = "red"),overlaying = "y",side = "right",title = "Frequency"),paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',xaxis=list(autotick=T,showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = FALSE,side="bottom"),margin=list(b=80),yaxis=list(rangemode = "tozero",title="Context Volatility",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T))
  })
  output$coocs<-renderDataTable({
    eventdata<-event_data("plotly_click",source = as.character(input$Det_VA_Select_Word))
    
    validate(need(!is.null(eventdata), "click in the graph to see co-occurrences for the corresponding time"))
    datapoint<-as.numeric(eventdata$pointNumber)[1]
    #get data from coocYears
    data<-sort(values$va_cy[[datapoint+1]][input$Det_VA_Select_Word,],decreasing=T)[1:50]
    zero<-which(data==0)
    if(length(zero>0)){
      data<-data[-zero]
    }
    validate(
      need(length(data)>1,"no co-occurrences found")
    )
    data<-as.data.frame(cbind(names(data),data),stringsAsFactors = F)
    colnames(data)<-c("word","significance")
    class(data$significance)<-"numeric"
    data<-data.frame(data)
  },options = list(pageLength = 5, autoWidth = TRUE),
  rownames= FALSE)
  
  output$volat_coocs_headline<-renderUI({
    eventdata<-event_data("plotly_click",source = as.character(input$Det_VA_Select_Word))
    validate(need(!is.null(eventdata), message=FALSE))
    datapoint<-as.numeric(eventdata$pointNumber)[1]
    date<-label[datapoint+1]
    tags$h4(paste0("Top co-occurrences for ",date))
  })
  
  output$wc<-renderWordcloud2({
    eventdata<-event_data("plotly_click",source = as.character(input$Det_VA_Select_Word))
    validate(need(!is.null(eventdata), "click in the graph to see the wordcloud for the corresponding time"))
    datapoint<-as.numeric(eventdata$pointNumber)[1]
    data<-sort(values$va_cy[[datapoint+1]][input$Det_VA_Select_Word,],decreasing=T)[1:50]
    zero<-which(data==0)
    if(length(zero>0)){
      data<-data[-zero]
    }
    validate(
      need(length(data)>1,"no co-occurrences found")
    )
    data<-data/max(data)  
    data<-data*20
    data<-data.frame(names(data),data)
    wordcloud2(data,size=0.35,fontFamily = "Helvetica",color = "random-light",backgroundColor = "black")
    
  })
  output$coocs2<-renderDataTable({
    eventdata<-event_data("plotly_click",source = as.character(input$Det_VA_Select_Word))
    validate(need(!is.null(eventdata), "click on graph to see coocs for that time"))
    db1<<-db2
    db2<<-(as.numeric(eventdata$pointNumber)[1])+1
    print("db1:")  
    print(db1)
    print("db2")
    print(db2)
    
    
    daten1<-values$va_cy[[db1]][input$Det_VA_Select_Word,]
    daten2<-values$va_cy[[db2]][input$Det_VA_Select_Word,]
    
    # data<-NULL
    diff<-daten2-daten1
    names(diff)<-termss
    diff<-diff[which(diff!=0)]
    
    #values$open<-T
    
    data<-data.frame(names(diff),diff)
    colnames(data)<-c("word","significance change")
    data<-data[order(data[,2]),]
    return(data)
    
  },options = list(pageLength = 5, autoWidth = TRUE),
  rownames= FALSE)
  
  #wordcloud
  output$wc2<-renderWordcloud2({
    eventdata<-event_data("plotly_click",source = as.character(input$Det_VA_Select_Word))
    validate(need(!is.null(eventdata), "click in the graph to see co-occurrences for the corresponding time"))
    
    colfuncrb <- colorRampPalette(c("gold", "springgreen"))
    colfuncbb <- colorRampPalette(c("deeppink", "gold"))
    
    validate(need(db1!=db2,"Please select 2 different points in time"))
    daten1<-values$va_cy[[db1]][input$Det_VA_Select_Word,]
    daten2<-values$va_cy[[db2]][input$Det_VA_Select_Word,]
    # data<-NULL
    
    diff<-daten2-daten1
    names(diff)<-termss
    diff<-diff[which(diff!=0)]
    
    idx<-order(abs(diff),decreasing = T)[1:min(length(diff),as.numeric(input$max_words))]
    d<-cbind(names(diff)[idx],diff[idx])
    colnames(d)<-c("word","significance change")
    d<-data.frame(d)
    
    
    d[,2]<-as.numeric(as.character(d[,2]))
    d[,1]<-(as.character(d[,1]))
    
    d<-d[order(d[,2]),]
    ind1<-which(d[,2]<0)
    ind2<-which(d[,2]>0)
    
    colors<-c(colfuncbb(length(ind1)),(colfuncrb(length(ind2))))
    if(input$was=="new"){
      d<-d[ind2,]
      colors<-colors[ind2]
    }
    if(input$was=="missing"){
      d<-d[ind1,]
      colors<-colors[ind1] 
    }
    
    d[,2]<-abs(d[,2])
    colors<-colors[order(d[,2],decreasing = T)]
    d<-d[order(d[,2],decreasing = T),]
    d[,2]<-d[,2]/max(d[,2])  
    d[,2]<-d[,2]*20
    
    wordcloud2(d,size=0.35,color = colors,backgroundColor = "black",fontFamily = "Helvetica")
  }) 
  
  output$volat_divergent_headline<-renderUI({
    eventdata<-event_data("plotly_click",source = as.character(input$Det_VA_Select_Word))
    validate(need(!is.null(eventdata), message=FALSE))
    date1<-label[db1]
    date2<-label[db2]
    tags$h4(paste0("Top divergent co-occurrences from ",date1," to ", date2))
  })
  
  
  
  output$VA_Multi_plot<-renderPlotly({
    words<-(isolate(input$Det_VA_Select_Words))
    if(length(words)>0){
      p<-plot_ly(y= values$va_voldata[words[1],],x=values$va_un_dates,type="scatter",mode="lines+markers",name=words[1])
      if(length(words)>1){
        for(i in 2:length(words)){
          p<-add_trace(p,x=values$va_un_dates,y= values$va_voldata[words[i],],name=words[i])
          
        }
      }
      p<-layout(p,legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1),margin=list(b=100,l=80,pad=20,autoexpand=TRUE))
      return(p)
    }
    else{
      return()
    }
    
  })
  output$Det_VA_highV<-DT::renderDataTable({
    words<-rownames( values$va_voldata)
    freqs<-rowMeans(values$va_freq)
    volat<-rowMeans( values$va_voldata)
    data<-cbind(words,freqs,volat)  
    if(length(isolate(input$Det_VA_maxFreq_high))>0){
      data<-data[which(as.numeric(data[,2])<isolate(input$Det_VA_maxFreq_high)),]
    }
    if(length(isolate(input$Det_VA_minFreq_high))>0){
      data<-data[which(as.numeric(data[,2])>isolate(input$Det_VA_minFreq_high)),]
    }
    data<-data[order(data[,3],decreasing = T),]
    data<-data.frame(data)
    return(datatable(data = data,rownames = FALSE))
  })
  
  output$Det_VA_lowV<-DT::renderDataTable({
    words<-rownames( values$va_voldata)
    freqs<-rowMeans(values$va_freq)
    volat<-rowMeans( values$va_voldata)
    data<-cbind(words,freqs,volat)  
    if(length(isolate(input$Det_VA_maxFreq_low))>0){
      data<-data[which(as.numeric(data[,2])<isolate(input$Det_VA_maxFreq_low)),]
    }
    if(length(isolate(input$Det_VA_minFreq_low))>0){
      data<-data[which(as.numeric(data[,2])>isolate(input$Det_VA_minFreq_low)),]
    }
    data<-data[order(data[,3],decreasing = F),]
    data<-data.frame(data)
    return(datatable(data = data,rownames = FALSE))
  })
  
  output$Det_VA_highest_period<-renderPlotly({
    id<-which(values$va_un_dates==(input$Det_VA_time))
    frequence<-values$va_freq[,id]
    volat<-values$va_voldata[,id]
    words<-rownames(values$va_voldata)
    if(input$Det_VA_POS!="all"){
      words<-values$va_pos_tags[which(values$va_pos_tags[,2]==input$Det_VA_POS),1]
      frequence<-frequence[words]
      volat<-volat[words]
    }
    if(input$Det_VA_NER!="all"){
      words<-intersect(values$va_ner_tags[which(values$va_ner_tags[,2]==input$Det_VA_NER),1],words)
      frequence<-frequence[words]
      volat<-volat[words]
    }
    
    p<-plot_ly(x=frequence,y=volat,text=words,color=volat,size=(volat*frequence),colors=c("firebrick","limegreen"))
    p<-layout(p,yaxis=list(zeroline=FALSE,title="Context Volatility"),xaxis=list(zeroline=FALSE,title="Frequency",type="log"))
    return(p)
  })
})

output$Det_VA_WC<-renderUI({
  voldata<-values$va_voldata
  freqs<-values$va_freq
  label<-values$va_un_dates
  if(input$Det_VA_WC_TIME=="year"){
    values$VA_number_of_plots<-length(unique(substr(label,1,4)))
    values$VA_dates_timeintervall_un<-unique(substr(label,1,4))
    values$VA_dates_timeintervall<-substr(label,1,4)
  }
  if(input$Det_VA_WC_TIME=="month"){
    values$VA_number_of_plots<-length(unique(substr(label,1,7)))
    values$VA_dates_timeintervall_un<-unique(substr(label,1,7))
    values$VA_dates_timeintervall<-substr(label,1,7)
  }
  
  wordcloud_list<-lapply(1:values$VA_number_of_plots,function(i){
    box_title<-paste0("Year: ",values$VA_dates_timeintervall_un[i])
    wc_id<-paste0("va_wc_",i)
    tb_id<-paste0("va_tb_",i)
    return(box(title = box_title,width = 12,
               tags$head(
                 tags$style(HTML('div#wcLabel {display: none;}'))
               ),
               column(6,
                      wordcloud2Output(outputId = wc_id,width = "auto")
               ),
               column(6,
                      DT::dataTableOutput(outputId = tb_id)      
               )
    )
    )
  }
  )
  return(do.call(tagList,wordcloud_list))
})




observeEvent(input$Det_VA_Update,{
  validate(
    need(length(values$VA_number_of_plots)>0,message=FALSE)
  )
  for(i in 1:values$VA_number_of_plots){
    local({
      print(i)
      ids<-which(isolate(values$VA_dates_timeintervall)==isolate(values$VA_dates_timeintervall_un)[i])
      if(length(ids)>1){
        vol<-rowMeans(isolate(values$va_voldata)[,ids])
        freq<-rowMeans(isolate(values$va_freq)[,ids])
      }
      else{
        vol<-(isolate(values$va_voldata)[,ids])
        freq<-(isolate(values$va_freq)[,ids])
      }
      words<-rownames(isolate(values$va_voldata))
      
      if(input$Det_VA_WC_POS!="all"){
        words<-values$va_pos_tags[which(values$va_pos_tags[,2]==input$Det_VA_WC_POS),1]
        freq<-freq[words]
        vol<-vol[words]
      }
      if(input$Det_VA_WC_NER!="all"){
        words<-intersect(values$va_ner_tags[which(values$va_ner_tags[,2]==input$Det_VA_WC_NER),1],words)
        freq<-freq[words]
        vol<-vol[words]
      }
      
      data<-cbind(words,freq,vol)
      validate(
        need(dim(data)[2]==3,
             message="no data"
        )
      )
      
      if(length((input$Det_VA_WC_maxFreq_low))>0){
        data<-matrix(data[which(as.numeric(data[,2])<isolate(input$Det_VA_WC_maxFreq_low)),],ncol=3)
      }
      if(length((input$Det_VA_WC_minFreq_low))>0){
        data<-matrix(data[which(as.numeric(data[,2])>isolate(input$Det_VA_WC_minFreq_low)),],ncol=3)
      }
      
      data_raw<-matrix(data[order(as.numeric(data[,3]),decreasing = T),],ncol=3)
      
      if(length(data)==0){
        plotname_wc<-paste0("va_wc_",i)
        output[[plotname_wc]]<-renderWordcloud2(
          {
            wordcloud2(data = data.frame(words=c("no","data","left","for","this","configuration"),counts=c(6,5,4,3,2,1),stringsAsFactors = F),size = 0.3,minSize = 0.1,fontFamily = "Helvetica",backgroundColor = "black",color = "random-light")
          }
        )
        tablename<-paste0("va_tb_",i)
        output[[tablename]]<-DT::renderDataTable({
          datatable(data = data.frame(words=NULL,frequency=NULL,volatility=NULL,stringsAsFactors = F))
        }
        )
      }
      else
      {
        data<-matrix(data[order(as.numeric(as.character(data[,3])),decreasing = T),],ncol=3)
        data<-matrix(data[1:min(input$DET_VA_WC_N,dim(data)[1]),c(1,3)],ncol=2)
        data<-data.frame(words=data[,1],counts=as.numeric(data[,2]),stringsAsFactors = F)
        
        plotname_wc<-paste0("va_wc_",i)
        output[[plotname_wc]]<-renderWordcloud2(
          {
            wordcloud2(data = data,size = 0.4,minSize = 0.2,fontFamily = "Helvetica",color = "random-dark")
          }
        )
        tablename<-paste0("va_tb_",i)
        output[[tablename]]<-DT::renderDataTable({
          datatable(data = data.frame(words=data_raw[,1],frequency=round(as.numeric(data_raw[,2]),digits = 4),volatility=round(as.numeric(data_raw[,3]),digits = 4),stringsAsFactors = F))
        }
        )
      }
    })
  }
  
  
})