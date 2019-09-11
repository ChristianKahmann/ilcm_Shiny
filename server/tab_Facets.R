output$Fac_meta_UI<-renderUI({
  ava<-(values$metadata_available)
  empty_metadata<-names(which(apply(ava,MARGIN = 2,function(x){all(is.na(x))})))
  not_empty_metadata<-names(which(!apply(ava,MARGIN = 2,function(x){all(is.na(x))})))[-1]
  meta<-setdiff(c("mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9"),empty_metadata)
  names<-apply(ava[,meta],MARGIN = 2,FUN = function(x){paste(x,collapse="/")})
  names<-stringr::str_replace_all(string = names,pattern = "/NA","")
  names<-stringr::str_replace_all(string = names,pattern = "NA/","")
  values$facet_names<-names
  tagList(
    checkboxGroupButtons(inputId = "Fac_meta",label = NULL,choiceValues = c("Number of Token","Named Entities",meta),choiceNames = c("Number of Token","Named Entities",names),justified = T,status = "info"
                         ,individual = T,checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))),
    
    conditionalPanel(condition = 'input.Fac_meta.includes("Number of Token")',
                     column(2,
                            materialSwitch(inputId = "Fac_Token_rel",label = "relative?",value = F,status = "primary")
                     ),
                     column(2,
                            materialSwitch(inputId = "Fac_Token_log",label = "log?",value = F,status = "primary")
                     ),
                     tags$div(style='width:100%;',
                              column(12,
                                     plotlyOutput(outputId = "Fac_Token")%>% withSpinner(type = 6)
                              )
                     )
                     
    ),
    conditionalPanel(condition = 'input.Fac_meta.includes("Named Entities")',
                     column(6,
                            uiOutput(outputId = "Fac_Entities_abs")%>% withSpinner(type = 6)
                     ),
                     column(6,
                            uiOutput(outputId = "Fac_Entities_rel")%>% withSpinner(type = 6)
                     )
    ),
    conditionalPanel(condition = 'input.Fac_meta.includes("mde1")',
                     sliderInput(inputId = "Fac_mde1_minOcc",label = "min Occurrences",min = 1,max = 1000,value = 2,width = "20%"),
                     column(6,
                            uiOutput(outputId = "Fac_mde1_abs")%>% withSpinner(type = 6)
                     ),
                     column(6,
                            uiOutput(outputId = "Fac_mde1_rel")%>% withSpinner(type = 6)
                     )
    ),
    conditionalPanel(condition = 'input.Fac_meta.includes("mde2")',
                     sliderInput(inputId = "Fac_mde2_minOcc",label = "min Occurrences",min = 1,max = 1000,value = 2,width = "20%"),
                     column(6,
                            uiOutput(outputId = "Fac_mde2_abs")%>% withSpinner(type = 6)
                     ),
                     column(6,
                            uiOutput(outputId = "Fac_mde2_rel")%>% withSpinner(type = 6)
                     )
    ),
    conditionalPanel(condition = 'input.Fac_meta.includes("mde3")',
                     sliderInput(inputId = "Fac_mde3_minOcc",label = "min Occurrences",min = 1,max = 1000,value = 2,width = "20%"),
                     column(6,
                            uiOutput(outputId = "Fac_mde3_abs")%>% withSpinner(type = 6)
                     ),
                     column(6,
                            uiOutput(outputId = "Fac_mde3_rel")%>% withSpinner(type = 6)
                     )
    ),
    conditionalPanel(condition = 'input.Fac_meta.includes("mde4")',
                     sliderInput(inputId = "Fac_mde4_minOcc",label = "min Occurrences",min = 1,max = 1000,value = 2,width = "20%"),
                     column(6,
                            uiOutput(outputId = "Fac_mde4_abs")%>% withSpinner(type = 6)
                     ),
                     column(6,
                            uiOutput(outputId = "Fac_mde4_rel")%>% withSpinner(type = 6)
                     )
    ),
    conditionalPanel(condition = 'input.Fac_meta.includes("mde5")',
                     sliderInput(inputId = "Fac_mde5_minOcc",label = "min Occurrences",min = 1,max = 1000,value = 2,width = "20%"),
                     column(6,
                            uiOutput(outputId = "Fac_mde5_abs")%>% withSpinner(type = 6)
                     ),
                     column(6,
                            uiOutput(outputId = "Fac_mde5_rel")%>% withSpinner(type = 6)
                     )
    ),
    conditionalPanel(condition = 'input.Fac_meta.includes("mde6")',
                     sliderInput(inputId = "Fac_mde6_minOcc",label = "min Occurrences",min = 1,max = 1000,value = 2,width = "20%"),
                     column(6,
                            uiOutput(outputId = "Fac_mde6_abs")%>% withSpinner(type = 6)
                     ),
                     column(6,
                            uiOutput(outputId = "Fac_mde6_rel")%>% withSpinner(type = 6)
                     )
    ),
    conditionalPanel(condition = 'input.Fac_meta.includes("mde7")',
                     sliderInput(inputId = "Fac_mde7_minOcc",label = "min Occurrences",min = 1,max = 1000,value = 2,width = "20%"),
                     column(6,
                            uiOutput(outputId = "Fac_mde7_abs")%>% withSpinner(type = 6)
                     ),
                     column(6,
                            uiOutput(outputId = "Fac_mde7_rel")%>% withSpinner(type = 6)
                     )
    ),
    conditionalPanel(condition = 'input.Fac_meta.includes("mde8")',
                     sliderInput(inputId = "Fac_mde8_minOcc",label = "min Occurrences",min = 1,max = 1000,value = 2,width = "20%"),
                     column(6,
                            uiOutput(outputId = "Fac_mde8_abs")%>% withSpinner(type = 6)
                     ),
                     column(6,
                            uiOutput(outputId = "Fac_mde8_rel")%>% withSpinner(type = 6)
                     )
    ),
    conditionalPanel(condition = 'input.Fac_meta.includes("mde9")',
                     sliderInput(inputId = "Fac_mde9_minOcc",label = "min Occurrences",min = 1,max = 1000,value = 2,width = "20%"),
                     column(6,
                            uiOutput(outputId = "Fac_mde9_abs")%>% withSpinner(type = 6)
                     ),
                     column(6,
                            uiOutput(outputId = "Fac_mde9_rel")%>% withSpinner(type = 6)
                     )
    )
  )
})



###mde1 facets
output$Fac_mde1_abs<-renderUI({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),"no search to calculate facets for")
  )
  #get facets for field author_ss from solr
  data<-NULL
  values$Fac_mde1_abs<-NULL
  try({
    data<-matrix(unlist(facet_meta(q = values$q,base = values$url,fq=values$fq,facet.field = "mde1_ss",facet.limit = -1,facet.mincount = input$Fac_mde1_minOcc)[[2]]),ncol=2)
  },silent=T)
  #check if solr returned results
  validate(
    need(!is.null(data),"     no results for this configuration")
  )
  #sort authors by their count value
  rownames(data)<-data[,1]
  empty<-which(rownames(data)=="")
  if(length(empty)>0){
    data<-data[-empty,]
  }
  
  if(dim(data)[1]>1){
    data<-data[order(as.numeric(data[,2]),decreasing = T),]
  }
  values$Fac_mde1_abs<-data
  height<-round(max((dim(data)[1]/30)*100,25),digits = 0)
  plot_output_list <- lapply(1:1, function(i) {
    plotname <- paste0("plotly_mde1_abs")
    plot_output_object <- plotlyOutput(plotname)
    plot_output_object <- renderPlotly({
      p<-plot_ly(x=rev(as.numeric((values$Fac_mde1_abs)[,2])),y=rev(isolate(values$Fac_mde1_abs)[,1]),type = "bar",orientation="h",
                 marker = list(color = '#FF9999',line = list(color = '#FF9933', width = 1.5)),
                 textposition="auto",text=rev(as.numeric(isolate(values$Fac_mde1_abs)[,2])))
      p<-layout(p,title=paste(values$facet_names[1]," (absolute)"),yaxis=list(type="category",categoryorder="trace",automargin=T),margin=list(t=50),xaxis=list(side="top"))
      p<-plotly::config(p,displayModeBar=FALSE)
      return(p)
    })
  })
  # attr () needed  because of bug in plotlyOutput not giving height attribute
  attr(plot_output_list[[1]],'outputArgs')<-list(height=paste0(height,"vh"))
  return(plot_output_list)
})


output$Fac_mde1_rel<-renderUI({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),message=FALSE)
  )
  validate(
    need(!is.null(values$Fac_mde1_abs),message=FALSE)
  )
  #get data from solr facet using facet.query parameter based on the words used in absolute entities plot
  if(length(isolate(input$dataset))==0){
    fq<-"dataset_s:(*:*)"
  }
  if(length(isolate(input$dataset))==1){
    fq<-paste("dataset_s:",isolate(input$dataset),sep="")
  }
  if(length(isolate(input$dataset))>1){
    fq<-paste0("dataset_s:(",isolate(input$dataset)[1])
    for(i in 2:length(isolate(input$dataset))){
      fq<-paste0(fq," OR ",isolate(input$dataset)[i])
    }
    fq<-paste0(fq,")")
  } 
  data<-matrix(unlist(facet_meta(q = "*",base = values$url,fq=fq,facet.field = "mde1_ss",facet.limit = -1,facet.mincount = isolate(input$Fac_mde1_minOcc))[[2]]),ncol=2)
  #check if solr returned results
  validate(
    need(!is.null(data),"no results for this configuration")
  )
  rownames(data)<-data[,1]
  empty<-which(rownames(data)=="")
  if(length(empty)>0){
    data<-data[-empty,]
  }
  data<-matrix(data[rownames(values$Fac_mde1_abs),],ncol=2)
  #get relative counts by dividing search specific counts by counts for whole dataset
  data[,2]<-as.numeric((values$Fac_mde1_abs[,2]))/as.numeric(data[,2])
   values$Fac_mde1_rel<-data
  height<-round(max((dim(data)[1]/30)*100,25),digits = 0)
  plot_output_list <- lapply(1:1, function(i) {
    plotname <- paste0("plotly_mde1_rel")
    plot_output_object <- plotlyOutput(plotname)
    plot_output_object <- renderPlotly({
      p<-plot_ly(x=round(rev(as.numeric((values$Fac_mde1_rel)[,2])),digits = 2),y=rev(isolate(values$Fac_mde1_rel)[,1]),type = "bar",orientation="h"
                 ,marker = list(color = '#FF9999',line = list(color = '#FF9933', width = 1.5))
                 ,textposition="auto",text=round(rev(as.numeric(isolate(values$Fac_mde1_rel)[,2])),digits = 2))
      p<-layout(p,title=paste(values$facet_names[1]," (relative)"),yaxis=list(type="category",categoryorder="trace",automargin=T),margin=list(t=50),xaxis=list(side="top"))
      p<-plotly::config(p,displayModeBar=FALSE)
      return(p)
    })
  })
  # attr () needed  because of bug in plotlyOutput not giving height attribute
  attr(plot_output_list[[1]],'outputArgs')<-list(height=paste0(height,"vh"))
  return(plot_output_list)
})


###mde2 facets
output$Fac_mde2_abs<-renderUI({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),"no search to calculate facets for")
  )
  #get facets for field author_ss from solr
  data<-NULL
  values$Fac_mde2_abs<-NULL
  try({
    data<-matrix(unlist(facet_meta(q = values$q,base = values$url,fq=values$fq,facet.field = "mde2_ss",facet.limit = -1,facet.mincount = input$Fac_mde2_minOcc)[[2]]),ncol=2)
  },silent=T)
  #check if solr returned results
  validate(
    need(!is.null(data),"     no results for this configuration")
  )
  #sort authors by their count value
  rownames(data)<-data[,1]
  empty<-which(rownames(data)=="")
  if(length(empty)>0){
    data<-data[-empty,]
  }
  
  if(dim(data)[1]>1){
    data<-data[order(as.numeric(data[,2]),decreasing = T),]
  }
  values$Fac_mde2_abs<-data
  height<-round(max((dim(data)[1]/30)*100,25),digits = 0)
  plot_output_list <- lapply(1:1, function(i) {
    plotname <- paste0("plotly_mde2_abs")
    plot_output_object <- plotlyOutput(plotname)
    plot_output_object <- renderPlotly({
      p<-plot_ly(x=rev(as.numeric((values$Fac_mde2_abs)[,2])),y=rev(isolate(values$Fac_mde2_abs)[,1]),type = "bar",orientation="h",
                 marker = list(color = '#FF9999',line = list(color = '#FF9933', width = 1.5)),
                 textposition="auto",text=rev(as.numeric(isolate(values$Fac_mde2_abs)[,2])))
      p<-layout(p,title=paste(values$facet_names[2]," (absolute)"),yaxis=list(type="category",categoryorder="trace",automargin=T),margin=list(t=50),xaxis=list(side="top"))
      p<-plotly::config(p,displayModeBar=FALSE)
      return(p)
    })
  })
  # attr () needed  because of bug in plotlyOutput not giving height attribute
  attr(plot_output_list[[1]],'outputArgs')<-list(height=paste0(height,"vh"))
  return(plot_output_list)
})


output$Fac_mde2_rel<-renderUI({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),message=FALSE)
  )
  validate(
    need(!is.null(values$Fac_mde2_abs),message=FALSE)
  )
  #get data from solr facet using facet.query parameter based on the words used in absolute entities plot
  if(length(isolate(input$dataset))==0){
    fq<-"dataset_s:(*:*)"
  }
  if(length(isolate(input$dataset))==1){
    fq<-paste("dataset_s:",isolate(input$dataset),sep="")
  }
  if(length(isolate(input$dataset))>1){
    fq<-paste0("dataset_s:(",isolate(input$dataset)[1])
    for(i in 2:length(isolate(input$dataset))){
      fq<-paste0(fq," OR ",isolate(input$dataset)[i])
    }
    fq<-paste0(fq,")")
  } 
  data<-matrix(unlist(facet_meta(q = "*",base = values$url,fq=fq,facet.field = "mde2_ss",facet.limit = -1,facet.mincount = isolate(input$Fac_mde2_minOcc))[[2]]),ncol=2)
  #check if solr returned results
  validate(
    need(!is.null(data),"no results for this configuration")
  )
  rownames(data)<-data[,1]
  empty<-which(rownames(data)=="")
  if(length(empty)>0){
    data<-data[-empty,]
  }
  data<-matrix(data[rownames(values$Fac_mde2_abs),],ncol=2)
  #get relative counts by dividing search specific counts by counts for whole dataset
  data[,2]<-as.numeric((values$Fac_mde2_abs[,2]))/as.numeric(data[,2])
  values$Fac_mde2_rel<-data
  height<-round(max((dim(data)[1]/30)*100,25),digits = 0)
  plot_output_list <- lapply(1:1, function(i) {
    plotname <- paste0("plotly_mde2_rel")
    plot_output_object <- plotlyOutput(plotname)
    plot_output_object <- renderPlotly({
      p<-plot_ly(x=round(rev(as.numeric((values$Fac_mde2_rel)[,2])),digits = 2),y=rev(isolate(values$Fac_mde2_rel)[,1]),type = "bar",orientation="h"
                 ,marker = list(color = '#FF9999',line = list(color = '#FF9933', width = 1.5))
                 ,textposition="auto",text=round(rev(as.numeric(isolate(values$Fac_mde2_rel)[,2])),digits = 2))
      p<-layout(p,title=paste(values$facet_names[2]," (relative)"),yaxis=list(type="category",categoryorder="trace",automargin=T),margin=list(t=50),xaxis=list(side="top"))
      p<-plotly::config(p,displayModeBar=FALSE)
      return(p)
    })
  })
  # attr () needed  because of bug in plotlyOutput not giving height attribute
  attr(plot_output_list[[1]],'outputArgs')<-list(height=paste0(height,"vh"))
  return(plot_output_list)
})


###mde3 facets
output$Fac_mde3_abs<-renderUI({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),"no search to calculate facets for")
  )
  #get facets for field author_ss from solr
  data<-NULL
  values$Fac_mde3_abs<-NULL
  try({
    data<-matrix(unlist(facet_meta(q = values$q,base = values$url,fq=values$fq,facet.field = "mde3_ss",facet.limit = -1,facet.mincount = input$Fac_mde3_minOcc)[[2]]),ncol=2)
  },silent=T)
  #check if solr returned results
  validate(
    need(!is.null(data),"     no results for this configuration")
  )
  #sort authors by their count value
  rownames(data)<-data[,1]
  empty<-which(rownames(data)=="")
  if(length(empty)>0){
    data<-data[-empty,]
  }
  
  if(dim(data)[1]>1){
    data<-data[order(as.numeric(data[,2]),decreasing = T),]
  }
  too_long<-which(nchar(data[,1])>40)
  if(length(too_long)>0){
    data[too_long,1]<-paste(substr(data[too_long,1],1,40),"...",sep = "")
  }
  values$Fac_mde3_abs<-data
  height<-round(max((dim(data)[1]/30)*100,25),digits = 0)
  plot_output_list <- lapply(1:1, function(i) {
    plotname <- paste0("plotly_mde3_abs")
    plot_output_object <- plotlyOutput(plotname)
    plot_output_object <- renderPlotly({
      p<-plot_ly(x=rev(as.numeric((values$Fac_mde3_abs)[,2])),y=rev(isolate(values$Fac_mde3_abs)[,1]),type = "bar",orientation="h",
                 marker = list(color = '#FF9999',line = list(color = '#FF9933', width = 1.5)),
                 textposition="auto",text=rev(as.numeric(isolate(values$Fac_mde3_abs)[,2])))
      p<-layout(p,title=paste(values$facet_names[3]," (absolute)"),yaxis=list(type="category",categoryorder="trace",automargin=T),margin=list(t=50),xaxis=list(side="top"))
      p<-plotly::config(p,displayModeBar=FALSE)
      return(p)
    })
  })
  # attr () needed  because of bug in plotlyOutput not giving height attribute
  attr(plot_output_list[[1]],'outputArgs')<-list(height=paste0(height,"vh"))
  return(plot_output_list)
})


output$Fac_mde3_rel<-renderUI({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),message=FALSE)
  )
  validate(
    need(!is.null(values$Fac_mde3_abs),message=FALSE)
  )
  #get data from solr facet using facet.query parameter based on the words used in absolute entities plot
  if(length(isolate(input$dataset))==0){
    fq<-"dataset_s:(*:*)"
  }
  if(length(isolate(input$dataset))==1){
    fq<-paste("dataset_s:",isolate(input$dataset),sep="")
  }
  if(length(isolate(input$dataset))>1){
    fq<-paste0("dataset_s:(",isolate(input$dataset)[1])
    for(i in 2:length(isolate(input$dataset))){
      fq<-paste0(fq," OR ",isolate(input$dataset)[i])
    }
    fq<-paste0(fq,")")
  } 
  data<-matrix(unlist(facet_meta(q = "*",base = values$url,fq=fq,facet.field = "mde3_ss",facet.limit = -1,facet.mincount = isolate(input$Fac_mde3_minOcc))[[2]]),ncol=2)
  #check if solr returned results
  validate(
    need(!is.null(data),"no results for this configuration")
  )
  rownames(data)<-data[,1]
  empty<-which(rownames(data)=="")
  if(length(empty)>0){
    data<-data[-empty,]
  }
  data<-matrix(data[rownames(values$Fac_mde3_abs),],ncol=2)
  #get relative counts by dividing search specific counts by counts for whole dataset
  data[,2]<-as.numeric((values$Fac_mde3_abs[,2]))/as.numeric(data[,2])
  too_long<-which(nchar(data[,1])>40)
  if(length(too_long)>0){
    data[too_long,1]<-paste(substr(data[too_long,1],1,40),"...",sep = "")
  }
  values$Fac_mde3_rel<-data
  height<-round(max((dim(data)[1]/30)*100,25),digits = 0)
  plot_output_list <- lapply(1:1, function(i) {
    plotname <- paste0("plotly_mde3_rel")
    plot_output_object <- plotlyOutput(plotname)
    plot_output_object <- renderPlotly({
      p<-plot_ly(x=round(rev(as.numeric((values$Fac_mde3_rel)[,2])),digits = 2),y=rev(isolate(values$Fac_mde3_rel)[,1]),type = "bar",orientation="h"
                 ,marker = list(color = '#FF9999',line = list(color = '#FF9933', width = 1.5))
                 ,textposition="auto",text=round(rev(as.numeric(isolate(values$Fac_mde3_rel)[,2])),digits = 2))
      p<-layout(p,title=paste(values$facet_names[3]," (relative)"),yaxis=list(type="category",categoryorder="trace",automargin=T),margin=list(t=50),xaxis=list(side="top"))
      p<-plotly::config(p,displayModeBar=FALSE)
      return(p)
    })
  })
  # attr () needed  because of bug in plotlyOutput not giving height attribute
  attr(plot_output_list[[1]],'outputArgs')<-list(height=paste0(height,"vh"))
  return(plot_output_list)
})


###mde4 facets
output$Fac_mde4_abs<-renderUI({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),"no search to calculate facets for")
  )
  #get facets for field author_ss from solr
  data<-NULL
  values$Fac_mde4_abs<-NULL
  try({
    data<-matrix(unlist(facet_meta(q = values$q,base = values$url,fq=values$fq,facet.field = "mde4_ss",facet.limit = -1,facet.mincount = input$Fac_mde4_minOcc)[[2]]),ncol=2)
  },silent=T)
  #check if solr returned results
  validate(
    need(!is.null(data),"     no results for this configuration")
  )
  #sort authors by their count value
  rownames(data)<-data[,1]
  empty<-which(rownames(data)=="")
  if(length(empty)>0){
    data<-data[-empty,]
  }
  
  if(dim(data)[1]>1){
    data<-data[order(as.numeric(data[,2]),decreasing = T),]
  }
  too_long<-which(nchar(data[,1])>40)
  if(length(too_long)>0){
    data[too_long,1]<-paste(substr(data[too_long,1],1,40),"...",sep = "")
  }
  values$Fac_mde4_abs<-data
  height<-round(max((dim(data)[1]/30)*100,25),digits = 0)
  plot_output_list <- lapply(1:1, function(i) {
    plotname <- paste0("plotly_mde4_abs")
    plot_output_object <- plotlyOutput(plotname)
    plot_output_object <- renderPlotly({
      p<-plot_ly(x=rev(as.numeric((values$Fac_mde4_abs)[,2])),y=rev(isolate(values$Fac_mde4_abs)[,1]),type = "bar",orientation="h",
                 marker = list(color = '#FF9999',line = list(color = '#FF9933', width = 1.5)),
                 textposition="auto",text=rev(as.numeric(isolate(values$Fac_mde4_abs)[,2])))
      p<-layout(p,title=paste(values$facet_names[4]," (absolute)"),yaxis=list(type="category",categoryorder="trace",automargin=T),margin=list(t=50),xaxis=list(side="top"))
      p<-plotly::config(p,displayModeBar=FALSE)
      return(p)
    })
  })
  # attr () needed  because of bug in plotlyOutput not giving height attribute
  attr(plot_output_list[[1]],'outputArgs')<-list(height=paste0(height,"vh"))
  return(plot_output_list)
})


output$Fac_mde4_rel<-renderUI({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),message=FALSE)
  )
  validate(
    need(!is.null(values$Fac_mde4_abs),message=FALSE)
  )
  #get data from solr facet using facet.query parameter based on the words used in absolute entities plot
  if(length(isolate(input$dataset))==0){
    fq<-"dataset_s:(*:*)"
  }
  if(length(isolate(input$dataset))==1){
    fq<-paste("dataset_s:",isolate(input$dataset),sep="")
  }
  if(length(isolate(input$dataset))>1){
    fq<-paste0("dataset_s:(",isolate(input$dataset)[1])
    for(i in 2:length(isolate(input$dataset))){
      fq<-paste0(fq," OR ",isolate(input$dataset)[i])
    }
    fq<-paste0(fq,")")
  } 
  data<-matrix(unlist(facet_meta(q = "*",base = values$url,fq=fq,facet.field = "mde4_ss",facet.limit = -1,facet.mincount = isolate(input$Fac_mde4_minOcc))[[2]]),ncol=2)
  #check if solr returned results
  validate(
    need(!is.null(data),"no results for this configuration")
  )
  rownames(data)<-data[,1]
  empty<-which(rownames(data)=="")
  if(length(empty)>0){
    data<-data[-empty,]
  }
  data<-matrix(data[rownames(values$Fac_mde4_abs),],ncol=2)
  #get relative counts by dividing search specific counts by counts for whole dataset
  data[,2]<-as.numeric((values$Fac_mde4_abs[,2]))/as.numeric(data[,2])
  too_long<-which(nchar(data[,1])>40)
  if(length(too_long)>0){
    data[too_long,1]<-paste(substr(data[too_long,1],1,40),"...",sep = "")
  }
  values$Fac_mde4_rel<-data
  height<-round(max((dim(data)[1]/30)*100,25),digits = 0)
  plot_output_list <- lapply(1:1, function(i) {
    plotname <- paste0("plotly_mde4_rel")
    plot_output_object <- plotlyOutput(plotname)
    plot_output_object <- renderPlotly({
      p<-plot_ly(x=round(rev(as.numeric((values$Fac_mde4_rel)[,2])),digits = 2),y=rev(isolate(values$Fac_mde4_rel)[,1]),type = "bar",orientation="h"
                 ,marker = list(color = '#FF9999',line = list(color = '#FF9933', width = 1.5))
                 ,textposition="auto",text=round(rev(as.numeric(isolate(values$Fac_mde4_rel)[,2])),digits = 2))
      p<-layout(p,title=paste(values$facet_names[4]," (relative)"),yaxis=list(type="category",categoryorder="trace",automargin=T),margin=list(t=50),xaxis=list(side="top"))
      p<-plotly::config(p,displayModeBar=FALSE)
      return(p)
    })
  })
  # attr () needed  because of bug in plotlyOutput not giving height attribute
  attr(plot_output_list[[1]],'outputArgs')<-list(height=paste0(height,"vh"))
  return(plot_output_list)
})


###mde5 facets
output$Fac_mde5_abs<-renderUI({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),"no search to calculate facets for")
  )
  #get facets for field author_ss from solr
  data<-NULL
  values$Fac_mde5_abs<-NULL
  try({
    data<-matrix(unlist(facet_meta(q = values$q,base = values$url,fq=values$fq,facet.field = "mde5_ss",facet.limit = -1,facet.mincount = input$Fac_mde5_minOcc)[[2]]),ncol=2)
  },silent=T)
  #check if solr returned results
  validate(
    need(!is.null(data),"     no results for this configuration")
  )
  #sort authors by their count value
  rownames(data)<-data[,1]
  empty<-which(rownames(data)=="")
  if(length(empty)>0){
    data<-data[-empty,]
  }
  
  if(dim(data)[1]>1){
    data<-data[order(as.numeric(data[,2]),decreasing = T),]
  }
  too_long<-which(nchar(data[,1])>40)
  if(length(too_long)>0){
    data[too_long,1]<-paste(substr(data[too_long,1],1,40),"...",sep = "")
  }
  values$Fac_mde5_abs<-data
  height<-round(max((dim(data)[1]/30)*100,25),digits = 0)
  plot_output_list <- lapply(1:1, function(i) {
    plotname <- paste0("plotly_mde5_abs")
    plot_output_object <- plotlyOutput(plotname)
    plot_output_object <- renderPlotly({
      p<-plot_ly(x=rev(as.numeric((values$Fac_mde5_abs)[,2])),y=rev(isolate(values$Fac_mde5_abs)[,1]),type = "bar",orientation="h",
                 marker = list(color = '#FF9999',line = list(color = '#FF9933', width = 1.5)),
                 textposition="auto",text=rev(as.numeric(isolate(values$Fac_mde5_abs)[,2])))
      p<-layout(p,title=paste(values$facet_names[5]," (absolute)"),yaxis=list(type="category",categoryorder="trace",automargin=T),margin=list(t=50),xaxis=list(side="top"))
      p<-plotly::config(p,displayModeBar=FALSE)
      return(p)
    })
  })
  # attr () needed  because of bug in plotlyOutput not giving height attribute
  attr(plot_output_list[[1]],'outputArgs')<-list(height=paste0(height,"vh"))
  return(plot_output_list)
})


output$Fac_mde5_rel<-renderUI({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),message=FALSE)
  )
  validate(
    need(!is.null(values$Fac_mde5_abs),message=FALSE)
  )
  #get data from solr facet using facet.query parameter based on the words used in absolute entities plot
  if(length(isolate(input$dataset))==0){
    fq<-"dataset_s:(*:*)"
  }
  if(length(isolate(input$dataset))==1){
    fq<-paste("dataset_s:",isolate(input$dataset),sep="")
  }
  if(length(isolate(input$dataset))>1){
    fq<-paste0("dataset_s:(",isolate(input$dataset)[1])
    for(i in 2:length(isolate(input$dataset))){
      fq<-paste0(fq," OR ",isolate(input$dataset)[i])
    }
    fq<-paste0(fq,")")
  } 
  data<-matrix(unlist(facet_meta(q = "*",base = values$url,fq=fq,facet.field = "mde5_ss",facet.limit = -1,facet.mincount = isolate(input$Fac_mde5_minOcc))[[2]]),ncol=2)
  #check if solr returned results
  validate(
    need(!is.null(data),"no results for this configuration")
  )
  rownames(data)<-data[,1]
  empty<-which(rownames(data)=="")
  if(length(empty)>0){
    data<-data[-empty,]
  }
  data<-matrix(data[rownames(values$Fac_mde5_abs),],ncol=2)
  #get relative counts by dividing search specific counts by counts for whole dataset
  data[,2]<-as.numeric((values$Fac_mde5_abs[,2]))/as.numeric(data[,2])
  too_long<-which(nchar(data[,1])>40)
  if(length(too_long)>0){
    data[too_long,1]<-paste(substr(data[too_long,1],1,40),"...",sep = "")
  }
  values$Fac_mde5_rel<-data
  height<-round(max((dim(data)[1]/30)*100,25),digits = 0)
  plot_output_list <- lapply(1:1, function(i) {
    plotname <- paste0("plotly_mde5_rel")
    plot_output_object <- plotlyOutput(plotname)
    plot_output_object <- renderPlotly({
      p<-plot_ly(x=round(rev(as.numeric((values$Fac_mde5_rel)[,2])),digits = 2),y=rev(isolate(values$Fac_mde5_rel)[,1]),type = "bar",orientation="h"
                 ,marker = list(color = '#FF9999',line = list(color = '#FF9933', width = 1.5))
                 ,textposition="auto",text=round(rev(as.numeric(isolate(values$Fac_mde5_rel)[,2])),digits = 2))
      p<-layout(p,title=paste(values$facet_names[5]," (relative)"),yaxis=list(type="category",categoryorder="trace",automargin=T),margin=list(t=50),xaxis=list(side="top"))
      p<-plotly::config(p,displayModeBar=FALSE)
      return(p)
    })
  })
  # attr () needed  because of bug in plotlyOutput not giving height attribute
  attr(plot_output_list[[1]],'outputArgs')<-list(height=paste0(height,"vh"))
  return(plot_output_list)
})


###mde6 facets
output$Fac_mde6_abs<-renderUI({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),"no search to calculate facets for")
  )
  #get facets for field author_ss from solr
  data<-NULL
  values$Fac_mde6_abs<-NULL
  try({
    data<-matrix(unlist(facet_meta(q = values$q,base = values$url,fq=values$fq,facet.field = "mde6_ss",facet.limit = -1,facet.mincount = input$Fac_mde6_minOcc)[[2]]),ncol=2)
  },silent=T)
  #check if solr returned results
  validate(
    need(!is.null(data),"     no results for this configuration")
  )
  #sort authors by their count value
  rownames(data)<-data[,1]
  empty<-which(rownames(data)=="")
  if(length(empty)>0){
    data<-data[-empty,]
  }
  
  if(dim(data)[1]>1){
    data<-data[order(as.numeric(data[,2]),decreasing = T),]
  }
  too_long<-which(nchar(data[,1])>40)
  if(length(too_long)>0){
    data[too_long,1]<-paste(substr(data[too_long,1],1,40),"...",sep = "")
  }
  values$Fac_mde6_abs<-data
  height<-round(max((dim(data)[1]/30)*100,25),digits = 0)
  plot_output_list <- lapply(1:1, function(i) {
    plotname <- paste0("plotly_mde6_abs")
    plot_output_object <- plotlyOutput(plotname)
    plot_output_object <- renderPlotly({
      p<-plot_ly(x=rev(as.numeric((values$Fac_mde6_abs)[,2])),y=rev(isolate(values$Fac_mde6_abs)[,1]),type = "bar",orientation="h",
                 marker = list(color = '#FF9999',line = list(color = '#FF9933', width = 1.5)),
                 textposition="auto",text=rev(as.numeric(isolate(values$Fac_mde6_abs)[,2])))
      p<-layout(p,title=paste(values$facet_names[6]," (absolute)"),yaxis=list(type="category",categoryorder="trace",automargin=T),margin=list(t=50),xaxis=list(side="top"))
      p<-plotly::config(p,displayModeBar=FALSE)
      return(p)
    })
  })
  # attr () needed  because of bug in plotlyOutput not giving height attribute
  attr(plot_output_list[[1]],'outputArgs')<-list(height=paste0(height,"vh"))
  return(plot_output_list)
})


output$Fac_mde6_rel<-renderUI({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),message=FALSE)
  )
  validate(
    need(!is.null(values$Fac_mde6_abs),message=FALSE)
  )
  #get data from solr facet using facet.query parameter based on the words used in absolute entities plot
  if(length(isolate(input$dataset))==0){
    fq<-"dataset_s:(*:*)"
  }
  if(length(isolate(input$dataset))==1){
    fq<-paste("dataset_s:",isolate(input$dataset),sep="")
  }
  if(length(isolate(input$dataset))>1){
    fq<-paste0("dataset_s:(",isolate(input$dataset)[1])
    for(i in 2:length(isolate(input$dataset))){
      fq<-paste0(fq," OR ",isolate(input$dataset)[i])
    }
    fq<-paste0(fq,")")
  } 
  data<-matrix(unlist(facet_meta(q = "*",base = values$url,fq=fq,facet.field = "mde6_ss",facet.limit = -1,facet.mincount = isolate(input$Fac_mde6_minOcc))[[2]]),ncol=2)
  #check if solr returned results
  validate(
    need(!is.null(data),"no results for this configuration")
  )
  rownames(data)<-data[,1]
  empty<-which(rownames(data)=="")
  if(length(empty)>0){
    data<-data[-empty,]
  }
  data<-matrix(data[rownames(values$Fac_mde6_abs),],ncol=2)
  #get relative counts by dividing search specific counts by counts for whole dataset
  data[,2]<-as.numeric((values$Fac_mde6_abs[,2]))/as.numeric(data[,2])
  too_long<-which(nchar(data[,1])>40)
  if(length(too_long)>0){
    data[too_long,1]<-paste(substr(data[too_long,1],1,40),"...",sep = "")
  }
  values$Fac_mde6_rel<-data
  height<-round(max((dim(data)[1]/30)*100,25),digits = 0)
  plot_output_list <- lapply(1:1, function(i) {
    plotname <- paste0("plotly_mde6_rel")
    plot_output_object <- plotlyOutput(plotname)
    plot_output_object <- renderPlotly({
      p<-plot_ly(x=round(rev(as.numeric((values$Fac_mde6_rel)[,2])),digits = 2),y=rev(isolate(values$Fac_mde6_rel)[,1]),type = "bar",orientation="h"
                 ,marker = list(color = '#FF9999',line = list(color = '#FF9933', width = 1.5))
                 ,textposition="auto",text=round(rev(as.numeric(isolate(values$Fac_mde6_rel)[,2])),digits = 2))
      p<-layout(p,title=paste(values$facet_names[6]," (relative)"),yaxis=list(type="category",categoryorder="trace",automargin=T),margin=list(t=50),xaxis=list(side="top"))
      p<-plotly::config(p,displayModeBar=FALSE)
      return(p)
    })
  })
  # attr () needed  because of bug in plotlyOutput not giving height attribute
  attr(plot_output_list[[1]],'outputArgs')<-list(height=paste0(height,"vh"))
  return(plot_output_list)
})


###mde7 facets
output$Fac_mde7_abs<-renderUI({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),"no search to calculate facets for")
  )
  #get facets for field author_ss from solr
  data<-NULL
  values$Fac_mde7_abs<-NULL
  try({
    data<-matrix(unlist(facet_meta(q = values$q,base = values$url,fq=values$fq,facet.field = "mde7_ss",facet.limit = -1,facet.mincount = input$Fac_mde7_minOcc)[[2]]),ncol=2)
  },silent=T)
  #check if solr returned results
  validate(
    need(!is.null(data),"     no results for this configuration")
  )
  #sort authors by their count value
  rownames(data)<-data[,1]
  empty<-which(rownames(data)=="")
  if(length(empty)>0){
    data<-data[-empty,]
  }
  
  if(dim(data)[1]>1){
    data<-data[order(as.numeric(data[,2]),decreasing = T),]
  }
  too_long<-which(nchar(data[,1])>40)
  if(length(too_long)>0){
    data[too_long,1]<-paste(substr(data[too_long,1],1,40),"...",sep = "")
  }
  values$Fac_mde7_abs<-data
  height<-round(max((dim(data)[1]/30)*100,25),digits = 0)
  plot_output_list <- lapply(1:1, function(i) {
    plotname <- paste0("plotly_mde7_abs")
    plot_output_object <- plotlyOutput(plotname)
    plot_output_object <- renderPlotly({
      p<-plot_ly(x=rev(as.numeric((values$Fac_mde7_abs)[,2])),y=rev(isolate(values$Fac_mde7_abs)[,1]),type = "bar",orientation="h",
                 marker = list(color = '#FF9999',line = list(color = '#FF9933', width = 1.5)),
                 textposition="auto",text=rev(as.numeric(isolate(values$Fac_mde7_abs)[,2])))
      p<-layout(p,title=paste(values$facet_names[7]," (absolute)"),yaxis=list(type="category",categoryorder="trace",automargin=T),margin=list(t=50),xaxis=list(side="top"))
      p<-plotly::config(p,displayModeBar=FALSE)
      return(p)
    })
  })
  # attr () needed  because of bug in plotlyOutput not giving height attribute
  attr(plot_output_list[[1]],'outputArgs')<-list(height=paste0(height,"vh"))
  return(plot_output_list)
})


output$Fac_mde7_rel<-renderUI({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),message=FALSE)
  )
  validate(
    need(!is.null(values$Fac_mde7_abs),message=FALSE)
  )
  #get data from solr facet using facet.query parameter based on the words used in absolute entities plot
  if(length(isolate(input$dataset))==0){
    fq<-"dataset_s:(*:*)"
  }
  if(length(isolate(input$dataset))==1){
    fq<-paste("dataset_s:",isolate(input$dataset),sep="")
  }
  if(length(isolate(input$dataset))>1){
    fq<-paste0("dataset_s:(",isolate(input$dataset)[1])
    for(i in 2:length(isolate(input$dataset))){
      fq<-paste0(fq," OR ",isolate(input$dataset)[i])
    }
    fq<-paste0(fq,")")
  } 
  data<-matrix(unlist(facet_meta(q = "*",base = values$url,fq=fq,facet.field = "mde7_ss",facet.limit = -1,facet.mincount = isolate(input$Fac_mde7_minOcc))[[2]]),ncol=2)
  #check if solr returned results
  validate(
    need(!is.null(data),"no results for this configuration")
  )
  rownames(data)<-data[,1]
  empty<-which(rownames(data)=="")
  if(length(empty)>0){
    data<-data[-empty,]
  }
  data<-matrix(data[rownames(values$Fac_mde7_abs),],ncol=2)
  #get relative counts by dividing search specific counts by counts for whole dataset
  data[,2]<-as.numeric((values$Fac_mde7_abs[,2]))/as.numeric(data[,2])
  too_long<-which(nchar(data[,1])>40)
  if(length(too_long)>0){
    data[too_long,1]<-paste(substr(data[too_long,1],1,40),"...",sep = "")
  }
  values$Fac_mde7_rel<-data
  height<-round(max((dim(data)[1]/30)*100,25),digits = 0)
  plot_output_list <- lapply(1:1, function(i) {
    plotname <- paste0("plotly_mde7_rel")
    plot_output_object <- plotlyOutput(plotname)
    plot_output_object <- renderPlotly({
      p<-plot_ly(x=round(rev(as.numeric((values$Fac_mde7_rel)[,2])),digits = 2),y=rev(isolate(values$Fac_mde7_rel)[,1]),type = "bar",orientation="h"
                 ,marker = list(color = '#FF9999',line = list(color = '#FF9933', width = 1.5))
                 ,textposition="auto",text=round(rev(as.numeric(isolate(values$Fac_mde7_rel)[,2])),digits = 2))
      p<-layout(p,title=paste(values$facet_names[7]," (relative)"),yaxis=list(type="category",categoryorder="trace",automargin=T),margin=list(t=50),xaxis=list(side="top"))
      p<-plotly::config(p,displayModeBar=FALSE)
      return(p)
    })
  })
  # attr () needed  because of bug in plotlyOutput not giving height attribute
  attr(plot_output_list[[1]],'outputArgs')<-list(height=paste0(height,"vh"))
  return(plot_output_list)
})


###mde8 facets
output$Fac_mde8_abs<-renderUI({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),"no search to calculate facets for")
  )
  #get facets for field author_ss from solr
  data<-NULL
  values$Fac_mde8_abs<-NULL
  try({
    data<-matrix(unlist(facet_meta(q = values$q,base = values$url,fq=values$fq,facet.field = "mde8_ss",facet.limit = -1,facet.mincount = input$Fac_mde8_minOcc)[[2]]),ncol=2)
  },silent=T)
  #check if solr returned results
  validate(
    need(!is.null(data),"     no results for this configuration")
  )
  #sort authors by their count value
  rownames(data)<-data[,1]
  empty<-which(rownames(data)=="")
  if(length(empty)>0){
    data<-data[-empty,]
  }
  
  if(dim(data)[1]>1){
    data<-data[order(as.numeric(data[,2]),decreasing = T),]
  }
  too_long<-which(nchar(data[,1])>40)
  if(length(too_long)>0){
    data[too_long,1]<-paste(substr(data[too_long,1],1,40),"...",sep = "")
  }
  values$Fac_mde8_abs<-data
  height<-round(max((dim(data)[1]/30)*100,25),digits = 0)
  plot_output_list <- lapply(1:1, function(i) {
    plotname <- paste0("plotly_mde8_abs")
    plot_output_object <- plotlyOutput(plotname)
    plot_output_object <- renderPlotly({
      p<-plot_ly(x=rev(as.numeric((values$Fac_mde8_abs)[,2])),y=rev(isolate(values$Fac_mde8_abs)[,1]),type = "bar",orientation="h",
                 marker = list(color = '#FF9999',line = list(color = '#FF9933', width = 1.5)),
                 textposition="auto",text=rev(as.numeric(isolate(values$Fac_mde8_abs)[,2])))
      p<-layout(p,title=paste(values$facet_names[8]," (absolute)"),yaxis=list(type="category",categoryorder="trace",automargin=T),margin=list(t=50),xaxis=list(side="top"))
      p<-plotly::config(p,displayModeBar=FALSE)
      return(p)
    })
  })
  # attr () needed  because of bug in plotlyOutput not giving height attribute
  attr(plot_output_list[[1]],'outputArgs')<-list(height=paste0(height,"vh"))
  return(plot_output_list)
})


output$Fac_mde8_rel<-renderUI({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),message=FALSE)
  )
  validate(
    need(!is.null(values$Fac_mde8_abs),message=FALSE)
  )
  #get data from solr facet using facet.query parameter based on the words used in absolute entities plot
  if(length(isolate(input$dataset))==0){
    fq<-"dataset_s:(*:*)"
  }
  if(length(isolate(input$dataset))==1){
    fq<-paste("dataset_s:",isolate(input$dataset),sep="")
  }
  if(length(isolate(input$dataset))>1){
    fq<-paste0("dataset_s:(",isolate(input$dataset)[1])
    for(i in 2:length(isolate(input$dataset))){
      fq<-paste0(fq," OR ",isolate(input$dataset)[i])
    }
    fq<-paste0(fq,")")
  } 
  data<-matrix(unlist(facet_meta(q = "*",base = values$url,fq=fq,facet.field = "mde8_ss",facet.limit = -1,facet.mincount = isolate(input$Fac_mde8_minOcc))[[2]]),ncol=2)
  #check if solr returned results
  validate(
    need(!is.null(data),"no results for this configuration")
  )
  rownames(data)<-data[,1]
  empty<-which(rownames(data)=="")
  if(length(empty)>0){
    data<-data[-empty,]
  }
  data<-matrix(data[rownames(values$Fac_mde8_abs),],ncol=2)
  #get relative counts by dividing search specific counts by counts for whole dataset
  data[,2]<-as.numeric((values$Fac_mde8_abs[,2]))/as.numeric(data[,2])
  too_long<-which(nchar(data[,1])>40)
  if(length(too_long)>0){
    data[too_long,1]<-paste(substr(data[too_long,1],1,40),"...",sep = "")
  }
  values$Fac_mde8_rel<-data
  height<-round(max((dim(data)[1]/30)*100,25),digits = 0)
  plot_output_list <- lapply(1:1, function(i) {
    plotname <- paste0("plotly_mde8_rel")
    plot_output_object <- plotlyOutput(plotname)
    plot_output_object <- renderPlotly({
      p<-plot_ly(x=round(rev(as.numeric((values$Fac_mde8_rel)[,2])),digits = 2),y=rev(isolate(values$Fac_mde8_rel)[,1]),type = "bar",orientation="h"
                 ,marker = list(color = '#FF9999',line = list(color = '#FF9933', width = 1.5))
                 ,textposition="auto",text=round(rev(as.numeric(isolate(values$Fac_mde8_rel)[,2])),digits = 2))
      p<-layout(p,title=paste(values$facet_names[8]," (relative)"),yaxis=list(type="category",categoryorder="trace",automargin=T),margin=list(t=50),xaxis=list(side="top"))
      p<-plotly::config(p,displayModeBar=FALSE)
      return(p)
    })
  })
  # attr () needed  because of bug in plotlyOutput not giving height attribute
  attr(plot_output_list[[1]],'outputArgs')<-list(height=paste0(height,"vh"))
  return(plot_output_list)
})


###mde9 facets
output$Fac_mde9_abs<-renderUI({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),"no search to calculate facets for")
  )
  #get facets for field author_ss from solr
  data<-NULL
  values$Fac_mde9_abs<-NULL
  try({
    data<-matrix(unlist(facet_meta(q = values$q,base = values$url,fq=values$fq,facet.field = "mde9_ss",facet.limit = -1,facet.mincount = input$Fac_mde9_minOcc)[[2]]),ncol=2)
  },silent=T)
  #check if solr returned results
  validate(
    need(!is.null(data),"     no results for this configuration")
  )
  #sort authors by their count value
  rownames(data)<-data[,1]
  empty<-which(rownames(data)=="")
  if(length(empty)>0){
    data<-data[-empty,]
  }
  
  if(dim(data)[1]>1){
    data<-data[order(as.numeric(data[,2]),decreasing = T),]
  }
  too_long<-which(nchar(data[,1])>40)
  if(length(too_long)>0){
    data[too_long,1]<-paste(substr(data[too_long,1],1,40),"...",sep = "")
  }
  values$Fac_mde9_abs<-data
  height<-round(max((dim(data)[1]/30)*100,25),digits = 0)
  plot_output_list <- lapply(1:1, function(i) {
    plotname <- paste0("plotly_mde9_abs")
    plot_output_object <- plotlyOutput(plotname)
    plot_output_object <- renderPlotly({
      p<-plot_ly(x=rev(as.numeric((values$Fac_mde9_abs)[,2])),y=rev(isolate(values$Fac_mde9_abs)[,1]),type = "bar",orientation="h",
                 marker = list(color = '#FF9999',line = list(color = '#FF9933', width = 1.5)),
                 textposition="auto",text=rev(as.numeric(isolate(values$Fac_mde9_abs)[,2])))
      p<-layout(p,title=paste(values$facet_names[9]," (absolute)"),yaxis=list(type="category",categoryorder="trace",automargin=T),margin=list(t=50),xaxis=list(side="top"))
      p<-plotly::config(p,displayModeBar=FALSE)
      return(p)
    })
  })
  # attr () needed  because of bug in plotlyOutput not giving height attribute
  attr(plot_output_list[[1]],'outputArgs')<-list(height=paste0(height,"vh"))
  return(plot_output_list)
})


output$Fac_mde9_rel<-renderUI({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),message=FALSE)
  )
  validate(
    need(!is.null(values$Fac_mde9_abs),message=FALSE)
  )
  #get data from solr facet using facet.query parameter based on the words used in absolute entities plot
  if(length(isolate(input$dataset))==0){
    fq<-"dataset_s:(*:*)"
  }
  if(length(isolate(input$dataset))==1){
    fq<-paste("dataset_s:",isolate(input$dataset),sep="")
  }
  if(length(isolate(input$dataset))>1){
    fq<-paste0("dataset_s:(",isolate(input$dataset)[1])
    for(i in 2:length(isolate(input$dataset))){
      fq<-paste0(fq," OR ",isolate(input$dataset)[i])
    }
    fq<-paste0(fq,")")
  } 
  data<-matrix(unlist(facet_meta(q = "*",base = values$url,fq=fq,facet.field = "mde9_ss",facet.limit = -1,facet.mincount = isolate(input$Fac_mde9_minOcc))[[2]]),ncol=2)
  #check if solr returned results
  validate(
    need(!is.null(data),"no results for this configuration")
  )
  rownames(data)<-data[,1]
  empty<-which(rownames(data)=="")
  if(length(empty)>0){
    data<-data[-empty,]
  }
  data<-matrix(data[rownames(values$Fac_mde9_abs),],ncol=2)
  #get relative counts by dividing search specific counts by counts for whole dataset
  data[,2]<-as.numeric((values$Fac_mde9_abs[,2]))/as.numeric(data[,2])
  too_long<-which(nchar(data[,1])>40)
  if(length(too_long)>0){
    data[too_long,1]<-paste(substr(data[too_long,1],1,40),"...",sep = "")
  }
  values$Fac_mde9_rel<-data
  height<-round(max((dim(data)[1]/30)*100,25),digits = 0)
  plot_output_list <- lapply(1:1, function(i) {
    plotname <- paste0("plotly_mde9_rel")
    plot_output_object <- plotlyOutput(plotname)
    plot_output_object <- renderPlotly({
      p<-plot_ly(x=round(rev(as.numeric((values$Fac_mde9_rel)[,2])),digits = 2),y=rev(isolate(values$Fac_mde9_rel)[,1]),type = "bar",orientation="h"
                 ,marker = list(color = '#FF9999',line = list(color = '#FF9933', width = 1.5))
                 ,textposition="auto",text=round(rev(as.numeric(isolate(values$Fac_mde9_rel)[,2])),digits = 2))
      p<-layout(p,title=paste(values$facet_names[9]," (relative)"),yaxis=list(type="category",categoryorder="trace",automargin=T),margin=list(t=50),xaxis=list(side="top"))
      p<-plotly::config(p,displayModeBar=FALSE)
      return(p)
    })
  })
  # attr () needed  because of bug in plotlyOutput not giving height attribute
  attr(plot_output_list[[1]],'outputArgs')<-list(height=paste0(height,"vh"))
  return(plot_output_list)
})





#render Facet View for Entieties absolute
output$Fac_Entities_abs<-renderUI({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),"no search to calculate facets for")
  )
  #get data from solr facet to collect words
  data<-matrix(unlist(facet_meta(q = values$q,base = values$url,fq=values$fq,facet.field = "entities_txt_en_noStemming",facet.limit = 250)[[2]]),ncol=2)
  #delete numbers, stopwords and one-char words
  toDelete<-which(str_detect(string = data[,1],pattern = "[0-9]+"))
  toDelete<-union(toDelete,which(data[,1]%in%tm::stopwords("en")))
  toDelete<-union(toDelete,which(nchar(data[,1])<2))
  if(length(toDelete)>0){
    data<-data[-toDelete,]
  }
  #keep max 75 words
  if(dim(data)[1]>75){
    data<-data[1:75,]
  }
  #get counts based on facet.query for chosen terms
  data<-matrix(unlist(facet_meta(q = values$q,base = values$url,fq=values$fq,facet.field = "entities_txt_en_noStemming",facet.limit = 75,facet.query = data[,1])[[1]]),ncol=2)
  if(dim(data)[1]>1){
    data<-data[order(as.numeric(data[,2]),decreasing = T),]
  }
  values$Fac_Ent_abs<-data
  height<-round(max((dim(data)[1]/30)*100,25),digits = 0)
  plot_output_list <- lapply(1:1, function(i) {
    plotname <- paste0("plotly_entities_abs")
    plot_output_object <- plotlyOutput(plotname)
    plot_output_object <- renderPlotly({
      p<-plot_ly(x=rev(as.numeric(isolate(values$Fac_Ent_abs)[,2])),y=rev(isolate(values$Fac_Ent_abs)[,1]),type = "bar",orientation="h",
                 marker = list(color = '#FF9999',line = list(color = '#FF9933', width = 1.5)),
                 textposition="middle right",text=rev(as.numeric(isolate(values$Fac_Ent_abs)[,2])))
      p<-layout(p,title="Entities (absolute)",yaxis=list(type="category",categoryorder="trace",automargin=T),margin=list(t=50),xaxis=list(side="top"))
      p<-plotly::config(p,displayModeBar=FALSE)
      return(p)
    })
  })
  # attr () needed  because of bug in plotlyOutput not giving height attribute
  attr(plot_output_list[[1]],'outputArgs')<-list(height=paste0(height,"vh"))
  return(plot_output_list)
})

#render Facet View for Entieties relative
output$Fac_Entities_rel<-renderUI({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),"no search to calculate facets for")
  )
  #get data from solr facet using facet.query parameter based on the words used in absolute entities plot
  if(length(isolate(input$dataset))==0){
    fq<-"dataset_s:(*:*)"
  }
  if(length(isolate(input$dataset))==1){
    fq<-paste("dataset_s:",isolate(input$dataset),sep="")
  }
  if(length(isolate(input$dataset))>1){
    fq<-paste0("dataset_s:(",isolate(input$dataset)[1])
    for(i in 2:length(isolate(input$dataset))){
      fq<-paste0(fq," OR ",isolate(input$dataset)[i])
    }
    fq<-paste0(fq,")")
  } 
  data<-matrix(unlist(facet_meta(q = "*:*",base = values$url,fq=fq,facet.field = "entities_txt_en_noStemming",facet.limit = 75,facet.query = isolate(values$Fac_Ent_abs[,1]))[[1]]),ncol=2)
  #get relative counts by dividing search specific counts by counts for whole dataset
  data[,2]<-as.numeric((values$Fac_Ent_abs[,2]))/as.numeric(data[,2])
  values$Fac_Ent_rel<-data
  height<-round(max((dim(data)[1]/30)*100,25),digits = 0)
  plot_output_list <- lapply(1:1, function(i) {
    plotname <- paste0("plotly_entities_rel")
    plot_output_object <- plotlyOutput(plotname)
    plot_output_object <- renderPlotly({
      p<-plot_ly(x=round(rev(as.numeric(isolate(values$Fac_Ent_rel)[,2])),digits = 2),y=rev(isolate(values$Fac_Ent_rel)[,1]),type = "bar",orientation="h"
                 ,marker = list(color = '#FF9999',line = list(color = '#FF9933', width = 1.5))
                 ,textposition="auto",text=round(rev(as.numeric(isolate(values$Fac_Ent_rel)[,2])),digits = 2))
      p<-layout(p,title="Entities (relative)",yaxis=list(type="category",categoryorder="trace",automargin=T),margin=list(t=50),xaxis=list(side="top"))
      p<-plotly::config(p,displayModeBar=FALSE)
      return(p)
    })
  })
  # attr () needed  because of bug in plotlyOutput not giving height attribute
  attr(plot_output_list[[1]],'outputArgs')<-list(height=paste0(height,"vh"))
  return(plot_output_list)
})


output$Fac_Token<-renderPlotly({
  #check wheather search parameters were specified
  validate(
    need(!is.null(values$url),"no search to calculate facets for")
  )
  if(length(isolate(input$dataset))==0){
    fq<-"dataset_s:(*:*)"
  }
  if(length(isolate(input$dataset))==1){
    fq<-paste("dataset_s:",isolate(input$dataset),sep="")
  }
  if(length(isolate(input$dataset))>1){
    fq<-paste0("dataset_s:(",isolate(input$dataset)[1])
    for(i in 2:length(isolate(input$dataset))){
      fq<-paste0(fq," OR ",isolate(input$dataset)[i])
    }
    fq<-paste0(fq,")")
  } 
  data_all<-matrix(as.numeric(unlist(facet_meta(q = "*",base = values$url,fq=fq,facet.field = "token_i",facet.limit = -1)[[2]])),ncol=2)
  data_search<-matrix(as.numeric(unlist(facet_meta(q = values$q,base = values$url,fq=values$fq,facet.field = "token_i",facet.limit = -1)[[2]])),ncol=2)
  to_Delete<-intersect(which(data_all[,2]==0),which(data_search[,2]==0))
  if(length(to_Delete)>0){
    data_all<-data_all[-to_Delete,]
    data_search<-data_search[-to_Delete,]
  }
  breaks<-seq(min(data_all[,1]),max(data_all[,1]),l=min(50,dim(data_all)[1]))
  data_all<-make_ready_for_hist(counts = data_all,breaks)
  data_search<-make_ready_for_hist(counts = data_search,breaks)
  
  if(input$Fac_Token_rel==TRUE){
    data_all[2,]<-data_all[2,]/max(data_all[2,])
    data_search[2,]<-data_search[2,]/max(data_search[2,])
  }
  p<-plot_ly(x=ceiling(data_all[1,]),y=data_all[2,],type="bar",name="all")
  p<-add_trace(p,y=data_search[2,],name="search")
  if(input$Fac_Token_log==TRUE){
    p<-layout(p,yaxis=list(type="log"),title="distribution of document lengths") 
  }
  return(p)
})


