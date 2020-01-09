source("global/functions_used_in_scripts.R")


#link downloadbutton for theta in Topic Models Tab
output$download_theta<-downloadHandler(
  filename = function() {
    paste('Theta-', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-as.matrix(values$tm_theta)
    write.csv(data, con)
  }
)  


#link downloadbutton for phi in Topic Models Tab
output$download_phi<-downloadHandler(
  filename = function() {
    paste('Phi-', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-as.matrix(values$tm_phi)
    write.csv(data, con)
  }
)  





#render LDA Viz plot based on the calculated topic models
output$TM_LDAvis <- LDAvis::renderVis({
  #svd_tsne <- function(x) tsne::tsne(svd(x)$u)
  validate(
    need(!is.null(input$coll),message=F)
  )
  if(input$coll=="Results"){
    return(NULL)
  }
  else{
    if(!is.null(input$nTerms)){
      #vocab<-stringr::str_replace_all(string = values$tm_vocab,pattern = "\\\\",replacement="")
      #tm<-LDAvis::createJSON(values$tm_phi, values$tm_theta, values$tm_doc.length, values$tm_vocab, values$tm_term.frequency, 
      #                       R = input$nTerms,reorder.topics = F)#mds.method = svd_tsne )
      
      return(values$tm_json)
    }
  }
})

#render wordclouds and sparklines in topic modeling parameters tab
observeEvent(values$tm_phi,{
  relevance<-calculate_topic_relevance(lambda=0.3,phi=values$tm_phi,theta=values$tm_theta,doc.length=values$tm_doc.length)
  values$tm_relevance<-relevance
  for( i in 1:dim((values$tm_phi))[1]){
    local({
      data<-sort(relevance[,i],decreasing = T)[1:20]  
      data<-data.frame(cbind(names(data),data),stringsAsFactors = FALSE)
      class(data$data)<-"numeric"
      #normalize weights for wordcloud
      data$data<-data$data-min(data$data)
      data$data<-data$data/max(data$data)
      
      
      dates<-isolate(values$tm_dates)[which(apply(X = isolate(values$tm_theta),MARGIN = 1,FUN = function(j){return(which.max(j))})==i),]
      missing_dates<-seq.Date(from = min(as.Date(isolate(values$tm_dates)[,1])),to = max(as.Date(isolate(values$tm_dates)[,1])),by = "day")
      toDelete<-which(as.character(missing_dates)%in%dates)
      if(length(toDelete)>0){
        missing_dates<-missing_dates[-toDelete] 
      }
      missing_dates<-as.matrix(table(missing_dates))
      missing_dates[,1]<-0
      missing_dates<-cbind(rownames(missing_dates),missing_dates[,1])
      dates<-as.matrix(table(dates))
      dates<-cbind(rownames(dates),dates[,1])
      dates<-rbind(dates,missing_dates)
      #order data by date
      dates<-dates[order(dates[,1]),]
      dates<-matrix(dates,ncol = 2)
      plotname_wc=paste0("tm_wc_",i)
      plotname_sp=paste0("tm_sp_",i)
      output[[plotname_wc]]<-renderWordcloud2({
        wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1)
      }
      )
      output[[plotname_sp]]<-renderSparkline({
        sparkline(values = dates[,2])
      }
      )
    })
  }
})

#observe add button ins topic modeling paramters tab and when clicked add topic to timeline data
observeEvent(values$tm_random,{
  values$observers<-lapply(
    X=1:isolate(values$tm_number_of_topics),
    FUN=function(i){
      observeEvent(input[[paste0("tm_ac_",values$tm_random,"_",i)]],{
        if(input[[paste0("tm_ac_",values$tm_random,"_",i)]]>0){
          if(i%in%isolate(values$tm_timeline_ids)){
            shinyWidgets::sendSweetAlert(session = session,title = "Topic already added",type = "warning")
          }
          isolate(values$tm_timeline_ids<-unique(c(values$tm_timeline_ids,i)))
        }
      })
    }
  )
}
)

#render topic model timelineplot based on added topics 
output$TM_Timeline<-renderPlotly({
  validate(need(!is.null(values$tm_timeline_ids), "Add topic by a click at it's add button"))
  timeline_data<-NULL
  count=0
  for(i in values$tm_timeline_ids){
    count<-count+1
    if(input$TM_Timeline_Measure=="Document Probability"){
      dates<-cbind(isolate(values$tm_dates),values$tm_theta[,i])
      colnames(dates)<-c("date","probability")
      missing_dates<-seq.Date(from = min(as.Date(isolate(values$tm_dates)[,1])),to = max(as.Date(isolate(values$tm_dates)[,1])),by = "day")
      
      missing_dates<-data.frame(as.matrix(cbind(names(table(missing_dates)),table(missing_dates)),ncol=2))
      missing_dates[,2]<-0
      colnames(missing_dates)<-c("date","probability")
      
      if(input$TM_Timeline_Range=="Month"){
        dates[,1]<-substr(dates[,1],1,7)
        missing_dates[,1]<-substr(missing_dates[,1],1,7)
      }
      if(input$TM_Timeline_Range=="Year"){
        dates[,1]<-substr(dates[,1],1,4)
        missing_dates[,1]<-substr(missing_dates[,1],1,4)
      }
      toDelete<-which(as.character(missing_dates[,1])%in%dates[,1])
      if(length(toDelete)>0){
        missing_dates<-missing_dates[-toDelete,] 
      }
      dates<-rbind(dates,missing_dates)
      dates<-aggregate(dates[,2],by=list(dates[,1]),FUN=sum)
      timeline_data<-cbind(timeline_data,dates[,2])
      rownames(timeline_data)<-dates[,1]
      timeline_data<-matrix(timeline_data[order(rownames(timeline_data)),],nrow=length(dates[,1]))
    }
    else{
      if(input$TM_Timeline_Rank1==T){
        dates<-as.character(isolate(values$tm_dates)[which(apply(X = isolate(values$tm_theta),MARGIN = 1,FUN = function(j){return(which.max(j))})==i),])
      }
      else{
        ids<-unlist(lapply(X = apply(X = isolate(values$tm_theta),MARGIN = 1,FUN = function(j){return(which(j>input$TM_Timeline_Prob))}),FUN = function(x){
          return(any(is.element(x,i)))
        }))
        if(length(ids)>0){
          dates<-isolate(values$tm_dates)[which(ids),]
        }
        else{
          dates<-NULL
        }
      }
      missing_dates<-seq.Date(from = min(as.Date(isolate(values$tm_dates)[,1])),to = max(as.Date(isolate(values$tm_dates)[,1])),by = "day")
      if(input$TM_Timeline_Range=="Month"){
        dates<-substr(dates,1,7)
        missing_dates<-substr(missing_dates,1,7)
      }
      if(input$TM_Timeline_Range=="Year"){
        dates<-substr(dates,1,4)
        missing_dates<-substr(missing_dates,1,4)
      }
      
      toDelete<-which(as.character(missing_dates)%in%dates)
      if(length(toDelete)>0){
        missing_dates<-missing_dates[-toDelete] 
      }
      missing_dates<-as.matrix(table(missing_dates))
      missing_dates[,1]<-0
      missing_dates<-cbind(rownames(missing_dates),missing_dates[,1])
      dates<-as.matrix(table(dates))
      dates<-cbind(rownames(dates),dates[,1])
      dates<-rbind(dates,missing_dates)
      #order data by date
      dates<-dates[order(dates[,1]),]
      dates<-matrix(dates,ncol = 2)
      timeline_data<-cbind(timeline_data,dates[,2])
      rownames(timeline_data)<-dates[,1]
      if(input$TM_Timeline_Measure=="relative Document Count"){
        glob_dates<-as.matrix(values$tm_dates)
        if(input$TM_Timeline_Range=="Month"){
          glob_dates<-substr(glob_dates,1,7)
        }
        if(input$TM_Timeline_Range=="Year"){
          glob_dates<-substr(glob_dates,1,4)
        }
        glob_dates<-matrix(cbind(names(table(glob_dates)),table(glob_dates)),ncol=2)
        timeline_data[glob_dates[,1],count]<-as.numeric(timeline_data[glob_dates[,1],count])/as.numeric(glob_dates[,2])
      }
    }
  }
  p<-plot_ly(x=rownames(timeline_data),y=timeline_data[,1],type = "scatter",mode="lines+markers",name=paste0("Topic:",values$tm_timeline_ids[1]," ",paste(names(sort(isolate(values$tm_phi)[values$tm_timeline_ids[1],],decreasing = T))[1:5],collapse = " ")))
  if(length(values$tm_timeline_ids)>1){
    for(i in 2:length(values$tm_timeline_ids)){
      p<-add_trace(p,y=timeline_data[,i],name=paste0("Topic:",values$tm_timeline_ids[i]," ",paste(names(sort(isolate(values$tm_phi)[values$tm_timeline_ids[i],],decreasing = T))[1:5],collapse = " ")))
    }
  }
  if(input$TM_Timeline_Measure=="relative Document Count"){
    p<-layout(p,paper_bgcolor='rgb(255,255,255)',showlegend=TRUE,legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1), plot_bgcolor='rgb(229,229,229)',xaxis=list(autotick=T,showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = FALSE,side="bottom")
              ,margin=list(b=80),yaxis=list(rangemode = "tozero",title=" relative Document Count",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T))
  }
  if(input$TM_Timeline_Measure=="Document Probability"){
    p<-layout(p,paper_bgcolor='rgb(255,255,255)',showlegend=TRUE,legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1), plot_bgcolor='rgb(229,229,229)',xaxis=list(autotick=T,showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = FALSE,side="bottom")
              ,margin=list(b=80),yaxis=list(rangemode = "tozero",title="Document Probability",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T))
  }
  if(input$TM_Timeline_Measure=="Document Count"){
    p<-layout(p,paper_bgcolor='rgb(255,255,255)',showlegend=TRUE,legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1), plot_bgcolor='rgb(229,229,229)',xaxis=list(autotick=T,showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = FALSE,side="bottom")
              ,margin=list(b=80),yaxis=list(rangemode = "tozero",title="Document Count",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T))
  }
  
  return(p)
})

#render datatable with topics displayed by reflecting words, click rows to select for subcollections
output$TM_Subcollection_Table<-DT::renderDataTable({
  if(length(values$tm_timeline_ids)>=1){
    words<-list()
    count=0
    for(i in values$tm_timeline_ids){
      count=count+1
      words[[count]]<-paste0("<b style='color:",plotly_colors[count],";'>",paste0(names(sort(values$tm_relevance[,i],decreasing=T))[1:15],collapse = " "),"</b>")
    }
    remove_existing_material(1:dim(values$tm_phi)[1])
    words<-do.call(rbind,words)
    words<-cbind(paste("<h4>Topic: ",values$tm_timeline_ids,"</h4>",sep = ""),words,
                 keep=shinyInput_material(shinyWidgets::materialSwitch,dim(words)[1],"tmsubcoll_",values=c(isolate(values$tm_sub_selected),FALSE),label=NULL,status="info"))
    colnames(words)<-c("Topic","most coherent words","Use topic for sub-collection?")
    values$tm_random2<-runif(1,0,1)
    return(datatable(data = words,rownames = F,options = list(dom="pt",ordering=F,preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                                              drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')),escape = F,selection = "none"))
  }
  else{
    return()
  }
},server=F)


output$TM_subColl_UI<-renderUI({
  validate(
    need(!is.null(values$tm_sub_selected),message=F)
  )
  if(length(which(values$tm_sub_selected==TRUE))>0){
    return(tagList(
      tags$br(),
      fluidRow(
        column(5,offset = 1,
               textInput(inputId = "TM_Subcollection_Name",label = "Name for Subcollection",value = paste0(values$tm_info[[5]],":"),width = "100%"),
               actionButton(inputId = "TM_Subcollection_save",label = "Save Subcollection",styleclass = "info")
        )
      )
    )
    )
  }
  else{
    return()
  }
})

##############
# STM
##############

output$tm_method<-reactive({
  values$tm_method
})

output$tm_stm_parameters_contentFormula <- reactive({
  values$tm_stm_parameters_contentFormula
})

outputOptions(output, "tm_stm_parameters_contentFormula", suspendWhenHidden = FALSE)

output$tm_stm_parameters_contentFormulaIsSet <- reactive({
  if(nchar(values$tm_stm_parameters_contentFormula)>0){
    return(TRUE)
  }else{
    return(FALSE)
  }
})
outputOptions(output, "tm_stm_parameters_contentFormulaIsSet", suspendWhenHidden = FALSE)


#stm
# plot.STM summary
output$TM_stm_visu_summary <- renderPlot({
  if(nchar(values$tm_stm_parameters_contentFormula)>0){# if content formula was set in stm model, the label type is not selectable
    plot.STM(x = values$tm_stm_model, type = "summary", n = input$tm_stm_visu_numberOfWordsToLabelTopic)
  }else{
    plot.STM(x = values$tm_stm_model, type = "summary", n = input$tm_stm_visu_numberOfWordsToLabelTopic, labeltype = input$tm_stm_visu_labeltype, frexw = input$tm_stm_visu_frexweight)
    
  }
})
# plot.STM labels
output$TM_stm_visu_labels <- renderPlot({
  if(nchar(values$tm_stm_parameters_contentFormula)>0){# if content formula was set in stm model, the label type is not selectable
    plot.STM(x = values$tm_stm_model, type = "labels", n = input$tm_stm_visu_numberOfWordsToLabelTopic)
  }else{
    plot.STM(x = values$tm_stm_model, type = "labels", n = input$tm_stm_visu_numberOfWordsToLabelTopic, labeltype = input$tm_stm_visu_labeltype, frexw = input$tm_stm_visu_frexweight)
  }
})

# plot.STM perspectives
output$TM_stm_visu_perspectives <- renderPlot({
  validate(
    need(!is.null(input$tm_stm_visu_perspectives_topic1),message="please select topic 1")
  )
  validate(
    need(!is.null(input$tm_stm_visu_perspectives_topic2),message="please select topic 2")
  )
  selectedTopic1 <- as.integer(input$tm_stm_visu_perspectives_topic1)
  selectedTopic2 <- as.integer(input$tm_stm_visu_perspectives_topic2)

  # contentFormula in stm model set
  if(nchar(values$tm_stm_parameters_contentFormula)>0){
    # use additional parameters tm_stm_visu_perspectives_covariateValue1 and tm_stm_visu_perspectives_covariateValue1
    validate(
      need(!is.null(input$tm_stm_visu_perspectives_covariateValue1) && nchar(input$tm_stm_visu_perspectives_covariateValue1)>0,message="please select covariate value 1")
    )
    validate(
      need(!is.null(input$tm_stm_visu_perspectives_covariateValue2) && nchar(input$tm_stm_visu_perspectives_covariateValue2)>0,message="please select covariate value 2")
    )
    covValue1 <- input$tm_stm_visu_perspectives_covariateValue1
    covValue2 <- input$tm_stm_visu_perspectives_covariateValue2
    
    plot.STM(x = values$tm_stm_model, type = "perspectives", topics = c(selectedTopic1, selectedTopic2), n = input$tm_stm_visu_numberOfWordsToLabelTopic, covarlevels = c(covValue1, covValue2))
    
    
  }else{ # contentFormula in stm model not set
    plot.STM(x = values$tm_stm_model, type = "perspectives", topics = c(selectedTopic1, selectedTopic2), n = input$tm_stm_visu_numberOfWordsToLabelTopic)
  }
  
})

# plot.STM hist
output$TM_stm_visu_hist <- renderPlot({
  if(nchar(values$tm_stm_parameters_contentFormula)>0){# if content formula was set in stm model, the label type is not selectable
    plot.STM(x = values$tm_stm_model, type = "hist", n = input$tm_stm_visu_numberOfWordsToLabelTopic)
  }else{
    plot.STM(x = values$tm_stm_model, type = "hist", n = input$tm_stm_visu_numberOfWordsToLabelTopic, labeltype = input$tm_stm_visu_labeltype, frexw = input$tm_stm_visu_frexweight)
  }
})


# topic correlation
observeEvent(input$tm_stm_visu_topicCorr_start,{
  values$tm_stm_visu_topicCorr_show <- TRUE
  })

output$TM_stm_visu_topicCorr_show<-reactive({
  return(values$tm_stm_visu_topicCorr_show)
})

output$TM_stm_visu_topicCorr_calc <- renderPlot({
  values$tm_stm_visu_topicCorr_method <- "simple"
  topicCorrResult <- topicCorr(model = values$tm_stm_model, method = values$tm_stm_visu_topicCorr_method)
  plot.topicCorr(x = topicCorrResult)
})
outputOptions(output, "TM_stm_visu_topicCorr_show", suspendWhenHidden = FALSE)


#estimateEffect
observeEvent(input$tm_stm_visu_estimateEffect_calcButton,{

  #convert to factors and numeric
  metaVarsToConvertToFactor <- input$tm_stm_visu_estimateEffect_metaVarsToConvertToFactor
  metaVarsToConvertToNumeric <- input$tm_stm_visu_estimateEffect_metaVarsToConvertToNumeric
  values$tm_stm_metaDataConverted <- values$tm_stm_metaData
  for(i in 1:length(metaVarsToConvertToFactor)){
    metaName <- metaVarsToConvertToFactor[i]
    values$tm_stm_metaDataConverted[[metaName]] <-as.factor(values$tm_stm_metaData[[metaName]])
  }
  for(i in 1:length(metaVarsToConvertToNumeric)){
    metaName <- metaVarsToConvertToNumeric[i]
    values$tm_stm_metaDataConverted[[metaName]] <-as.numeric(values$tm_stm_metaData[[metaName]])
  }

  # read formula and estimate effect
  values$tm_stm_visu_estimateEffect_calcParam_formula <- NULL
  if(is.null(input$tm_stm_visu_estimateEffect_calcParam_formula) || nchar(input$tm_stm_visu_estimateEffect_calcParam_formula)==0) {
    shinyWidgets::sendSweetAlert(type = "warning",session = session,title = "You have to provide a formula!")
  }
  else{
      values$tm_stm_visu_estimateEffect_calcParam_formula <- as.formula(input$tm_stm_visu_estimateEffect_calcParam_formula)
      values$tm_stm_visu_estimateEffectResult  <- estimateEffect(formula = values$tm_stm_visu_estimateEffect_calcParam_formula, stmobj = values$tm_stm_model, metadata = values$tm_stm_metaDataConverted)
      values$tm_stm_visu_estimateEffect_show <- TRUE
      values$tm_stm_visu_estimateEffect_plot_show <- FALSE
  }
})

output$TM_stm_visu_estimateEffect_show<-reactive({
  values$tm_stm_visu_estimateEffect_show
})

outputOptions(output, "TM_stm_visu_estimateEffect_show", suspendWhenHidden = FALSE)


# estimate effect summary
output$TM_stm_visu_estimateEffect_summary <- renderPrint({
  summary(values$tm_stm_visu_estimateEffectResult)
})

# estimate effect plot
observeEvent(input$tm_stm_visu_estimateEffect_plotupdate,{
  values$tm_stm_visu_estimateEffect_plot_show <- TRUE
})

output$TM_stm_visu_estimateEffect_plot_show<-reactive({
  values$tm_stm_visu_estimateEffect_plot_show
})

output$TM_stm_visu_estimateEffect_plot <- renderPlot({

  plottingMethod <- input$tm_stm_visu_estimateEffect_plot_method
  if(plottingMethod =="difference"){
    validate(
      need(!is.null(input$tm_stm_visu_estimateEffect_plot_difference_covValue1),message="please select covariate value 1")
    )
    validate(
      need(!is.null(input$tm_stm_visu_estimateEffect_plot_difference_covValue2),message="please select covariate value 2")
    )
    covValue1 <- input$tm_stm_visu_estimateEffect_plot_difference_covValue1
    covValue2 <- input$tm_stm_visu_estimateEffect_plot_difference_covValue2
    plot.estimateEffect(x = values$tm_stm_visu_estimateEffectResult, covariate = input$tm_stm_visu_estimateEffect_plot_covariate, topics = input$tm_stm_visu_estimateEffect_plot_topics, method = plottingMethod, 
                        cov.value1 = covValue1, cov.value2 = covValue2,
                        xlab = paste("More ", covValue2, " ... More ", covValue1)
                        )
    
  }else if(plottingMethod =="continuous"){
    
    covariateOfInterest <- input$tm_stm_visu_estimateEffect_plot_covariate
    
    if(covariateOfInterest == "date"){
      
      # plot original dates on x-axes instead converted numeric values
      minValueBeforeConversion <- min(values$tm_stm_metaData[[covariateOfInterest]]) # use from originaL data (not converted)
      maxValueBeforeConversion <- max(values$tm_stm_metaData[[covariateOfInterest]])
      
      diff_in_days = difftime(maxValueBeforeConversion, minValueBeforeConversion, units = "days") # TODO: consider using different x axis labels depending on time span
      
      #set start and end date to beginn / end of month
      minValueToUse <- format(as.Date(minValueBeforeConversion,"%Y-%m-%d"),"%Y-%m-01") # get first day in given month
      maxValueToUse <- as.Date(format(as.Date(format(as.Date(maxValueBeforeConversion,"%Y-%m-%d"), "%Y-%m-01"), "%Y-%m-%d")+31,"%Y-%m-01"), "%Y-%m-%d")-1 # get the last day of given month
      monthseq <- seq(from = as.Date(minValueToUse), to = as.Date(maxValueToUse), by = "month")

      plot.estimateEffect(x = values$tm_stm_visu_estimateEffectResult, covariate = input$tm_stm_visu_estimateEffect_plot_covariate, topics = input$tm_stm_visu_estimateEffect_plot_topics, method = plottingMethod, xaxt = "n")
      axis.Date(1, at=monthseq, format = "%Y-%m")
            
    }else{
      #TODO: consider using original values as labels for x axes ticks instead of converted numeric ones similar to date above
      plot.estimateEffect(x = values$tm_stm_visu_estimateEffectResult, covariate = input$tm_stm_visu_estimateEffect_plot_covariate, topics = input$tm_stm_visu_estimateEffect_plot_topics, method = plottingMethod)
      
    }
   
  }else{ # plotting method == pointestimate
    plot.estimateEffect(x = values$tm_stm_visu_estimateEffectResult, covariate = input$tm_stm_visu_estimateEffect_plot_covariate, topics = input$tm_stm_visu_estimateEffect_plot_topics, method = plottingMethod)
    
  }
  values$tm_stm_visu_estimateEffect_plot_show <- TRUE
})

outputOptions(output, "TM_stm_visu_estimateEffect_plot_show", suspendWhenHidden = FALSE)

#############
# end of STM
##############

observe({
  values$tm_random2
  values$tm_sub_selected<-unlist(lapply(X = 1:length(isolate(values$tm_timeline_ids)),FUN = function(x){
    input[[paste0("tmsubcoll_",x)]]
  }))
  print(isolate(values$tm_sub_selected))
  #browser()
})



observeEvent(input$TM_Subcollection_save,{
  #check wheather a topic is selected
  topics_selected<-values$tm_timeline_ids[which(values$tm_sub_selected==T)]
  if(length(topics_selected)==0){
    shinyWidgets::sendSweetAlert(type = "warning",session = session,title = "You have to select at least one topic in the table!")
  }
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
  coll_names_in_db<-RMariaDB::dbGetQuery(mydb, 'Select distinct name from Collections')
  RMariaDB::dbDisconnect(mydb)
  if(isolate(input$TM_Subcollection_Name)%in%coll_names_in_db[,1]){
    shinyWidgets::sendSweetAlert(type = "warning",session = session,title = "Collection Name is already used. Please use another one.")
  }
  else{
    validate(
      need(length(topics_selected)>0,message=FALSE)
    )
    ids<-values$tm_info[[1]]
    theta<-values$tm_theta
    ranks<-t(apply(X = theta,MARGIN = 1,FUN = function(x){rank(-x)}))
    if(isolate(input$TM_Timeline_Rank1)==T){
      index_subcoll<-which(apply(X = matrix(ranks[,as.numeric(topics_selected)],ncol=length(topics_selected)),MARGIN = 1,FUN = function(x){return(any(is.element(x,1)))}))
      ids_subcoll<-data.frame(ids[index_subcoll,1])
      dataset_subcoll<-data.frame(as.character(values$tm_info[[2]][index_subcoll,1]))
      id_docs_subcoll<-data.frame(as.numeric(as.character(values$tm_info[[3]][index_subcoll,1])))
      dates_subcoll<-data.frame(as.character(values$tm_info[[6]][index_subcoll,1]),stringsAsFactors = F)
      scores_subcoll<-data.frame(as.character(values$tm_info[[7]][index_subcoll,1]))
    }
    else{
      index_subcoll<-which(apply(X = matrix(theta[,as.numeric(topics_selected)],ncol=length(topics_selected)),MARGIN = 1,FUN = function(x){return(any(x>input$TM_Timeline_Prob))}))
      ids_subcoll<-data.frame(ids[index_subcoll,1])
      dataset_subcoll<-data.frame(as.character(values$tm_info[[2]][index_subcoll,1]))
      id_docs_subcoll<-data.frame(as.numeric(as.character(values$tm_info[[3]][index_subcoll,1])))
      dates_subcoll<-data.frame(as.character(values$tm_info[[6]][index_subcoll,1]),stringsAsFactors = F)
      scores_subcoll<-data.frame(as.character(values$tm_info[[7]][index_subcoll,1]))
    }
    if(dim(ids_subcoll)[1]==0){
      shinyWidgets::sendSweetAlert(type = "warning",session = session,text = "No documents found for current configuration")
    }
    else{
      if(stringr::str_detect(string = input$TM_Subcollection_Name ,pattern= "_")){
        shinyWidgets::sendSweetAlert(type = "warning",session = session,title = "'_' used in collection name",text = "Please don't use '_' in the collection name.")
      }
      else{
        info<-list(ids_subcoll,dataset_subcoll,id_docs_subcoll,paste(values$tm_info[[4]],isolate(values$current_task_id),sep="   subcollection by Topic Model:"),
                   input$TM_Subcollection_Name,dates_subcoll,scores_subcoll,values$tm_info[[8]],values$tm_info[[9]],values$tm_info[[10]])
        save(info,file=paste("collections/collections/",isolate(input$TM_Subcollection_Name),".RData",sep = ""))
        save_collection_to_db(info)
        host_solr<-values$update_solr_url
        port_solr<-values$update_solr_port
        try({future::future(expr = {
          body<-create_body_solr_update_add(ids = id_docs_subcoll[,1],field_name = "collections",values = rep(input$TM_Subcollection_Name,length(id_docs_subcoll[,1])))
          conn<-solrium::SolrClient$new(host = host_solr,port = port_solr,path="search")
          try(silent = T,{
            rm(solr_update_working)
            conn$update_atomic_json(name = "iLCM",body = body)->solr_update_working
          })
          if(!exists("solr_update_working")){
            conn$update_atomic_json(name = "iLCM",body = body)
          }
          solrium::commit(conn = conn,name="iLCM")
        }) %...>% future:::ClusterRegistry("stop")
        })
        shinyWidgets::sendSweetAlert(type = "success",session = session,title =  paste0("Created Subcollection '",input$TM_Subcollection_Name,"' with ",dim(ids_subcoll)[1]," documents!"))
      }
    }
  }
})
#if reset buttin is clicked, set selected topics to NULL
observeEvent(input$TM_Timeline_Reset,{
  values$tm_timeline_ids<-NULL
  values$tm_sub_selected<-NULL
})



##################################################################################################
#                             topic coherence                                                    #
##################################################################################################

observeEvent(input$TM_Coherence_start,{
  error=try({
    load(paste0(values$Details_Data_TM,"/documents_TM.RData"))
    load(paste0(values$Details_Data_TM,"/dtm_TM.RData"))
  })
  validate(
    need(class(error)!="try-error",message="no documents or dtm found")
  )
  try({
    documents<-documents_original
  })
  error<-try({
    load(paste0(values$Details_Data_TM,"/coherence_results_TM.RData"))
    values$topic_intrusion_results<-topic_intrusion_results
    values$right_prediction<-length(which((apply(values$topic_intrusion_results,1,FUN = function(x){x[2]==x[3]}))==TRUE))/dim(values$topic_intrusion_results)[1]
    values$word_intrusion_results<-word_intrusion_results
    values$right_prediction_word<-length(which((apply(values$word_intrusion_results,1,FUN = function(x){x[2]==x[3]}))==TRUE))/dim(values$word_intrusion_results)[1]
  })
  if(class(error)=="try-error"){
    values$topic_intrusion_results<-data.frame(doc=numeric(0),IntruderT=numeric(0),IntruderG=numeric(0))
    values$word_intrusion_results<-data.frame(doc=numeric(0),IntruderT=numeric(0),IntruderG=numeric(0))
  }
  values$TM_Coherence_dtm<-dtm
  values$TM_Coherence_documents<-documents
  values$TM_Coherence_show<-TRUE
})


output$TM_Coherence_show<-reactive({
  values$TM_Coherence_show
})
outputOptions(output, "TM_Coherence_show", suspendWhenHidden = FALSE)


output$TM_Coherence_topic_coherence<-renderPlotly({
  topic_coherence<-tmca.util::tmca_topic_coherence(DTM = values$TM_Coherence_dtm,phi = values$tm_phi)
  values$topic_coherence_results<-topic_coherence
  p<-plot_ly(x=factor(paste("topic:",1:length(topic_coherence))),y=topic_coherence,type = "scatter", marker = list(size = 15,
                                                                                                                   color = 'rgba(255, 182, 193, .9)',
                                                                                                                   line = list(color = 'rgba(152, 0, 0, .8)',
                                                                                                                               width = 2)))
  p<-layout(p,xaxis=list(categoryorder="array",categoryarray=factor(paste("topic:",1:length(topic_coherence)))))  
  return(p)
})

output$TM_Coherence_topic_coherence_mean_box<-renderValueBox({
  valueBox(subtitle = "avg. coherence",icon = icon("list"),value = round(mean(values$topic_coherence_results),3))
})


#############topic intrusion#################

output$TM_Coherence_topic_intrusion<-renderUI({
  return(tagList(
    valueBoxOutput(outputId = "TM_Coherence_topic_intrusion_result_box"),
    valueBoxOutput(outputId = "TM_Coherence_topic_intrusion_docs_box"),
    valueBoxOutput(outputId = "TM_Coherence_topic_intrusion_correct_box"),
    tags$br(),
    bsButton(inputId = "TM_Coherence_topic_intrusion_start",label = "start topic intrusion test",style = "primary",icon = icon("keyboard")),
    bsButton(inputId = "TM_Coherence_topic_intrusion_reset",label = "reset topic intrusion results",style = "danger",icon = icon("trash")),
    conditionalPanel(
      condition='output.TM_Intrusion_show==true',
      htmlOutput(outputId = "TM_Coherence_topic_intrusion_iteration"),
      tags$br(),
      fillRow(
        tags$b("Please select the topic which does NOT describe the document")
      ),
      tags$br(),
      column(4,
             box(title="Topics",solidHeader = T,width = 12,status = "primary",
                 dataTableOutput(outputId = "TM_Coherence_topic_intrusion_topics")
             )
      ),
      column(8,
             box(title = paste0("Document: ",values$topic_intrusion_random_doc_number),solidHeader = T,width=12,status = "primary",
                 div(style = 'height: 38vh; overflow-y: auto;',
                     htmlOutput(outputId = "TM_Coherence_topic_intrusion_documents")
                 )
             )
      )
    )  
  )
  )
})

output$TM_Intrusion_show<-reactive({
  values$TM_Intrusion_show
})
outputOptions(output, "TM_Intrusion_show", suspendWhenHidden = FALSE)


observeEvent(input$TM_Coherence_topic_intrusion_start,{
  values$TM_topic_intrusion_run<-1
  values$TM_Intrusion_show<-TRUE
  shinyjs::useShinyjs()
  isolate(shinyjs::runjs('Shiny.onInputChange(\"wrong_topic\",  "topic_intrusion_button_0")'))
})


observeEvent(input$TM_Coherence_topic_intrusion_reset,{
  values$topic_intrusion_results<-data.frame(doc=numeric(0),IntruderT=numeric(0),IntruderG=numeric(0))
  values$TM_topic_intrusion_run<-NULL
  values$right_prediction<-0
  values$TM_Intrusion_show<-FALSE
  values$TM_topic_intrusion_docs<-0
})


output$TM_Coherence_topic_intrusion_topics<-renderDataTable({
  data<-data.frame(values$TM_Coherence_topic_intrusion_topics,
                   WrongTopic = shinyInput(
                     shinyBS::bsButton,
                     isolate(input$TM_Coherence_setsize),
                     'topic_intrusion_button_',
                     label = "Wrong",
                     style="danger",
                     icon=icon("trash"),
                     onclick = 'Shiny.onInputChange(\"wrong_topic\",  this.id)'
                   ),
                   stringsAsFactors = FALSE
                   
  )
  colnames(data)<-c("topic","wrong?")
  datatable(data=data,options = list(dom="T"),escape=F,selection = "none",rownames = F)
})

output$TM_Coherence_topic_intrusion_documents<-renderUI({
  document<-values$TM_Coherence_topic_intrusion_documents
  return(document)
})

observe({
  validate(
    need(!is.null(values$TM_topic_intrusion_run),message=FALSE),
    need(values$TM_topic_intrusion_run<=isolate(input$TM_Coherence_runs),message=FALSE)
  )
  values$TM_topic_intrusion_run
  #################
  #get random document
  
  K<-nrow(values$tm_phi)
  random_doc_number<-ceiling(runif(1,1,dim(values$TM_Coherence_documents)[1]))
  values$topic_intrusion_random_doc_number<-random_doc_number
  document_identifier<-values$TM_Coherence_documents[random_doc_number,1]
  dataset<-stringr::str_split(string = document_identifier,pattern = "_",simplify = T)[1]
  doc_id<-stringr::str_split(string = document_identifier,pattern = "_",simplify = T)[2]
  
  token<-get_token_from_db(dataset = dataset,doc_ids = doc_id,sentence_ids = NULL,host=values$host,port=values$port)
  document<-paste(token[,"word"],collapse=" ")
  
  topic_names <- apply(isolate(values$tm_relevance), 2, FUN = function(x) {
    paste(names(sort(x, decreasing = T)[1:10]), collapse = " ")
  })
  
  top_topics <- as.numeric(names(values$tm_theta[random_doc_number,])[order(values$tm_theta[random_doc_number, ], decreasing = T)])
  
  top_topic_names <- topic_names[top_topics[1:(input$TM_Coherence_setsize-1)]]
  
  if(K < isolate(input$TM_Coherence_setsize)){
    shinyWidgets::sendSweetAlert(session=session,title = "Number of Topics is smaller than the chosen setsize",text = "Increase the setsize",type = "warning")
  }
  
  if (ceiling(K/2) <=  isolate(input$TM_Coherence_setsize)){
    intruder_topic <- sample(top_topics[isolate(input$TM_Coherence_setsize):length(top_topics)], 1)
  }
  else{
    intruder_topic <- sample(top_topics[ceiling((K+1)/2):K], 1)
  }
  intruder_topic_name <- topic_names[intruder_topic]
  eval_list <- sample(c(top_topic_names, intruder_topic_name))
  intruder_true_position <- which(eval_list == intruder_topic_name)
  
  values$topic_intrusion_results<-rbind(isolate(values$topic_intrusion_results),c(random_doc_number,intruder_true_position,0))
  values$TM_Coherence_topic_intrusion_topics<-eval_list
  values$TM_Coherence_topic_intrusion_documents<-document
})

output$TM_Coherence_topic_intrusion_iteration<-renderUI({
  validate(
    need(!is.null(values$TM_topic_intrusion_run),message=FALSE)
  )
  if(values$TM_topic_intrusion_run<=isolate(input$TM_Coherence_runs)){
    text<-paste0("Iteration: ",values$TM_topic_intrusion_run," of ",isolate(input$TM_Coherence_runs))
  }
  else{
    text<-"You finished all iterations."
  }
  
  return(tags$h4(text,style='color:black;'))
})


observeEvent(input$wrong_topic,{
  validate(
    need(!is.null(input$wrong_topic),message=FALSE),
    need(!is.null(isolate(values$TM_topic_intrusion_run)),message=FALSE),
    need(as.numeric(stringr::str_split(string=input$wrong_topic,pattern = "_",simplify = T)[1,4])!=0,message=FALSE)
  )
  if(values$TM_topic_intrusion_run>isolate(input$TM_Coherence_runs)){
    shinyWidgets::sendSweetAlert(session=session,title = "finished all runs already",text = "start a new iteration",type = "warning")
  }
  else{
    wrong_topic<-as.numeric(stringr::str_split(string=input$wrong_topic,pattern = "_",simplify = T)[1,4])
    shinyjs::useShinyjs()
    isolate(shinyjs::runjs('Shiny.onInputChange(\"wrong_topic\",  "topic_intrusion_button_0")'))
    validate(
      need(wrong_topic>0,message=FALSE)
    )
    values$topic_intrusion_results[dim(isolate(values$topic_intrusion_results))[1],3]<-wrong_topic
    if(values$topic_intrusion_results[dim(isolate(values$topic_intrusion_results))[1],2]==values$topic_intrusion_results[dim(isolate(values$topic_intrusion_results))[1],3]){
      shinyWidgets::sendSweetAlert(session=session,title = "Correct",text = "You found the intruder!",type = "success")
    }
    else{
      shinyWidgets::sendSweetAlert(session=session,title = "False",text = "You did not found the intruder!",type = "error")
    }
    values$right_prediction<-length(which((apply(values$topic_intrusion_results,1,FUN = function(x){x[2]==x[3]}))==TRUE))/dim(values$topic_intrusion_results)[1]
    
    values$TM_topic_intrusion_run<-isolate(values$TM_topic_intrusion_run)+1
  }
})



output$TM_Coherence_topic_intrusion_result_box<-renderValueBox({
  performance<-values$right_prediction
  if(input$TM_Coherence_chance_correction==TRUE){
    random_probabilty <- 1 / input$TM_Coherence_setsize
    performance <- (performance - random_probabilty) / (1 - random_probabilty)
    
  }
  valueBox(value = paste(round(performance*100,3),"%"),subtitle = "right predictions",icon = icon("list"))
})


output$TM_Coherence_topic_intrusion_docs_box<-renderValueBox({
  values$TM_topic_intrusion_run
  values$TM_topic_intrusion_docs
  valueBox(value = length(which(isolate(values$topic_intrusion_results[,3])!=0)),subtitle = "documents assessed",icon = icon("search"),color="purple")
})


output$TM_Coherence_topic_intrusion_correct_box<-renderValueBox({
  valueBox(value = length(which((apply(values$topic_intrusion_results,1,FUN = function(x){x[2]==x[3]}))==TRUE)),subtitle = "intruders found",icon = icon("thumbs-up",lib="glyphicon"),color="yellow")
})

observeEvent(values$TM_topic_intrusion_run,{
  validate(
    need(!is.null(values$TM_topic_intrusion_run),message=FALSE)
  )
  if(values$TM_topic_intrusion_run==(isolate(input$TM_Coherence_runs)+1)){
    topic_intrusion_results<- isolate(values$topic_intrusion_results)
    word_intrusion_results<- isolate(values$word_intrusion_results)
    save(topic_intrusion_results,word_intrusion_results,file=paste0(isolate(values$Details_Data_TM),"/coherence_results_TM.RData"))
  }
})

##########word intruson################


output$TM_Coherence_word_intrusion<-renderUI({
  return(tagList(
    valueBoxOutput(outputId = "TM_Coherence_word_intrusion_result_box"),
    valueBoxOutput(outputId = "TM_Coherence_word_intrusion_docs_box"),
    valueBoxOutput(outputId = "TM_Coherence_word_intrusion_correct_box"),
    tags$br(),
    bsButton(inputId = "TM_Coherence_word_intrusion_start",label = "start word intrusion test",style = "primary",icon = icon("keyboard")),
    bsButton(inputId = "TM_Coherence_word_intrusion_reset",label = "reset word intrusion results",style = "danger",icon = icon("trash")),
    conditionalPanel(
      condition='output.TM_Intrusion_word_show==true',
      htmlOutput(outputId = "TM_Coherence_word_intrusion_iteration"),
      tags$br(),
      fillRow(
        tags$b("Please select the word which does NOT fit with the other words")
      ),
      tags$br(),
      column(4,
             box(title="Word",solidHeader = T,width = 12,status = "primary",
                 dataTableOutput(outputId = "TM_Coherence_word_intrusion_words")
             )
      ),
      column(8,
             wordcloud2Output(outputId = "TM_Coherence_word_intrusion_wordcloud")
      )
    )  
  )
  )
})

output$TM_Intrusion_word_show<-reactive({
  values$TM_Intrusion_word_show
})
outputOptions(output, "TM_Intrusion_word_show", suspendWhenHidden = FALSE)


observeEvent(input$TM_Coherence_word_intrusion_start,{
  values$TM_word_intrusion_run<-1
  values$TM_Intrusion_word_show<-TRUE
})

observeEvent(input$TM_Coherence_word_intrusion_reset,{
  values$word_intrusion_results<-data.frame(doc=numeric(0),IntruderT=numeric(0),IntruderG=numeric(0))
  values$TM_word_intrusion_run<-NULL
  values$right_prediction_word<-0
  values$TM_Intrusion_word_show<-FALSE
  values$TM_word_intrusion_docs<-0
})


output$TM_Coherence_word_intrusion_words<-renderDataTable({
  data<-data.frame(values$TM_Coherence_word_intrusion_words,
                   WrongTopic = shinyInput(
                     shinyBS::bsButton,
                     isolate(input$TM_Coherence_setsize),
                     'word_intrusion_button_',
                     label = "Wrong",
                     style="danger",
                     icon=icon("trash"),
                     onclick = 'Shiny.onInputChange(\"wrong_word\",  this.id)'
                   ),
                   stringsAsFactors = FALSE
                   
  )
  colnames(data)<-c("word","wrong?")
  datatable(data=data,options = list(dom="T"),escape=F,selection = "none",rownames = F)
})

output$TM_Coherence_word_intrusion_wordcloud<-renderWordcloud2({
  data = data.frame(words= values$TM_Coherence_word_intrusion_words,counts=rep(1,length(values$TM_Coherence_word_intrusion_words)),stringsAsFactors = F)
  wordcloud2(data = data,fontFamily = "Helvetica",backgroundColor = "azure",color = "random-dark",size=1.5/input$TM_Coherence_setsize)
})

observe({
  validate(
    need(!is.null(values$TM_word_intrusion_run),message=FALSE),
    need(values$TM_word_intrusion_run<=isolate(input$TM_Coherence_runs),message=FALSE)
  )
  values$TM_word_intrusion_run
  #################
  #get random document
  K<-nrow(values$tm_phi)
  random_topic_number<-ceiling(runif(1,1,dim(values$tm_phi)[1]))
  values$word_intrusion_random_topic_number<-random_topic_number
  
  #use relevance score from ldavis paper
  theta=0.25
  pw<-colSums(values$tm_phi)/sum(colSums(values$tm_phi))
  pwt<-isolate(values$tm_phi)[as.numeric(random_topic_number),]
  
  data<-theta*log(pwt)+(1-theta)*log(pwt/pw)
  #sample most likely data, so its not always the same worsd occcuring
  top_words<-sort(data, decreasing = T)
  top_words_setsize<-names(sort(data,decreasing = T))[sample(x = max(25,(input$TM_Coherence_setsize - 1)),size = (input$TM_Coherence_setsize - 1),prob = max(25,(input$TM_Coherence_setsize - 1)):1 )]  
  
  #top_words <- sort(values$tm_phi[random_topic_number, ], decreasing = T)
  #top_words_setsize <- names(top_words[1:(input$TM_Coherence_setsize - 1)])
  
  V <- ncol(values$tm_phi)
  topN <- min(c(V, 25))
  high_prob_terms <- apply(values$tm_phi[-random_topic_number, ], 1, FUN = function(x) {
    pwt<-x
    data<-theta*log(pwt)+(1-theta)*log(pwt/pw)
    names(sort(data, decreasing = T)[1:topN])
  })
  high_prob_terms <- unique(as.vector(high_prob_terms))
  low_prob_terms <- names(top_words[ceiling(V * 0.25):V])
  selection_terms <- intersect(high_prob_terms, low_prob_terms)
  if(length(selection_terms)==0){
    selection_terms<-low_prob_terms
    print("Actung kein wort, das im topic niedrig wahrscheinlich aber in anderen wahrscheinlich ist, konnte gefunden werden")
  }
  intruder <- sample(selection_terms, 1)
  
  eval_list <- sample(c(top_words_setsize, intruder))
  intruder_true_position <- which(eval_list == intruder)

  values$word_intrusion_results<-rbind(isolate(values$word_intrusion_results),c(random_topic_number,intruder_true_position,0))
  values$TM_Coherence_word_intrusion_words<-eval_list
})



output$TM_Coherence_word_intrusion_iteration<-renderUI({
  validate(
    need(!is.null(values$TM_word_intrusion_run),message=FALSE)
  )
  if(values$TM_word_intrusion_run<=isolate(input$TM_Coherence_runs)){
    text<-paste0("Iteration: ",values$TM_word_intrusion_run," of ",isolate(input$TM_Coherence_runs))
  }
  else{
    text<-"You finished all iterations."
  }
  return(tags$h4(text,style='color:black;'))
})


observeEvent(input$wrong_word,{
  validate(
    need(!is.null(input$wrong_word),message=FALSE),
    need(!is.null(isolate(values$TM_word_intrusion_run)),message=FALSE),
    need(as.numeric(stringr::str_split(string=input$wrong_word,pattern = "_",simplify = T)[1,4])!=0,message=FALSE)
  )
  if(values$TM_word_intrusion_run>isolate(input$TM_Coherence_runs)){
    shinyWidgets::sendSweetAlert(session=session,title = "finished all runs already",text = "start a new iteration",type = "warning")
  }
  else{
    wrong_word<-as.numeric(stringr::str_split(string=input$wrong_word,pattern = "_",simplify = T)[1,4])
    shinyjs::useShinyjs()
    isolate(shinyjs::runjs('Shiny.onInputChange(\"wrong_word\",  "word_intrusion_button_0")'))
    validate(
      need(wrong_word>0,message=FALSE)
    )
    values$word_intrusion_results[dim(isolate(values$word_intrusion_results))[1],3]<-wrong_word
    if(values$word_intrusion_results[dim(isolate(values$word_intrusion_results))[1],2]==values$word_intrusion_results[dim(isolate(values$word_intrusion_results))[1],3]){
      shinyWidgets::sendSweetAlert(session=session,title = "Correct",text = "You found the intruder!",type = "success")
    }
    else{
      shinyWidgets::sendSweetAlert(session=session,title = "False",text = (paste0("You did not found the intruder! It would have been: ",(values$TM_Coherence_word_intrusion_words[values$word_intrusion_results[dim(isolate(values$word_intrusion_results))[1],2]]))),type = "error")
    }
    values$right_prediction_word<-length(which((apply(values$word_intrusion_results,1,FUN = function(x){x[2]==x[3]}))==TRUE))/dim(values$word_intrusion_results)[1]
    
    values$TM_word_intrusion_run<-isolate(values$TM_word_intrusion_run)+1
  }
})



output$TM_Coherence_word_intrusion_result_box<-renderValueBox({
  performance<-values$right_prediction_word
  if(input$TM_Coherence_chance_correction==TRUE){
    random_probabilty <- 1 / input$TM_Coherence_setsize
    performance <- (performance - random_probabilty) / (1 - random_probabilty)
  }
  valueBox(value = paste(round(performance*100,3),"%"),subtitle = "right predictions",icon = icon("list"))
})



output$TM_Coherence_word_intrusion_docs_box<-renderValueBox({
  values$TM_word_intrusion_docs
  values$TM_word_intrusion_run
  valueBox(value = length(which(isolate(values$word_intrusion_results[,3])!=0)),subtitle = "documents assessed",icon = icon("search"),color="purple")
})


output$TM_Coherence_word_intrusion_correct_box<-renderValueBox({
  valueBox(value = length(which((apply(values$word_intrusion_results,1,FUN = function(x){x[2]==x[3]}))==TRUE)),subtitle = "intruders found",icon = icon("thumbs-up",lib="glyphicon"),color="yellow")
})

observeEvent(values$TM_word_intrusion_run,{
  validate(
    need(!is.null(values$TM_word_intrusion_run),message=FALSE)
  )
  if(values$TM_word_intrusion_run==(isolate(input$TM_Coherence_runs)+1)){
    word_intrusion_results<- isolate(values$word_intrusion_results)
    topic_intrusion_results<- isolate(values$topic_intrusion_results)
    save(word_intrusion_results,topic_intrusion_results,file=paste0(isolate(values$Details_Data_TM),"/coherence_results_TM.RData"))
  }
})



output$TM_dict_topics_ui<-renderUI({
  checkboxGroupInput(inputId = "TM_dict_topics",label = "Topcis",
                     choiceNames =  paste("Topic",1:values$tm_number_of_topics,sep=" ") ,inline = T,choiceValues = 1:values$tm_number_of_topics
  )
})

output$TM_dict_categories_names<-renderUI({
  validate(
    need(!is.null(input$TM_dict_topics),message=FALSE)
  )
  tagList(lapply(X = input$TM_dict_topics,function(x){
    return(tagList(
      textInput(inputId = paste0("tm_dict_category_names_",x),label = paste0("Category name for Topic ",x),value = isolate(values$TM_dict_headers_help)[[as.numeric(x)]]),
      tags$h4(paste(names(sort(isolate(values$tm_phi)[as.numeric(x),],decreasing = T)[1:20]),collapse=" ")),
      tags$hr()
    )
    )
  }))
})

observe({
  validate(
    need(!is.null((input$TM_dict_topics)),message=FALSE),
    need(length((input$TM_dict_topics))>0,message=FALSE)
  )
  headers_help<-lapply(1:values$tm_number_of_topics,FUN = function(i){
    er<-try({
      name<-input[[paste0("tm_dict_category_names_",i)]]
    })
    if(class(er)=="try-error"){
      name=""
    }
    return(name)
  }
  )
  headers<-lapply(input$TM_dict_topics,FUN = function(i){
    name<-input[[paste0("tm_dict_category_names_",i)]]
    return(name)
  }
  )
  values$TM_dict_headers_help<-headers_help
  values$TM_dict_headers<-do.call(c,headers)
})

output$TM_dict_save_ui<-renderUI({
  validate(
    need(!is.null(input$TM_dict_topics),message=FALSE),
    need(length(input$TM_dict_topics)>0,message=FALSE)
  )
  tagList(
    tags$br(),
    textInput(inputId = "TM_dict_name",label = "Dictionary name",value = ""),
    bsButton(inputId = "TM_dict_save",label = "Save Dictionary",icon = icon("save"),style = "success")
  )
})



observeEvent(input$TM_dict_save,{
  if(any(nchar(values$TM_dict_headers)==0)){
    shinyWidgets::sendSweetAlert(session=session,title = "Not all categories have names!",text = "Please specify a name for every category!",type = "warning")
  }
  else{
    if(length(unique(values$TM_dict_headers))!=length(values$TM_dict_headers)){
      shinyWidgets::sendSweetAlert(session=session,title = "Not all categories have unique names!",text = "Please ensure every category has a unique name!",type = "warning")
    }
    else{
      if(input$TM_dict_name==""){
        shinyWidgets::sendSweetAlert(session=session,title = "No dictionary name given!",text = "Please specify a name for the dictionary!",type = "warning")
      }
      else{
        if(input$TM_dict_name %in% stringr::str_remove_all(string = list.files("collections/dictionaries/"),pattern = ".RData")){
          shinyWidgets::sendSweetAlert(session=session,title = "Dictionary Name already in Use",text = "Please specify a other name!",type = "warning")
        }
        else{
          df<-NULL
          for(i in input$TM_dict_topics){
            df<-cbind(df,names(sort(isolate(values$tm_phi)[as.numeric(i),],decreasing = T)[1:input$TM_dict_number_of_words]))
          }
          colnames(df)<-values$TM_dict_headers
          dict<-DF_to_Dict(df)
          save(dict,file=paste0("collections/dictionaries/",input$TM_dict_name,".RData"))
          shinyWidgets::sendSweetAlert(session=session,title = "Dictionary saved",type = "success")
        }
      }
    }
  }
})



output$TM_meta_ui<-renderUI({
  validate(
    need(file.exists(paste0(values$Details_Data_TM,"/meta_TM.RData")),message="no detailed metadata analyis selected in task scheduler")
  )
  load(paste0(values$Details_Data_TM,"/meta_TM.RData"))
  #browser()
  mde_use<-colnames(meta_names[1,2:dim(meta_names)[2]])[which(!is.na(meta_names[1,2:dim(meta_names)[2]]))]
  meta<-meta[,c("id","dataset","id_doc","token","language",mde_use)]
  colnames(meta)<-c("id","dataset","id_doc","token","language",meta_names[,mde_use])
  values$TM_meta<-meta
  return(tagList(
    uiOutput(outputId = "Det_TM_Meta1"),
    uiOutput(outputId = "Det_TM_Meta2"),
    uiOutput(outputId = "Det_TM_Meta3"),
    uiOutput(outputId = "Det_TM_Meta4"),
    uiOutput(outputId = "Det_TM_Meta5"),
    uiOutput(outputId = "Det_TM_Meta6"),
    uiOutput(outputId = "Det_TM_Meta7"),
    uiOutput(outputId = "Det_TM_Meta8"),
    uiOutput(outputId = "Det_TM_Meta9"),
    uiOutput(outputId = "Det_TM_Meta10"),
    uiOutput(outputId = "Det_TM_Meta11")
  ))
})

output$Det_meta_select_ui<-renderUI({
  validate(
    need(!is.null(values$TM_meta),message=F
    )
  )
  tagList(
    selectInput(inputId = "Det_meta_select",label = "Choose a Meta Category",choices = colnames(values$TM_meta)[4:length(colnames(values$TM_meta))]),
    numericInput(inputId = "Det_meta_topic",label="Which topic should be analyzed?",value = 1,min = 1,max = dim(values$tm_theta)[2],step = 1),
    materialSwitch(inputId = "TM_meta_Rank1",label = "Use Rank1 for selecting document memebership",value = T,status = "warning"),
    conditionalPanel(condition = 'input.TM_meta_Rank1==false',
                     knobInput(inputId = "TM_meta_Prob",label = "Minimal Probability",value = 0.5,min = 0,max = 1,step = 0.01)
    )
  )
})


output$Det_TM_meta_wordcloud1<-renderWordcloud2({
  theta=0.25
  pw<-colSums(values$tm_phi)/sum(colSums(values$tm_phi))
  pwt<-isolate(values$tm_phi)[as.numeric(input$Det_meta_topic),]
  
  data<-theta*log(pwt)+(1-theta)*log(pwt/pw)
  data<-sort(data,decreasing = T)[1:20]  
  
  data<-data.frame(cbind(names(data),data),stringsAsFactors = FALSE)
  class(data$data)<-"numeric"
  #normalize weights for wordcloud
  data$data<-data$data-min(data$data)
  data$data<-data$data/max(data$data)
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1)
})

output$Det_TM_meta_wordcloud2<-renderWordcloud2({
  theta=0.25
  pw<-colSums(values$tm_phi)/sum(colSums(values$tm_phi))
  pwt<-isolate(values$tm_phi)[as.numeric(input$Det_meta_topic),]
  
  data<-theta*log(pwt)+(1-theta)*log(pwt/pw)
  data<-sort(data,decreasing = T)[1:20]  
  
  data<-data.frame(cbind(names(data),data),stringsAsFactors = FALSE)
  class(data$data)<-"numeric"
  #normalize weights for wordcloud
  data$data<-data$data-min(data$data)
  data$data<-data$data/max(data$data)
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1)
})

output$Det_TM_meta_wordcloud3<-renderWordcloud2({
  theta=0.25
  pw<-colSums(values$tm_phi)/sum(colSums(values$tm_phi))
  pwt<-isolate(values$tm_phi)[as.numeric(input$Det_meta_topic),]
  
  data<-theta*log(pwt)+(1-theta)*log(pwt/pw)
  data<-sort(data,decreasing = T)[1:20]  
  
  data<-data.frame(cbind(names(data),data),stringsAsFactors = FALSE)
  class(data$data)<-"numeric"
  #normalize weights for wordcloud
  data$data<-data$data-min(data$data)
  data$data<-data$data/max(data$data)
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1)
})
output$Det_TM_meta_wordcloud4<-renderWordcloud2({
  theta=0.25
  pw<-colSums(values$tm_phi)/sum(colSums(values$tm_phi))
  pwt<-isolate(values$tm_phi)[as.numeric(input$Det_meta_topic),]
  
  data<-theta*log(pwt)+(1-theta)*log(pwt/pw)
  data<-sort(data,decreasing = T)[1:20]  
  
  data<-data.frame(cbind(names(data),data),stringsAsFactors = FALSE)
  class(data$data)<-"numeric"
  #normalize weights for wordcloud
  data$data<-data$data-min(data$data)
  data$data<-data$data/max(data$data)
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1)
})
output$Det_TM_meta_wordcloud5<-renderWordcloud2({
  theta=0.25
  pw<-colSums(values$tm_phi)/sum(colSums(values$tm_phi))
  pwt<-isolate(values$tm_phi)[as.numeric(input$Det_meta_topic),]
  
  data<-theta*log(pwt)+(1-theta)*log(pwt/pw)
  data<-sort(data,decreasing = T)[1:20]  
  
  data<-data.frame(cbind(names(data),data),stringsAsFactors = FALSE)
  class(data$data)<-"numeric"
  #normalize weights for wordcloud
  data$data<-data$data-min(data$data)
  data$data<-data$data/max(data$data)
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1)
})
output$Det_TM_meta_wordcloud6<-renderWordcloud2({
  theta=0.25
  pw<-colSums(values$tm_phi)/sum(colSums(values$tm_phi))
  pwt<-isolate(values$tm_phi)[as.numeric(input$Det_meta_topic),]
  
  data<-theta*log(pwt)+(1-theta)*log(pwt/pw)
  data<-sort(data,decreasing = T)[1:20]  
  
  data<-data.frame(cbind(names(data),data),stringsAsFactors = FALSE)
  class(data$data)<-"numeric"
  #normalize weights for wordcloud
  data$data<-data$data-min(data$data)
  data$data<-data$data/max(data$data)
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1)
})
output$Det_TM_meta_wordcloud7<-renderWordcloud2({
  theta=0.25
  pw<-colSums(values$tm_phi)/sum(colSums(values$tm_phi))
  pwt<-isolate(values$tm_phi)[as.numeric(input$Det_meta_topic),]
  
  data<-theta*log(pwt)+(1-theta)*log(pwt/pw)
  data<-sort(data,decreasing = T)[1:20]  
  
  data<-data.frame(cbind(names(data),data),stringsAsFactors = FALSE)
  class(data$data)<-"numeric"
  #normalize weights for wordcloud
  data$data<-data$data-min(data$data)
  data$data<-data$data/max(data$data)
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1)
})
output$Det_TM_meta_wordcloud8<-renderWordcloud2({
  theta=0.25
  pw<-colSums(values$tm_phi)/sum(colSums(values$tm_phi))
  pwt<-isolate(values$tm_phi)[as.numeric(input$Det_meta_topic),]
  
  data<-theta*log(pwt)+(1-theta)*log(pwt/pw)
  data<-sort(data,decreasing = T)[1:20]  
  
  data<-data.frame(cbind(names(data),data),stringsAsFactors = FALSE)
  class(data$data)<-"numeric"
  #normalize weights for wordcloud
  data$data<-data$data-min(data$data)
  data$data<-data$data/max(data$data)
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1)
})
output$Det_TM_meta_wordcloud9<-renderWordcloud2({
  theta=0.25
  pw<-colSums(values$tm_phi)/sum(colSums(values$tm_phi))
  pwt<-isolate(values$tm_phi)[as.numeric(input$Det_meta_topic),]
  
  data<-theta*log(pwt)+(1-theta)*log(pwt/pw)
  data<-sort(data,decreasing = T)[1:20]  
  
  data<-data.frame(cbind(names(data),data),stringsAsFactors = FALSE)
  class(data$data)<-"numeric"
  #normalize weights for wordcloud
  data$data<-data$data-min(data$data)
  data$data<-data$data/max(data$data)
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1)
})
output$Det_TM_meta_wordcloud10<-renderWordcloud2({
  theta=0.25
  pw<-colSums(values$tm_phi)/sum(colSums(values$tm_phi))
  pwt<-isolate(values$tm_phi)[as.numeric(input$Det_meta_topic),]
  
  data<-theta*log(pwt)+(1-theta)*log(pwt/pw)
  data<-sort(data,decreasing = T)[1:20]  
  
  data<-data.frame(cbind(names(data),data),stringsAsFactors = FALSE)
  class(data$data)<-"numeric"
  #normalize weights for wordcloud
  data$data<-data$data-min(data$data)
  data$data<-data$data/max(data$data)
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1)
})
output$Det_TM_meta_wordcloud11<-renderWordcloud2({
  theta=0.25
  pw<-colSums(values$tm_phi)/sum(colSums(values$tm_phi))
  pwt<-isolate(values$tm_phi)[as.numeric(input$Det_meta_topic),]
  
  data<-theta*log(pwt)+(1-theta)*log(pwt/pw)
  data<-sort(data,decreasing = T)[1:20]  
  
  data<-data.frame(cbind(names(data),data),stringsAsFactors = FALSE)
  class(data$data)<-"numeric"
  #normalize weights for wordcloud
  data$data<-data$data-min(data$data)
  data$data<-data$data/max(data$data)
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1)
})
output$Det_TM_Meta1<-renderUI({
  if("token"%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    #relevant_words<-
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),"token"]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    
    output$Det_TM_meta_plot_token<-renderPlotly({
      p <- plot_ly(counts, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = 'Meta distribution for meta category: "token"',legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_1<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/sum(counts[,2]),digits = 3))
      colnames(counts)<-c(colnames(values$TM_meta)[5],"absolute","relative")
      datatable(data = counts,rownames = F,options = list(dom="tp"))
    })
    return(tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               text),
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$div(paste("most relevant words for topic: ",input$Det_meta_topic ," with lambda=0.25")),
                      wordcloud2Output("Det_TM_meta_wordcloud1")
               ),
               column(5,
                      plotlyOutput("Det_TM_meta_plot_token")
               ),
               column(3,
                      DT::dataTableOutput("Det_TM_meta_table_1")
               )
      )
    ))
  }
  else{
    return(NULL)
  }
})


output$Det_TM_Meta2<-renderUI({
  if(colnames(values$TM_meta)[5]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    #relevant_words<-
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),colnames(values$TM_meta)[5]]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_2<-renderPlotly({
      p <- plot_ly(counts, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',colnames(values$TM_meta)[5],'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_2<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/sum(counts[,2]),digits = 3))
      colnames(counts)<-c(colnames(values$TM_meta)[5],"absolute","relative")
      datatable(data = counts,rownames = F,options = list(dom="tp"))
    })
    
    return(tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               text),
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$div(paste("most relevant words for topic: ",input$Det_meta_topic ," with lambda=0.25")),
                      wordcloud2Output("Det_TM_meta_wordcloud2")
               ),
               column(5,
                      plotlyOutput("Det_TM_meta_plot_2")
               ),
               column(3,
                      DT::dataTableOutput("Det_TM_meta_table_2")
               )
      )
    ))
  }
  else{
    return(NULL)
  }
})



output$Det_TM_Meta3<-renderUI({
  if(colnames(values$TM_meta)[6]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    #relevant_words<-
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),colnames(values$TM_meta)[6]]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_3<-renderPlotly({
      p <- plot_ly(counts, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',colnames(values$TM_meta)[6],'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_3<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/sum(counts[,2]),digits = 3))
      colnames(counts)<-c(colnames(values$TM_meta)[6],"absolute","relative")
      datatable(data = counts,rownames = F,options = list(dom="tp"))
    })
    
    return(tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               text),
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$div(paste("most relevant words for topic: ",input$Det_meta_topic ," with lambda=0.25")),
                      wordcloud2Output("Det_TM_meta_wordcloud3")
               ),
               column(5,
                      plotlyOutput("Det_TM_meta_plot_3")
               ),
               column(3,
                      DT::dataTableOutput("Det_TM_meta_table_3")
               )
      )
    ))
  }
  else{
    return(NULL)
  }
})



output$Det_TM_Meta4<-renderUI({
  if(colnames(values$TM_meta)[7]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    #relevant_words<-
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),colnames(values$TM_meta)[7]]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_4<-renderPlotly({
      p <- plot_ly(counts, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',colnames(values$TM_meta)[7],'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_4<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/sum(counts[,2]),digits = 3))
      colnames(counts)<-c(colnames(values$TM_meta)[7],"absolute","relative")
      datatable(data = counts,rownames = F,options = list(dom="tp"))
    })
    
    return(tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               text),
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$div(paste("most relevant words for topic: ",input$Det_meta_topic ," with lambda=0.25")),
                      wordcloud2Output("Det_TM_meta_wordcloud4")
               ),
               column(5,
                      plotlyOutput("Det_TM_meta_plot_4")
               ),
               column(3,
                      DT::dataTableOutput("Det_TM_meta_table_4")
               )
      )
    ))
  }
  else{
    return(NULL)
  }
})

output$Det_TM_Meta5<-renderUI({
  if(colnames(values$TM_meta)[8]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    #relevant_words<-
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),colnames(values$TM_meta)[8]]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_5<-renderPlotly({
      p <- plot_ly(counts, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',colnames(values$TM_meta)[8],'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_5<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/sum(counts[,2]),digits = 3))
      colnames(counts)<-c(colnames(values$TM_meta)[8],"absolute","relative")
      datatable(data = counts,rownames = F,options = list(dom="tp"))
    })
    
    return(tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               text),
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$div(paste("most relevant words for topic: ",input$Det_meta_topic ," with lambda=0.25")),
                      wordcloud2Output("Det_TM_meta_wordcloud5")
               ),
               column(5,
                      plotlyOutput("Det_TM_meta_plot_5")
               ),
               column(3,
                      DT::dataTableOutput("Det_TM_meta_table_5")
               )
      )
    ))
  }
  else{
    return(NULL)
  }
})



output$Det_TM_Meta6<-renderUI({
  if(colnames(values$TM_meta)[9]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    #relevant_words<-
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),colnames(values$TM_meta)[9]]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_6<-renderPlotly({
      p <- plot_ly(counts, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',colnames(values$TM_meta)[9],'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_6<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/sum(counts[,2]),digits = 3))
      colnames(counts)<-c(colnames(values$TM_meta)[9],"absolute","relative")
      datatable(data = counts,rownames = F,options = list(dom="tp"))
    })
    
    return(tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               text),
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$div(paste("most relevant words for topic: ",input$Det_meta_topic ," with lambda=0.25")),
                      wordcloud2Output("Det_TM_meta_wordcloud6")
               ),
               column(5,
                      plotlyOutput("Det_TM_meta_plot_6")
               ),
               column(3,
                      DT::dataTableOutput("Det_TM_meta_table_6")
               )
      )
    ))
  }
  else{
    return(NULL)
  }
})



output$Det_TM_Meta7<-renderUI({
  if(colnames(values$TM_meta)[10]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    #relevant_words<-
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),colnames(values$TM_meta)[10]]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_7<-renderPlotly({
      p <- plot_ly(counts, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',colnames(values$TM_meta)[10],'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_7<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/sum(counts[,2]),digits = 3))
      colnames(counts)<-c(colnames(values$TM_meta)[10],"absolute","relative")
      datatable(data = counts,rownames = F,options = list(dom="tp"))
    })
    
    return(tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               text),
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$div(paste("most relevant words for topic: ",input$Det_meta_topic ," with lambda=0.25")),
                      wordcloud2Output("Det_TM_meta_wordcloud7")
               ),
               column(5,
                      plotlyOutput("Det_TM_meta_plot_7")
               ),
               column(3,
                      DT::dataTableOutput("Det_TM_meta_table_7")
               )
      )
    ))
  }
  else{
    return(NULL)
  }
})



output$Det_TM_Meta8<-renderUI({
  if(colnames(values$TM_meta)[11]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    #relevant_words<-
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),colnames(values$TM_meta)[11]]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_8<-renderPlotly({
      p <- plot_ly(counts, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',colnames(values$TM_meta)[11],'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_8<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/sum(counts[,2]),digits = 3))
      colnames(counts)<-c(colnames(values$TM_meta)[11],"absolute","relative")
      datatable(data = counts,rownames = F,options = list(dom="tp"))
    })
    
    return(tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               text),
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$div(paste("most relevant words for topic: ",input$Det_meta_topic ," with lambda=0.25")),
                      wordcloud2Output("Det_TM_meta_wordcloud8")
               ),
               column(5,
                      plotlyOutput("Det_TM_meta_plot_8")
               ),
               column(3,
                      DT::dataTableOutput("Det_TM_meta_table_8")
               )
      )
    ))
  }
  else{
    return(NULL)
  }
})



output$Det_TM_Meta9<-renderUI({
  if(colnames(values$TM_meta)[12]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    #relevant_words<-
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),colnames(values$TM_meta)[12]]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_8<-renderPlotly({
      p <- plot_ly(counts, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',colnames(values$TM_meta)[12],'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_8<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/sum(counts[,2]),digits = 3))
      colnames(counts)<-c(colnames(values$TM_meta)[12],"absolute","relative")
      datatable(data = counts,rownames = F,options = list(dom="tp"))
    })
    
    return(tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               text),
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$div(paste("most relevant words for topic: ",input$Det_meta_topic ," with lambda=0.25")),
                      wordcloud2Output("Det_TM_meta_wordcloud8")
               ),
               column(5,
                      plotlyOutput("Det_TM_meta_plot_8")
               ),
               column(3,
                      DT::dataTableOutput("Det_TM_meta_table_8")
               )
      )
    ))
  }
  else{
    return(NULL)
  }
})




output$Det_TM_Meta10<-renderUI({
  if(colnames(values$TM_meta)[13]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    #relevant_words<-
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),colnames(values$TM_meta)[13]]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_9<-renderPlotly({
      p <- plot_ly(counts, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',colnames(values$TM_meta)[13],'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_9<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/sum(counts[,2]),digits = 3))
      colnames(counts)<-c(colnames(values$TM_meta)[13],"absolute","relative")
      datatable(data = counts,rownames = F,options = list(dom="tp"))
    })
    
    return(tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               text),
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$div(paste("most relevant words for topic: ",input$Det_meta_topic ," with lambda=0.25")),
                      wordcloud2Output("Det_TM_meta_wordcloud9")
               ),
               column(5,
                      plotlyOutput("Det_TM_meta_plot_9")
               ),
               column(3,
                      DT::dataTableOutput("Det_TM_meta_table_9")
               )
      )
    ))
  }
  else{
    return(NULL)
  }
})


output$Det_TM_Meta11<-renderUI({
  if(colnames(values$TM_meta)[14]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    #relevant_words<-
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),colnames(values$TM_meta)[14]]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_10<-renderPlotly({
      p <- plot_ly(counts, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',colnames(values$TM_meta)[14],'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_10<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/sum(counts[,2]),digits = 3))
      colnames(counts)<-c(colnames(values$TM_meta)[14],"absolute","relative")
      datatable(data = counts,rownames = F,options = list(dom="tp"))
    })
    
    return(tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               text),
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$div(paste("most relevant words for topic: ",input$Det_meta_topic ," with lambda=0.25")),
                      wordcloud2Output("Det_TM_meta_wordcloud10")
               ),
               column(5,
                      plotlyOutput("Det_TM_meta_plot_10")
               ),
               column(3,
                      DT::dataTableOutput("Det_TM_meta_table_10")
               )
      )
    ))
  }
  else{
    return(NULL)
  }
})



output$Det_TM_Meta12<-renderUI({
  if(colnames(values$TM_meta)[15]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    #relevant_words<-
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),colnames(values$TM_meta)[15]]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_11<-renderPlotly({
      p <- plot_ly(counts, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',colnames(values$TM_meta)[15],'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_11<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/sum(counts[,2]),digits = 3))
      colnames(counts)<-c(colnames(values$TM_meta)[15],"absolute","relative")
      datatable(data = counts,rownames = F,options = list(dom="tp"))
    })
    
    return(tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               text),
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$div(paste("most relevant words for topic: ",input$Det_meta_topic ," with lambda=0.25")),
                      wordcloud2Output("Det_TM_meta_wordcloud11")
               ),
               column(5,
                      plotlyOutput("Det_TM_meta_plot_11")
               ),
               column(3,
                      DT::dataTableOutput("Det_TM_meta_table_11")
               )
      )
    ))
  }
  else{
    return(NULL)
  }
})




######################################################
#########  estimated word frequencies ################
######################################################



output$TM_ewf_bar<-renderPlotly({
  validate(
    need(!is.null(input$Det_TM_ewf_word),message="specify at least one word")
  )
  data<-values$tm_rel_counts[,input$Det_TM_ewf_word,drop=F]
  if(input$Det_TM_emf_rel==TRUE){
    data<-do.call(cbind,lapply(X = 1:dim(data)[2],FUN = function(x){
      data[,x]/sum(data[,x])
    })
    )
  }
  p<-plot_ly(x=paste("topic:",1:dim(data)[1],sep=" " ),y=data[,1],type = "bar",name=input$Det_TM_ewf_word[1])
  
  if(dim(data)[2]>1){
    for(i in 2:dim(data)[2]){
      p<-add_trace(p,y=data[,i],name=input$Det_TM_ewf_word[i])    
    }
  }
  p<-layout(p,title="Estimated word frequencies",xaxis=list(categoryorder="array",categoryarray=paste("topic:",1:dim(data)[1],sep=" " )))
  return(p)
})


output$TM_ewf_table<-DT::renderDataTable({
  validate(
    need(!is.null(input$Det_TM_ewf_word),message=FALSE)
  )
  data<-values$tm_rel_counts[,input$Det_TM_ewf_word,drop=F]
  if(input$Det_TM_emf_rel==TRUE){
    data<-do.call(cbind,lapply(X = 1:dim(data)[2],FUN = function(x){
      data[,x]/sum(data[,x])
    })
    )
    data<-round(data,digits = 2)
  }
  colnames(data)<-input$Det_TM_ewf_word
  rownames(data)<-paste("topic:",1:dim(data)[1],sep=" " )
  values$tm_counts<-data
  datatable(data = data,options=list(dom="pt"))
})



output$download_rel_counts<-downloadHandler(
  filename = function() {
    paste('estimated_counts', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-as.matrix(values$tm_counts)
    write.csv(data, con)
  }
)  


output$Det_TM_validation_metadata_UI<-renderUI({
  validate(
    need(
      !is.null(input$Det_TM_validation_document),message=F
    )
  )
  identifier<-stringr::str_split(string = input$Det_TM_validation_document,pattern = "_",simplify = T)
  dataset<-identifier[1]
  doc_id<-identifier[2]
  metadata<-get_metadata_from_db(dataset = dataset,doc_ids = doc_id,host = values$host,port=values$port)
  return(tagList(
    tags$h4("Metadata"),
    tags$b("title:"),
    tags$div(metadata["title"]),
    tags$b("date:"),
    tags$div(metadata["date"]),
    tags$b("number of token:"),
    tags$div(metadata["token"])
  ))
})

output$TM_validation_UI<-renderUI({
  validate(
    need(
      !is.null(input$Det_TM_validation_document),message=FALSE
    )
  )
  identifier<-stringr::str_split(string = input$Det_TM_validation_document,pattern = "_",simplify = T)
  dataset<-identifier[1]
  doc_id<-identifier[2]
  token<-get_token_from_db(dataset = dataset,doc_ids = doc_id,sentence_ids = NULL,host=values$host,port=values$port)
  load(paste0(values$Details_Data_TM,"/parameters.RData"))
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
  data<-values$tm_phi[input$Det_TM_validation_topic,]
  max<-log(max(values$tm_phi))
  
  data<-data.frame(features=names(data),weight=data)
  m<-merge(x = token,y=data,by="features",all.x=TRUE)
  m<-m[order(m[,2]),]
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))
  colors<-getPalette(dim(values$tm_phi)[1])
  #colors<-colors[order(values$tm_theta[input$Det_TM_validation_document,],decreasing = F)]
  color<-colors[input$Det_TM_validation_topic]
  rbPal_pos <- colorRampPalette(c('floralwhite',color))
  m<-cbind(m,rep("",dim(m)[1]))
  if(length(intersect(which(!is.na(m$weight)),which(m$weight>0)))>0){
    m[intersect(which(!is.na(m$weight)),which(m$weight>0)),12]<-  rbPal_pos(100)[as.numeric(cut(m$weight[intersect(which(!is.na(m$weight)),which(m$weight>0))],breaks = 100))] #Alternative#seq(0,to = max(data$weight),length.out = 100) #original m$weight[intersect(which(!is.na(m$weight)),which(m$weight>0))]
  }
  strings<-apply(m,MARGIN = 1,FUN = function(x){
    if(is.na(x[11])){
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
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$br(),
                      plotly::plotlyOutput("Det_TM_validation_document_topic_pie")
               ),
               column(8,
                      tags$br(),
                      tags$p(a)
               )
      )
      
    ) 
  )
})


output$Det_TM_validation_document_topic_pie<-plotly::renderPlotly({
  validate(
    need(
      !is.null(input$Det_TM_validation_document),message=FALSE
    )
  )
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))
  colors<-getPalette(dim(values$tm_phi)[1])
  
  data<-values$tm_theta[input$Det_TM_validation_document,]
  data<-data.frame(class=paste("Topic: ",names(data)),likelihood=data)
  
  p <- plot_ly(data, labels = ~factor(class), values = ~likelihood, textposition = 'inside',marker = list(colors = colors),
               textinfo = 'label+percent') %>%
    plotly::add_pie(hole = 0.6) %>%
    plotly::layout(title = paste('Distribution of topics for chosen document'),legend=T,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   legend = list(x = 0.1, y = 0.9)
    )
  return(p)
})
