source("global/functions_used_in_scripts.R")


#' link downloadbutton for theta in Topic Models Tab
#'  depends on:
#'   values$tm_theta: theta button in parameter bar in Topic Models Tab
output$download_theta<-downloadHandler(
  filename = function() {
    paste('Theta-', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-as.matrix(values$tm_theta)
    write.csv(data, con)
  }
)

#' link downloadbutton for phi in Topic Models Tab
#' depends on:
#'  values$tm_phi: phi button in parameter bar in Topic Models Tab
output$download_phi<-downloadHandler(
  filename = function() {
    paste('Phi-', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-as.matrix(values$tm_phi)
    write.csv(data, con)
  }
)  


#' link downloadbutton for lda vis in Topic Models Tab
#' depends on:
#'   input$download_ldavis: information if download-button for lda vis was clicked
observeEvent(input$download_ldavis,{
  shinyscreenshot::screenshot(id = "TM_LDAvis",filename =  paste('LDAvis-', Sys.Date(), '.png', sep=''))
})


# output$download_ldavis<-downloadHandler(
#   filename = function() {
#     paste('LDAvis-', Sys.Date(), '.png', sep='')
#   },
#   content = function(con) {
#     screenshot(selector = "#TM_LDAvis",filename = con)
#     #LDAvis::serVis(json = values$tm_json,open.browser = F,out.dir = "collections/tmp/ldavis/",encoding = "UTF-8")
#     #zip::zipr(zipfile = "collections/tmp/ldavis.zip",files = "collections/tmp/ldavis/",recurse = T,include_directories=F)
#     #file.copy(from = "collections/tmp/ldavis.zip",to = con)
#   }
# )  



#' render LDA Viz plot based on the calculated topic models
#' depends on:
#'   input$coll: collumn names of calculatet topic model data
#'   input$nTerms: number of words in lda vis
#'   values$tm_phi: phi in Topic Models Tab
#'   values$tm_theta: theta in Topic Models Tab
#'   values$tm_doc.length: Topic Model document length
#'   values$tm_vocab: Topic Model vocabulary
#'   values$tm_term.frequency: Topic Model term frequency
#'   values$tm_json: json file for Topic Models
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
      # default number of words in lda vis is 30; if user uses other number --> need to create newjson object 
      if(input$nTerms!=30){
        #vocab<-stringr::str_replace_all(string = values$tm_vocab,pattern = "\\\\",replacement="")
        tm<-LDAvis::createJSON(values$tm_phi, values$tm_theta, values$tm_doc.length, values$tm_vocab, values$tm_term.frequency, 
                               R = input$nTerms,reorder.topics = F)#mds.method = svd_tsne )
        return(tm)
      }
      else{
        return(values$tm_json)
      }
    }
  }
})
#' observe events to calculate word clouds
#' depends on:
#'   values$tm_phi: phi in Topic Models Tab
#'   values$tm_theta: theta in Topic Models Tab
#'   values$tm_doc.length: Topic Model document length
#'   values$tm_dates: dates from documents used in Topic Model 
observeEvent(values$tm_phi,{
  relevance<-calculate_topic_relevance(lambda=0.3,phi=values$tm_phi,theta=values$tm_theta,doc.length=values$tm_doc.length)
  values$tm_relevance <- relevance
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
        wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1,minRotation = -pi/2, maxRotation = -pi/2)
      }
      )
      output[[plotname_sp]]<-renderSparkline({
        sparkline(values = dates[,2])
      }
      )
    })
  }
})

#' observe add button in topic modeling paramters tab and when clicked add topic to timeline data
#' depends:
#'   values$tm_random: random settings for Topic Model calculation
#'   values$observers: observer information for the Topic Model
#'   values$tm_timeline_ids: Topic Model timeline ids
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

#' render topic model timelineplot based on added topics 
#' depends on:
#'   values$tm_timeline_ids: Topic Model timeline ids
#'   values$tm_dates: Topic Model dates
#'   values$tm_theta:  theta in Topic Models Tab
#'   input$TM_Timeline_Range: Topic Model timeline range
#'   input$TM_Timeline_Rank: Topic Model timeline rank
#'   input$TM_Timeline_Measure: Topic Model timeline measure (month, year)
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

#' render datatable with topics displayed by reflecting words, click rows to select for subcollections
#' depends on:
#'   values$tm_timeline_ids: Topic Models timeline ids
#'   values$tm_relevance: Topic Model relevance
#'   values$tm_sub_selected: Topic Model selected subcollection
#'   values$tm_random2: Topic Model random setting
#'   values$tm_phi: phi in Topic Models Tab
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
    if((length(isolate(values$tm_sub_selected))+1)<nrow(words)){
      print("ersatz")
      isolate(values$tm_sub_selected<-c(isolate(values$tm_sub_selected),FALSE))
    }
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

#' render interface to manage subcollection
#' depends on: 
#'   values$tm_sub_selected: Topic Model selected subcollection
#'   values$tm_info: Topic Model information
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
#' check the selectes Topic Model method
#' depends on:
#'  values$tm_method: selected Topic Model method
output$tm_method<-reactive({
  values$tm_method
})

#' update the content formula for parameter 
#' depends on:
#'  values$tm_stm_parameters_contentFormula: stm content formula with parameters
output$tm_stm_parameters_contentFormula <- reactive({
  values$tm_stm_parameters_contentFormula
})

outputOptions(output, "tm_stm_parameters_contentFormula", suspendWhenHidden = FALSE)

#' check if stm content formula is set
#' depends on:
#'  values$tm_stm_parameters_contentFormula: stm content formula with parameters
output$tm_stm_parameters_contentFormulaIsSet <- reactive({
  if(nchar(values$tm_stm_parameters_contentFormula)>0){
    return(TRUE)
  }else{
    return(FALSE)
  }
})
outputOptions(output, "tm_stm_parameters_contentFormulaIsSet", suspendWhenHidden = FALSE)


#' stm
#' plot.STM summary
#' depends on:
#'   values$tm_stm_model: choosen Topic Model stm
#'   input$tm_stm_visu_numberOfWordsToLabelTopic: Topic Model stm number of words and corresponding labels from visualisation
output$TM_stm_visu_summary <- renderPlot({
  if(nchar(values$tm_stm_parameters_contentFormula)>0){# if content formula was set in stm model, the label type is not selectable
    plot.STM(x = values$tm_stm_model, type = "summary", n = input$tm_stm_visu_numberOfWordsToLabelTopic)
  }else{
    plot.STM(x = values$tm_stm_model, type = "summary", n = input$tm_stm_visu_numberOfWordsToLabelTopic, labeltype = input$tm_stm_visu_labeltype, frexw = input$tm_stm_visu_frexweight)
    
  }
})
#' plot.STM labels
#' depends on:
#'   values$tm_stm_parameters_contentFormula: stm content formula parameters
#'   values$tm_stm_model: Topic Model stm
#'   input$tm_stm_visu_numberOfWordsToLabelTopic: Topic Model stm number of words and corresponding labels from visualisation
#'   input$tm_stm_visu_labeltype: Topic Model stm label type from visualisation
#'   input$tm_stm_visu_frexweight: Topic Model stm weight from visualisation
output$TM_stm_visu_labels <- renderPlot({
  if(nchar(values$tm_stm_parameters_contentFormula)>0){# if content formula was set in stm model, the label type is not selectable
    plot.STM(x = values$tm_stm_model, type = "labels", n = input$tm_stm_visu_numberOfWordsToLabelTopic)
  }else{
    plot.STM(x = values$tm_stm_model, type = "labels", n = input$tm_stm_visu_numberOfWordsToLabelTopic, labeltype = input$tm_stm_visu_labeltype, frexw = input$tm_stm_visu_frexweight)
  }
})

#' plot.STM perspectives
#' depends on:
#'   input$tm_stm_visu_perspectives_topic1: perspecitve Topic 1
#'   input$tm_stm_visu_perspectives_topic2: perspective Topic 2
#'   values$tm_stm_parameters_contentFormula: parameter from content formular
#'   input$tm_stm_visu_perspectives_covariateValue1: additional parameters 
#'   input$tm_stm_visu_perspectives_covariateValue2: additional parameters
#'   values$tm_stm_model: selected Topic Model stm
#'   input$tm_stm_visu_numberOfWordsToLabelTopic: Topic Model stm number of words and corresponding labels from visualisation
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

#' plot.STM hist
#' depends on:
#'   values$tm_stm_parameters_contentFormula: content formula set in stm model
#'   input$tm_stm_visu_numberOfWordsToLabelTopic: Topic Model stm number of words and corresponding labels from visualisation
#'   values$tm_stm_model: current STM model
#'   input$tm_stm_visu_labeltype: labeltype of used visualisation from stm
#'   input$tm_stm_visu_frexweight: Topic Model stm frex-weight from visualisation
output$TM_stm_visu_hist <- renderPlot({
  if(nchar(values$tm_stm_parameters_contentFormula)>0){# if content formula was set in stm model, the label type is not selectable
    plot.STM(x = values$tm_stm_model, type = "hist", n = input$tm_stm_visu_numberOfWordsToLabelTopic)
  }else{
    plot.STM(x = values$tm_stm_model, type = "hist", n = input$tm_stm_visu_numberOfWordsToLabelTopic, labeltype = input$tm_stm_visu_labeltype, frexw = input$tm_stm_visu_frexweight)
  }
})


#' topic correlation
#' depends on:
#'   input$tm_stm_visu_topicCorr_start: topic correlation Startpoint for the visualisation of the stm Topic Model
#'   values$tm_stm_visu_topicCorr_show: topic correlation initiate visualisation
observeEvent(input$tm_stm_visu_topicCorr_start,{
  values$tm_stm_visu_topicCorr_show <- TRUE
})

#' start process of displaying topic correlation
#' depends on:
#'  values$tm_stm_visu_topicCorr_show: topic correlation initiate visualisation
output$TM_stm_visu_topicCorr_show<-reactive({
  return(values$tm_stm_visu_topicCorr_show)
})

#' calculation of topic correlation
#' depends on: 
#'   values$tm_stm_visu_topicCorr_method: method of the used topic correlation
#'   values$tm_stm_model: used stm model
output$TM_stm_visu_topicCorr_calc <- renderPlot({
  values$tm_stm_visu_topicCorr_method <- "simple"
  topicCorrResult <- topicCorr(model = values$tm_stm_model, method = values$tm_stm_visu_topicCorr_method)
  plot.topicCorr(x = topicCorrResult)
})
outputOptions(output, "TM_stm_visu_topicCorr_show", suspendWhenHidden = FALSE)


#' estimateEffect
#' depends on:
#'   input$tm_stm_visu_estimateEffect_calcButton: information if calculation button for the estimated effect was pressed
#'   input$tm_stm_visu_estimateEffect_metaVarsToConvertToFactor: convert meta Variables to factors
#'   input$tm_stm_visu_estimateEffect_metaVarsToConvertToNumeric: convert meta Variables to numerix values
#'   input$tm_stm_visu_estimateEffect_calcParam_formula: user provided formula 
#'   values$tm_stm_visu_estimateEffectResult: results from estimate effect calculation
#'   values$tm_stm_model: used stm model
#'   values$tm_stm_metaDataConverted: converted meta data from stm
#'   values$tm_stm_visu_estimateEffect_show: should the calculatet estimated effect be shown
#'   values$tm_stm_visu_estimateEffect_plot_show: should the calculatet estimated effect plot be shown 
#'   values$tm_stm_metaData: stm meta data
observeEvent(input$tm_stm_visu_estimateEffect_calcButton,{
  # convert to factors and numeric
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

#' visualization of estimate effect
#' depends on:
#'    values$tm_stm_visu_estimateEffect_show: should the calculatet estimated effect be shown
output$TM_stm_visu_estimateEffect_show<-reactive({
  values$tm_stm_visu_estimateEffect_show
})

outputOptions(output, "TM_stm_visu_estimateEffect_show", suspendWhenHidden = FALSE)


#' estimate effect summary
#' depends on:
#'  values$tm_stm_visu_estimateEffectResult
output$TM_stm_visu_estimateEffect_summary <- renderPrint({
  summary(values$tm_stm_visu_estimateEffectResult)
})

#' estimate effect plot
#' depends on:
#'  values$tm_stm_visu_estimateEffect_plot_show: should the calculatet estimated effect plot be shown 
observeEvent(input$tm_stm_visu_estimateEffect_plotupdate,{
  values$tm_stm_visu_estimateEffect_plot_show <- TRUE
})

#' plot the vizualisation of estimate effect
#' depends on:
#'  values$tm_stm_visu_estimateEffect_plot_show: should the calculatet estimated effect plot be shown 
output$TM_stm_visu_estimateEffect_plot_show<-reactive({
  values$tm_stm_visu_estimateEffect_plot_show
})

#' plot estimate Effect
#' depends on:
#'   input$tm_stm_visu_estimateEffect_plot_difference_covValue1: changed additional parameter by the user
#'   input$tm_stm_visu_estimateEffect_plot_difference_covValue2: changed additional parameter by the user
#'   values$tm_stm_visu_estimateEffectResult: result from estimate effect calculation
#'   input$tm_stm_visu_estimateEffect_plot_covariate: covariate from estimate effect visualisation
#'   input$tm_stm_visu_estimateEffect_plot_topics: topics from estimate effect visualisation
#'   values$tm_stm_metaData: stm meta data
#'   values$tm_stm_visu_estimateEffect_plot_show: should the calculatet estimated effect plot be shown 
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

#' subcollection of the Topic models
#' depends on:
#'   values$tm_random2: random settings fr topic models
#'   values$tm_sub_selected: selected subcollection from the topic model
observe({
  values$tm_random2
  values$tm_sub_selected<-unlist(lapply(X = 1:length(isolate(values$tm_timeline_ids)),FUN = function(x){
    input[[paste0("tmsubcoll_",x)]]
  }))
  print(isolate(values$tm_sub_selected))
  #browser()
})

#' save subcollection from topic model
#' depends on:
#'   input$TM_Subcollection_save: save subcollection
#'   values$tm_timeline_ids: ids from topic model timeline
#'   values$tm_sub_selected: selected subcollection items 
#'   input$TM_Subcollection_Name: name of the subcollection
#'   values$tm_info: topic model information
#'   values$tm_theta: topic model theta
#'   input$TM_Timeline_Rank1: topic model rank
#'   values$current_task_id: current topic model task id
#'   values$update_solr_url: updated solr url
#'   values$update_solr_port: updated solr port
#'   values$coll_saved: saved subcollection 
#'   values$num_collections: number of the collection
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
        values$coll_saved<-runif(1,min = 0,max = 1)
        values$num_collections<-length(list.files("collections/collections/"))
        shinyWidgets::sendSweetAlert(type = "success",session = session,title =  paste0("Created Subcollection '",input$TM_Subcollection_Name,"' with ",dim(ids_subcoll)[1]," documents!"))
      }
    }
  }
})
#' if reset button is clicked, set selected topics to NULL
#' depends on:
#'   values$tm_timeline_ids: topic model timeline ids
#'   values$tm_sub_selected: selected topic model subcollection
observeEvent(input$TM_Timeline_Reset,{
  values$tm_timeline_ids<-NULL
  values$tm_sub_selected<-NULL
})



##################################################################################################
#                             topic coherence                                                    #
##################################################################################################

#' intiate topic coherence
#' depends on:
#'   input$TM_Coherence_start: start order for topic model coherence
#'   values$Details_Data_TM: topic model detailed data
#'   values$topic_intrusion_results: result of topic intrusion
#'   values$right_prediction: topic prediction - the right predicted ones
#'   values$word_intrusion_results: results of word intrusion
#'   values$right_prediction_word: word prediction - the right predicted ones
#'   values$TM_Coherence_dtm: topic model coherence- document term matrix
#'   values$TM_Coherence_documents: topic model coherence documents
#'   values$TM_Coherence_show: show the topic model coherence
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

#' show topic model coherence
#' depends on:
#'  values$TM_Coherence_show: initiate showing the coherence
output$TM_Coherence_show<-reactive({
  values$TM_Coherence_show
})
outputOptions(output, "TM_Coherence_show", suspendWhenHidden = FALSE)

#' plot toic coherence
#' depends on:
#'   values$TM_Coherence_dtm: topic model coherence document term matrix
#'   values$tm_phi: phi in Topic Models Tab
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

#' render plot 
#' depends on:
#'  values$topic_coherence_results: result for topic coherence
output$TM_Coherence_topic_coherence_mean_box<-renderValueBox({
  valueBox(subtitle = "avg. coherence",icon = icon("list"),value = round(mean(values$topic_coherence_results),3))
})


#############topic intrusion#################
#' render coherence topic intrusion
#' depends on:
#'  values$topic_intrusion_random_doc_number: choose random doc number for topic intrsusion
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

#' show topic model intrusion
#' depends on:
#'   values$TM_Intrusion_show: controll if topic model intrusion is shown
output$TM_Intrusion_show<-reactive({
  values$TM_Intrusion_show
})
outputOptions(output, "TM_Intrusion_show", suspendWhenHidden = FALSE)

#' start tooic intrusion
#' depends on:
#'   values$TM_topic_intrusion_run: information if topic intrusion is running
#'   values$TM_Intrusion_show: information if topic intrusion should be shown 
observeEvent(input$TM_Coherence_topic_intrusion_start,{
  values$TM_topic_intrusion_run<-1
  values$TM_Intrusion_show<-TRUE
  shinyjs::useShinyjs()
  isolate(shinyjs::runjs('Shiny.onInputChange(\"wrong_topic\",  "topic_intrusion_button_0")'))
})

#' reset topic intrusion
#' depends on:
#'   values$topic_intrusion_results: current results from topic intrusion
#'   values$right_prediction: right predicted topics
#'   values$TM_Intrusion_show: show topic model intrusion
#'   values$TM_topic_intrusion_docs: show documents from topic intrusion 
observeEvent(input$TM_Coherence_topic_intrusion_reset,{
  values$topic_intrusion_results<-data.frame(doc=numeric(0),IntruderT=numeric(0),IntruderG=numeric(0))
  values$TM_topic_intrusion_run<-NULL
  values$right_prediction<-0
  values$TM_Intrusion_show<-FALSE
  values$TM_topic_intrusion_docs<-0
})

#'render tabel for cohrerence topic intrusion
#'depends on:
#'   values$TM_Coherence_topic_intrusion_topics: cohrerence intrusion topics
#'   input$TM_Coherence_setsize: set the size of the topic model coherence
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

#' display document presentatopn for topic intrusion
#' depends on:
#'   values$TM_Coherence_topic_intrusion_documents: documents from topic intrusion
output$TM_Coherence_topic_intrusion_documents<-renderUI({
  document<-values$TM_Coherence_topic_intrusion_documents
  return(document)
})

#' run topic intrusion and get random documents
#' depends on:
#'   values$TM_topic_intrusion_run: run the topic intrusion
#'   input$TM_Coherence_runs: is topic model coherence running?
#'   values$tm_phi: phi in Topic Models Tab
#'   values$TM_Coherence_documents: documents for topic model coherence
#'   values$topic_intrusion_random_doc_number: random document number for topic intrusion
#'   values$host: used host
#'   values$port: used port
#'   values$tm_relevance: relvance od topic
#'   values$tm_theta: topic model theta
#'   input$TM_Coherence_setsize: set the size for topic model coherence
#'   values$topic_intrusion_results: result from topic intrusion
#'   values$TM_Coherence_topic_intrusion_topics: coherence topics from topic intrusion
#'   values$TM_Coherence_topic_intrusion_documents: documents from topic intrusion
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

#' display topic intrusion
#' depends on:
#'   values$TM_topic_intrusion_run: current topic intrusion run
#'   input$TM_Coherence_runs: all topic intrusion runs
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

#' wrong topics for the intrusion
#' depends on:
#'   input$wrong_topic: wrong topic choosen
#'   values$TM_topic_intrusion_run: current run of the topic intrusion
#'   input$TM_Coherence_runs: all runs from topic intrusion
#'   values$topic_intrusion_results: results from topic intrusion
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


#' render result box for topic intrusion
#' depends on:
#'   values$right_prediction: topic intrusion right predicted 
#'   input$TM_Coherence_chance_correction: topic model coherence chance correction
#'   input$TM_Coherence_setsize: set size of the topic model coherence
output$TM_Coherence_topic_intrusion_result_box<-renderValueBox({
  performance<-values$right_prediction
  if(input$TM_Coherence_chance_correction==TRUE){
    random_probabilty <- 1 / input$TM_Coherence_setsize
    performance <- (performance - random_probabilty) / (1 - random_probabilty)
    
  }
  valueBox(value = paste(round(performance*100,3),"%"),subtitle = "right predictions",icon = icon("list"))
})

#' render document box for coherence topic intrusion
#' depends on:
#'   values$TM_topic_intrusion_run: run topic intrusion
#'   values$TM_topic_intrusion_docs: docuemnts from topic intrusion
#'   values$topic_intrusion_results
output$TM_Coherence_topic_intrusion_docs_box<-renderValueBox({
  values$TM_topic_intrusion_run
  values$TM_topic_intrusion_docs
  valueBox(value = length(which(isolate(values$topic_intrusion_results[,3])!=0)),subtitle = "documents assessed",icon = icon("search"),color="purple")
})

#' display found intruders
#' depends on:
#'  values$topic_intrusion_results: results from topic intrusion
output$TM_Coherence_topic_intrusion_correct_box<-renderValueBox({
  valueBox(value = length(which((apply(values$topic_intrusion_results,1,FUN = function(x){x[2]==x[3]}))==TRUE)),subtitle = "intruders found",icon = icon("thumbs-up",lib="glyphicon"),color="yellow")
})

#' observe current topic intrusion run
#' depends on:
#'   values$TM_topic_intrusion_run: current topic intrusion run
#'   input$TM_Coherence_runs: all runs for cohrerence
#'   values$word_intrusion_results: results for word intrusion
#'   values$Details_Data_TM: topic model data details
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

#' render word intrusion
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

#' show word intrusion
#' depends on:
#'  values$TM_Intrusion_word_show: show word intrusion
output$TM_Intrusion_word_show<-reactive({
  values$TM_Intrusion_word_show
})
outputOptions(output, "TM_Intrusion_word_show", suspendWhenHidden = FALSE)

#' start word intrusion
#' depends on:
#'   values$TM_word_intrusion_run: run word intrusion
#'   values$TM_Intrusion_word_show: show word intrusion
observeEvent(input$TM_Coherence_word_intrusion_start,{
  values$TM_word_intrusion_run<-1
  values$TM_Intrusion_word_show<-TRUE
})

#' reset word intrusion
#' depends on:
#'   input$TM_Coherence_word_intrusion_reset: was reset button prest?
#'   values$TM_word_intrusion_run: run word intrusion
#'   values$right_prediction_word: made word prediction
#'   values$TM_Intrusion_word_show: show word intrusion
#'   values$TM_word_intrusion_docs: documents for word intrusion
observeEvent(input$TM_Coherence_word_intrusion_reset,{
  values$word_intrusion_results<-data.frame(doc=numeric(0),IntruderT=numeric(0),IntruderG=numeric(0))
  values$TM_word_intrusion_run<-NULL
  values$right_prediction_word<-0
  values$TM_Intrusion_word_show<-FALSE
  values$TM_word_intrusion_docs<-0
})

#' render table for coherence word intrusion 
#' depends on:
#'   values$TM_Coherence_word_intrusion_words: words for cohrerence intrusion
#'   input$TM_Coherence_setsize: set size for coherence
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

#' display word cloud for word intrusion
#' depends on:
#'  values$TM_Coherence_word_intrusion_words: words for word intrusion
output$TM_Coherence_word_intrusion_wordcloud<-renderWordcloud2({
  data = data.frame(words= values$TM_Coherence_word_intrusion_words,counts=rep(1,length(values$TM_Coherence_word_intrusion_words)),stringsAsFactors = F)
  wordcloud2(data = data,fontFamily = "Helvetica",backgroundColor = "azure",color = "random-dark",size=1.5/input$TM_Coherence_setsize,minRotation = -pi/2, maxRotation = -pi/2)
})

#' run word intrusion
#' depends on:
#'   values$TM_word_intrusion_run: run word intrusion
#'   values$word_intrusion_random_topic_number: random topic number for word intrusion
#'   values$tm_phi: Topic model phi
#'   values$word_intrusion_results: results from word intruision
#'   values$TM_Coherence_word_intrusion_words: words from word intrusion
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


#' iteration for word intrusion
#' depends on:
#'   values$TM_word_intrusion_run: run word intrusion
#'   input$TM_Coherence_runs: all coherence runs
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

#' chooses wrong words for intrusion
#' depends on:
#'   input$wrong_word: wrong word in intrusion
#'   values$TM_word_intrusion_run: run the word intrusion
#'   input$TM_Coherence_runs: all the coherence runs
#'   values$word_intrusion_results: results of word intrusion
#'   values$right_prediction_word: right predicted words
#'   values$TM_word_intrusion_run: run word intrusion
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


#' display result box
#' depens on:
#'   values$right_prediction_word: rate of right predicted words
#'   input$TM_Coherence_chance_correction: coherence correction chance
#'   input$TM_Coherence_setsize: set coherence size
output$TM_Coherence_word_intrusion_result_box<-renderValueBox({
  performance<-values$right_prediction_word
  if(input$TM_Coherence_chance_correction==TRUE){
    random_probabilty <- 1 / input$TM_Coherence_setsize
    performance <- (performance - random_probabilty) / (1 - random_probabilty)
  }
  valueBox(value = paste(round(performance*100,3),"%"),subtitle = "right predictions",icon = icon("list"))
})


#' display intrusion box 
#' depends on:
#'   values$TM_word_intrusion_docs: documents for word intrusion
#'   values$TM_word_intrusion_run: run word intrusion
output$TM_Coherence_word_intrusion_docs_box<-renderValueBox({
  values$TM_word_intrusion_docs
  values$TM_word_intrusion_run
  valueBox(value = length(which(isolate(values$word_intrusion_results[,3])!=0)),subtitle = "documents assessed",icon = icon("search"),color="purple")
})

#' check box for correct predicted word intrusions
#' depends on:
#'  values$word_intrusion_results: results from word intrusion
output$TM_Coherence_word_intrusion_correct_box<-renderValueBox({
  valueBox(value = length(which((apply(values$word_intrusion_results,1,FUN = function(x){x[2]==x[3]}))==TRUE)),subtitle = "intruders found",icon = icon("thumbs-up",lib="glyphicon"),color="yellow")
})

#' observe running process for topic model word intrusion
#' depends on:
#'   values$TM_word_intrusion_run: run word intrusion
#'   values$word_intrusion_results: results from word intrusion
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


#' topic model dictionary of possible topics
#' depends on:
#'  values$tm_number_of_topics: number of topics from topic model
output$TM_dict_topics_ui<-renderUI({
  checkboxGroupInput(inputId = "TM_dict_topics",label = "Topcis",
                     choiceNames =  paste("Topic",1:values$tm_number_of_topics,sep=" ") ,inline = T,choiceValues = 1:values$tm_number_of_topics
  )
})

#' category names from topic model dictionary
#' depends on:
#'   input$TM_dict_topics: topics from dictionary
#'   values$TM_dict_headers_help: header help for the dictionary
#'   values$tm_phi: topic model phi
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

#' observe potential changes in topic dictionares
#' depends on: 
#'   input$TM_dict_topics: topics in dictionary
#'   values$tm_number_of_topics: number of topics in topic model
#'   values$TM_dict_headers_help: help headers for dictionary
#'   values$TM_dict_headers: headers from dictionary
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

#' show saved topic model dictionaries
#' depends on:
#'  input$TM_dict_topics: topic dictionaries
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


#' observe saving process for topic model dictionary
#' depends on: 
#'   values$TM_dict_headers: dictionary headers (contains of category names)
#'   input$TM_dict_name: name for the dictionary
#'   input$TM_dict_topics: topics from dictionary
#'   values$tm_phi: topic model phi
#'   input$TM_dict_number_of_words: number of words in dictionary
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

######################################################################
#                         Metadata                                   #
######################################################################

#' output for metadata information
#' depends on:
#'   values$Details_Data_TM: details for data of topic models
output$TM_meta_ui<-renderUI({
  validate(
    need(file.exists(paste0(values$Details_Data_TM,"/meta_TM.RData")),message="no detailed metadata analyis selected in task scheduler")
  )
  return(tagList(
    tabsetPanel(id = "TM_meta",
                tabPanel(title = "Overview",
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
                         uiOutput(outputId = "Det_TM_Meta11"),
                         uiOutput(outputId = "Det_TM_Meta12"),
                         uiOutput(outputId = "Det_TM_Meta13"),
                         uiOutput(outputId = "Det_TM_Meta14"),
                         uiOutput(outputId = "Det_TM_Meta15"),
                         uiOutput(outputId = "Det_TM_Meta16"),
                         uiOutput(outputId = "Det_TM_Meta17"),
                         uiOutput(outputId = "Det_TM_Meta18"),
                         uiOutput(outputId = "Det_TM_Meta19"),
                         tags$hr(),
                         tags$h4("Scatter plot"),
                         fluidRow(style="margin-left:0px;margin-right:0px",
                                  column(12,
                                         uiOutput("Det_TM_scatter_plot_ui")%>%withSpinner()
                                  )
                         )
                ),
                tabPanel(title="Membership Heatmap",
                         fluidRow(style="margin-left:0px;margin-right:0px",
                                  column(12,
                                         #plotly::plotlyOutput(outputId = "Det_TM_meta_membership_heatmap")%>%withSpinner()
                                         DT::dataTableOutput(outputId = "Det_TM_meta_membership_heatmap_table")%>%withSpinner()
                                  )
                         )
                ),
                tabPanel(title = "Documents",
                         tags$div(style="overflow-x:auto;overflow-y:auto;",
                                  tags$h4("found documents"),
                                  DT::dataTableOutput(outputId = "Det_TM_Meta_Table")
                         )
                ),
                tabPanel(title = "Topic Meta Correlation",
                         tags$div(style="overflow-x:auto;overflow-y:auto;",
                                  tags$h4("Correlations"),
                                  DT::dataTableOutput(outputId = "Det_TM_Meta_Correlations_table_simple")%>%withSpinner()
                         )
                )
    )
  ))
})

#' show deteministic topic model heatmap table for membership of the documents
#' depens on:
#'   input$Det_meta_select: selected meta data
#'   values$tm_theta: topic model theta
#'   values$TM_meta: topic model meta data
#'   input$TM_meta_Rank1: rank for topic model meta data
#'   input$TM_meta_Prob: meta data with topic probability
#'   input$Det_TM_meta_multi_valued: multi values from meta data selected
#'   input$Det_TM_meta_multi_valued_seperator: seperator for the selected multi values 
#'   input$Det_TM_meta_use_quantiles: should quantiles be used?
output$Det_TM_meta_membership_heatmap_table<-DT::renderDataTable({
  validate(
    need(!is.null(input$Det_meta_select),message=F)
  )
  theta<-values$tm_theta
  meta_all<-values$TM_meta[,input$Det_meta_select]
  if(input$TM_meta_Rank1==TRUE){
    rank1<-apply(X = theta,MARGIN = 1,which.max)
    theta[,]<-0
    for(i in 1:length(rank1)){
      theta[i,rank1[i]]<-1
    }
  }
  else{
    greater_than_treshold<-which(theta>=input$TM_meta_Prob,arr.ind = T)
    theta[,]<-0
    theta[greater_than_treshold]<-1
  }
  data<-data.frame(cbind(meta_all,theta),stringsAsFactors = F)
  for(j in 2:ncol(data)){
    class(data[,j])<-"numeric"
  }
  
  if(input$Det_TM_meta_multi_valued==TRUE){
    meta_all <- trimws(unlist(stringr::str_split(string = data[,1],pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both")
  }
  if(input$Det_TM_meta_use_quantiles==TRUE){
    if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
      data[,1]<-get_quantile_belonging(all_data = meta_all,sample = data[,1])
      data[is.na(data[,1]),1]<-"NA"
      meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
      meta_all[is.na(meta_all)]<-"NA"
      
    }
  }
  meta_classes<-unique(meta_all)
  result<-matrix(c(0),length(meta_classes),ncol(theta))
  for(i in 1:length(meta_classes)){
    print(i)
    # if data is numeric use == else use grepl for characters
    if(all(varhandle::check.numeric(v = data[,1],exceptions = "NA"))&&input$Det_TM_meta_use_quantiles!=TRUE){
      relevant_rows<-which(data[,1]==meta_classes[i])
    }
    else{
      if(input$Det_TM_meta_multi_valued==TRUE){
        relevant_rows<-which(unlist(lapply(X = 1:nrow(data),FUN = function(x){
          meta_classes[i]%in%trimws(unlist(stringr::str_split(string = data[x,1],pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both")
        })))
        meta_all <- trimws(unlist(stringr::str_split(string = data[,1],pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both")
      }
      else{
        relevant_rows<-which(data[,1]==meta_classes[i])
      }
      #relevant_rows<-which(grepl(pattern = meta_classes[i],x = data[,1],fixed = T))
    }
    result[i,]<-colSums(data[relevant_rows,2:ncol(data),drop=F])
  }
  
  rownames(result)<-meta_classes
  colnames(result)<-1:ncol(values$tm_theta)
  df<-data.frame(result,stringsAsFactors = F)
  
  rownames(df)<-paste0(meta_classes," (n=",table(meta_all)[meta_classes],")")
  colnames(df)<-paste0("Topic ",colnames(result)," (n=",colSums(df),")")
  df<-df[order(rowSums(df),decreasing = T),]
  #get colors 
  
  
  brks<-seq(min(df),max(df),((max(df)-min(df))/25))
  clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
  datatable(df,selection = "none",escape=F,class = 'cell-border stripe',options = list(dom="ltp",pageLength = 15,lengthMenu = list(c(10,25, -1), c("10","25", "All")),
                                                                                       columnDefs = list(
                                                                                         list(className = "dt-center", targets = "_all")
                                                                                       ) )) %>% formatStyle(names(df), backgroundColor = styleInterval(brks, clrs))
  
})


#' show meta data membership as heatmap
#' depends on:
#'   input$Det_meta_select: chosen meta data
#'   values$tm_theta: topic model theta
#'   values$TM_meta: topic model meta data
#'   input$TM_meta_Prob: topic model meta data for topic probability
#'   input$Det_TM_meta_multi_valued: selected multiple values from meta data
#'   input$Det_TM_meta_multi_valued_seperator: seperator for the multiple values from meta data
#'   input$Det_TM_meta_use_quantiles: should quantiles be used?
#'   tnmatrix$value: tn matrix values
#'   tnmatrix$topic: tn matrix topics
output$Det_TM_meta_membership_heatmap<-plotly::renderPlotly({
  validate(
    need(!is.null(input$Det_meta_select),message=F)
  )
  theta<-values$tm_theta
  meta_all<-values$TM_meta[,input$Det_meta_select]
  if(input$TM_meta_Rank1==TRUE){
    rank1<-apply(X = theta,MARGIN = 1,which.max)
    theta[,]<-0
    for(i in 1:length(rank1)){
      theta[i,rank1[i]]<-1
    }
  }
  else{
    greater_than_treshold<-which(theta>=input$TM_meta_Prob,arr.ind = T)
    theta[,]<-0
    theta[greater_than_treshold]<-1
  }
  data<-data.frame(cbind(meta_all,theta),stringsAsFactors = F)
  for(j in 2:ncol(data)){
    class(data[,j])<-"numeric"
  }
  
  if(input$Det_TM_meta_multi_valued==TRUE){
    meta_all <- trimws(unlist(stringr::str_split(string = data[,1],pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both")
  }
  if(input$Det_TM_meta_use_quantiles==TRUE){
    if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
      data[,1]<-get_quantile_belonging(all_data = meta_all,sample = data[,1])
      data[is.na(data[,1]),1]<-"NA"
      meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
      meta_all[is.na(meta_all)]<-"NA"
      
    }
  }
  meta_classes<-unique(meta_all)
  result<-matrix(c(0),length(meta_classes),ncol(theta))
  for(i in 1:length(meta_classes)){
    print(i)
    # if data is numeric use == else use grepl for characters
    if(all(varhandle::check.numeric(v = data[,1],exceptions = "NA"))&&input$Det_TM_meta_use_quantiles!=TRUE){
      relevant_rows<-which(data[,1]==meta_classes[i])
    }
    else{
      if(input$Det_TM_meta_multi_valued==TRUE){
        relevant_rows<-which(unlist(lapply(X = 1:nrow(data),FUN = function(x){
          meta_classes[i]%in%trimws(unlist(stringr::str_split(string = data[x,1],pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both")
        })))
        meta_all <- trimws(unlist(stringr::str_split(string = data[,1],pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both")
      }
      else{
        relevant_rows<-which(data[,1]==meta_classes[i])
      }
      #relevant_rows<-which(grepl(pattern = meta_classes[i],x = data[,1],fixed = T))
    }
    result[i,]<-colSums(data[relevant_rows,2:ncol(data),drop=F])
  }
  
  rownames(result)<-meta_classes
  colnames(result)<-1:ncol(values$tm_theta)
  result<-result[order(rownames(result)),]
  #create input format for ggplot
  dummylist1 <- rownames(result)
  dummylist2 <- colnames(result)
  # Create an empty (0 value) matrix topics X groups
  tnmatrix<-data.frame(topic=NULL,meta=NULL,value=NULL)
  for (i in 1:length(dummylist1)) {
    for (j in 1:length(dummylist2)) {
      tnmatrix<-rbind(tnmatrix,c(dummylist1[i],dummylist2[j],result[dummylist1[i],dummylist2[j]]))
    }
  }
  colnames(tnmatrix)<-c("meta","topic","value")
  class(tnmatrix$value)<-"numeric"
  class(tnmatrix$topic)<-"numeric"
  # browser()
  
  p<-ggplot(tnmatrix, aes(x = topic, y = meta, fill = value)) +
    geom_tile() +
    geom_text(aes(label = round(value, 1))) +
    scale_fill_gradient(low = "white", high = "darkred") +
    scale_x_continuous() + # pull labels to include (n=..)
    scale_y_discrete(labels = rownames(result)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 0)) +
    labs(title = "Affiliation and topic membership (minimum threshold = 1/15)")
  
  ggplotly(p, tooltip = c("text", "size"))
  
  
})


#' compile table for correlation meta data
#' depends on:
#'    values$tm_theta: topic model theta
#'    values$TM_meta: meta data for topic modeling
output$Det_TM_Meta_Correlations_table_simple<-DT::renderDataTable({
  validate(
    need(!is.null(values$tm_theta),message = F),
    need(!is.null(values$TM_meta),message=F)
  )
  theta<-values$tm_theta
  meta<-values$TM_meta
  validate(
    need(
      ncol(meta)>4,message="No Metadata found"
    )
  )
  meta<-meta[,5:ncol(meta),drop=F]
  results<-matrix(c(0),ncol(theta),ncol(meta))
  for(topic_i in 1:ncol(theta)){
    print(topic_i)
    for(meta_i in 1:ncol(meta)){
      df<- cbind(theta[,topic_i],meta[,meta_i])
      df<-data.frame(df)
      class(df$X1)<-"numeric"
      if(all(varhandle::check.numeric(v = df$X2,exceptions = "NA"))){
        class(df$X2)<-"numeric" 
      }
      try({
        results[topic_i,meta_i]<-mixed_assoc(df = df)[1,"assoc"]
      },silent = T)
    }
  }
  colnames(results)<-colnames(meta)
  rownames(results)<-paste0("Topic:",colnames(theta))
  results<-round(results,digits = 3)
  return(DT::datatable(data = results, selection = "none",escape=F,class = 'cell-border stripe',options = list(paging=F,dom="t"))%>%
           formatStyle(colnames(results),
                       background = styleColorBar(range(results), 'lightblue'),
                       backgroundSize = '98% 88%',
                       backgroundRepeat = 'no-repeat',
                       backgroundPosition = 'center')
  )
})


#' select meta information via UI
#' depends on:
#'   values$TM_meta: topic model meta data
#'   values$tm_theta: topic model theta
output$Det_meta_select_ui<-renderUI({
  validate(
    need(!is.null(values$TM_meta),message=F
    )
  )
  tagList(
    selectInput(inputId = "Det_meta_select",label = "Choose a Meta Category",choices = colnames(values$TM_meta)[5:length(colnames(values$TM_meta))]),
    checkboxInput(inputId ="Det_TM_meta_multi_valued", label = "Metadata field multi valued?", FALSE),
    conditionalPanel(condition="input.Det_TM_meta_multi_valued==true",
                     textInput(inputId = "Det_TM_meta_multi_valued_seperator", label = "value seperator", value = "/")
    ),
    checkboxInput(inputId ="Det_TM_meta_use_quantiles", label = "Use Quantiles if metadata is numeric", TRUE),
    conditionalPanel(condition='input.Det_TM_meta_use_quantiles==true',
                     tableOutput(outputId = "Det_TM_meta_quantiles")
    ),
    numericInput(inputId = "Det_meta_topic",label="Which topic should be analyzed?",value = 1,min = 1,max = dim(values$tm_theta)[2],step = 1),
    materialSwitch(inputId = "TM_meta_Rank1",label = "Use Rank1 for selecting document memebership",value = T,status = "warning"),
    conditionalPanel(condition = 'input.TM_meta_Rank1==false',
                     knobInput(inputId = "TM_meta_Prob",label = "Minimal Probability",value = 0.5,min = 0,max = 1,step = 0.001)
    ),
    sliderInput(inputId = "Det_TM_meta_min_occurrences_for_pie", label = "minimal occurrences to include in pie chart",
                min = 1, max = 30, value = 1)
    
  )
})

#' select meta data from ui version 2
#' depends on:
#'    values$TM_meta: topic model meta data
#'    input$Det_meta_select: selected meta data
output$Det_meta_select_ui2<-renderUI({
  validate(
    need(!is.null(values$TM_meta),message=F
    )
  )
  if(is.null(input$Det_meta_select)){
    return(
      conditionalPanel(condition = 'input.TM_meta=="Overview"',
                       selectizeInput(inputId = "Det_meta_select2",label = "Choose a second meta category",choices = colnames(values$TM_meta)[5:length(colnames(values$TM_meta))])
      )
    )
  }
  else{
    return(conditionalPanel(condition = 'input.TM_meta=="Overview"',
                            selectizeInput(inputId = "Det_meta_select2",label = "Choose a second meta category",choices = setdiff(colnames(values$TM_meta)[5:length(colnames(values$TM_meta))],input$Det_meta_select))
    )
    )
  }
  
})

#' show first wordcloud of a certain topic
#' depends on:
#'    values$tm_phi: topic model phi
#'    input$Det_meta_topic: topics from meta data
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
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1,minRotation = -pi/2, maxRotation = -pi/2)
})

#' show wordcloud of a second topic
#' depends on:
#'    values$tm_phi: topic model phi
#'    input$Det_meta_topic: topics from meta data
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
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1, minRotation = -pi/2, maxRotation = -pi/2)
})
#' show wordcloud of a 3th topic
#' depends on:
#'    values$tm_phi: topic model phi
#'    input$Det_meta_topic: topics from meta data
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
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1, minRotation = -pi/2, maxRotation = -pi/2)
})

#' show wordcloud of a 4th topic
#' depends on:
#'    values$tm_phi: topic model phi
#'    input$Det_meta_topic: topics from meta data
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
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1, minRotation = -pi/2, maxRotation = -pi/2)
})

#' show wordcloud of a 5th topic
#' depends on:
#'    values$tm_phi: topic model phi
#'    input$Det_meta_topic: topics from meta data
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
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1, minRotation = -pi/2, maxRotation = -pi/2)
})

#' show wordcloud of a 6th topic
#' depends on:
#'    values$tm_phi: topic model phi
#'    input$Det_meta_topic: topics from meta data
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
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1, minRotation = -pi/2, maxRotation = -pi/2)
})
#' show wordcloud of a 7th topic
#' depends on:
#'    values$tm_phi: topic model phi
#'    input$Det_meta_topic: topics from meta data
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
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1, minRotation = -pi/2, maxRotation = -pi/2)
})
#' show wordcloud of a 8th topic
#' depends on:
#'    values$tm_phi: topic model phi
#'    input$Det_meta_topic: topics from meta data
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
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1, minRotation = -pi/2, maxRotation = -pi/2)
})
#' show wordcloud of a 9th topic
#' depends on:
#'    values$tm_phi: topic model phi
#'    input$Det_meta_topic: topics from meta data
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
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1, minRotation = -pi/2, maxRotation = -pi/2)
})
#' show wordcloud of a 10th topic
#' depends on:
#'    values$tm_phi: topic model phi
#'    input$Det_meta_topic: topics from meta data
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
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1, minRotation = -pi/2, maxRotation = -pi/2)
})
#' show wordcloud of a 11th topic
#' depends on:
#'    values$tm_phi: topic model phi
#'    input$Det_meta_topic: topics from meta data
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
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1, minRotation = -pi/2, maxRotation = -pi/2)
})
#' show wordcloud of a 12th topic
#' depends on:
#'    values$tm_phi: topic model phi
#'    input$Det_meta_topic: topics from meta data
output$Det_TM_meta_wordcloud12<-renderWordcloud2({
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
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1, minRotation = -pi/2, maxRotation = -pi/2)
})
#' show wordcloud of a 13th topic
#' depends on:
#'    values$tm_phi: topic model phi
#'    input$Det_meta_topic: topics from meta data
output$Det_TM_meta_wordcloud13<-renderWordcloud2({
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
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1, minRotation = -pi/2, maxRotation = -pi/2)
})
#' show wordcloud of a 14th topic
#' depends on:
#'    values$tm_phi: topic model phi
#'    input$Det_meta_topic: topics from meta data
output$Det_TM_meta_wordcloud14<-renderWordcloud2({
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
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1, minRotation = -pi/2, maxRotation = -pi/2)
})
#' show wordcloud of a 15th topic
#' depends on:
#'    values$tm_phi: topic model phi
#'    input$Det_meta_topic: topics from meta data
output$Det_TM_meta_wordcloud15<-renderWordcloud2({
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
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1, minRotation = -pi/2, maxRotation = -pi/2)
})
#' show wordcloud of a 16th topic
#' depends on:
#'    values$tm_phi: topic model phi
#'    input$Det_meta_topic: topics from meta data
output$Det_TM_meta_wordcloud16<-renderWordcloud2({
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
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1, minRotation = -pi/2, maxRotation = -pi/2)
})
#' show wordcloud of a 17th topic
#' depends on:
#'    values$tm_phi: topic model phi
#'    input$Det_meta_topic: topics from meta data
output$Det_TM_meta_wordcloud17<-renderWordcloud2({
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
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1, minRotation = -pi/2, maxRotation = -pi/2)
})
#' show wordcloud of a 18th topic
#' depends on:
#'    values$tm_phi: topic model phi
#'    input$Det_meta_topic: topics from meta data
output$Det_TM_meta_wordcloud18<-renderWordcloud2({
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
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1, minRotation = -pi/2, maxRotation = -pi/2)
})
#' show wordcloud of a 19th topic
#' depends on:
#'    values$tm_phi: topic model phi
#'    input$Det_meta_topic: topics from meta data
output$Det_TM_meta_wordcloud19<-renderWordcloud2({
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
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1, minRotation = -pi/2, maxRotation = -pi/2)
})



#################################################
#           Detailed Metadata for selected Topic#
#################################################

#' scatter Plot of meta vs topic
#' depends on:
#'   input$Det_meta_topic: detailed meta data topic
#'   values$TM_meta: topic model meta data 
#'   values$tm_theta: topic model theta
#'   input$TM_meta_Rank1: topic model meta data rank
#'   input$Det_meta_select: selected detailed meta data
#'   input$Det_meta_select2: second selected detailed meta data
#'   input$TM_meta_Prob: probability for specific topic model meta data
#'   values$Det_TM_meta_scatter_data: scattered data from topic model meta data
output$Det_TM_scatter_plot_ui<-renderUI({
  validate(
    need(!is.null(input$Det_meta_topic),message=F)
  )
  theta<-round(values$tm_theta[,as.character(input$Det_meta_topic)],digits=4)
  data<-data.frame(id=names(theta),theta=theta,meta=values$TM_meta[,input$Det_meta_select],belongs_to_topic=rep(FALSE,length(theta)),meta2=values$TM_meta[,input$Det_meta_select2],title=values$TM_meta[,"title"],doc_id=values$TM_meta[,"id_doc"],stringsAsFactors = F)
  theta<-values$tm_theta
  if(input$TM_meta_Rank1==TRUE){
    ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
  }
  else{
    ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
  }
  data[ids,4]<-TRUE
  values$Det_TM_meta_scatter_data<-data
  return(tagList(
    plotly::plotlyOutput(outputId = "Det_TM_scatter_plot")
  ))
})

#' plot detailes topic model scatter 
#' depends on:
#'    values$Det_TM_meta_scatter_data: scattred data from detailes topic model meta data
#'    input$Det_meta_select: selected detailed meta data
#'    input$Det_meta_select2: second selected detailed meta data
output$Det_TM_scatter_plot<-plotly::renderPlotly({
  data<- values$Det_TM_meta_scatter_data
  meta_numeric<-all(varhandle::check.numeric(v = data$meta,exceptions = "NA"))
  meta2_numeric<-all(varhandle::check.numeric(v = data$meta2,exceptions = "NA"))
  axe_type_meta<-"category"
  axe_type_meta2<-"category"
  if(meta_numeric==TRUE){
    axe_type_meta<-"number"
  }
  if(meta2_numeric==TRUE){
    axe_type_meta2<-"number"
  }
  p<-plotly::plot_ly(data = data,x=~meta,y=~meta2,color=~belongs_to_topic,type="scatter",colors=c("tomato","forestgreen"),symbol=~belongs_to_topic,symbols = c("cross","diamond"),
                     marker=list(size=10), text=~paste("Title: ",title,"<br>topic proportion: ",theta))
  p<-plotly::layout(p,xaxis=list(title=input$Det_meta_select,type=axe_type_meta),
                    yaxis=list(title=input$Det_meta_select2,type=axe_type_meta2),
                    margin=list(b=100))
  return(p)
})

#' show first detailed topic model meta data
#' depends on:
#'    values$TM_meta: topic model meta data
#'    input$Det_meta_select: selected detailed meta data 
#'    input$TM_meta_Rank1: topic model meta data rank
#'    values$tm_theta: topic model theta 
#'    input$Det_meta_topic: detailed meta data topic
#'    input$TM_meta_Prob: topic model meta data probability
#'    input$Det_TM_meta_use_quantiles: used quantiles for detailed topic model meta data
#'    input$Det_TM_meta_multi_valued: multiple values selcted from detailed topic model meta data
#'    input$Det_TM_meta_multi_valued_seperator: seprator for choosen values from meta data
#'    input$Det_TM_meta_min_occurrences_for_pie: minimal occurence for topic model meta data to apperar in the pie chart
output$Det_TM_Meta1<-renderUI({
  if(colnames(values$TM_meta)[5]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    # if no document is found
    validate(
      need(length(ids)>0,message = "no document found that matches the selected settings")
    )
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),input$Det_meta_select]
    meta_all<-values$TM_meta[,input$Det_meta_select]
    # check if > min occurrences
    # multi valued?
    if(input$Det_TM_meta_multi_valued==TRUE){
      meta <- trimws(unlist(stringr::str_split(string = meta,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both") 
      meta_all<-trimws(unlist(stringr::str_split(string = meta_all,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which="both")
    }
    if(input$Det_TM_meta_use_quantiles==TRUE){
      if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
        meta<-get_quantile_belonging(all_data = meta_all,sample = meta)
        meta[is.na(meta)]<-"NA"
        meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
        meta_all[is.na(meta_all)]<-"NA"
      }
    }
    counts_all<-as.data.frame(table(meta_all),stringsAsFactors = F)
    counts_all[,2]<-round(counts_all[,2]/ nrow(values$TM_meta),3)
    
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    # check if > min occurrences
    counts_pie <- counts[which(counts$Freq >= input$Det_TM_meta_min_occurrences_for_pie),]
    output$Det_TM_meta_plot_token<-renderPlotly({
      validate(
        need(nrow(counts_pie)>0,message="No results for current setting!")
      )
      p <- plot_ly(counts_pie, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = 'Meta distribution for meta category: "token"',legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_1<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/length(ids),digits = 3))
      #order data by absolute frequency
      counts <- counts[order(counts[,2], decreasing = T),]
      counts<-merge(counts,counts_all,by.x = 1,by.y = 1,all.y = F)
      colnames(counts)<-c(colnames(values$TM_meta)[5],"absolute","relative","overall percentage")
      counts<-counts[order(counts[,"relative"],decreasing = T),]
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

#' render second detailed topic model meta data presentation 
#' depends on:
#'   values$TM_meta: topic model meta data
#'   values$tm_theta: topic model theta
#'   input$TM_meta_Rank1: topic model meta data rank
#'   input$Det_meta_topic: detailed meta data topic
#'   input$TM_meta_Prob: topic model meta data probability
#'   input$Det_meta_select: selected detaoled meta data
#'   input$Det_TM_meta_min_occurrences_for_pie: min occurrences for detailed topic model meta data to appear in pie chart
#'   input$Det_TM_meta_multi_valued: use multiple values from detailed topic model meta data 
#'   input$Det_TM_meta_multi_valued_seperator: seperator for multiple values from detailed topic model
output$Det_TM_Meta2<-renderUI({
  if(colnames(values$TM_meta)[6]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    # if no document is found
    validate(
      need(length(ids)>0,message = "no document found that matches the selected settings")
    )
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),input$Det_meta_select]
    meta_all<-values$TM_meta[,input$Det_meta_select]
    # check if > min occurrences
    # multi valued?
    if(input$Det_TM_meta_multi_valued==TRUE){
      meta <- trimws(unlist(stringr::str_split(string = meta,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both") 
      meta_all<-trimws(unlist(stringr::str_split(string = meta_all,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which="both")
    }
    if(input$Det_TM_meta_use_quantiles==TRUE){
      if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
        meta<-get_quantile_belonging(all_data = meta_all,sample = meta)
        meta[is.na(meta)]<-"NA"
        meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
        meta_all[is.na(meta_all)]<-"NA"
      }
    }
    counts_all<-as.data.frame(table(meta_all),stringsAsFactors = F)
    counts_all[,2]<-round(counts_all[,2]/ nrow(values$TM_meta),3)  
    
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    counts_pie <- counts[which(counts$Freq >= input$Det_TM_meta_min_occurrences_for_pie),]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_2<-renderPlotly({
      validate(
        need(nrow(counts_pie)>0,message="No results for current setting!")
      )
      p <- plot_ly(counts_pie, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',input$Det_meta_select,'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_2<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/length(ids),digits = 3))
      #order data by absolute frequency
      counts <- counts[order(counts[,2], decreasing = T),]
      counts<-merge(counts,counts_all,by.x = 1,by.y = 1,all.y = F)
      colnames(counts)<-c(input$Det_meta_select,"absolute","relative","overall percentage")
      counts<-counts[order(counts[,"relative"],decreasing = T),]
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


#' show 3th detailed topic model meta data
#' depends on:
#'    values$TM_meta: topic model meta data
#'    input$Det_meta_select: selected detailed meta data 
#'    input$TM_meta_Rank1: topic model meta data rank
#'    values$tm_theta: topic model theta 
#'    input$Det_meta_topic: detailed meta data topic
#'    input$TM_meta_Prob: topic model meta data probability
#'    input$Det_TM_meta_use_quantiles: used quantiles for detailed topic model meta data
#'    input$Det_TM_meta_multi_valued: multiple values selcted from detailed topic model meta data
#'    input$Det_TM_meta_multi_valued_seperator: seprator for choosen values from meta data
#'    input$Det_TM_meta_min_occurrences_for_pie: minimal occurence for topic model meta data to apperar in the pie chart
output$Det_TM_Meta3<-renderUI({
  if(colnames(values$TM_meta)[7]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    # if no document is found
    validate(
      need(length(ids)>0,message = "no document found that matches the selected settings")
    )
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),input$Det_meta_select]
    meta_all<-values$TM_meta[,input$Det_meta_select]
    # check if > min occurrences
    # multi valued?
    if(input$Det_TM_meta_multi_valued==TRUE){
      meta <- trimws(unlist(stringr::str_split(string = meta,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both") 
      meta_all<-trimws(unlist(stringr::str_split(string = meta_all,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which="both")
    }
    if(input$Det_TM_meta_use_quantiles==TRUE){
      if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
        meta<-get_quantile_belonging(all_data = meta_all,sample = meta)
        meta[is.na(meta)]<-"NA"
        meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
        meta_all[is.na(meta_all)]<-"NA"
      }
    }
    counts_all<-as.data.frame(table(meta_all),stringsAsFactors = F)
    counts_all[,2]<-round(counts_all[,2]/ nrow(values$TM_meta),digits = 3)  
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    # check if > min occurrences
    counts_pie <- counts[which(counts$Freq >= input$Det_TM_meta_min_occurrences_for_pie),]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_3<-renderPlotly({
      validate(
        need(nrow(counts_pie)>0,message="No results for current setting!")
      )
      p <- plot_ly(counts_pie, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',input$Det_meta_select,'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_3<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/length(ids),digits = 3))
      #order data by absolute frequency
      counts <- counts[order(counts[,2], decreasing = T),]
      counts<-merge(counts,counts_all,by.x = 1,by.y = 1,all.y = F)
      colnames(counts)<-c(input$Det_meta_select,"absolute","relative","overall percentage")
      counts<-counts[order(counts[,"relative"],decreasing = T),]
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


#' show 4th detailed topic model meta data
#' depends on:
#'    values$TM_meta: topic model meta data
#'    input$Det_meta_select: selected detailed meta data 
#'    input$TM_meta_Rank1: topic model meta data rank
#'    values$tm_theta: topic model theta 
#'    input$Det_meta_topic: detailed meta data topic
#'    input$TM_meta_Prob: topic model meta data probability
#'    input$Det_TM_meta_use_quantiles: used quantiles for detailed topic model meta data
#'    input$Det_TM_meta_multi_valued: multiple values selcted from detailed topic model meta data
#'    input$Det_TM_meta_multi_valued_seperator: seprator for choosen values from meta data
#'    input$Det_TM_meta_min_occurrences_for_pie: minimal occurence for topic model meta data to apperar in the pie chart
output$Det_TM_Meta4<-renderUI({
  if(colnames(values$TM_meta)[8]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    # if no document is found
    validate(
      need(length(ids)>0,message = "no document found that matches the selected settings")
    )
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),input$Det_meta_select]
    meta_all<-values$TM_meta[,input$Det_meta_select]
    # check if > min occurrences
    # multi valued?
    if(input$Det_TM_meta_multi_valued==TRUE){
      meta <- trimws(unlist(stringr::str_split(string = meta,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both") 
      meta_all<-trimws(unlist(stringr::str_split(string = meta_all,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which="both")
    }
    if(input$Det_TM_meta_use_quantiles==TRUE){
      if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
        meta<-get_quantile_belonging(all_data = meta_all,sample = meta)
        meta[is.na(meta)]<-"NA"
        meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
        meta_all[is.na(meta_all)]<-"NA"
      }
    }
    counts_all<-as.data.frame(table(meta_all),stringsAsFactors = F)
    counts_all[,2]<-round(counts_all[,2]/ nrow(values$TM_meta),digits = 3)  
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    # check if > min occurrences
    counts_pie <- counts[which(counts$Freq >= input$Det_TM_meta_min_occurrences_for_pie),]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_4<-renderPlotly({
      validate(
        need(nrow(counts_pie)>0,message="No results for current setting!")
      )
      p <- plot_ly(counts_pie, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',input$Det_meta_select,'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_4<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/length(ids),digits = 3))
      #order data by absolute frequency
      counts <- counts[order(counts[,2], decreasing = T),]
      counts<-merge(counts,counts_all,by.x = 1,by.y = 1,all.y = F)
      colnames(counts)<-c(input$Det_meta_select,"absolute","relative","overall percentage")
      counts<-counts[order(counts[,"relative"],decreasing = T),]
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
#' show 5th detailed topic model meta data
#' depends on:
#'    values$TM_meta: topic model meta data
#'    input$Det_meta_select: selected detailed meta data 
#'    input$TM_meta_Rank1: topic model meta data rank
#'    values$tm_theta: topic model theta 
#'    input$Det_meta_topic: detailed meta data topic
#'    input$TM_meta_Prob: topic model meta data probability
#'    input$Det_TM_meta_use_quantiles: used quantiles for detailed topic model meta data
#'    input$Det_TM_meta_multi_valued: multiple values selcted from detailed topic model meta data
#'    input$Det_TM_meta_multi_valued_seperator: seprator for choosen values from meta data
#'    input$Det_TM_meta_min_occurrences_for_pie: minimal occurence for topic model meta data to apperar in the pie chart
output$Det_TM_Meta5<-renderUI({
  if(colnames(values$TM_meta)[9]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    # if no document is found
    validate(
      need(length(ids)>0,message = "no document found that matches the selected settings")
    )
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),input$Det_meta_select]
    meta_all<-values$TM_meta[,input$Det_meta_select]
    # check if > min occurrences
    # multi valued?
    if(input$Det_TM_meta_multi_valued==TRUE){
      meta <- trimws(unlist(stringr::str_split(string = meta,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both") 
      meta_all<-trimws(unlist(stringr::str_split(string = meta_all,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which="both")
    }
    if(input$Det_TM_meta_use_quantiles==TRUE){
      if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
        meta<-get_quantile_belonging(all_data = meta_all,sample = meta)
        meta[is.na(meta)]<-"NA"
        meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
        meta_all[is.na(meta_all)]<-"NA"
      }
    }
    counts_all<-as.data.frame(table(meta_all),stringsAsFactors = F)
    counts_all[,2]<-round(counts_all[,2]/ nrow(values$TM_meta),digits = 3)  
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    # check if > min occurrences
    counts_pie <- counts[which(counts$Freq >= input$Det_TM_meta_min_occurrences_for_pie),]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_5<-renderPlotly({
      validate(
        need(nrow(counts_pie)>0,message="No results for current setting!")
      )
      p <- plot_ly(counts_pie, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',input$Det_meta_select,'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_5<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/length(ids),digits = 3))
      #order data by absolute frequency
      counts <- counts[order(counts[,2], decreasing = T),]
      counts<-merge(counts,counts_all,by.x = 1,by.y = 1,all.y = F)
      colnames(counts)<-c(input$Det_meta_select,"absolute","relative","overall percentage")
      counts<-counts[order(counts[,"relative"],decreasing = T),]
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


#' show 6th detailed topic model meta data
#' depends on:
#'    values$TM_meta: topic model meta data
#'    input$Det_meta_select: selected detailed meta data 
#'    input$TM_meta_Rank1: topic model meta data rank
#'    values$tm_theta: topic model theta 
#'    input$Det_meta_topic: detailed meta data topic
#'    input$TM_meta_Prob: topic model meta data probability
#'    input$Det_TM_meta_use_quantiles: used quantiles for detailed topic model meta data
#'    input$Det_TM_meta_multi_valued: multiple values selcted from detailed topic model meta data
#'    input$Det_TM_meta_multi_valued_seperator: seprator for choosen values from meta data
#'    input$Det_TM_meta_min_occurrences_for_pie: minimal occurence for topic model meta data to apperar in the pie chart
output$Det_TM_Meta6<-renderUI({
  if(colnames(values$TM_meta)[10]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    # if no document is found
    validate(
      need(length(ids)>0,message = "no document found that matches the selected settings")
    )
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),input$Det_meta_select]
    meta_all<-values$TM_meta[,input$Det_meta_select]
    # check if > min occurrences
    # multi valued?
    if(input$Det_TM_meta_multi_valued==TRUE){
      meta <- trimws(unlist(stringr::str_split(string = meta,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both") 
      meta_all<-trimws(unlist(stringr::str_split(string = meta_all,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which="both")
    }
    if(input$Det_TM_meta_use_quantiles==TRUE){
      if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
        meta<-get_quantile_belonging(all_data = meta_all,sample = meta)
        meta[is.na(meta)]<-"NA"
        meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
        meta_all[is.na(meta_all)]<-"NA"
      }
    }
    counts_all<-as.data.frame(table(meta_all),stringsAsFactors = F)
    counts_all[,2]<-round(counts_all[,2]/ nrow(values$TM_meta),digits = 3) 
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    # check if > min occurrences
    counts_pie <- counts[which(counts$Freq >= input$Det_TM_meta_min_occurrences_for_pie),]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_6<-renderPlotly({
      validate(
        need(nrow(counts_pie)>0,message="No results for current setting!")
      )
      p <- plot_ly(counts_pie, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',input$Det_meta_select,'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_6<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/length(ids),digits = 3))
      #order data by absolute frequency
      counts <- counts[order(counts[,2], decreasing = T),]
      counts<-merge(counts,counts_all,by.x = 1,by.y = 1,all.y = F)
      colnames(counts)<-c(input$Det_meta_select,"absolute","relative","overall percentage")
      counts<-counts[order(counts[,"relative"],decreasing = T),]
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


#' show 7th detailed topic model meta data
#' depends on:
#'    values$TM_meta: topic model meta data
#'    input$Det_meta_select: selected detailed meta data 
#'    input$TM_meta_Rank1: topic model meta data rank
#'    values$tm_theta: topic model theta 
#'    input$Det_meta_topic: detailed meta data topic
#'    input$TM_meta_Prob: topic model meta data probability
#'    input$Det_TM_meta_use_quantiles: used quantiles for detailed topic model meta data
#'    input$Det_TM_meta_multi_valued: multiple values selcted from detailed topic model meta data
#'    input$Det_TM_meta_multi_valued_seperator: seprator for choosen values from meta data
#'    input$Det_TM_meta_min_occurrences_for_pie: minimal occurence for topic model meta data to apperar in the pie chart
output$Det_TM_Meta7<-renderUI({
  if(colnames(values$TM_meta)[11]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    # if no document is found
    validate(
      need(length(ids)>0,message = "no document found that matches the selected settings")
    )
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),input$Det_meta_select]
    meta_all<-values$TM_meta[,input$Det_meta_select]
    # check if > min occurrences
    # multi valued?
    if(input$Det_TM_meta_multi_valued==TRUE){
      meta <- trimws(unlist(stringr::str_split(string = meta,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both") 
      meta_all<-trimws(unlist(stringr::str_split(string = meta_all,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which="both")
    }
    if(input$Det_TM_meta_use_quantiles==TRUE){
      if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
        meta<-get_quantile_belonging(all_data = meta_all,sample = meta)
        meta[is.na(meta)]<-"NA"
        meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
        meta_all[is.na(meta_all)]<-"NA"
      }
    }
    counts_all<-as.data.frame(table(meta_all),stringsAsFactors = F)
    counts_all[,2]<-round(counts_all[,2]/ nrow(values$TM_meta),digits = 3) 
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    # check if > min occurrences
    counts_pie <- counts[which(counts$Freq >= input$Det_TM_meta_min_occurrences_for_pie),]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_7<-renderPlotly({
      validate(
        need(nrow(counts_pie)>0,message="No results for current setting!")
      )
      p <- plot_ly(counts_pie, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',input$Det_meta_select,'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_7<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/length(ids),digits = 3))
      #order data by absolute frequency
      counts <- counts[order(counts[,2], decreasing = T),]
      counts<-merge(counts,counts_all,by.x = 1,by.y = 1,all.y = F)
      colnames(counts)<-c(input$Det_meta_select,"absolute","relative","overall percentage")
      counts<-counts[order(counts[,"relative"],decreasing = T),]
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


#' show 8th detailed topic model meta data
#' depends on:
#'    values$TM_meta: topic model meta data
#'    input$Det_meta_select: selected detailed meta data 
#'    input$TM_meta_Rank1: topic model meta data rank
#'    values$tm_theta: topic model theta 
#'    input$Det_meta_topic: detailed meta data topic
#'    input$TM_meta_Prob: topic model meta data probability
#'    input$Det_TM_meta_use_quantiles: used quantiles for detailed topic model meta data
#'    input$Det_TM_meta_multi_valued: multiple values selcted from detailed topic model meta data
#'    input$Det_TM_meta_multi_valued_seperator: seprator for choosen values from meta data
#'    input$Det_TM_meta_min_occurrences_for_pie: minimal occurence for topic model meta data to apperar in the pie chart
output$Det_TM_Meta8<-renderUI({
  if(colnames(values$TM_meta)[12]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    # if no document is found
    validate(
      need(length(ids)>0,message = "no document found that matches the selected settings")
    )
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),input$Det_meta_select]
    meta_all<-values$TM_meta[,input$Det_meta_select]
    # check if > min occurrences
    # multi valued?
    if(input$Det_TM_meta_multi_valued==TRUE){
      meta <- trimws(unlist(stringr::str_split(string = meta,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both") 
      meta_all<-trimws(unlist(stringr::str_split(string = meta_all,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which="both")
    }
    if(input$Det_TM_meta_use_quantiles==TRUE){
      if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
        meta<-get_quantile_belonging(all_data = meta_all,sample = meta)
        meta[is.na(meta)]<-"NA"
        meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
        meta_all[is.na(meta_all)]<-"NA"
      }
    }
    counts_all<-as.data.frame(table(meta_all),stringsAsFactors = F)
    counts_all[,2]<-round(counts_all[,2]/ nrow(values$TM_meta),digits = 3) 
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    # check if > min occurrences
    counts_pie <- counts[which(counts$Freq >= input$Det_TM_meta_min_occurrences_for_pie),]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_8<-renderPlotly({
      validate(
        need(nrow(counts_pie)>0,message="No results for current setting!")
      )
      p <- plot_ly(counts_pie, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',input$Det_meta_select,'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_8<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/length(ids),digits = 3))
      #order data by absolute frequency
      counts <- counts[order(counts[,2], decreasing = T),]
      counts<-merge(counts,counts_all,by.x = 1,by.y = 1,all.y = F)
      colnames(counts)<-c(input$Det_meta_select,"absolute","relative","overall percentage")
      counts<-counts[order(counts[,"relative"],decreasing = T),]
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


#' show 9th detailed topic model meta data
#' depends on:
#'    values$TM_meta: topic model meta data
#'    input$Det_meta_select: selected detailed meta data 
#'    input$TM_meta_Rank1: topic model meta data rank
#'    values$tm_theta: topic model theta 
#'    input$Det_meta_topic: detailed meta data topic
#'    input$TM_meta_Prob: topic model meta data probability
#'    input$Det_TM_meta_use_quantiles: used quantiles for detailed topic model meta data
#'    input$Det_TM_meta_multi_valued: multiple values selcted from detailed topic model meta data
#'    input$Det_TM_meta_multi_valued_seperator: seprator for choosen values from meta data
#'    input$Det_TM_meta_min_occurrences_for_pie: minimal occurence for topic model meta data to apperar in the pie chart
output$Det_TM_Meta9<-renderUI({
  if(colnames(values$TM_meta)[13]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    # if no document is found
    validate(
      need(length(ids)>0,message = "no document found that matches the selected settings")
    )
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),input$Det_meta_select]
    meta_all<-values$TM_meta[,input$Det_meta_select]
    # check if > min occurrences
    # multi valued?
    if(input$Det_TM_meta_multi_valued==TRUE){
      meta <- trimws(unlist(stringr::str_split(string = meta,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both") 
      meta_all<-trimws(unlist(stringr::str_split(string = meta_all,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which="both")
    }
    if(input$Det_TM_meta_use_quantiles==TRUE){
      if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
        meta<-get_quantile_belonging(all_data = meta_all,sample = meta)
        meta[is.na(meta)]<-"NA"
        meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
        meta_all[is.na(meta_all)]<-"NA"
      }
    }
    counts_all<-as.data.frame(table(meta_all),stringsAsFactors = F)
    counts_all[,2]<-round(counts_all[,2]/ nrow(values$TM_meta),digits = 3) 
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    # check if > min occurrences
    counts_pie <- counts[which(counts$Freq >= input$Det_TM_meta_min_occurrences_for_pie),]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_9<-renderPlotly({
      validate(
        need(nrow(counts_pie)>0,message="No results for current setting!")
      )
      p <- plot_ly(counts_pie, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',input$Det_meta_select,'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_9<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/length(ids),digits = 3))
      #order data by absolute frequency
      counts <- counts[order(counts[,2], decreasing = T),]
      counts<-merge(counts,counts_all,by.x = 1,by.y = 1,all.y = F)
      colnames(counts)<-c(input$Det_meta_select,"absolute","relative","overall percentage")
      counts<-counts[order(counts[,"relative"],decreasing = T),]
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

#' show 10th detailed topic model meta data
#' depends on:
#'    values$TM_meta: topic model meta data
#'    input$Det_meta_select: selected detailed meta data 
#'    input$TM_meta_Rank1: topic model meta data rank
#'    values$tm_theta: topic model theta 
#'    input$Det_meta_topic: detailed meta data topic
#'    input$TM_meta_Prob: topic model meta data probability
#'    input$Det_TM_meta_use_quantiles: used quantiles for detailed topic model meta data
#'    input$Det_TM_meta_multi_valued: multiple values selcted from detailed topic model meta data
#'    input$Det_TM_meta_multi_valued_seperator: seprator for choosen values from meta data
#'    input$Det_TM_meta_min_occurrences_for_pie: minimal occurence for topic model meta data to apperar in the pie chart
output$Det_TM_Meta10<-renderUI({
  if(colnames(values$TM_meta)[14]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    # if no document is found
    validate(
      need(length(ids)>0,message = "no document found that matches the selected settings")
    )
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),input$Det_meta_select]
    meta_all<-values$TM_meta[,input$Det_meta_select]
    # check if > min occurrences
    # multi valued?
    if(input$Det_TM_meta_multi_valued==TRUE){
      meta <- trimws(unlist(stringr::str_split(string = meta,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both") 
      meta_all<-trimws(unlist(stringr::str_split(string = meta_all,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which="both")
    }
    if(input$Det_TM_meta_use_quantiles==TRUE){
      if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
        meta<-get_quantile_belonging(all_data = meta_all,sample = meta)
        meta[is.na(meta)]<-"NA"
        meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
        meta_all[is.na(meta_all)]<-"NA"
      }
    }
    counts_all<-as.data.frame(table(meta_all),stringsAsFactors = F)
    counts_all[,2]<-round(counts_all[,2]/ nrow(values$TM_meta),digits = 3) 
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    # check if > min occurrences
    counts_pie <- counts[which(counts$Freq >= input$Det_TM_meta_min_occurrences_for_pie),]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_10<-renderPlotly({
      validate(
        need(nrow(counts_pie)>0,message="No results for current setting!")
      )
      p <- plot_ly(counts_pie, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',input$Det_meta_select,'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_10<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/length(ids),digits = 3))
      #order data by absolute frequency
      counts <- counts[order(counts[,2], decreasing = T),]
      counts<-merge(counts,counts_all,by.x = 1,by.y = 1,all.y = F)
      colnames(counts)<-c(input$Det_meta_select,"absolute","relative","overall percentage")
      counts<-counts[order(counts[,"relative"],decreasing = T),]
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

#' show 11th detailed topic model meta data
#' depends on:
#'    values$TM_meta: topic model meta data
#'    input$Det_meta_select: selected detailed meta data 
#'    input$TM_meta_Rank1: topic model meta data rank
#'    values$tm_theta: topic model theta 
#'    input$Det_meta_topic: detailed meta data topic
#'    input$TM_meta_Prob: topic model meta data probability
#'    input$Det_TM_meta_use_quantiles: used quantiles for detailed topic model meta data
#'    input$Det_TM_meta_multi_valued: multiple values selcted from detailed topic model meta data
#'    input$Det_TM_meta_multi_valued_seperator: seprator for choosen values from meta data
#'    input$Det_TM_meta_min_occurrences_for_pie: minimal occurence for topic model meta data to apperar in the pie chart
output$Det_TM_Meta11<-renderUI({
  if(colnames(values$TM_meta)[15]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    # if no document is found
    validate(
      need(length(ids)>0,message = "no document found that matches the selected settings")
    )
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),input$Det_meta_select]
    meta_all<-values$TM_meta[,input$Det_meta_select]
    # check if > min occurrences
    # multi valued?
    if(input$Det_TM_meta_multi_valued==TRUE){
      meta <- trimws(unlist(stringr::str_split(string = meta,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both") 
      meta_all<-trimws(unlist(stringr::str_split(string = meta_all,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which="both")
    }
    if(input$Det_TM_meta_use_quantiles==TRUE){
      if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
        meta<-get_quantile_belonging(all_data = meta_all,sample = meta)
        meta[is.na(meta)]<-"NA"
        meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
        meta_all[is.na(meta_all)]<-"NA"
      }
    }
    counts_all<-as.data.frame(table(meta_all),stringsAsFactors = F)
    counts_all[,2]<-round(counts_all[,2]/ nrow(values$TM_meta),digits = 3) 
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    # check if > min occurrences
    counts_pie <- counts[which(counts$Freq >= input$Det_TM_meta_min_occurrences_for_pie),]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_11<-renderPlotly({
      validate(
        need(nrow(counts_pie)>0,message="No results for current setting!")
      )
      p <- plot_ly(counts_pie, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',input$Det_meta_select,'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_11<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/length(ids),digits = 3))
      #order data by absolute frequency
      counts <- counts[order(counts[,2], decreasing = T),]
      counts<-merge(counts,counts_all,by.x = 1,by.y = 1,all.y = F)
      colnames(counts)<-c(input$Det_meta_select,"absolute","relative","overall percentage")
      counts<-counts[order(counts[,"relative"],decreasing = T),]
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


#' show 12th detailed topic model meta data
#' depends on:
#'    values$TM_meta: topic model meta data
#'    input$Det_meta_select: selected detailed meta data 
#'    input$TM_meta_Rank1: topic model meta data rank
#'    values$tm_theta: topic model theta 
#'    input$Det_meta_topic: detailed meta data topic
#'    input$TM_meta_Prob: topic model meta data probability
#'    input$Det_TM_meta_use_quantiles: used quantiles for detailed topic model meta data
#'    input$Det_TM_meta_multi_valued: multiple values selcted from detailed topic model meta data
#'    input$Det_TM_meta_multi_valued_seperator: seprator for choosen values from meta data
#'    input$Det_TM_meta_min_occurrences_for_pie: minimal occurence for topic model meta data to apperar in the pie chart
output$Det_TM_Meta12<-renderUI({
  if(colnames(values$TM_meta)[16]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    # if no document is found
    validate(
      need(length(ids)>0,message = "no document found that matches the selected settings")
    )
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),input$Det_meta_select]
    meta_all<-values$TM_meta[,input$Det_meta_select]
    # check if > min occurrences
    # multi valued?
    if(input$Det_TM_meta_multi_valued==TRUE){
      meta <- trimws(unlist(stringr::str_split(string = meta,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both") 
      meta_all<-trimws(unlist(stringr::str_split(string = meta_all,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which="both")
    }
    if(input$Det_TM_meta_use_quantiles==TRUE){
      if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
        meta<-get_quantile_belonging(all_data = meta_all,sample = meta)
        meta[is.na(meta)]<-"NA"
        meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
        meta_all[is.na(meta_all)]<-"NA"
      }
    }
    counts_all<-as.data.frame(table(meta_all),stringsAsFactors = F)
    counts_all[,2]<-round(counts_all[,2]/nrow(values$TM_meta),digits = 3)  
    
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    # check if > min occurrences
    counts_pie <- counts[which(counts$Freq >= input$Det_TM_meta_min_occurrences_for_pie),]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_12<-renderPlotly({
      validate(
        need(nrow(counts_pie)>0,message="No results for current setting!")
      )
      p <- plot_ly(counts_pie, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',input$Det_meta_select,'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_12<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/length(ids),digits = 3))
      #order data by absolute frequency
      counts <- counts[order(counts[,2], decreasing = T),]
      counts<-merge(counts,counts_all,by.x = 1,by.y = 1,all.y = F)
      colnames(counts)<-c(input$Det_meta_select,"absolute","relative","overall percentage")
      counts<-counts[order(counts[,"relative"],decreasing = T),]
      datatable(data = counts,rownames = F,options = list(dom="tp"))
    })
    
    return(tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               text),
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$div(paste("most relevant words for topic: ",input$Det_meta_topic ," with lambda=0.25")),
                      wordcloud2Output("Det_TM_meta_wordcloud12")
               ),
               column(5,
                      plotlyOutput("Det_TM_meta_plot_12")
               ),
               column(3,
                      DT::dataTableOutput("Det_TM_meta_table_12")
               )
      )
    ))
  }
  else{
    return(NULL)
  }
})


#' show 13th detailed topic model meta data
#' depends on:
#'    values$TM_meta: topic model meta data
#'    input$Det_meta_select: selected detailed meta data 
#'    input$TM_meta_Rank1: topic model meta data rank
#'    values$tm_theta: topic model theta 
#'    input$Det_meta_topic: detailed meta data topic
#'    input$TM_meta_Prob: topic model meta data probability
#'    input$Det_TM_meta_use_quantiles: used quantiles for detailed topic model meta data
#'    input$Det_TM_meta_multi_valued: multiple values selcted from detailed topic model meta data
#'    input$Det_TM_meta_multi_valued_seperator: seprator for choosen values from meta data
#'    input$Det_TM_meta_min_occurrences_for_pie: minimal occurence for topic model meta data to apperar in the pie chart
output$Det_TM_Meta13<-renderUI({
  if(colnames(values$TM_meta)[17]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    # if no document is found
    validate(
      need(length(ids)>0,message = "no document found that matches the selected settings")
    )
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    #relevant_words<-
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),input$Det_meta_select]
    meta_all<-values$TM_meta[,input$Det_meta_select]
    # check if > min occurrences
    # multi valued?
    if(input$Det_TM_meta_multi_valued==TRUE){
      meta <- trimws(unlist(stringr::str_split(string = meta,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both") 
      meta_all<-trimws(unlist(stringr::str_split(string = meta_all,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which="both")
    }
    if(input$Det_TM_meta_use_quantiles==TRUE){
      if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
        meta<-get_quantile_belonging(all_data = meta_all,sample = meta)
        meta[is.na(meta)]<-"NA"
        meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
        meta_all[is.na(meta_all)]<-"NA"
      }
    }
    counts_all<-as.data.frame(table(meta_all),stringsAsFactors = F)
    counts_all[,2]<-round(counts_all[,2]/ nrow(values$TM_meta),digits = 3) 
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    # check if > min occurrences
    counts_pie <- counts[which(counts$Freq >= input$Det_TM_meta_min_occurrences_for_pie),]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_13<-renderPlotly({
      validate(
        need(nrow(counts_pie)>0,message="No results for current setting!")
      )
      p <- plot_ly(counts_pie, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',input$Det_meta_select,'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_13<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/length(ids),digits = 3))
      #order data by absolute frequency
      counts <- counts[order(counts[,2], decreasing = T),]
      counts<-merge(counts,counts_all,by.x = 1,by.y = 1,all.y = F)
      colnames(counts)<-c(input$Det_meta_select,"absolute","relative","overall percentage")
      counts<-counts[order(counts[,"relative"],decreasing = T),]
      datatable(data = counts,rownames = F,options = list(dom="tp"))
    })
    
    return(tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               text),
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$div(paste("most relevant words for topic: ",input$Det_meta_topic ," with lambda=0.25")),
                      wordcloud2Output("Det_TM_meta_wordcloud13")
               ),
               column(5,
                      plotlyOutput("Det_TM_meta_plot_13")
               ),
               column(3,
                      DT::dataTableOutput("Det_TM_meta_table_13")
               )
      )
    ))
  }
  else{
    return(NULL)
  }
})

#' show 14th detailed topic model meta data
#' depends on:
#'    values$TM_meta: topic model meta data
#'    input$Det_meta_select: selected detailed meta data 
#'    input$TM_meta_Rank1: topic model meta data rank
#'    values$tm_theta: topic model theta 
#'    input$Det_meta_topic: detailed meta data topic
#'    input$TM_meta_Prob: topic model meta data probability
#'    input$Det_TM_meta_use_quantiles: used quantiles for detailed topic model meta data
#'    input$Det_TM_meta_multi_valued: multiple values selcted from detailed topic model meta data
#'    input$Det_TM_meta_multi_valued_seperator: seprator for choosen values from meta data
#'    input$Det_TM_meta_min_occurrences_for_pie: minimal occurence for topic model meta data to apperar in the pie chart
output$Det_TM_Meta14<-renderUI({
  if(colnames(values$TM_meta)[18]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    # if no document is found
    validate(
      need(length(ids)>0,message = "no document found that matches the selected settings")
    )
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    #relevant_words<-
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),input$Det_meta_select]
    meta_all<-values$TM_meta[,input$Det_meta_select]
    # check if > min occurrences
    # multi valued?
    if(input$Det_TM_meta_multi_valued==TRUE){
      meta <- trimws(unlist(stringr::str_split(string = meta,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both") 
      meta_all<-trimws(unlist(stringr::str_split(string = meta_all,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which="both")
    }
    if(input$Det_TM_meta_use_quantiles==TRUE){
      if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
        meta<-get_quantile_belonging(all_data = meta_all,sample = meta)
        meta[is.na(meta)]<-"NA"
        meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
        meta_all[is.na(meta_all)]<-"NA"
      }
    }
    counts_all<-as.data.frame(table(meta_all),stringsAsFactors = F)
    counts_all[,2]<-round(counts_all[,2]/ nrow(values$TM_meta),digits = 3) 
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    # check if > min occurrences
    counts_pie <- counts[which(counts$Freq >= input$Det_TM_meta_min_occurrences_for_pie),]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_14<-renderPlotly({
      validate(
        need(nrow(counts_pie)>0,message="No results for current setting!")
      )
      p <- plot_ly(counts_pie, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',input$Det_meta_select,'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_14<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/length(ids),digits = 3))
      #order data by absolute frequency
      counts <- counts[order(counts[,2], decreasing = T),]
      counts<-merge(counts,counts_all,by.x = 1,by.y = 1,all.y = F)
      colnames(counts)<-c(input$Det_meta_select,"absolute","relative","overall percentage")
      counts<-counts[order(counts[,"relative"],decreasing = T),]
      datatable(data = counts,rownames = F,options = list(dom="tp"))
    })
    
    return(tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               text),
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$div(paste("most relevant words for topic: ",input$Det_meta_topic ," with lambda=0.25")),
                      wordcloud2Output("Det_TM_meta_wordcloud14")
               ),
               column(5,
                      plotlyOutput("Det_TM_meta_plot_14")
               ),
               column(3,
                      DT::dataTableOutput("Det_TM_meta_table_14")
               )
      )
    ))
  }
  else{
    return(NULL)
  }
})

#' show 15th detailed topic model meta data
#' depends on:
#'    values$TM_meta: topic model meta data
#'    input$Det_meta_select: selected detailed meta data 
#'    input$TM_meta_Rank1: topic model meta data rank
#'    values$tm_theta: topic model theta 
#'    input$Det_meta_topic: detailed meta data topic
#'    input$TM_meta_Prob: topic model meta data probability
#'    input$Det_TM_meta_use_quantiles: used quantiles for detailed topic model meta data
#'    input$Det_TM_meta_multi_valued: multiple values selcted from detailed topic model meta data
#'    input$Det_TM_meta_multi_valued_seperator: seprator for choosen values from meta data
#'    input$Det_TM_meta_min_occurrences_for_pie: minimal occurence for topic model meta data to apperar in the pie chart
output$Det_TM_Meta15<-renderUI({
  if(colnames(values$TM_meta)[19]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    # if no document is found
    validate(
      need(length(ids)>0,message = "no document found that matches the selected settings")
    )
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    #relevant_words<-
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),input$Det_meta_select]
    meta_all<-values$TM_meta[,input$Det_meta_select]
    # check if > min occurrences
    # multi valued?
    if(input$Det_TM_meta_multi_valued==TRUE){
      meta <- trimws(unlist(stringr::str_split(string = meta,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both") 
      meta_all<-trimws(unlist(stringr::str_split(string = meta_all,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which="both")
    }
    if(input$Det_TM_meta_use_quantiles==TRUE){
      if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
        meta<-get_quantile_belonging(all_data = meta_all,sample = meta)
        meta[is.na(meta)]<-"NA"
        meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
        meta_all[is.na(meta_all)]<-"NA"
      }
    }
    counts_all<-as.data.frame(table(meta_all),stringsAsFactors = F)
    counts_all[,2]<-round(counts_all[,2]/ nrow(values$TM_meta),digits = 3) 
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    # check if > min occurrences
    counts_pie <- counts[which(counts$Freq >= input$Det_TM_meta_min_occurrences_for_pie),]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_15<-renderPlotly({
      validate(
        need(nrow(counts_pie)>0,message="No results for current setting!")
      )
      p <- plot_ly(counts_pie, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',input$Det_meta_select,'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_15<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/length(ids),digits = 3))
      #order data by absolute frequency
      counts <- counts[order(counts[,2], decreasing = T),]
      counts<-merge(counts,counts_all,by.x = 1,by.y = 1,all.y = F)
      colnames(counts)<-c(input$Det_meta_select,"absolute","relative","overall percentage")
      counts<-counts[order(counts[,"relative"],decreasing = T),]
      datatable(data = counts,rownames = F,options = list(dom="tp"))
    })
    
    return(tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               text),
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$div(paste("most relevant words for topic: ",input$Det_meta_topic ," with lambda=0.25")),
                      wordcloud2Output("Det_TM_meta_wordcloud15")
               ),
               column(5,
                      plotlyOutput("Det_TM_meta_plot_15")
               ),
               column(3,
                      DT::dataTableOutput("Det_TM_meta_table_15")
               )
      )
    ))
  }
  else{
    return(NULL)
  }
})

#' show 16th detailed topic model meta data
#' depends on:
#'    values$TM_meta: topic model meta data
#'    input$Det_meta_select: selected detailed meta data 
#'    input$TM_meta_Rank1: topic model meta data rank
#'    values$tm_theta: topic model theta 
#'    input$Det_meta_topic: detailed meta data topic
#'    input$TM_meta_Prob: topic model meta data probability
#'    input$Det_TM_meta_use_quantiles: used quantiles for detailed topic model meta data
#'    input$Det_TM_meta_multi_valued: multiple values selcted from detailed topic model meta data
#'    input$Det_TM_meta_multi_valued_seperator: seprator for choosen values from meta data
#'    input$Det_TM_meta_min_occurrences_for_pie: minimal occurence for topic model meta data to apperar in the pie chart
output$Det_TM_Meta16<-renderUI({
  if(colnames(values$TM_meta)[20]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    # if no document is found
    validate(
      need(length(ids)>0,message = "no document found that matches the selected settings")
    )
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    #relevant_words<-
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),cinput$Det_meta_select]
    meta_all<-values$TM_meta[,input$Det_meta_select]
    # check if > min occurrences
    # multi valued?
    if(input$Det_TM_meta_multi_valued==TRUE){
      meta <- trimws(unlist(stringr::str_split(string = meta,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both") 
      meta_all<-trimws(unlist(stringr::str_split(string = meta_all,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which="both")
    }
    if(input$Det_TM_meta_use_quantiles==TRUE){
      if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
        meta<-get_quantile_belonging(all_data = meta_all,sample = meta)
        meta[is.na(meta)]<-"NA"
        meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
        meta_all[is.na(meta_all)]<-"NA"
      }
    }
    counts_all<-as.data.frame(table(meta_all),stringsAsFactors = F)
    counts_all[,2]<-round(counts_all[,2]/ nrow(values$TM_meta),digits = 3) 
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    # check if > min occurrences
    counts_pie <- counts[which(counts$Freq >= input$Det_TM_meta_min_occurrences_for_pie),]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_16<-renderPlotly({
      validate(
        need(nrow(counts_pie)>0,message="No results for current setting!")
      )
      p <- plot_ly(counts_pie, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',input$Det_meta_select,'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_16<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/length(ids),digits = 3))
      #order data by absolute frequency
      counts <- counts[order(counts[,2], decreasing = T),]
      counts<-merge(counts,counts_all,by.x = 1,by.y = 1,all.y = F)
      colnames(counts)<-c(input$Det_meta_select,"absolute","relative","overall percentage")
      counts<-counts[order(counts[,"relative"],decreasing = T),]
      datatable(data = counts,rownames = F,options = list(dom="tp"))
    })
    
    return(tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               text),
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$div(paste("most relevant words for topic: ",input$Det_meta_topic ," with lambda=0.25")),
                      wordcloud2Output("Det_TM_meta_wordcloud16")
               ),
               column(5,
                      plotlyOutput("Det_TM_meta_plot_16")
               ),
               column(3,
                      DT::dataTableOutput("Det_TM_meta_table_16")
               )
      )
    ))
  }
  else{
    return(NULL)
  }
})
#' show 17th detailed topic model meta data
#' depends on:
#'    values$TM_meta: topic model meta data
#'    input$Det_meta_select: selected detailed meta data 
#'    input$TM_meta_Rank1: topic model meta data rank
#'    values$tm_theta: topic model theta 
#'    input$Det_meta_topic: detailed meta data topic
#'    input$TM_meta_Prob: topic model meta data probability
#'    input$Det_TM_meta_use_quantiles: used quantiles for detailed topic model meta data
#'    input$Det_TM_meta_multi_valued: multiple values selcted from detailed topic model meta data
#'    input$Det_TM_meta_multi_valued_seperator: seprator for choosen values from meta data
#'    input$Det_TM_meta_min_occurrences_for_pie: minimal occurence for topic model meta data to apperar in the pie chart
output$Det_TM_Meta17<-renderUI({
  if(colnames(values$TM_meta)[21]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    # if no document is found
    validate(
      need(length(ids)>0,message = "no document found that matches the selected settings")
    )
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    #relevant_words<-
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),input$Det_meta_select]
    meta_all<-values$TM_meta[,input$Det_meta_select]
    # check if > min occurrences
    # multi valued?
    if(input$Det_TM_meta_multi_valued==TRUE){
      meta <- trimws(unlist(stringr::str_split(string = meta,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both") 
      meta_all<-trimws(unlist(stringr::str_split(string = meta_all,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which="both")
    }
    if(input$Det_TM_meta_use_quantiles==TRUE){
      if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
        meta<-get_quantile_belonging(all_data = meta_all,sample = meta)
        meta[is.na(meta)]<-"NA"
        meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
        meta_all[is.na(meta_all)]<-"NA"
      }
    }
    counts_all<-as.data.frame(table(meta_all),stringsAsFactors = F)
    counts_all[,2]<-round(counts_all[,2]/ nrow(values$TM_meta),digits = 3) 
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    # check if > min occurrences
    counts_pie <- counts[which(counts$Freq >= input$Det_TM_meta_min_occurrences_for_pie),]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_17<-renderPlotly({
      validate(
        need(nrow(counts_pie)>0,message="No results for current setting!")
      )
      p <- plot_ly(counts_pie, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',input$Det_meta_select,'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_17<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/length(ids),digits = 3))
      #order data by absolute frequency
      counts <- counts[order(counts[,2], decreasing = T),]
      counts<-merge(counts,counts_all,by.x = 1,by.y = 1,all.y = F)
      colnames(counts)<-c(input$Det_meta_select,"absolute","relative","overall percentage")
      counts<-counts[order(counts[,"relative"],decreasing = T),]
      datatable(data = counts,rownames = F,options = list(dom="tp"))
    })
    
    return(tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               text),
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$div(paste("most relevant words for topic: ",input$Det_meta_topic ," with lambda=0.25")),
                      wordcloud2Output("Det_TM_meta_wordcloud17")
               ),
               column(5,
                      plotlyOutput("Det_TM_meta_plot_17")
               ),
               column(3,
                      DT::dataTableOutput("Det_TM_meta_table_17")
               )
      )
    ))
  }
  else{
    return(NULL)
  }
})

#' show 18th detailed topic model meta data
#' depends on:
#'    values$TM_meta: topic model meta data
#'    input$Det_meta_select: selected detailed meta data 
#'    input$TM_meta_Rank1: topic model meta data rank
#'    values$tm_theta: topic model theta 
#'    input$Det_meta_topic: detailed meta data topic
#'    input$TM_meta_Prob: topic model meta data probability
#'    input$Det_TM_meta_use_quantiles: used quantiles for detailed topic model meta data
#'    input$Det_TM_meta_multi_valued: multiple values selcted from detailed topic model meta data
#'    input$Det_TM_meta_multi_valued_seperator: seprator for choosen values from meta data
#'    input$Det_TM_meta_min_occurrences_for_pie: minimal occurence for topic model meta data to apperar in the pie chart
output$Det_TM_Meta18<-renderUI({
  if(colnames(values$TM_meta)[22]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    # if no document is found
    validate(
      need(length(ids)>0,message = "no document found that matches the selected settings")
    )
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    #relevant_words<-
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),input$Det_meta_select]
    meta_all<-values$TM_meta[,input$Det_meta_select]
    # check if > min occurrences
    # multi valued?
    if(input$Det_TM_meta_multi_valued==TRUE){
      meta <- trimws(unlist(stringr::str_split(string = meta,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both") 
      meta_all<-trimws(unlist(stringr::str_split(string = meta_all,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which="both")
    }
    if(input$Det_TM_meta_use_quantiles==TRUE){
      if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
        meta<-get_quantile_belonging(all_data = meta_all,sample = meta)
        meta[is.na(meta)]<-"NA"
        meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
        meta_all[is.na(meta_all)]<-"NA"
      }
    }
    counts_all<-as.data.frame(table(meta_all),stringsAsFactors = F)
    counts_all[,2]<-round(counts_all[,2]/ nrow(values$TM_meta),digits = 3) 
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    # check if > min occurrences
    counts_pie <- counts[which(counts$Freq >= input$Det_TM_meta_min_occurrences_for_pie),]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_18<-renderPlotly({
      validate(
        need(nrow(counts_pie)>0,message="No results for current setting!")
      )
      p <- plot_ly(counts_pie, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',input$Det_meta_select,'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_18<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/length(ids),digits = 3))
      #order data by absolute frequency
      counts <- counts[order(counts[,2], decreasing = T),]
      counts<-merge(counts,counts_all,by.x = 1,by.y = 1,all.y = F)
      colnames(counts)<-c(input$Det_meta_select,"absolute","relative","overall percentage")
      counts<-counts[order(counts[,"relative"],decreasing = T),]
      datatable(data = counts,rownames = F,options = list(dom="tp"))
    })
    
    return(tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               text),
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$div(paste("most relevant words for topic: ",input$Det_meta_topic ," with lambda=0.25")),
                      wordcloud2Output("Det_TM_meta_wordcloud18")
               ),
               column(5,
                      plotlyOutput("Det_TM_meta_plot_18")
               ),
               column(3,
                      DT::dataTableOutput("Det_TM_meta_table_18")
               )
      )
    ))
  }
  else{
    return(NULL)
  }
})
#' show 19th detailed topic model meta data
#' depends on:
#'    values$TM_meta: topic model meta data
#'    input$Det_meta_select: selected detailed meta data 
#'    input$TM_meta_Rank1: topic model meta data rank
#'    values$tm_theta: topic model theta 
#'    input$Det_meta_topic: detailed meta data topic
#'    input$TM_meta_Prob: topic model meta data probability
#'    input$Det_TM_meta_use_quantiles: used quantiles for detailed topic model meta data
#'    input$Det_TM_meta_multi_valued: multiple values selcted from detailed topic model meta data
#'    input$Det_TM_meta_multi_valued_seperator: seprator for choosen values from meta data
#'    input$Det_TM_meta_min_occurrences_for_pie: minimal occurence for topic model meta data to apperar in the pie chart
output$Det_TM_Meta19<-renderUI({
  if(colnames(values$TM_meta)[23]%in%input$Det_meta_select){
    theta<-values$tm_theta
    if(input$TM_meta_Rank1==TRUE){
      ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
    }
    else{
      ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
    }
    # if no document is found
    validate(
      need(length(ids)>0,message = "no document found that matches the selected settings")
    )
    text<-tags$h4(paste0(length(ids)," documents belong to topic:", input$Det_meta_topic))
    #relevant_words<-
    meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),input$Det_meta_select]
    meta_all<-values$TM_meta[,input$Det_meta_select]
    # check if > min occurrences
    # multi valued?
    if(input$Det_TM_meta_multi_valued==TRUE){
      meta <- trimws(unlist(stringr::str_split(string = meta,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which = "both") 
      meta_all<-trimws(unlist(stringr::str_split(string = meta_all,pattern = input$Det_TM_meta_multi_valued_seperator, simplify = F)),which="both")
    }
    if(input$Det_TM_meta_use_quantiles==TRUE){
      if(all(varhandle::check.numeric(v = meta_all,exceptions = "NA"))){
        meta<-get_quantile_belonging(all_data = meta_all,sample = meta)
        meta[is.na(meta)]<-"NA"
        meta_all<-get_quantile_belonging(all_data = meta_all,sample = meta_all)
        meta_all[is.na(meta_all)]<-"NA"
      }
    }
    counts_all<-as.data.frame(table(meta_all),stringsAsFactors = F)
    counts_all[,2]<-round(counts_all[,2]/ nrow(values$TM_meta),digits = 3) 
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    # check if > min occurrences
    counts_pie <- counts[which(counts$Freq >= input$Det_TM_meta_min_occurrences_for_pie),]
    counts<-as.data.frame(table(meta),stringsAsFactors = F)
    output$Det_TM_meta_plot_19<-renderPlotly({
      validate(
        need(nrow(counts_pie)>0,message="No results for current setting!")
      )
      p <- plot_ly(counts_pie, labels = ~meta, values = ~Freq, type = 'pie') %>%
        layout(title = paste('Meta distribution for meta category: "',input$Det_meta_select,'"'),legend=T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    output$Det_TM_meta_table_19<-DT::renderDataTable({
      counts<-cbind(counts,round(counts[,2]/length(ids),digits = 3))
      #order data by absolute frequency
      counts <- counts[order(counts[,2], decreasing = T),]
      counts<-merge(counts,counts_all,by.x = 1,by.y = 1,all.y = F)
      colnames(counts)<-c(input$Det_meta_select,"absolute","relative","overall percentage")
      counts<-counts[order(counts[,"relative"],decreasing = T),]
      datatable(data = counts,rownames = F,options = list(dom="tp"))
    })
    
    return(tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               text),
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$div(paste("most relevant words for topic: ",input$Det_meta_topic ," with lambda=0.25")),
                      wordcloud2Output("Det_TM_meta_wordcloud19")
               ),
               column(5,
                      plotlyOutput("Det_TM_meta_plot_19")
               ),
               column(3,
                      DT::dataTableOutput("Det_TM_meta_table_19")
               )
      )
    ))
  }
  else{
    return(NULL)
  }
})


#' render meta data table
#' depends on: 
#'   input$TM_meta_Rank1: topic model meta data rank
#'   input$Det_meta_topic: detailed topic modle meta data topic
#'   input$TM_meta_Prob: topic model meta data probability
#'   values$TM_meta: topic model meta data
#'   input$Det_meta_select: selected detailed meta data 
output$Det_TM_Meta_Table<-DT::renderDataTable({
  validate(
    need(!is.null(input$TM_meta_Rank1),message=F),
    need(!is.null(input$Det_meta_topic),message=F)
  )
  theta<-values$tm_theta
  if(input$TM_meta_Rank1==TRUE){
    ids<-names(which(apply(theta,1,max)==theta[,as.character(input$Det_meta_topic)]))
  }
  else{
    ids<-names(which(theta[,as.character(input$Det_meta_topic)]>=input$TM_meta_Prob))
  }
  # if no document is found
  validate(
    need(length(ids)>0, message=F)
  )
  meta<-values$TM_meta[which(values$TM_meta[,"id_doc"]%in%ids),]
  meta<-meta[,-c(1,2,4)]
  try({
    meta$title<-paste0("<b>",meta$title,"</b>")
  })
  datatable(data = meta,escape = F,class = 'cell-border stripe',rownames=T)
})

#' render datatable for detailed topic model meta data quantiles
#' depends on: input$Det_meta_select: selected detailed meta data
output$Det_TM_meta_quantiles<-renderTable({
  meta<-values$TM_meta[,input$Det_meta_select]
  validate(
    need(all(varhandle::check.numeric(v = meta,exceptions = "NA")),message = "Not all values are numeric")
  )
  quantiles<-quantile(x = as.numeric(meta),na.rm = T)
  data<-t(data.frame(quantiles=quantiles))
  colnames(data)<-names(quantiles)
  
  return(data)
})

######################################################
#########  estimated word frequencies ################
######################################################


#' plot estimated word frequencies in bar chart
#' depends on:
#'   input$Det_TM_ewf_word: word for estimated word frequencies 
#'   values$tm_rel_counts: topic model word count
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

#' table for estimated word frequency from topic model
#' depends on:
#'   input$Det_TM_ewf_word: word top calculate estimated word frequence
#'   input$Det_TM_emf_rel: relative distribution over topics for choosen word
#'   values$tm_counts: topic model counts
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


#'  start downloading real counts for estimated word frequency
#'  depends on:
#'    values$tm_counts: count of words from topic model
output$download_rel_counts<-downloadHandler(
  filename = function() {
    paste('estimated_counts', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-as.matrix(values$tm_counts)
    write.csv(data, con)
  }
)  

#' UI validation of detailed topic model meta data
#' depends on:
#'    input$Det_TM_validation_document: validate documents from detailed topic model
#'    values$host: used host 
#'    values$port: used port
output$Det_TM_validation_metadata_UI<-renderUI({
  validate(
    need(
      !is.null(input$Det_TM_validation_document),message=F
    ),
    need(
      input$Det_TM_validation_document!="",message=F
    )
  )
  identifier<-stringr::str_split(string = input$Det_TM_validation_document,pattern = "_",simplify = T)
  dataset<-identifier[1]
  doc_id<-identifier[2]
  metadata<-get_metadata_from_db(dataset = dataset,doc_ids = doc_id,host = values$host,port=values$port)
  return(tagList(
    tags$h4("Metadata"),
    tags$b("title:"),
    tags$div(metadata[1,"title"]),
    tags$b("date:"),
    tags$div(metadata[1,"date"]),
    tags$b("number of token:"),
    tags$div(metadata[1,"token"])
  ))
})


#' plot for topic validation tab showing the document with the words highlighted, which are relevant in the chosen topic
#' depends on:
#'   input$Det_TM_validation_document: document from topic model with validation
#'   values$host: used host
#'   values$port: used port
#'   values$Details_Data_TM: details of topic model data
#'   values$tm_phi: topic model phi
#'   input$Det_TM_validation_relevance_measure: relebance measurement for detailed topic model validation
#'   values$tm_rel_counts: topic model relative counts
#'   values$Det_TM_validation_topic: validation of topics from detailed topic model
#'   input$Det_TM_validation_lambda: lambda validation from detailed topic model 
#'   values$tm_theta: topic model theta
#'   values$tm_doc.length: topic model document length
#'   input$Det_TM_validation_minmax_gobal: global minimum and maximum for detailed topic model validation
#'   input$Det_TM_validation_color_least_important: detailed topic model validation colour for least important values
#'   input$Det_TM_validation_color_most_important: detailed topic model validation colour for most important values
#'   input$Det_TM_validation_color_use_pie_colors: validation color to use in pie chart for detailed topic models 
output$TM_validation_UI<-renderUI({
  validate(
    need(
      !is.null(input$Det_TM_validation_document),message=FALSE
    ),
    need(
      input$Det_TM_validation_document!="",message="please choose a document"
    )
  )
  identifier<-stringr::str_split(string = input$Det_TM_validation_document,pattern = "_",simplify = T)
  dataset<-identifier[1]
  doc_id<-identifier[2]
  token<-get_token_from_db(dataset = dataset,doc_ids = doc_id,sentence_ids = NULL,host=values$host,port=values$port)
  # remove idx column from token
  token<-token[,-ncol(token)]
  
  load(paste0(values$Details_Data_TM,"/parameters.RData"))
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
  phi<-values$tm_phi
  
  
  if(input$Det_TM_validation_relevance_measure=="estimated relative word frequency per topic"){
    data<-values$tm_rel_counts
    data<-do.call(cbind,lapply(X = 1:dim(data)[2],FUN = function(x){
      data[,x]/sum(data[,x])
    })
    )
    data<-round(data,digits = 2)
    colnames(data)<-colnames(values$tm_rel_counts)
    data<-data[input$Det_TM_validation_topic,intersect(unique(features),colnames(data))]
    data<-data.frame(features=names(data),weight=data)
    min=0
    max=1
    
  }
  if(input$Det_TM_validation_relevance_measure=="relevance score"){
    relevance<-calculate_topic_relevance(lambda=input$Det_TM_validation_lambda,phi=values$tm_phi,theta=values$tm_theta,doc.length=values$tm_doc.length)
    #normalize relevance
    #relevance<-relevance-apply(relevance,1, FUN=min)
    #relevance<-t(t(as.matrix(relevance))/rowSums(relevance))
    #relevance<-relevance/max(relevance)
    data<-relevance[,input$Det_TM_validation_topic]
    data<-data.frame(features=names(data),weight=data)
    if(input$Det_TM_validation_minmax_gobal=="over all topics"){
      min=min(relevance)
      max=max(relevance)
    }
    if(input$Det_TM_validation_minmax_gobal=="inside chosen topic"){
      min=min(relevance[,input$Det_TM_validation_topic])
      max=max(relevance[,input$Det_TM_validation_topic])
    }
    if(input$Det_TM_validation_minmax_gobal=="inside chosen document"){
      max<-max(data[intersect(unique(features),rownames(data)),"weight"])
      min<-min(data[intersect(unique(features),rownames(data)),"weight"])
    }
  }
  if(input$Det_TM_validation_relevance_measure=="word probability"){
    data<-values$tm_phi[input$Det_TM_validation_topic,]
    data<-data.frame(features=names(data),weight=data)
    if(input$Det_TM_validation_minmax_gobal=="over all topics"){
      max<-max(values$tm_phi)
      min<-min(values$tm_phi)
    }
    if(input$Det_TM_validation_minmax_gobal=="inside chosen topic"){
      max<-max(data$weight)
      min<-min(data$weight)
    }
    if(input$Det_TM_validation_minmax_gobal=="inside chosen document"){
      max<-max(data[intersect(unique(features),rownames(data)),"weight"])
      min<-min(data[intersect(unique(features),rownames(data)),"weight"])
    }
  }
  
  
  m<-merge(x = token,y=data,by="features",all.x=TRUE)
  m<-m[order(m[,2]),]
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))
  colors<-getPalette(dim(values$tm_phi)[1])
  #colors<-colors[order(values$tm_theta[input$Det_TM_validation_document,],decreasing = F)]
  color<-colors[input$Det_TM_validation_topic]
  if(input$Det_TM_validation_color_use_pie_colors==TRUE){
    rbPal_pos <- colorRampPalette(c('floralwhite',color))
  }
  else{
    rbPal_pos <- colorRampPalette(c(input$Det_TM_validation_color_least_important,input$Det_TM_validation_color_most_important))
  }
  m<-cbind(m,rep("",dim(m)[1]))
  if(length(intersect(which(!is.na(m$weight)),which(m$weight!=0)))>0){
    m[intersect(which(!is.na(m$weight)),which(m$weight!=0)),12]<-  rbPal_pos(100)[as.numeric(cut(c(max,min,m$weight[intersect(which(!is.na(m$weight)),which(m$weight!=0))]),breaks = 100))[-c(1,2)]] #Alternative#seq(0,to = max(data$weight),length.out = 100) #original m$weight[intersect(which(!is.na(m$weight)),which(m$weight>0))]
  }
  
  strings<-apply(m,MARGIN = 1,FUN = function(x){
    if(is.na(x[11])){
      return(x[7])
    }
    else{
      return( paste0('<font style="background-color:',x[12],';"','title="feature: ',x[1],' with weight: ',round(as.numeric(x[11]),digits = 5),'">',x[7],'</font>'))
    }
  })
  
  document<-list()
  for(i in 1:dim(m)[1]){
    document[[i]]<-paste0("<span span_nr='",i,"'>",strings[i],"</span>")
  }
  document<-do.call(rbind,document)
  document<-HTML(document)
  return(
    tagList(
      fluidRow(style="margin-left:0px;margin-right:0px",
               column(4,
                      tags$br(),
                      plotly::plotlyOutput("Det_TM_validation_document_topic_pie"),
                      tags$h4("Most relevant words for chosen topic"),
                      wordcloud2Output(outputId = "Det_TM_validation_wordcloud")
               ),
               column(8,
                      tags$br(),
                      tags$p(document)
               )
      )
      
    ) 
  )
})


#' wordcloud showing the relevant words for the chosen topic
#' depends on:
#'  input$Det_TM_validation_topic: choosen topic from detailed topic models for validation
output$Det_TM_validation_wordcloud <- wordcloud2::renderWordcloud2({
  # @values$tm_relevance calculated with lamda= 0.3
  data <- values$tm_relevance[,input$Det_TM_validation_topic]
  data <- data[order(data,decreasing=T)][1:50]
  data <- data.frame(cbind(names(data),data),stringsAsFactors = FALSE)
  class(data$data) <- "numeric"
  # normalize weights for wordcloud
  data$data <- data$data-min(data$data)
  data$data <- data$data/max(data$data)
  wordcloud2(data = data,size=0.32,fontFamily = "Helvetica",color = "random-dark",minSize = 0.1,minRotation = -pi/2,maxRotation = -pi/2)
})



#' pie chart showing the topic distribution of a selected topic 
#' depends on:
#'   input$Det_TM_validation_document: chosen document using doc_id as identifier
#'   values$tm_theta: topic model theta
output$Det_TM_validation_document_topic_pie<-plotly::renderPlotly({
  validate(
    need(
      !is.null(input$Det_TM_validation_document),message=FALSE
    ),
    need(
      input$Det_TM_validation_document!="",message=F
    )
  )
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))
  colors<-getPalette(dim(values$tm_phi)[1])
  data<-values$tm_theta[input$Det_TM_validation_document,]
  data<-data.frame(class=paste("Topic: ",names(data)),likelihood=data)
  
  p <- plot_ly(data, labels = ~factor(class), values = ~likelihood, textposition = 'inside',source="tm_validation_pie",marker = list(colors = colors),
               textinfo = 'label+percent') %>%
    plotly::add_pie(hole = 0.6) %>%
    plotly::layout(title = paste('Distribution of topics for chosen document'),legend=T,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   legend = list(x = 0.1, y = 0.9)
    )
  return(p)
})

click_pie_tm_validation<-reactive({
  currentEventData<-event_data(event = "plotly_click", source = "tm_validation_pie",session = session)
})

#' change selected topic if user click on a topic in the pie chart
observe({
  validate(
    need(!is.null(click_pie_tm_validation()),message = F)
  )
  updateSliderInput(session = session,inputId = "Det_TM_validation_topic",value = as.numeric(click_pie_tm_validation()$pointNumber+1) )
})



#' output object for topic dispersion 
output$TM_dispersion_ui<-renderUI({
  return(tagList(
    tabsetPanel(type="tabs",
                tabPanel(title = "Summary",
                         DT::dataTableOutput(outputId = "Det_TM_dispersion_summary_table")%>% withSpinner()
                ),
                tabPanel(title = "Details",
                         tags$h4("Topic Probability Distributions"),
                         plotly::plotlyOutput(outputId = "Det_TM_dispersion_detailed_single_hist"),
                         plotly::plotlyOutput(outputId = "Det_TM_dispersion_detailed_plot")
                )
    )
  ))
})




#' summary table for topic dispersion
#' depends on:
#'  values$tm_theta: topic model - theta selection 
output$Det_TM_dispersion_summary_table<-DT::renderDataTable({
  validate(
    need(!is.null(values$tm_theta),message=F)
  )
  values$tm_random
  theta<-values$tm_theta
  topic_importance<-round(Matrix::colSums(theta),digits = 2)
  greater010<-apply(theta,MARGIN = 2,function(x){
    length(which(x>0.1))
  })
  greater025<-apply(theta,MARGIN = 2,function(x){
    length(which(x>0.25))
  })
  greater050<-apply(theta,MARGIN = 2,function(x){
    length(which(x>0.5))
  })
  greater075<-apply(theta,MARGIN = 2,function(x){
    length(which(x>0.75))
  })
  greater090<-apply(theta,MARGIN = 2,function(x){
    length(which(x>0.9))
  })
  data<-data.frame(Topic_Importnace=topic_importance, Greater010=greater010,Greater025=greater025,Greater050=greater050,Greater075=greater075,Greater090=greater090)
  rownames(data)<-paste("Topic:",1:nrow(data))
  colnames(data)<-c("topic importance",">0.1", ">0.25", ">0.5", ">0.75", ">0.9")
  datatable(data = data,options = list(dom="T",pageLength = nrow(data), info = FALSE,
                                       lengthMenu = list(c(15, -1), c("15", "All"))))%>%
    formatStyle(columns = 1,
                background = styleColorBar(range(data[,1]), 'lightblue'),
                backgroundSize = '98% 88%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center')%>%
    formatStyle(columns = 2,
                background = styleColorBar(range(data[,2]), 'lightblue'),
                backgroundSize = '98% 88%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center')%>%
    formatStyle(columns = 3,
                background = styleColorBar(range(data[,3]), 'lightblue'),
                backgroundSize = '98% 88%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center')%>%
    formatStyle(columns = 4,
                background = styleColorBar(range(data[,4]), 'lightblue'),
                backgroundSize = '98% 88%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center')%>%
    formatStyle(columns = 5,
                background = styleColorBar(range(data[,5]), 'lightblue'),
                backgroundSize = '98% 88%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center')%>%
    formatStyle(columns = 6,
                background = styleColorBar(range(data[,6]), 'lightblue'),
                backgroundSize = '98% 88%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center')
})


#' detailed dispersion plot; multiple histograms
#' depends on:
#'    input$Det_TM_dispersion_topic: topic for dispersion from detailed topic model
#'    input$Det_TM_dispersion_probability_threshold: probability threshold for detailed topic model dispersion
#'    values$tm_theta: topic model theta
output$Det_TM_dispersion_detailed_plot<-plotly::renderPlotly({
  validate(
    need(length(input$Det_TM_dispersion_topic)>0,message="Please choose at least one topic")
  )
  data<-values$tm_theta[,input$Det_TM_dispersion_topic,drop=F]
  data[which(data<input$Det_TM_dispersion_probability_threshold,arr.ind = T)]<-NA
  plot_list<-list()
  plot_list[[1]]<-plot_ly(x = data[,input$Det_TM_dispersion_topic[1]]) %>%
    add_histogram(name=paste("Topic:",input$Det_TM_dispersion_topic[1]))
  if(length(input$Det_TM_dispersion_topic)>1){
    for(i in 2:length(input$Det_TM_dispersion_topic)){
      plot_list[[i]]<-plot_ly(x = data[,input$Det_TM_dispersion_topic[i]]) %>%
        add_histogram(name=paste("Topic:",input$Det_TM_dispersion_topic[i]))
    }
  }
  return(
    subplot(plot_list)
  )
})


#' detailed dispersion plot with a single histogram
#' depends on:
#'    input$Det_TM_dispersion_topic: topic from detailed topic model for dispersion
#'    input$Det_TM_dispersion_probability_threshold: threshold of probabilties from dispersion
output$Det_TM_dispersion_detailed_single_hist<-plotly::renderPlotly({
  validate(
    need(length(input$Det_TM_dispersion_topic)>0,message="Please choose at least one topic")
  )
  data<-values$tm_theta[,input$Det_TM_dispersion_topic,drop=F]
  data[which(data<input$Det_TM_dispersion_probability_threshold,arr.ind = T)]<-NA
  hist<-plot_ly(x = data[,input$Det_TM_dispersion_topic[1]],alpha = 0.7) %>%
    add_histogram(name=paste("Topic:",input$Det_TM_dispersion_topic[1]))
  if(length(input$Det_TM_dispersion_topic)>1){
    for(i in 2:length(input$Det_TM_dispersion_topic)){
      hist<-add_histogram(p=hist,x = data[,input$Det_TM_dispersion_topic[i]],name=paste("Topic:",input$Det_TM_dispersion_topic[i]))
    }
  }
  hist<-layout(p=hist,barmode = "overlay",
               xaxis = list(title = "Topic Probability",
                            zeroline = FALSE),
               yaxis = list(title = "Count",
                            zeroline = FALSE))
  return(
    hist
  )
}
)



########################################
##       Document Comparison           #
########################################

#' UI for document compariosn tab in visualisation of topic models
output$TM_document_comparison_UI<-renderUI({
  tabsetPanel(type="tabs",id = "tabBox_TM_document_comparison",
              tabPanel(title = "Table",
                       tags$div(style="overflow-x:auto;overflow-y:auto;",
                                DT::dataTableOutput(outputId = "Det_TM_document_comparison_table")
                       )
              ),
              tabPanel(title = "Pie Charts",
                       plotly::plotlyOutput(outputId = "Det_TM_document_comparison_pie")
              ),
              tabPanel(title="Correlation",
                       tags$br(),
                       tags$h3("Correlation"),
                       plotly::plotlyOutput(outputId = "Det_TM_document_comparison_correlation_heatmap_correlation"),
                       tags$hr(),
                       tags$h3("Cosine Similarity"),
                       plotly::plotlyOutput(outputId = "Det_TM_document_comparison_correlation_heatmap_cosine_similarity"),
                       tags$hr(),
                       tags$h3("Euclidean Distance"),
                       plotly::plotlyOutput(outputId = "Det_TM_document_comparison_correlation_heatmap_euclidean_distance")
              )
  )
})


#' table showing the topic distributions for chosen words
#' depends on: 
#'    input$Det_TM_document_comparison_document: documents for comparison from detailed topic model
#'    values$tm_theta: topic model theta
#'    values$tm_meta: meta data from topic model
output$Det_TM_document_comparison_table<-DT::renderDataTable({
  validate(
    need(length(input$Det_TM_document_comparison_document)>0,message = "Choose at least one document!")
  )
  data<-t(values$tm_theta[input$Det_TM_document_comparison_document,,drop=F])
  data<-round(data,digits = 3)
  titles<-values$tm_meta[which(values$tm_meta[,"id_doc"]%in%input$Det_TM_document_comparison_document),c("id_doc","title"),drop=F]
  rownames(titles)<-titles$id_doc
  titles<-titles[colnames(data),]
  titles<-paste0(titles$title," (",titles$id_doc,")")
  rownames(data) <- paste0("<b>Topic ",1:ncol(values$tm_theta),"</b>")
  colnames(data) <- titles
  return(DT::datatable(data = data, selection = "none",escape=F,class = 'cell-border stripe',options = list(paging=F,dom="t"))%>%
           formatStyle(colnames(data),
                       background = styleColorBar(range(data), 'lightblue'),
                       backgroundSize = '98% 88%',
                       backgroundRepeat = 'no-repeat',
                       backgroundPosition = 'center')
  )
})

#' pie chart for document comparison
#' depends on:
#'    input$Det_TM_document_comparison_document: documents for comparision from detailed topic model
#'    values$tm_theta: topic model theta
#'    values$tm_meta: meta data from topic model
output$Det_TM_document_comparison_pie<-plotly::renderPlotly({
  validate(
    need(length(input$Det_TM_document_comparison_document)>0,message = "Choose at least one document!")
  )
  theta<-values$tm_theta
  row=0
  column=0
  subplots<-plotly::plot_ly()
  data<-t(values$tm_theta[input$Det_TM_document_comparison_document,,drop=F])
  titles<-values$tm_meta[which(values$tm_meta[,"id_doc"]%in%input$Det_TM_document_comparison_document),c("id_doc","title"),drop=F]
  rownames(titles)<-titles$id_doc
  titles<-titles[colnames(data),]
  titles<-paste0(titles$title," (",titles$id_doc,")")
  data<-t(data)
  data<-round(data,digits = 3)
  for(k in 1:length(input$Det_TM_document_comparison_document)){
    data_pie<-data.frame(prob=data[k,], names=paste0("Topic: ",colnames(theta)))
    subplots<-plotly::add_pie(subplots,data = data_pie,values=~prob, labels=~names,
                              name=  titles[k],
                              textposition="inside",
                              marker = list(colors = values$tm_colors,
                                            line = list(color = '#FFFFFF', width = 1)),
                              #The 'pull' attribute can also be used to create space between the sectors
                              domain = list(row = row, column = column)
    )
    if(k%%2==0){
      row=row+1
    }
    column<-(k%%2)
  }
  subplots <- plotly::layout(subplots,title = "Topic Distributions for chosen documents", showlegend = T,
                             grid=list(rows=(length(input$Det_TM_document_comparison_document)+1)%/%2, columns=2),
                             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
  )
  subplots
})


#' heatmap for comparison correlation of documents:
#' depends on:
#'    input$Det_TM_document_comparison_document: documents for comparison from detailed topic model
#'    values$tm_meta: meta data from topic model
#'    input$Det_TM_document_comparison_correlation_method: comparison correlation method for documents from detailed topic model
#'    values$tm_theta: topic model theta
#'    input$Det_TM_document_comparison_color_low: color for low impact values from document comparision
#'    input$Det_TM_document_comparison_color_high: color for high impact values from document comparision
output$Det_TM_document_comparison_correlation_heatmap_correlation<-plotly::renderPlotly({
  validate(
    need(length(input$Det_TM_document_comparison_document)>1,message = "Choose at least two documents!")
  )
  data<-t(values$tm_theta[input$Det_TM_document_comparison_document,,drop=F])
  titles<-values$tm_meta[which(values$tm_meta[,"id_doc"]%in%input$Det_TM_document_comparison_document),c("id_doc","title"),drop=F]
  rownames(titles)<-titles$id_doc
  titles<-titles[colnames(data),]
  titles<-paste0(titles$title," (",titles$id_doc,")")
  
  correlation<-cor((data),method = input$Det_TM_document_comparison_correlation_method)
  heatmap<-plotly::plot_ly(z=correlation,type="heatmap",x=titles,y=titles,
                           colors= colorRamp(c(input$Det_TM_document_comparison_color_low, input$Det_TM_document_comparison_color_high))
  )
  return(heatmap)
})

#' heatmap fo document comparision correlation calculated with cosine similarity
#' depends on:
#'    input$Det_TM_document_comparison_document: documents for comparison
#'    values$tm_meta: topic model meta data
#'    input$Det_TM_document_comparison_color_low: color for low impact values from document comparision
#'    input$Det_TM_document_comparison_color_high: color for high impact values from document comparision
output$Det_TM_document_comparison_correlation_heatmap_cosine_similarity<-plotly::renderPlotly({
  validate(
    need(length(input$Det_TM_document_comparison_document)>1,message = "Choose at least two documents!")
  )
  data<-t(values$tm_theta[input$Det_TM_document_comparison_document,,drop=F])
  titles<-values$tm_meta[which(values$tm_meta[,"id_doc"]%in%input$Det_TM_document_comparison_document),c("id_doc","title"),drop=F]
  rownames(titles)<-titles$id_doc
  titles<-titles[colnames(data),]
  titles<-paste0(titles$title," (",titles$id_doc,")")
  
  data<-t(data)
  cosine_similarity<-wordVectors::cosineSimilarity(x = data,y = data)
  heatmap<-plotly::plot_ly(z=cosine_similarity,type="heatmap",x=titles,y=titles,
                           colors= colorRamp(c(input$Det_TM_document_comparison_color_low, input$Det_TM_document_comparison_color_high))
  )
  return(heatmap)
})

#' render heatmap with euclidean distance correlations
#' depends on:
#'    input$Det_TM_document_comparison_document: document for comparison from detailed topic model 
#'    values$tm_theta: topic model theta
#'    values$tm_meta: topic model meta data
#'    input$Det_TM_document_comparison_color_low: color for low impact values from document comparison
#'    input$Det_TM_document_comparison_color_high: color for high impact values from document comparison
output$Det_TM_document_comparison_correlation_heatmap_euclidean_distance<-plotly::renderPlotly({
  validate(
    need(length(input$Det_TM_document_comparison_document)>1,message = "Choose at least two documents!")
  )
  data<-t(values$tm_theta[input$Det_TM_document_comparison_document,,drop=F])
  titles<-values$tm_meta[which(values$tm_meta[,"id_doc"]%in%input$Det_TM_document_comparison_document),c("id_doc","title"),drop=F]
  rownames(titles)<-titles$id_doc
  titles<-titles[colnames(data),]
  titles<-paste0(titles$title," (",titles$id_doc,")")
  
  data<-t(data)
  euclidean_distance<-as.matrix(dist(x = data,method = "euclidean",diag = T,upper = T))
  heatmap<-plotly::plot_ly(z=euclidean_distance,type="heatmap",x=titles,y=titles,
                           colors= colorRamp(c(input$Det_TM_document_comparison_color_low, input$Det_TM_document_comparison_color_high))
  )
  return(heatmap)
})




#############################
###    document outlier   ###
#############################

#' render document outlier
output$TM_document_outlier_UI<-renderUI({
  return(tagList(
    tags$h4("Average document similarity"),
    DT::dataTableOutput(outputId = "Det_TM_document_outlier_table"),
    plotly::plotlyOutput(outputId = "Det_TM_document_outlier_heatmap")
    
  )
  )
})

#' show heatmap from document outlier
#' depends on:
#'    values$TM_document_outlier_tabledata: datatable information for document outlier of the topic model 
#'    input$Det_TM_document_outlier_table_rows_selected: selected rows from document outlier datable
#'    values$Det_TM_document_outlier_comparison_matrix: matrix for document outlier comparison
output$Det_TM_document_outlier_heatmap<-plotly::renderPlotly({
  selected_ids<-values$TM_document_outlier_tabledata[input$Det_TM_document_outlier_table_rows_selected,"id_doc"]
  titles<-values$TM_document_outlier_tabledata[input$Det_TM_document_outlier_table_rows_selected,"title"]
  
  validate(
    need(length(selected_ids)>0,message=F)
  )
  
  comparison_matrix <- values$Det_TM_document_outlier_comparison_matrix[selected_ids,,drop=F]
  ids_rest<-data.frame(id_doc=colnames(values$Det_TM_document_outlier_comparison_matrix),stringsAsFactors = F)
  all_titles<-plyr::join(y=values$TM_document_outlier_tabledata,x=ids_rest,by="id_doc")[,"title"]
  rownames(comparison_matrix)<-titles
  colnames(comparison_matrix)<-all_titles
  
  #colors= colorRamp(c(input$Det_TM_document_outlier_color_low,"white", input$Det_TM_document_outlier_color_high))
  heatmap<-plotly::plot_ly(z=comparison_matrix,type="heatmap",x=all_titles,y=titles
  )%>%
    plotly::layout(xaxis=list(type="category"),yaxis=list(type="category"),margin = list(b=200))
  
  return(heatmap)
})

#' render datatable for document outlier
#' depends on:
#'    values$tm_theta: topic model theta
#'    values$tm_meta: topic model meta data
#'    input$Det_TM_document_outlier_measure: measures for detailed topic model document outlier
#'    input$Det_TM_document_outlier_correlation_method: correlation method for document outlier
#'    values$Det_TM_document_outlier_comparison_matrix: comparison matrix for document outlier
#'    values$TM_document_outlier_tabledata: datatable information for document outlier
output$Det_TM_document_outlier_table<-DT::renderDataTable({
  data<-t(values$tm_theta)
  titles<-values$tm_meta[,c("id_doc","title"),drop=F]
  rownames(titles)<-titles$id_doc
  titles<-titles[colnames(data),]
  titles<-paste0(titles$title," (",titles$id_doc,")")
  
  data<-t(data)
  if(input$Det_TM_document_outlier_measure=="Cosine Similarity"){
    comparison_matrix<-wordVectors::cosineSimilarity(x = data,y = data)
  }
  if(input$Det_TM_document_outlier_measure=="Correlation"){
    comparison_matrix<-cor(t(data),method = input$Det_TM_document_outlier_correlation_method)
  }
  if(input$Det_TM_document_outlier_measure=="Euclidean Distance"){
    comparison_matrix<-as.matrix(dist(x = data,method = "euclidean",diag = T,upper = T))
  }
  values$Det_TM_document_outlier_comparison_matrix<-comparison_matrix
  
  avg_comparison<-colSums(comparison_matrix)/ncol(comparison_matrix)
  
  if(input$Det_TM_document_outlier_measure=="Euclidean Distance"){
    avg_comparison<-avg_comparison[order(avg_comparison,decreasing=T)]
  }
  else{
    avg_comparison<-avg_comparison[order(avg_comparison,decreasing=F)]
  }
  avg_comparison<-data.frame(id_doc=names(avg_comparison),avg_similarity=avg_comparison)
  meta<-values$tm_meta
  data<-merge(meta,avg_comparison,by = "id_doc")[,c("id_doc","title","date","avg_similarity")]
  if(input$Det_TM_document_outlier_measure=="Euclidean Distance"){
    data<-data[order(data$avg_similarity,decreasing=T),]
  }
  else{
    data<-data[order(data$avg_similarity,decreasing=F),]
  }
  values$TM_document_outlier_tabledata<-data
  data$avg_similarity<-paste0("<b>",data$avg_similarity,"</b")
  datatable(data=data,escape = F,rownames = F)
})

#' download for document similarity matrix
#' depends on:
#'   values$Det_TM_document_outlier_comparison_matrix: matrix of document outliers
#'   input$Det_TM_document_outlier_measure: measures for document outlier
output$Det_TM_outlier_download_document_document_similarity_matrix<-downloadHandler(
  filename = function() {
    paste('outlier_similarity_matrix_',input$Det_TM_document_outlier_measure,"_",  Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-values$Det_TM_document_outlier_comparison_matrix
    write.csv(data, con)
  }
)

#' download the list of outliers
#' depends on: 
#'    input$Det_TM_document_outlier_measure: measures for document outlier from detailed topic model 
#'    values$TM_document_outlier_tabledata: datatable information from document outlier
output$Det_TM_outlier_download_list_of_outliers<-downloadHandler(
  filename = function() {
    paste('outlier_avg_similarity_',input$Det_TM_document_outlier_measure,"_", Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-values$TM_document_outlier_tabledata
    write.csv(data, con)
  }
)



#############################
#        Clustering         #
#############################
#' show UI for document clustering 
#' depends on:
#'   values$tm_theta: topic model theta
#'   values$tm_meta: topic model meta data
#'   input$Det_TM_document_clustering_k: choosen parameter k (k-means) for document clustering
#'   input$Det_TM_document_clustering_max_iterations: choosen maximal number of iterations for document clustering
#'   input$Det_TM_document_clustering_n_start: choosen start point n for random sets for document clustering
#'   values$Det_TM_document_clustering_cluster_data: cluster data from document clustering
#'   values$Det_TM_document_clustering_titles: titles of documents from document clustering 
output$TM_document_clustering_UI<-renderUI({
  data<-t(values$tm_theta)
  titles<-values$tm_meta[,c("id_doc","title"),drop=F]
  rownames(titles)<-titles$id_doc
  titles<-titles[colnames(data),]
  titles<-paste0(titles$title," (",titles$id_doc,")")
  data<-t(data)
  # do kmeans clustering
  km.res <- stats::kmeans(data, centers = input$Det_TM_document_clustering_k, iter.max = input$Det_TM_document_clustering_max_iterations,nstart = input$Det_TM_document_clustering_n_start)
  # apply dimension reduction using pca from factoextra package
  cluster_data<-factoextra::fviz_cluster(data = data,object = km.res)
  values$Det_TM_document_clustering_cluster_data<-cluster_data
  values$Det_TM_document_clustering_titles<-titles
  return(
    tagList(
      tabsetPanel(id = "TM Clustering",
                  tabPanel(title = "Graph",
                           plotly::plotlyOutput(outputId = "Det_TM_document_clustering_kmeans"),
                           tags$br(),
                           tags$hr(),
                           uiOutput(outputId = "Det_TM_document_clustering_metadata_UI")
                  ),
                  tabPanel(title = "Table",
                           DT::dataTableOutput(outputId="Det_TM_document_clustering_table")
                  )
      )
    )
  )
})

#' show kmeans clustering of docuemnts from detailed topic model
#' depends on:
#'   values$Det_TM_document_clustering_cluster_data: cluster data from document clustering
#'   values$Det_TM_document_clustering_titles:
output$Det_TM_document_clustering_kmeans<-plotly::renderPlotly({
  validate(
    need(!is.null(values$Det_TM_document_clustering_cluster_data),message=F)
  )
  cluster_data<-values$Det_TM_document_clustering_cluster_data
  plot_data<-data.frame(cbind(as.character(cluster_data$data$name),cluster_data$data$x,cluster_data$data$y,cluster_data$data$coord,cluster_data$data$cluster),stringsAsFactors = F)
  values$Det_TM_document_clustering_cluster_summary<-plot_data
  x_lab<-cluster_data$labels$x
  y_lab<-cluster_data$labels$y
  colnames(plot_data)<-c("name","x","y","coord","cluster")
  plot_data$x<-round(as.numeric(plot_data$x),digits = 3)
  plot_data$y<-round(as.numeric(plot_data$y),digits = 3)
  fig<-plot_ly(source = "Det_TM_document_clustering_kmeans", data=plot_data,x=~x,y=~y,type="scatter",mode="markers",color=~cluster,symbol=~cluster,marker=list(size=input$Det_TM_document_clustering_marker_size),
               text=values$Det_TM_document_clustering_titles, key=~name)
  fig<-layout(fig,xaxis=list(title=x_lab),yaxis=list(title=y_lab))
  fig
})


#' download document clustering results
#' depends on:
#'   values$Det_TM_document_clustering_cluster_summary: summary of cluster from document clustering
output$Det_TM_document_clustering_download_clustering_result<-downloadHandler(
  filename = function() {
    paste('cluster_result', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    data<-as.matrix(values$Det_TM_document_clustering_cluster_summary)
    write.csv(data, con)
  }
)


#' show meta data from document clustering
#' depends on:
#'  values$tm_meta: topic model meta data
output$Det_TM_document_clustering_metadata_UI<-renderUI({
  eventdata<-event_data("plotly_click",source = "Det_TM_document_clustering_kmeans")
  validate(
    need(!is.null(eventdata), "Click on a marker to get more information for the corresponding document")
  )
  doc_id<-eventdata$key
  meta<-values$tm_meta[which(values$tm_meta[,"id_doc"]==doc_id),,drop=F]
  meta_extra<-values$TM_meta[which(values$tm_meta[,"id_doc"]==doc_id),,drop=F]
  return(
    tagList(
      tags$h5(tags$b("Document ID")),
      tags$div(doc_id),
      tags$h5(tags$b("Title")),
      tags$div(meta$title),
      tags$h5(tags$b("Date")),
      tags$div(meta$date),
      tags$h5(tags$b("Number of Token")),
      tags$div(meta$token),
      shinyWidgets::materialSwitch(inputId = "Det_TM_document_clustering_show_body",label = "show whole document",value = F,status = "default"),
      conditionalPanel(condition='input.Det_TM_document_clustering_show_body==true',
                       tags$h5(tags$b("Text")),
                       tags$div(meta$body)
      ),
      tags$hr(),
      shinyWidgets::materialSwitch(inputId = "Det_TM_document_clustering_show_metadata",label = "show all metadata",value = F,status = "default"),
      conditionalPanel(condition='input.Det_TM_document_clustering_show_metadata==true',
                       lapply(X = setdiff(colnames(meta_extra),c("id_doc","token")),FUN = function(x){
                         return(tagList(
                           tags$h5(tags$b(x)),
                           tags$div(meta_extra[1,x])
                         ))
                       })        
                       
      )
    )
  )
})

#' view cluster datatable from current document clustering
#' depends on:
#'   values$Det_TM_document_clustering_cluster_summary: summary of clusters from current document clustering
#'   values$Det_TM_document_clustering_titles: titles of the documents in currenct document clustering
output$Det_TM_document_clustering_table<-DT::renderDataTable({
  cluster_result<-values$Det_TM_document_clustering_cluster_summary
  cluster_result<-cbind(cluster_result,values$Det_TM_document_clustering_titles)
  #browser()
  clusters<-1:length(unique(cluster_result[,5]))
  clusters_elements<-lapply(X = clusters,FUN = function(x){
    data.frame(cluster_result[which(cluster_result[,5]==x),6],stringsAsFactors = F)
  })
  data<-matrix(c(""),max(unlist(lapply(X = clusters_elements,FUN = nrow))),length(clusters))
  for(i in clusters){
    data[1: nrow(clusters_elements[[i]]),i]<-clusters_elements[[i]][,1]
  }  
  colnames(data)<-paste0("Cluster ",clusters)
  datatable(data = data,rownames = T,selection = "none",options=list(
    pageLength=max(unlist(lapply(X = clusters_elements,FUN = nrow)))
  )
  )
})



####################################################
###                Document Grouping             ###
####################################################

#' show page for document grouping
output$TM_document_grouping_UI<-renderUI({
  
  
  return(
    tagList(
      tabsetPanel(id = "Det_TM_grouping",
                  tabPanel(title = "Group 1",style='padding-right:0px; padding-left:0px;',
                           checkboxInput(inputId="Det_TM_grouping_quantile_group1",label="use quantiles",value=T),
                           conditionalPanel(condition="input.Det_TM_grouping_quantile_group1==true",
                                            tags$h3("Quantiles"),
                                            DT::dataTableOutput(outputId = "Det_TM_grouping_quantile_group1_quantiles"),
                                            tags$hr()
                           ),
                           tags$h3("Countries"),
                           DT::dataTableOutput(outputId = "Det_TM_grouping_group1")
                  ),
                  tabPanel(title = "Group 2",style='padding-right:0px; padding-left:0px;',
                           checkboxInput(inputId="Det_TM_grouping_quantile_group2",label="use quantiles",value=T),
                           conditionalPanel(condition="input.Det_TM_grouping_quantile_group2==true",
                                            tags$h3("Quantiles"),
                                            DT::dataTableOutput(outputId = "Det_TM_grouping_quantile_group2_quantiles"),
                                            tags$hr()
                           ),
                           tags$h3("Countries"),
                           DT::dataTableOutput(outputId = "Det_TM_grouping_group2")
                  ),
                  tabPanel(title="Comparison",
                           box(title ="Countries Group 1",width = 6,solidHeader = T,status = "primary",collapsible = T,
                               wordcloud2Output(outputId = "Det_TM_grouping_wc_group1")
                           ),
                           box(title ="Countries Group 2",width = 6,solidHeader = T,status = "primary",collapsible = T,
                               wordcloud2Output(outputId = "Det_TM_grouping_wc_group2")      
                           ),
                           tags$hr(),
                           tabsetPanel(id = "Det_TM_grouping_output",
                                       tabPanel(title = "Table",
                                                tags$h4("Average Topic Distributons"),
                                                DT::dataTableOutput(outputId = "Det_TM_grouping_avg_topic_distribution_table")
                                       ),
                                       tabPanel(title = "Plot",
                                                tags$h4("Average Topic Distributons"),
                                                plotlyOutput(outputId = "Det_TM_grouping_avg_topic_distribution_plot")
                                       )
                           ),
                           tags$h4("Correlation"),
                           uiOutput(outputId = "Det_TM_grouping_correlation")
                           
                           
                  )
      )
    ))
})

#' grouping correlations from detailed topic models
#' depends on:
#'   values$TM_meta: topic model meta data
#'   input$Det_TM_grouping_group1_rows_all: all rows from group1 elements
#'   values$tm_theta: topic model theta
#'   input$Det_TM_grouping_group2_rows_all: all rows from group2 elements
output$Det_TM_grouping_correlation<-renderUI(({
  correlation<-cor(x = colMeans(values$tm_theta[values$TM_meta[input$Det_TM_grouping_group1_rows_all,"id_doc"],,drop=F]),y = colMeans(values$tm_theta[values$TM_meta[input$Det_TM_grouping_group2_rows_all,"id_doc"],,drop=F]))
  text<-HTML(paste0("Correlation: <b>",correlation,"</b>"))
  return(text)
}))


#' show average topic from grouped documents of the detailed topic model
#' depends on:
#'   values$TM_meta: topic model meta data
#'   input$Det_TM_grouping_group1_rows_all: all rows of documents from group 1
#'   input$Det_TM_grouping_group2_rows_all: all rows of documents from group 2
#'   values$tm_theta: topic model theta
output$Det_TM_grouping_avg_topic_distribution_plot<-plotly::renderPlotly({
  validate(
    need(!is.null(input$Det_TM_grouping_group1_rows_all),message=F)
  )
  
  ids_group1<-values$TM_meta[input$Det_TM_grouping_group1_rows_all,"id_doc"]
  ids_group2<-values$TM_meta[input$Det_TM_grouping_group2_rows_all,"id_doc"]
  theta_group1<-colMeans(values$tm_theta[ids_group1,,drop=F])
  theta_group2<-colMeans(values$tm_theta[ids_group2,,drop=F])
  
  p<-plotly::plot_ly(z=rbind(theta_group2,theta_group1),x=paste0("Topic:",names(theta_group1)),y=c("Group 2","Group 1"),type="heatmap")%>%
    plotly::layout(xaxis=list(type="category"),yaxis=list(type="category"))
  return(p)
})

#' average topic distribution datatable from documents from grouping
#' depends on:
#'   input$Det_TM_grouping_group1_rows_all: all rows of documents from group 1
#'   input$Det_TM_grouping_group2_rows_all: all rows of documents from group 2
#'   values$TM_meta: topic model meta data
#'   values$tm_theta: topic model theta
output$Det_TM_grouping_avg_topic_distribution_table<-DT::renderDataTable({
  validate(
    need(!is.null(input$Det_TM_grouping_group1_rows_all),message=F)
  )
  ids_group1<-values$TM_meta[input$Det_TM_grouping_group1_rows_all,"id_doc"]
  ids_group2<-values$TM_meta[input$Det_TM_grouping_group2_rows_all,"id_doc"]
  theta_group1<-colMeans(values$tm_theta[ids_group1,,drop=F])
  theta_group2<-colMeans(values$tm_theta[ids_group2,,drop=F])
  #browser()
  
  data<-rbind(theta_group1,theta_group2)
  data<-round(data,digits = 3)
  
  colnames(data) <- paste0("<b>Topic ",1:ncol(values$tm_theta),"</b>")
  rownames(data) <- c("Group 1","Group 2")
  return(DT::datatable(data = data, selection = "none",escape=F,class = 'cell-border stripe',options = list(paging=F,dom="t"))%>%
           formatStyle(colnames(data),
                       background = styleColorBar(range(data), 'lightblue'),
                       backgroundSize = '98% 88%',
                       backgroundRepeat = 'no-repeat',
                       backgroundPosition = 'center')
  )
})

#' datatable of documents in group 1
#' depends on:
#'   input$Det_TM_grouping_group1_columns: possible parameters to group documents found in columns of the document information
#'   values$TM_meta: topic model meta data
#'   input$Det_TM_grouping_quantile_group1: quantiles for group 1
output$Det_TM_grouping_group1<-DT::renderDataTable({
  validate(
    need(length(input$Det_TM_grouping_group1_columns)>0,message="Select atleast one column for filtering")
  )
  meta<-values$TM_meta[,unique(union("title",input$Det_TM_grouping_group1_columns)),drop=F]
  quantiles<-matrix(c(0),0,5)
  for(i in 1:ncol(meta)){
    if(all(varhandle::check.numeric(v = meta[,i],exceptions = "NA"))){
      if(input$Det_TM_grouping_quantile_group1==TRUE){
        quantiles<-rbind(quantiles,quantile(as.numeric(meta[,i]),na.rm = T))
        rownames(quantiles)[nrow(quantiles)]<-colnames(meta)[i]
        meta[,i]<-get_quantile_belonging(all_data = meta[,i],sample = meta[,i])
        meta[which(is.na(meta[,i])),i]<-"NA"
        meta[,i]<-as.factor(meta[,i])
      }
      else{
        class(meta[,i])<-"numeric"
      }
    }
    else{
      if(colnames(meta)[i]=="affils"){
        print("use regexp")
      }
      else{
        meta[,i]<-as.factor(meta[,i])
      }
    }
  }
  values$Det_TM_grouping_quantiles_group1<-quantiles
  datatable(data = meta,rownames = F,escape = F,selection = "none",filter = list(position='top',clear=TRUE),options=list(
    autoWidth = TRUE,
    search = list(regex = TRUE, caseInsensitive = FALSE))
  )
})

#' datatable for quantiles for group 1
#' depends on:
#'  values$Det_TM_grouping_quantiles_group1: quantiles for group one
output$Det_TM_grouping_quantile_group1_quantiles<-DT::renderDataTable({
  validate(
    need(
      !is.null(values$Det_TM_grouping_quantiles_group1),message=F
    )
  )
  data=values$Det_TM_grouping_quantiles_group1
  datatable(data=data,rownames = T,selection="none",options=list(dom="t"))
})



#' datatable for elements of group 2
#' depends on:
#'   input$Det_TM_grouping_group2_columns: possible parameters to group documents found in columns of the document information
#'   input$Det_TM_grouping_quantile_group2: quantiles to group documents 
output$Det_TM_grouping_group2<-renderDataTable({
  validate(
    need(length(input$Det_TM_grouping_group2_columns)>0,message="Select atleast one column for filtering")
  )
  meta<-values$TM_meta[,unique(union("title",input$Det_TM_grouping_group2_columns)),drop=F]
  quantiles<-matrix(c(0),0,5)
  for(i in 1:ncol(meta)){
    if(all(varhandle::check.numeric(v = meta[,i],exceptions = "NA"))){
      if(input$Det_TM_grouping_quantile_group2==TRUE){
        quantiles<-rbind(quantiles,quantile(as.numeric(meta[,i]),na.rm = T))
        rownames(quantiles)[nrow(quantiles)]<-colnames(meta)[i]
        meta[,i]<-get_quantile_belonging(all_data = meta[,i],sample = meta[,i])
        meta[which(is.na(meta[,i])),i]<-"NA"
        meta[,i]<-as.factor(meta[,i])
      }
      else{
        class(meta[,i])<-"numeric"
      }
    }
    else{
      if(colnames(meta)[i]=="affils"){
        print("use regexp")
      }
      else{
        meta[,i]<-as.factor(meta[,i])
      }
    }
  }
  values$Det_TM_grouping_quantiles_group2<-quantiles
  datatable(data = meta,rownames = F,escape = F,selection = "none",filter = list(position='top',clear=TRUE),options=list(
    autoWidth = TRUE,
    search = list(regex = TRUE, caseInsensitive = FALSE))
  )
})

#' datatable for quantiles for group 2
#' depends on:
#'  values$Det_TM_grouping_quantiles_group2: quantiles for group 2
output$Det_TM_grouping_quantile_group2_quantiles<-DT::renderDataTable({
  validate(
    need(
      !is.null(values$Det_TM_grouping_quantiles_group2),message=F
    )
  )
  data=values$Det_TM_grouping_quantiles_group2
  datatable(data=data,rownames = T,selection="none",options=list(dom="t"))
})


#' render word cloud depending of elements of group1
#' depends on:
#'  input$Det_TM_grouping_group1_rows_all: all rows of docuements from group 1
output$Det_TM_grouping_wc_group1<-renderWordcloud2({
  validate(
    need(!is.null(input$Det_TM_grouping_group1_rows_all),message=F)
  )
  titles<-values$TM_meta[ input$Det_TM_grouping_group1_rows_all,"title"]
  wordcloud2(data = data.frame(word=titles,freq=rep(1,length(titles))),size = 0.15,color = "random-light", backgroundColor = "black",fontFamily = "Helvetica", minRotation = -pi/2, maxRotation = -pi/2)
  
})

#' render word cloud depending of elements of group2
#' depends on:
#'  input$Det_TM_grouping_group2_rows_all: all rows of docuements from group 2
output$Det_TM_grouping_wc_group2<-renderWordcloud2({
  validate(
    need(!is.null(input$Det_TM_grouping_group2_rows_all),message=F)
  )
  titles<-values$TM_meta[ input$Det_TM_grouping_group2_rows_all,"title"]
  wordcloud2(data = data.frame(word=titles,freq=rep(1,length(titles))),size = 0.15,color = "random-light", backgroundColor = "black",fontFamily = "Helvetica", minRotation = -pi/2, maxRotation = -pi/2)
  
})





###########################################################
#               Model Reproducibility                     #
###########################################################

#' show model reproducibility
#' depends on: 
#'   values$tm_phi: topic model phi
#'   input$Det_TM_reproducibility_models: topic model information for reproducibility of the models
output$TM_model_reproducibility_UI<-renderUI({
  
  
  return(tagList(
    tags$br(),
    uiOutput("Det_TM_model_reproducibility_avg_overlap")%>%withSpinner(),
    tags$hr(),
    DT::dataTableOutput(outputId = "Det_TM_model_reproducibility_result_table"),
    
    tags$h3("Specific Information"),
    tags$br(),
    column(1,
           selectInput(inputId="Det_TM_model_reproducibility_specific_topic","Topic to analyze:",choices=1:nrow(values$tm_phi))
    ),
    column(5,
           selectInput(inputId="Det_TM_model_reproducibility_specific_resultset","Resultset to compare with:",choices=input$Det_TM_reproducibility_models)
    ),
    tags$br(),
    tags$hr(),
    DT::dataTableOutput(outputId = "Det_TM_model_reproducibility_specific_table"),
    tags$br(),
    tags$hr(),
    uiOutput(outputId = "Det_TM_model_reproducibility_specific_topic_reference_UI"),
    wordcloud2Output(outputId = "Det_TM_model_reproducibility_specific_wordcloud")
    
  ))
  
  
  
})

#' datatable for specific reproducibility of the topic model 
#' depends on:
#'   values$Det_TM_model_reproducibility_top_words_per_result_per_topic: top words per topic of all results for model reproducibility
#'   input$Det_TM_reproducibility_models: choosen models for reproducability calculation 
#'   input$Det_TM_model_reproducibility_specific_resultset: reproducibility for specific sets of results
#'   input$Det_TM_reproducibility_number_of_words: reproducibility of the number of words
#'   values$Det_TM_model_reproducibility_specific_number_of_reference_topics: reproducibility of a specific number of reference topics
#'   values$Det_TM_model_reproducibility_top_words_per_result_per_topic: top words of topics per result 
output$Det_TM_model_reproducibility_specific_table<-DT::renderDataTable({
  validate(
    need(!is.null(values$Det_TM_model_reproducibility_top_words_per_result_per_topic),"Please calculate the Average Values first"),
    need(!is.null(isolate(input$Det_TM_reproducibility_models)),message=F)
  )
  found_match_percentage<-NULL
  
  topic_i<-values$Det_TM_model_reproducibility_top_words_per_result_per_topic[[1]][[as.numeric(input$Det_TM_model_reproducibility_specific_topic)]][1:isolate(input$Det_TM_reproducibility_number_of_words)]
  matches<-NULL
  max_sim<-NULL
  j<-which(isolate(input$Det_TM_reproducibility_models)==input$Det_TM_model_reproducibility_specific_resultset)+1
  similarities<-unlist(lapply(values$Det_TM_model_reproducibility_top_words_per_result_per_topic[[j]],FUN = function(x){
    relative_number_of_shared_elements(a = topic_i,x[1:isolate(input$Det_TM_reproducibility_number_of_words)])
  }))
  data<-similarities
  values$Det_TM_model_reproducibility_specific_number_of_reference_topics<-length(values$Det_TM_model_reproducibility_top_words_per_result_per_topic[[j]])
  names(data)<-paste("Topic:",1:length(values$Det_TM_model_reproducibility_top_words_per_result_per_topic[[j]]))
  data<-round(data,digits=3)
  data<-t(as.data.frame(data))
  datatable(data = data,selection=list(mode="single", target="cell"),rownames = F,class = 'cell-border stripe',
            options=list(dom="t",pageLength=length(values$Det_TM_model_reproducibility_top_words_per_result_per_topic[[j]]))) %>%
    formatStyle(colnames(data),
                background = styleColorBar(range(data), 'lightblue'),
                backgroundSize = '98% 88%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center')
})

#' show reproducibility of a referenced specific topic
#' depends on:
#'  values$Det_TM_model_reproducibility_specific_number_of_reference_topics: reproducibility of a specific number of referenced topics
output$Det_TM_model_reproducibility_specific_topic_reference_UI<-renderUI({
  validate(
    need(!is.null(values$Det_TM_model_reproducibility_specific_number_of_reference_topics),message=F)
  )
  return(tagList(
    fluidRow(
      column(2,offset=1,
             selectInput(inputId="Det_TM_model_reproducibility_specific_topic_reference",label="Reference Topic:",choices=1:values$Det_TM_model_reproducibility_specific_number_of_reference_topics)
      ),
      column(2,
             numericInput(inputId="Det_TM_model_reproducibility_specific_wordcloud_size",value=0.3,min=0.001,max=2,step=0.05,label="wordcloud word size")
      )
    )
  ))
}) 

#' wordcloud for specific model reproducibility
#' depends on:
#'   input$Det_TM_model_reproducibility_specific_topic_reference: model reproducibility of a specific topic for reference reasons
#'   values$Det_TM_model_reproducibility_calculated: calculated model reproducibility for the detailed topic model
#'   values$Det_TM_model_reproducibility_top_words_per_result_per_topic: reproducibility of the top words per topic of the different results
#'   input$Det_TM_model_reproducibility_specific_topic: reproducibility of a specific topic
#'   input$Det_TM_reproducibility_models: models for reproducability calculation
#'   input$Det_TM_model_reproducibility_specific_resultset: reproducibilitx for a specific set of results
#'   input$Det_TM_reproducibility_number_of_words: reproducibility of number of words
#'   input$Det_TM_model_reproducibility_specific_wordcloud_size: reproducibility of specific wordcloud size
output$Det_TM_model_reproducibility_specific_wordcloud<-wordcloud2::renderWordcloud2({
  validate(
    need(!is.null(input$Det_TM_model_reproducibility_specific_topic_reference),message=F),
    need(isTRUE(values$Det_TM_model_reproducibility_calculated),"Please press Calculate first")
  )
  topic_i<-values$Det_TM_model_reproducibility_top_words_per_result_per_topic[[1]][[as.numeric(input$Det_TM_model_reproducibility_specific_topic)]][1:isolate(input$Det_TM_reproducibility_number_of_words)]
  topic_j<-values$Det_TM_model_reproducibility_top_words_per_result_per_topic[[which(isolate(input$Det_TM_reproducibility_models)==input$Det_TM_model_reproducibility_specific_resultset)+1]][[as.numeric(input$Det_TM_model_reproducibility_specific_topic_reference)]][1:isolate(input$Det_TM_reproducibility_number_of_words)]
  shared<-intersect(topic_i,topic_j)
  just_i<-setdiff(topic_i,topic_j)
  just_j<-setdiff(topic_j,topic_i)
  colors_shared<-rep("chartreuse",length(shared))
  colors_i<-rep("darkorange",length(just_i))
  colors_j<-rep("aquamarine",length(just_j))
  d<-c(shared,just_i,just_j)
  d<-as.data.frame(cbind(d,rep(0.1,length(d))))
  class(d$V2)<-"numeric"
  colors<-c(colors_shared,colors_i,colors_j)
  wc<-wordcloud2::wordcloud2(d,size=input$Det_TM_model_reproducibility_specific_wordcloud_size,color = colors,backgroundColor = "black", minRotation = -pi/2, maxRotation = -pi/2,fontFamily = "Helvetica")   
  return(wc)
})

#' observe reproducibility calculation
#' depends on:
#'   input$Det_TM_reproducibility_calculate:
#'   values$Details_Data_TM: details of topic model data
#'   input$Det_TM_reproducibility_models: reproducibility of choosen models
#'   input$Det_TM_reproducibility_lambda: parameter lambda for reproducibility calculation 
#'   input$Det_TM_reproducibility_number_of_words: reproducibility for number of words
#'   values$Det_TM_model_reproducibility_top_words_per_result_per_topic: reproducibility for top words per topic of different results
#'   values$Det_TM_model_reproducibility_calculated: calculated model reprocucibility
#'   input$Det_TM_reproducibility_overlap: overlapping elements in reproduciblity calculation 
#'   values$Det_TM_model_reproducibility_found_match_percentage: reproducibility for found matches in percentage
observeEvent(input$Det_TM_reproducibility_calculate,ignoreNULL = F,{
  output$Det_TM_model_reproducibility_avg_overlap<-renderUI({
    top_words_per_result_per_topic<-list()
    results<-c(values$Details_Data_TM,isolate(input$Det_TM_reproducibility_models))
    validate(
      need(length(results)>1,"Plese specify atleast one model to compare")
    )
    results<-paste0(results,"/data_TM.RData")
    for(i in 1:length(results)){
      load(results[i])
      topics<-list()
      rel<-calculate_topic_relevance(lambda = isolate(input$Det_TM_reproducibility_lambda),phi = phi,theta = theta,doc.length = doc.length)
      for(j in 1:nrow(phi)){
        topics[[j]]<-names(sort(rel[,j],decreasing = T)[1:isolate(input$Det_TM_reproducibility_number_of_words)])
      }
      top_words_per_result_per_topic[[i]]<-topics
    }
    values$Det_TM_model_reproducibility_top_words_per_result_per_topic<-top_words_per_result_per_topic
    values$Det_TM_model_reproducibility_calculated<-TRUE
    found_match_percentage<-NULL
    ausgangsresult=1
    for( i in 1:nrow(values$tm_phi)){
      topic_i<-top_words_per_result_per_topic[[ausgangsresult]][[i]][1:isolate(input$Det_TM_reproducibility_number_of_words)]
      matches<-NULL
      max_sim<-NULL
      for(j in setdiff(1:length(results),ausgangsresult)){
        max_sim<-max(unlist(lapply(top_words_per_result_per_topic[[j]],FUN = function(x){
          relative_number_of_shared_elements(a = topic_i,x[1:isolate(input$Det_TM_reproducibility_number_of_words)])
        }))
        )
        if(max_sim>isolate(input$Det_TM_reproducibility_overlap)){
          matches<-c(matches,1)
        }
        else{
          matches<-c(matches,0)
        }
      }
      matches<-mean(matches)
      found_match_percentage<-c(found_match_percentage,matches)
    }
    
    values$Det_TM_model_reproducibility_found_match_percentage<-found_match_percentage
    return(HTML(paste0("Average Topic Overlap: <b>",mean(found_match_percentage),"</b>")))
  })
  
  
  
  #' obsereve datatable for results of model reproducibility calculation
  #' depends on:
  #'   values$Det_TM_model_reproducibility_top_words_per_result_per_topic: reproducibility of top words per topic of different results
  #'   values$Det_TM_model_reproducibility_found_match_percentage: reproducibility of found matches in percentage
  #'   values$tm_phi: topic model parameter phi
  output$Det_TM_model_reproducibility_result_table<-DT::renderDataTable({
    validate(
      need(!is.null(isolate(values$Det_TM_model_reproducibility_top_words_per_result_per_topic)),message=F),
      need(!is.null(isolate(values$Det_TM_model_reproducibility_found_match_percentage)),message=F)
    )
    data<-paste("<b>",round(isolate(values$Det_TM_model_reproducibility_found_match_percentage),digits = 3),"</b>")
    relevant_words<-unlist(lapply(isolate(values$Det_TM_model_reproducibility_top_words_per_result_per_topic[[1]]),FUN = function(x){
      paste(x,collapse=", ")}
    )
    )
    data<-cbind(data,relevant_words)
    rownames(data)<-paste0("Topic: ",1:nrow(values$tm_phi))
    colnames(data)<-c("Reproducibility","most relevant words")
    datatable(data = data,options=list(dom="t",pageLength=nrow(values$tm_phi)),escape = F,selection = "single")
  })
})

#' change selected topic, if user selects a row in the overview table
#' depends on:
#'  input$Det_TM_model_reproducibility_result_table_rows_selected: selected rows in data table of the results of model reproducibility
observe({
  s = input$Det_TM_model_reproducibility_result_table_rows_selected
  if (length(s)) {
    updateSelectInput(session = session,inputId = "Det_TM_model_reproducibility_specific_topic",selected = as.numeric(s))
  }
})


#' change selected comparison topic, if user selects a row in the overview table
#' depends on:
#'  input$Det_TM_model_reproducibility_specific_table_cells_selected: selected cells from data table of specific model reproducibility
observe({
  s = input$Det_TM_model_reproducibility_specific_table_cells_selected
  if (length(s)) {
    updateSelectInput(session = session,inputId = "Det_TM_model_reproducibility_specific_topic_reference",selected = (as.numeric(s[1,2])+1))
  }
})





######################################################################################################################################################
#                                                          Topic Proportions                                                                         #
######################################################################################################################################################

#' create figure showing topic proportions over time
#' depends on:
#'   input$Det_TM_proportions_lambda: lambda value for calculating topic labels
#'   input$Det_TM_proportions_Date_Split_How: how to split given documents by their date information
#'   input$Det_TM_proportions_Chunksize: number of chunks
#'   input$Det_TM_proportions_ByDate_Type: what kind of time intervall should be used
#'   input$Det_TM_proportions_ByDate_n: should multiple time intervalls be aggregated
#'   input$Det_TM_proportions_number_of_words: number of words per topic label
#'   values$tm_meta: metadata for given documents
#'   values$tm_theta: document-topic distribution
#'   values$tm_phi: topic-word distributions
#'   values$tm_doc.length: documents length
output$TM_topic_proportions_UI<-renderUI({
  meta<-values$tm_meta
  theta<-values$tm_theta
  phi<-values$tm_phi
  
  #create time slices
  dates<-meta$date
  
  if(input$Det_TM_proportions_ByDate_Type=="Year"){
    dates<-substr(dates,0,4)
  }
  if(input$Det_TM_proportions_ByDate_Type=="Month"){
    dates<-substr(dates,0,7)
  }
  if(input$Det_TM_proportions_ByDate_Type=="Week"){
    dates<-strftime(as.character(dates),format="%Y-%V")
  }
  if(input$Det_TM_proportions_ByDate_Type=="Day"){
    dates<-substr(dates,0,11)
  }
  unique_dates <- unique(dates)
  doc_belongings_to_time_slices<-rep(0,nrow(theta))
  time_slices <- NULL 
  time_slice_names<-NULL
  if(input$Det_TM_proportions_Date_Split_How=="By Date"){
    n<-input$Det_TM_proportions_ByDate_n
    for (i in 1:ceiling(length(unique_dates)/n)){
      date_start<-unique_dates[(((i-1)*n)+1)]
      date_end<-unique_dates[min(length(unique_dates),(((i)*n)))]
      if(n==1){
        time_slice_names<-c(time_slice_names,date_start)
      }
      else{
        if(date_start==date_end){
          time_slice_names<-c(time_slice_names,date_start)
        }
        else{
          time_slice_names<-c(time_slice_names,paste0(date_start," - ",date_end))
        }
        
      }
      time_slices<-c(time_slices,length(which(dates%in%unique_dates[(i*n):min(length(unique_dates),(((i+1)*n)-1))])))
      doc_belongings_to_time_slices[which(dates%in%unique_dates[(((i-1)*n)+1):min(length(unique_dates),(((i)*n)))])]<-i
    }
  }
  else{
    n<-input$Det_TM_proportions_Chunksize
    validate(
      need(n<=length(unique_dates),"The number of chunks can't be bigger than the number of unique dates")
    )
    dates_table<-data.frame(table(dates),stringsAsFactors = F)
    dates_table<-data.frame(min_date=dates_table$dates,max_date=dates_table$dates,count=dates_table$Freq,all_dates=as.character(dates_table$dates),stringsAsFactors = F)
    if(nrow(dates_table)>n){
      repeat{
        min<-which.min(unlist(lapply(1:(nrow(dates_table)-1),FUN = function(x){
          sum(as.numeric(dates_table$count[c(x,(x+1))]))
        })
        )
        )
        ind<-c(min,(min+1))
        min_date<-min(as.character(dates_table$min_date[ind]))
        max_date<-max(as.character(dates_table$max_date[ind]))
        count_new<-sum(as.numeric(dates_table$count[ind]))
        all_dates_in_this_chunk<-paste0(unique(union(dates_table$all_dates[ind[1]],dates_table$all_dates[ind[2]])),collapse = ",")
        dates_table<-rbind(dates_table,c(min_date,max_date,count_new,all_dates_in_this_chunk))
        dates_table<-dates_table[-ind,]
        dates_table<-dates_table[order(dates_table$min_date,decreasing = F),]
        if(nrow(dates_table)==n){
          break
        }
      }
    }
    time_slices<-as.numeric(dates_table$count)
    time_slice_names<-unlist(lapply(X = 1:nrow(dates_table),FUN = function(x){
      min_date<-as.character(dates_table$min_date[x])
      max_date<-as.character(dates_table$max_date[x])
      if(min_date==max_date){
        return(max_date) 
      }
      else{
        return(paste(min_date,"-",max_date))
      }
    })
    )
    for(i in 1:nrow(dates_table)){
      doc_belongings_to_time_slices[which(dates%in%stringr::str_split(string = dates_table$all_dates[i],pattern = ",",simplify = T))]<-i
    }
    
  }
  time_slice_names<-paste0(time_slice_names," (",time_slices,")")
  
  time<-doc_belongings_to_time_slices
  # get mean topic proportions per decade
  topic_proportion_per_time_intervall <- aggregate(theta, by = list(time = time), mean)
  # set topic names to aggregated columns
  topicNames<-unlist(lapply(1:nrow(phi),FUN = function(x){
    relevance<-calculate_topic_relevance(lambda = input$Det_TM_proportions_lambda,phi = phi,theta = theta,doc.length = values$tm_doc.length)
    paste(names(sort(calculate_topic_relevance(lambda = input$Det_TM_proportions_lambda,phi = phi,theta = theta,doc.length = values$tm_doc.length)[,x],decreasing = T)[1:input$Det_TM_proportions_number_of_words]),collapse = ", ")
  }))
  colnames(topic_proportion_per_time_intervall)[2:(nrow(phi)+1)] <- topicNames
  
  # reshape data frame
  vizDataFrame <- melt(topic_proportion_per_time_intervall, id.vars = "time")
  
  # plot topic proportions per deacde as bar plot
  values$Det_TM_vizDataFrame<-vizDataFrame
  values$Det_TM_time_slice_names<-time_slice_names
  return(tagList(
    div(style = "height: 68vh;",
        plotlyOutput(outputId = "Det_TM_proportion_plot",height = "95%")
    ))
  )
  
})

#' render plot showing topic proportions over time
#' depends on:
#'   values$Det_TM_vizDataFrame: dataframe containing nec. information for plot
#'   values$Det_TM_time_slice_names: names of time slices
output$Det_TM_proportion_plot<-renderPlotly({
  require(pals)
  data_real<-values$Det_TM_vizDataFrame
  #reshape data
  n_dates<-length(unique(data_real$time))
  data_t<-data_real[1:n_dates,c(1,3)]
  colnames(data_t)[2]<-as.character(data_real[1,2])
  for(i in 2:length(unique(data_real$variable))){
    data_t<-cbind(data_t,data_real[(((i-1)*n_dates)+1):(i*n_dates),3])
    colnames(data_t)[ncol(data_t)]<-as.character(data_real[(i*n_dates),2])
  }
  getPalette = colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))
  colors<-getPalette((ncol(data_t)-1))
  
  p<-plot_ly(x=values$Det_TM_time_slice_names,y=data_t[,2],type="bar",name=colnames(data_t)[2],marker=list(color = colors[1]))
  for(i in 3:ncol(data_t)){
    p<-add_trace(p=p, y=data_t[,i],name=colnames(data_t)[i],marker=list(color=colors[(i-1)]))  
  }
  p <- layout(p,barmode="stack",legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1))
  p
})




########################################################################
##                  topic topic connection                            ##
########################################################################

#' visualuzation for topic topic connection
#' depends on:
#'   input$Det_TM_topic_topic_connection_lambda: detailed topic modeling parameter lambda for topic topic connection
#'   values$tm_phi: topic model phi
#'   values$tm_theta: topic model theta
#'   values$tm_doc.length: topic model document length
#'   input$Det_TM_topic_topic_connection_threshold: threshold for topic-topic connection
#'   values$Det_TM_topic_topic_connection_matrix_corrs_theta: topic-topic connection matrix for correlation theta parameter
#'   values$Det_TM_topic_topic_connection_matrix_corrs_thresh: topic-topic connection matrix for correlation threshold
#'   values$Det_TM_topic_topic_connection_matrix_theta: topic-topic connection matrix for parameter theta
#'   values$Det_TM_topic_topic_connection_matrix_thresh: topic-topic connection matrix for threshold
#'   input$Det_TM_topic_topic_connection_number_of_words: number of words for topic-topic connection
#'   values$Det_TM_topic_topic_connection_dat_theta: dat parameter for topic topic connection
#'   values$Det_TM_topic_topic_connection_dat_thresh: dat threshold for topic topic connection
output$TM_topic_topic_connection_UI<-renderUI({
  relevance_matrix<-calculate_topic_relevance(lambda = input$Det_TM_topic_topic_connection_lambda,phi = values$tm_phi,theta = values$tm_theta,doc.length = values$tm_doc.length)
  theta<-values$tm_theta
  threshold_matrix<-theta
  threshold_matrix[which(threshold_matrix>=input$Det_TM_topic_topic_connection_threshold)]<-1
  threshold_matrix[which(threshold_matrix<input$Det_TM_topic_topic_connection_threshold)]<-0
  
  matrix_corrs_theta<-matrix(c(0),ncol(theta),ncol(theta))
  matrix_corrs_thresh<-matrix(c(0),ncol(theta),ncol(theta))
  
  for(i in 1:nrow(matrix_corrs_theta)){
    for(j in 1:ncol(matrix_corrs_theta)){
      matrix_corrs_theta[i,j]<-cor(x = theta[,i],y = theta[,j])
      matrix_corrs_thresh[i,j]<-cor(x = threshold_matrix[,i],y = threshold_matrix[,j])
    }
  }
  diag(matrix_corrs_thresh)<-NA
  diag(matrix_corrs_theta)<-NA
  
  values$Det_TM_topic_topic_connection_matrix_corrs_theta<-matrix_corrs_theta
  values$Det_TM_topic_topic_connection_matrix_corrs_thresh<-matrix_corrs_thresh
  
  values$Det_TM_topic_topic_connection_matrix_theta<-theta
  values$Det_TM_topic_topic_connection_matrix_thresh<-threshold_matrix
  
  
  topic_labels<-NULL
  for(i in 1:ncol(theta)){
    topic_labels<-c(topic_labels,paste(names(sort(relevance_matrix[,i],decreasing=T)[1:input$Det_TM_topic_topic_connection_number_of_words]),collapse=", "))
  }
  
  l1<-NULL
  for(i in 1:ncol(theta)){
    l1<-c(l1,rep(paste0("Topic:",i),ncol(theta)))
  }
  
  dat <- expand.grid(x = topic_labels, y = topic_labels,stringsAsFactors = T)
  dat$z <- c(matrix_corrs_theta)
  dat$l1<-l1
  dat$l2<-rep(paste("Topic:",1:ncol(theta)),ncol(theta))
  values$Det_TM_topic_topic_connection_dat_theta<-dat
  
  dat_thresh <- expand.grid(x = topic_labels, y = topic_labels,stringsAsFactors = T)
  dat_thresh$z <- c(matrix_corrs_thresh)
  dat_thresh$l1<-l1
  dat_thresh$l2<-rep(paste("Topic:",1:ncol(theta)),ncol(theta))
  values$Det_TM_topic_topic_connection_dat_thresh<-dat_thresh
  
  return(tagList(
    tags$h4("Topic Topic Correlation based on Theta Values"),
    plotlyOutput(outputId = "Det_TM_topic_topic_connection_heatmap_theta"),
    tags$h4("Topic Topic Correlation based on whether a documents exceeds set threshold"),
    plotlyOutput(outputId = "Det_TM_topic_topic_connection_heatmap_thresh")
  ))
  
  
})

#' topic-topic connnection heatmap theta
#' depends on:
#'   values$Det_TM_topic_topic_connection_dat_theta: dat theta for topic topic connection
#'   values$Det_TM_topic_topic_connection_matrix_corrs_theta: topic topic connection matrix for corresponding theta
output$Det_TM_topic_topic_connection_heatmap_theta<-plotly::renderPlotly({
  validate(
    need(!is.null(values$Det_TM_topic_topic_connection_dat_theta),message=F)
  )
  dat<-values$Det_TM_topic_topic_connection_dat_theta
  colorscale <- as.list(1:100)
  correlation<-values$Det_TM_topic_topic_connection_matrix_corrs_theta
  colorlength <- 100
  null_value <- (0 - min(correlation,na.rm = T)) / (max(correlation,na.rm = T) - min(correlation,na.rm = T))        
  border <- as.integer(null_value * colorlength)
  colorscale <- as.list(1:colorlength)
  #colorscale below zero
  s <- scales::seq_gradient_pal("blue", "white", "Lab")(seq(0,1,length.out=border))
  for (i in 1:border) {
    colorscale[[i]] <- c((i - 1) / colorlength, s[i])
  }
  #colorscale above zero
  s <- scales::seq_gradient_pal("white", "red", "Lab")(seq(0,1,length.out=colorlength - border))
  for (i in 1:(colorlength - border)) {
    colorscale[[i + border]] <- c((i + border) / colorlength, s[i])
  }
  plotly::plot_ly(data = dat, x = ~l1, y = ~l2, z = ~z,type="heatmap", hoverinfo = 'text',source = "Det_TM_topic_topic_heatmap_theta",
                  key=~z,
                  colorscale = colorscale, colorbar=list(len=1,limits=c(-1,1)),
                  text = ~paste("<br> Topic Y:</br>", dat$x,
                                "<br> Topic X:</br>", dat$y,
                                "<br> correlation:</br>", round(dat$z,digits = 3)))%>%
    layout(xaxis = list(categoryorder = "trace",title=""),yaxis = list(categoryorder = "trace",title=""))
})


observe({
  a<-event_data("plotly_click", source = "Det_TM_topic_topic_heatmap_theta")
  validate(
    need(!is.null(a),message = F)
  )
  x<-as.numeric(stringr::str_remove_all(string = a$x,pattern = "Topic:"))
  y<-as.numeric(stringr::str_remove_all(string = a$y,pattern = "Topic:"))
  data<-isolate(values$Det_TM_topic_topic_connection_matrix_theta[,c(x,y)])
  rownames(data)<-values$tm_meta[,"title"]
  colnames(data)[1]<-a$x
  colnames(data)[2]<-a$y
  showModal(
    modalDialog(size = "l",
                tags$h4("Distributions for chosen Topic Pair"),
                plot_ly(z=t(data),x=rownames(data),y=colnames(data),type="heatmap")%>%
                  layout(margin=list(b=200))
                
    )
  )
})

#' topic-topic connection threshold for heatmap
#' depends on:
#'   values$Det_TM_topic_topic_connection_dat_thresh: dat threshold for topic-topic connection
#'   values$Det_TM_topic_topic_connection_matrix_corrs_thresh: detailed topic modelling for topic-topic connection matrix and corresponding threshold 
output$Det_TM_topic_topic_connection_heatmap_thresh<-plotly::renderPlotly({
  validate(
    need(!is.null(values$Det_TM_topic_topic_connection_dat_thresh),message=F)
  )
  dat<-values$Det_TM_topic_topic_connection_dat_thresh
  colorscale <- as.list(1:100)
  correlation<-values$Det_TM_topic_topic_connection_matrix_corrs_thresh
  colorlength <- 100
  null_value <- (0 - min(correlation,na.rm = T)) / (max(correlation,na.rm = T) - min(correlation,na.rm = T))        
  border <- as.integer(null_value * colorlength)
  colorscale <- as.list(1:colorlength)
  #colorscale below zero
  s <- scales::seq_gradient_pal("blue", "white", "Lab")(seq(0,1,length.out=border))
  for (i in 1:border) {
    colorscale[[i]] <- c((i - 1) / colorlength, s[i])
  }
  #colorscale above zero
  s <- scales::seq_gradient_pal("white", "red", "Lab")(seq(0,1,length.out=colorlength - border))
  for (i in 1:(colorlength - border)) {
    colorscale[[i + border]] <- c((i + border) / colorlength, s[i])
  }
  plotly::plot_ly(data = dat, x = ~l1, y = ~l2, z = ~z,type="heatmap", hoverinfo = 'text',source = "Det_TM_topic_topic_heatmap_thresh",
                  key=~z,
                  colorscale = colorscale, colorbar=list(len=1,limits=c(-1,1)),
                  text = ~paste("<br> Topic Y:</br>", dat$x,
                                "<br> Topic X:</br>", dat$y,
                                "<br> correlation:</br>", round(dat$z,digits = 3)))%>%
    layout(xaxis = list(categoryorder = "trace",title=""),yaxis = list(categoryorder = "trace",title=""))
})


observe({
  a<-event_data("plotly_click", source = "Det_TM_topic_topic_heatmap_thresh")
  validate(
    need(!is.null(a),message = F)
  )
  x<-as.numeric(stringr::str_remove_all(string = a$x,pattern = "Topic:"))
  y<-as.numeric(stringr::str_remove_all(string = a$y,pattern = "Topic:"))
  data<-isolate(values$Det_TM_topic_topic_connection_matrix_thresh[,c(x,y)])
  rownames(data)<-values$tm_meta[,"title"]
  colnames(data)[1]<-a$x
  colnames(data)[2]<-a$y
  showModal(
    modalDialog(size = "l",
                tags$h4("Distributions for chosen Topic Pair"),
                plot_ly(z=t(data),x=rownames(data),y=colnames(data),type="heatmap")%>%
                  layout(margin=list(b=200))
                
    )
  )
})


#' Top Documents per Topic under LDAVis
#' depends on:
#'   values$tm_theta:topic model theta
#'   values$TM_meta: topic model meta data
#'   input$Det_TM_top_countries_per_topic_high_low: data from detailed topic modelling for top countries per topic seperated in high and low
#'   values$TM_Top_Documents_per_Topic_Data: topic modeling information about top documents per topic
output$TM_Top_Documents_per_Topic_UI<-renderUI({
  theta<-values$tm_theta
  meta<-values$TM_meta
  data<-data.frame(matrix(c(0),ncol(theta),10))
  colnames(data)[1:5]<-paste0("Rank ", 1:5)
  rownames(data)<-paste0("Topic  ",1:ncol(theta))
  for(i in 1:ncol(theta)){
    if(input$Det_TM_top_countries_per_topic_high_low==TRUE){
      top_documents<-order(theta[,i],decreasing=T)[1:5]
    }
    else{
      top_documents<-order(theta[,i],decreasing=F)[1:5]
    }
    titles<-meta[top_documents,"title"]
    top_values<-theta[top_documents,i]
    data[i,1:5]<-paste0("<b>",titles,"</b> (",round(top_values,digits = 2),")")
    data[i,6:10]<-top_values
  }
  values$TM_Top_Documents_per_Topic_Data<-data
  return(DT::dataTableOutput(outputId ="TM_Top_Documents_per_Topic_Table" ))  
})

#' visualize top documents per topic table
#' depends on: 
#'   values$TM_Top_Documents_per_Topic_Data: topic model data for top documents per topic
#'   values$tm_theta: topic model theta
output$TM_Top_Documents_per_Topic_Table<-DT::renderDataTable({
  validate(
    need(!is.null(values$TM_Top_Documents_per_Topic_Data),message=F)
  )
  data<-values$TM_Top_Documents_per_Topic_Data
  datatable(data=data,escape = F,selection = "none",class = 'cell-border stripe',options=list(autowidth=FALSE,pageLength = ncol(values$tm_theta),dom="t",columnDefs = list(list(visible=FALSE, targets=6:10))))%>%
    formatStyle(columns = 1:5,valueColumns = 6:10,target = "cell",
                background = styleColorBar(range(data[,6:10]), 'aquamarine',angle = -90),
                backgroundSize = '98% 88%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center')
  
})





#############################################################
#                   word occurrences                        #
#############################################################

#' visualize detailed topic model word cooccurrence
#' depends on: 
#'   values$Det_TM_dtm: detailed topic model document term matrix
#'   input$Det_TM_ewf_word: detailed topic model for ewf words
#'   values$TM_meta: topic model meta data
#'   values$Det_TM_word_occurrences_table_data: detailed topic model data for word occurrence table
output$Det_TM_word_occurrences_table<-DT::renderDataTable({
  validate(
    need(length(input$Det_TM_ewf_word)>0,message="Please specify atleast one word")
  )
  dtm<-values$Det_TM_dtm
  input$Det_TM_ewf_word
  if(length(input$Det_TM_ewf_word)==1){
    present<-which(dtm[,input$Det_TM_ewf_word]>0)
  }
  else{
    present<-which(rowSums(dtm[,input$Det_TM_ewf_word,drop=F])>0)
  }
  dtm_reduced<-as.matrix(dtm[present,input$Det_TM_ewf_word,drop=F])
  dtm_reduced<-dtm_reduced[order(rowSums(dtm_reduced),decreasing=T),,drop=F]
  dtm_reduced<-cbind(rownames(dtm_reduced),dtm_reduced)
  dtm_reduced[,1]<-unlist(lapply(dtm_reduced[,1],FUN = function(x){
    values$TM_meta$title[which(values$TM_meta$id_doc==x)]
  }))
  
  colnames(dtm_reduced)[1]<-"Document"
  values$Det_TM_word_occurrences_table_data<-dtm_reduced
  datatable(data = dtm_reduced,rownames = F,selection = "single")
})

#' observe which rows of the word occurrence table from detailed topic modeling are selected
#' depends on:
#'   input$Det_TM_word_occurrences_table_rows_selected: selected rows from user in the word occurrence table
#'   values$Det_TM_word_occurrences_table_data: all of the word occurrence table data
observe({
  s = input$Det_TM_word_occurrences_table_rows_selected
  if (length(s)) {
    doc_id<-rownames(values$Det_TM_word_occurrences_table_data)[s]
    updateSelectizeInput(session = session,inputId = "Det_TM_word_frequencies_document",selected = doc_id)
    updateTabsetPanel(session = session,
                      inputId = "Det_TM_words",
                      selected = "Documents")
  }
})

#' visualize the word occurrence in documents
#' depends on:
#'   input$Det_TM_word_frequencies_document: detailed topic model word frequency in documents
#'   values$host: selected host
#'   values$port: selected port
#'   values$Details_Data_TM: detailed data for topic models 
output$Det_TM_word_occurrences_document_UI<-renderUI({
  validate(
    need(
      !is.null(input$Det_TM_word_frequencies_document),message=FALSE
    ),
    need(
      input$Det_TM_word_frequencies_document!="",message="please choose a document"
    )
  )
  identifier<-stringr::str_split(string = input$Det_TM_word_frequencies_document,pattern = "_",simplify = T)
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
    features<-tolower(token[,"lemma"])  
  }
  if(parameters$baseform_reduction=="stemming"){
    features<-tolower(quanteda::tokens_wordstem(quanteda::tokens(paste(token[,"word"],collapse=" ")),lang)$text1)
  }
  token<-cbind(token,features)
  token<-cbind(1:dim(token)[1],token)
  token<-cbind(token,match=rep(FALSE,nrow(token)))
  token[which(features%in%input$Det_TM_ewf_word),"match"]<-TRUE
  
  strings<-apply(token,MARGIN = 1,FUN = function(x){
    if("FALSE"==(x["match"])){
      return(x["word"])
    }
    else{
      return( paste0('<font style="background-color:','#74c1fc"','>',x["word"],'</font>'))
    }
  })
  
  document<-list()
  for(i in 1:dim(token)[1]){
    document[[i]]<-paste0("<span span_nr='",i,"'>",strings[i],"</span>")
  }
  document<-do.call(rbind,document)
  document<-HTML(document)
  tags$p(document)
  
})


