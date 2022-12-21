output$enrichment_topic_control_UI<-renderUI({
  validate(
    need(!is.null( values$topic_enrichment_topic_model_phi),message=F)
  )
  return(tagList(
    numericInput(inputId="enrichment_topic_n","Select Topic:",min=1,max = ncol(values$topic_enrichment_topic_model_phi),value=1),
    uiOutput(outputId = "enrichment_topic_control_details_UI")
  ))
})

output$enrichment_topic_control_details_UI<-renderUI({
  validate(
    need(!is.null( input$enrichment_topic_n),message=F)
  )
  return(tagList(
    tags$div("Topic Label"),
    wordcloud2Output(outputId = "enrichment_topic_control_details_wordcloud")
  ))
})

output$enrichment_topic_control_details_wordcloud <- renderWordcloud2({
  data = data.frame(c("das","ist","ein","beispiel"),1:4)
  wordcloud2(data = data)
})



observeEvent(input$topic_enrichment,{
  validate(
    need(!is.null(input$enrichment_topic),message=F)
  )
  load(paste0("collections/results/topic-model/",input$topic_enrichment,"/data_TM.RData"))
  values$topic_enrichment_topic_model_phi <- phi
  values$topic_enrichment_topic_model_theta <- theta
  #values$topic_enrichment_topic_model_phi <- phi
  #values$topic_enrichment_topic_model_phi <- phi
})