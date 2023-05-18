
#' parameters for sentiment analysis
output$Analysis_Parameter_SA<-renderUI({
  tagList(
    tags$hr(),
    #standard parameters
    fluidRow(
      column(1,
             selectInput(inputId = "SA_baseform",label = "Baseform Reduction",choices = c("stemming","none"),selected = "none")%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Specify whether and how to do baseform reduction. The options are stemming, lemmatization or no baseorm reduction.", placement = "right"
                   )
               )
             ),
      column(1,
             checkboxInput(inputId = "SA_lowercase",label = "Transform tokens to lowercase?",value = T)%>%
               shinyInput_label_embed(
               shiny_iconlink() %>%
                 bs_embed_popover(
                   title = "Should lowercasing (set all characters to lower case) be applied?", placement = "left"
                 )
             )),
      column(2,
             selectInput(inputId = "SA_ngram",label = "ngrams?",choices = c(1,2,3),selected = 1,multiple = T)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Should n-grams be included in the analysis? They will be connected with '_'.", placement = "right"
                   )
               )
             ),
      column(2,
             textInput(inputId = "SA_remove_custom",label = HTML("remove custom words"),placeholder ="Add words (Seperated by ,)")%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Delete specific words from the analysis. Seperate them with ','.", placement = "right"
                   )
               )
      )
    ),
    # Interview specific Parameters
    tags$hr(),
    tags$h4("Interview-specific Parameters"),
    fluidRow(
      column(1,
             checkboxInput(inputId = "SA_interview_use_speaker_info",label="Restrict via speaker?",value=F),
      ),
      column(2,
             conditionalPanel(condition = "input.SA_interview_use_speaker_info==true",
                              selectizeInput(inputId="SA_interview_speaker_info", label= "Speaker Filter:",choices = c("INT_*","IP_*"),multiple=T,options=list(create=T))
             )
      ),
      column(2,
             conditionalPanel(condition = "input.SA_interview_use_speaker_info==true",
                              bsButton(inputId = "SA_interview_show_speakers",label = "Show Speakers in chosen Collection",icon = icon("search"),style = "primary")
             )
      ),
      
    ),
    #specific parameters
    tags$hr(),
    tags$h4("Sentiment parameters"),
    fluidRow(
      column(2,
             selectInput(inputId = "SA_sentiment_dictionary",label = "Sentiment Dictionary",choices = list.files("collections/sentiments/"))%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Specify the sentiment dictionary. You can put a csv file containing the words with their corresponding weights as csv in the directory: 'collections/sentiments/'. ", placement = "right"
                   )
               )
      ),
      column(2,
             selectInput(inputId = "SA_avg",label = "Build Document Score by:",choices = c("mean","sum"))%>%
                           shinyInput_label_embed(
                             shiny_iconlink() %>%
                               bs_embed_popover(
                                 title = "How to aggreagte a score for every document. Mean: average of all scores in a document (document length has small influence).
                                 Sum: sum of all scores in a document (document length can have large influence).", placement = "right"
                               )
                           )
      )
    ),
    bsButton(inputId = "SA_Submit_Script",label = "Submit Request",icon = icon("play-circle"),type = "primary")
  )
})


# show available speakers inside collection
observeEvent(input$SA_interview_show_speakers,{
  load(paste("collections/collections/",input$collection_selected,".RData",sep=""))
  dataset = info[[2]][1,1]
  id_docs = info[[1]][,1]
  id_docs = paste0(id_docs,collapse=" ")
  id_docs<-stringr::str_replace_all(string = as.character(id_docs),pattern = " ",",")
  
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=isolate(values$host),port=isolate(values$db_port))
  rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
  sprecher<-RMariaDB::dbGetQuery(mydb, paste("select sprecher from interview_info where id_doc in (",id_docs,")",
                                             " and trim(dataset)='",dataset,"';",sep = ""))
  if(nrow(sprecher)==0){
    shinyWidgets::sendSweetAlert(session = session,title = "No Speaker Data found",text = "Maybe you have not specified a collection containing interview type data?!",type = "warning")
  }
  else{
    values$SA_interview_speaker_info <- sprecher
    showModal(
      modalDialog(title = HTML(paste0("Speaker Distribution inside Collection: <b>", input$collection_selected,"</b>")),
                  DT::dataTableOutput(outputId = "SA_interview_speaker_info_dist")
      )
    )
  }
}
)

output$SA_interview_speaker_info_dist<-DT::renderDataTable({
  data = values$SA_interview_speaker_info
  data<-as.data.frame(table(data))
  data <- data[order(data[,2],decreasing = T),]
  datatable(data=data)
})

#' start Sentiment analysis script, if submit button is clicked
#' depends on:
#'   input$SA_Submit_Script: submit sentiment analysis script
#'   input$collection_selected: selected collection
#'   input$SA_baseform: should the baseform of words be used?
#'   input$SA_ngram: size of N-grams
#'   input$SA_lowercase: should the words be completely in lowercase
#'   input$SA_sentiment_dictionary: choose sentiment dictionary (depends on language of the text)
#'   input$SA_avg: selected document score aggregation
#'   input$SA_remove_custom: remove custome words
#'   input$analysis_selected: what analysis model is selected
#'   input$use_custom_script: use a custome script?
#'   input$custom_script_options: selected option for a customed script
observeEvent(input$SA_Submit_Script,{
  #save needed parameters
  parameters<-list(collection=input$collection_selected,
                   baseform_reduction=input$SA_baseform,
                   ngrams=input$SA_ngram,
                   lowercase=input$SA_lowercase,
                   Sentiment_Dictionary=input$SA_sentiment_dictionary,
                   Document_Score_Aggregation=input$SA_avg,
                   remove_custom=input$SA_remove_custom,
                   sa_interview_use_speaker=input$SA_interview_use_speaker_info,
                   sa_interview_speaker_filter=input$SA_interview_speaker_info
  )
  #create process ID
  ID<-get_task_id_counter()+1
  set_task_id_counter(ID)
  #save metadata for process
  process_info<-list(ID,isolate(input$collection_selected),isolate(input$analysis_selected),as.character(Sys.time()))
  #save logfile path
  logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
  #create logfile
  write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
  #save data needed in script execution 
  save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
  #start script
  if(input$use_custom_script==TRUE && !is.null(input$custom_script_options)){
    shinyWidgets::sendSweetAlert(session=session,title = "Starting a custom script",text = "You are about to start a custom script. Caution with the calculation and results!",type = "info")
    system(paste0('Rscript collections/scripts/Sentiment_Analysis/',input$custom_script_options,' &'))
  }
  else{
    system(paste('Rscript collections/scripts/Sentiment_Analysis_Script.R','&'))
    #show modal when process is started
    showModal(modalDialog(
      title = "Process started",
      "The process was succesfully started. Check details in 'My Tasks'."
    ))
  }
})
