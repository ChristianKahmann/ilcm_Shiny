
#' selected collections
#' depends on:
#'   input$refi_export_select_collection: refine selected collection for export
get_collection <- reactive({
  DB_get_collections(input$refi_export_select_collection)
})

#' refine collection table for export
output$refi_export_collection_table <- DT::renderDataTable({
  collection <- get_collection()
  num_collections <- seq(nrow(collection))
  btns <- paste0('<button id="refi_export_collection_button_', num_collections, '" data-collection="', collection$name, '" type="button" class="btn btn-default action-button" onclick="getCollection(this);"><i class="fa fa-download"></i></button>')
  
  reactive_data <- reactiveValues(
    data = data.frame(
      ID = collection$id
      , Name = collection$name
      , Created = collection$created
      , "Number of documents" = collection$'number of documents'
      , Export = btns
    )
  )
  DT::datatable(
    reactive_data$data
    , selection = 'none'
    , escape = FALSE
    , class = "compact"
    , rownames = FALSE
    , options = list(dom = 't')
  )
}, server = FALSE)

#' observe collections
#' depends on:
#'   input$collection: selected collection
observeEvent(input$collection, {
  showModal(dialog_collection_export())
})

#' export collection model
#' depends on:
#'   input$export_collection_modal_ok: collection model for export
#'   input$collection: selected collection
#'   input$modal_export_collection_project_name: project name for export
#'   values$refi_download_collection_name: download collection name
observeEvent(input$export_collection_modal_ok, {
  withBusyIndicatorServer("export_collection_modal_ok", {
    output_directory <- "collections/tmp/refi"
    collection_name <- input$collection
    projectname <- input$modal_export_collection_project_name
    output_directory<-paste0(output_directory,"/",projectname)
    unlink(paste0(output_directory,"/*"))
    refi_export_collection_success <- F
    refi_export_collection_success<-export_collection(output_directory = output_directory, projectname = projectname, collection_name = collection_name,session = session)
    removeModal(session)
  })
  if(refi_export_collection_success==TRUE){
    values$refi_download_collection_name<-projectname
    showModal(
      modalDialog(title = "REFI-QDA Project Export",
                  tags$div(HTML(paste0("Successfully prepared collection <b>",collection_name,"</b>"))),
                  tags$br(),
                  downloadButton(outputId = "refi_export_collection",label = "Download Collection as REFI")
                  
      )
    )
  }
})

#' export collection - download handler
#' depends on:
#'   values$refi_download_collection_name: collection name for download
#'   
output$refi_export_collection<-downloadHandler(
  filename = function() {
    files_found<-list.files(paste0("collections/tmp/refi/",values$refi_download_collection_name),full.names = T)
    if(length(files_found)==1){
      file_name<-paste(values$refi_download_collection_name,'.qdpx', sep='')
    }
    if(length(files_found)>1){
      file_name<-paste(values$refi_download_collection_name,'.zip', sep='')
    }
    return(file_name)
  },
  content = function(con) {
    files_found<-list.files(paste0("collections/tmp/refi/",values$refi_download_collection_name),full.names = T)
    if(length(files_found)==1){
      file.copy(from = files_found,to = con)
    }
    if(length(files_found)>1){
      zipr(con,files_found)
    }
    
  }
)  

#' annotation scheme
#' depends on:
#'   input$collectionAnnoScheme: annotation scheme of collection
#'   
observeEvent(input$collectionAnnoScheme, {
  showModal(dialog_collection_with_annotation_scheme_export())
})

#' export collection and annotation scheme
#' depends on:
#'   input$export_collection_with_annotation_scheme_modal_ok: annotation scheme modal and collection for export
#'    input$refi_export_select_collection: export selected collection
#'    input$modal_export_collection_with_annotation_scheme_project_name: project name of annotatiom scheme and collection
#'    
observeEvent(input$export_collection_with_annotation_scheme_modal_ok, {
  withBusyIndicatorServer("export_collection_with_annotation_scheme_modal_ok", {
    output_directory <- "collections/tmp/refi"
    collection_name <- input$refi_export_select_collection
    projectname <- input$modal_export_collection_with_annotation_scheme_project_name
    output_directory<-paste0(output_directory,"/",projectname)
    unlink(paste0(output_directory,"/*"))
    refi_export_collection_success <- F
    refi_export_collection_success<-export_collection(output_directory = output_directory,projectname = projectname, collection_name = collection_name, annotation_scheme = input$collectionAnnoScheme, session = session)
    removeModal(session)
  })
  if(refi_export_collection_success==TRUE){
    values$refi_download_collection_name<-projectname
    showModal(
      modalDialog(title = "REFI-QDA Project Export",
                  tags$div(HTML(paste0("Successfully prepared collection <b>",collection_name,"</b> with codebook <b>",input$collectionAnnoScheme,"</b>"))),
                  tags$br(),
                  downloadButton(outputId = "refi_export_collection",label = "Download Collection as REFI")
                  
      )
    )
  }
})


#' getter for important parameters to export a collection

get_collection_names <- reactive({
  DB_get_collection_names()
})

get_annotation_schemes <- reactive({
  IO_get_annotation_schemes()
})

get_used_annotation_schemes <- reactive({
  DB_get_used_annotation_schemes(input$refi_export_select_collection)
})

get_analysis <- reactive({
  IO_get_analysis(collection = input$refi_export_select_collection, analysis = input$refi_export_select_analysis)
})

get_topics <- reactive({
  IO_get_topics(input$selectedTopicModel)
})

get_classifications <- reactive({
  IO_get_classifications(input$refi_export_select_collection)
})

#' update collections and results if button is clicked
#' depends on:
#'   input$refi_export_reset: refine reset for export
#'   values$refi_export_update: refine update for export  
observeEvent(input$refi_export_reset,{
  values$refi_export_update<-runif(1,0,1)
})


#' update collections and results if button is clicked
#' depends on:
#'   values$refi_export_update: refine update for export
observe({
  values$refi_export_update
  updateSelectInput(session, inputId = "refi_export_select_collection", choices = DB_get_collection_names())
})

#' update collections and results if button is clicked
#' depends on:
#'   values$refi_export_update: refine update for export
observe({
  values$refi_export_update
  updateSelectInput(session, inputId = "refi_export_classification_select_input", choices = IO_get_classification_projects())
})

#' update collections and results if button is clicked
#' depends on:
#'   values$refi_export_update: refine update for export
observe({
  values$refi_export_update
  updateSelectInput(session, inputId = "refi_export_select_analysis", label = paste0("Analysis of collection '", input$refi_export_select_collection, "'"))
})

#' update collections and results if button is clicked
#' depends on:
#'   values$refi_export_update: refine update for export
#'   input$refi_export_select_collection: refine selected collection for export 
output$refi_export_detected_annotation_schemes_label <- renderText({
  values$refi_export_update
  schemes <- get_used_annotation_schemes()
  if (nrow(schemes) > 0) {
    label_text <- paste0("Export collection '", input$refi_export_select_collection, "' with annotation scheme")
  } else {
    label_text <- paste0("No annotation schemes found for collection '", input$refi_export_select_collection, "'")
  }
  label_text
})

output$refi_export_detected_annotation_schemes_table <- DT::renderDataTable({
  values$refi_export_update
  schemes <- get_used_annotation_schemes()
  if (nrow(schemes) > 0){
    num_schemes <- seq(length(schemes$Anno_set))
    btns <- paste0('<button id="refi_export_detected_annotation_schemes_button_', num_schemes, '" data-anno-scheme="', schemes$Anno_set, '" type="button" class="btn btn-default action-button" onclick="getCollectionAnnoScheme(this)"><i class="fa fa-download"></i></button>')
    reactive_data <- reactiveValues(
      data = data.frame(
        Schemes=schemes$Anno_set
        , Export = btns
      )
    )
    DT::datatable(
      reactive_data$data
      , selection = 'none'
      , escape = FALSE
      , class = "compact"
      , rownames = FALSE
      , options = list(dom = 't')
    )
  }
})

output$refi_export_table_annotation_scheme <- DT::renderDataTable({
  schemes <- get_annotation_schemes()
  num_schemes <- seq(length(schemes))
  btns <- paste0('<button id="refi_export_anno_scheme_button_', num_schemes, '" data-anno-scheme="', schemes, '" type="button" class="btn btn-default action-button" onclick="getAnnotationScheme(this)"><i class="fa fa-download"></i></button>')
  reactive_data <- reactiveValues(
    data = data.frame(
      Schemes=schemes
      , Export = btns
    )
  )
  DT::datatable(
    reactive_data$data
    , selection = list(mode = 'single', selected = c(1))
    , escape = FALSE
    , class = "compact"
    , rownames = FALSE
    , options = list(dom = 'tp')
  )
})

output$refi_export_topic_model_table <- DT::renderDataTable({
  analysis <- get_analysis()
  values$refi_topics<-analysis
  req(analysis)
  num_analysis <- seq(length(analysis))
  btns <- paste0('<button id="refi_export_topic_model_button_', num_analysis, '" data-analysis="', analysis, '" type="button" class="btn btn-default action-button" onclick="getTopicModel(this)"><i class="fa fa-download"></i></button>')
  reactive_data <- reactiveValues(
    data = data.frame(
      Analysis = analysis
      , Export = btns
    )
  )
  
  DT::datatable(
    reactive_data$data
    , selection = list(mode = 'single', selected = c(1))
    , escape = FALSE
    , class = "compact"
    , rownames = FALSE
    , options = list(dom = 'tp')
  )
}, server = FALSE)

output$refi_export_classification_table <- DT::renderDataTable({
  analysis <- get_analysis()
  req(analysis)
  reactive_data <- reactiveValues(
    data = data.frame(
      Analysis = analysis
      , Export = shinyInput(
        shinyBS::bsButton
        , length(analysis)
        , 'refi_export_classification_button_'
        , label = ""
        , icon=icon("download")
        , onclick = 'Shiny.onInputChange(\"refi_export_classification_button\",  this.id)'
      )
    )
  )
  DT::datatable(
    reactive_data$data
    , selection = 'none'
    , escape = FALSE
    , class = "compact"
    , rownames = FALSE
    , options = list(dom = 'tp')
  )
}, server = FALSE)

output$refi_export_topic_model_number_of_topics_table <- DT::renderDataTable({
  session$sendCustomMessage("getSelectedTopicModel", input$refi_export_topic_model_table_rows_selected)
  topics <- get_topics()
  validate(
    need(length(topics)>1,message=FALSE),
    need(length(values$refi_topics)>0,message=F)
  )
  rank_topics <- seq(length(topics))
  btns <- paste0('<div id="refi_export_topic_', rank_topics, '" data-topic-rank="', rank_topics, '"><span><b>', topics, '</b></span></div>')
  reactive_data <- reactiveValues(
    data = data.frame(
      Topic = rank_topics
      , Words = btns
    )
  )
  
  DT::datatable(
    reactive_data$data
    , selection = list(mode = 'multiple')
    , escape = FALSE
    , class = "compact"
    , rownames = FALSE
    , options = list(lengthMenu = FALSE, searching = FALSE, paging = FALSE)
  )
}, server = FALSE)

output$refi_export_classification_table <- DT::renderDataTable({
  classifications <- get_classifications()
  req(classifications)
  num_classifications <- seq(length(classifications))
  btns <- paste0('<button id="refi_export_topic_model_button_', num_classifications, '" data-classification="', classifications, '" type="button" class="btn btn-default action-button" onclick="getClassification(this)"><i class="fa fa-download"></i></button>')
  reactive_data <- reactiveValues(
    data = data.frame(
      Classification = classifications
      , Export = btns
    )
  )
  DT::datatable(
    reactive_data$data
    , selection = list(mode = 'single', selected = c(1))
    , escape = FALSE
    , class = "compact"
    , rownames = FALSE
    , options = list(dom = 't')
  )
}, server = FALSE)

observeEvent(input$export_annotation_scheme_modal_ok, {
  withBusyIndicatorServer("export_annotation_scheme_modal_ok", {
    output_directory <- "collections/tmp/refi"
    projectname <- input$modal_export_annotation_scheme_project_name
    output_directory<-paste0(output_directory,"/",projectname)
    unlink(paste0(output_directory,"/*"))
    refi_export_codebook_success <- F
    refi_export_codebook_success <- export_codebook(projectname, input$annotationScheme, output_directory, session = session)
    removeModal(session)
  })
  if(refi_export_codebook_success==TRUE){
    values$refi_download_codebook_name<-projectname
    showModal(
      modalDialog(title = "REFI-QDA Project Export",
                  tags$div(HTML(paste0("Successfully prepared codebook <b>",input$annotationScheme,"</b>"))),
                  tags$br(),
                  downloadButton(outputId = "refi_export_codebook",label = "Download Codebook as REFI")
                  
      )
    )
  }
})

output$refi_export_codebook<-downloadHandler(
  filename = function() {
    files_found<-list.files(paste0("collections/tmp/refi/",values$refi_download_codebook_name),full.names = T)
    if(length(files_found)==1){
      file_name<-paste(values$refi_download_codebook_name,'qdc', sep='')
    }
    if(length(files_found)>1){
      file_name<- paste(values$refi_download_codebook_name,'.zip', sep='')
    }
    return(file_name)
  },
  content = function(con) {
    files_found<-list.files(paste0("collections/tmp/refi/",values$refi_download_codebook_name),full.names = T)
    if(length(files_found)==1){
      file.copy(from = files_found,to = con)
    }
    if(length(files_found)>1){
      zipr(con,files_found)
    }
    
  }
)  





observeEvent(input$classification, {
  showModal(dialog_classification_export())
})

observeEvent(input$export_classification_modal_ok, {
  withBusyIndicatorServer("export_classification_modal_ok", {
    output_directory <- "collections/tmp/refi"
    projectname <- input$modal_export_classification_project_name
    collection_name <- input$refi_export_select_collection
    classification <- input$classification
    output_directory<-paste0(output_directory,"/",projectname)
    unlink(paste0(output_directory,"/*"))
    refi_export_collection_success <- F
    refi_export_collection_success <- export_classification(output_directory, projectname, collection_name, classification,session=session)
    removeModal(session)
  })
  if(refi_export_collection_success==TRUE){
    values$refi_download_collection_name<-projectname
    showModal(
      modalDialog(title = "REFI-QDA Project Export",
                  tags$div(HTML(paste0("Successfully prepared collection <b>",collection_name,"</b> with classification results <b>",input$classification,"</b>"))),
                  tags$br(),
                  downloadButton(outputId = "refi_export_collection",label = "Download Collection as REFI")
                  
      )
    )
  }
})

observeEvent(input$export_topic_model_modal_ok, {
  withBusyIndicatorServer("export_topic_model_modal_ok", {
    topic_model_file <- file.path(ANALYSIS_RESULTS_HOME, "topic-model", input$topicModel, "data_TM.RData")
    output_directory <- "collections/tmp/refi"
    collection_name <- input$refi_export_select_collection
    projectname <- input$modal_export_topic_model_project_name
    output_directory<-paste0(output_directory,"/",projectname)
    unlink(paste0(output_directory,"/*"))
    refi_export_collection_success <- F
    refi_export_collection_success <- export_topic_model(output_directory, projectname, collection_name, topic_model_file, selected_topics = input$selectedTopics,session=session)
    removeModal(session)
  })
  if(refi_export_collection_success==TRUE){
    values$refi_download_collection_name<-projectname
    showModal(
      modalDialog(title = "REFI-QDA Project Export",
                  tags$div(HTML(paste0("Successfully prepared collection <b>",collection_name,"</b>"))),
                  tags$br(),
                  downloadButton(outputId = "refi_export_collection",label = "Download Collection as REFI")
                  
      )
    )
  }
})

observeEvent(input$topicModel, {
  session$sendCustomMessage("getTopics", input$refi_export_topic_model_number_of_topics_table_rows_selected)
  showModal(dialog_topic_model_export())
})

observeEvent(input$annotationScheme, {
  showModal(dialog_annotation_scheme_export())
})

output$annotation_scheme_list <- renderUI({
  session$sendCustomMessage("getSelectedAnnotationScheme", input$refi_export_table_annotation_scheme_rows_selected)
  if (!is.null(input$selectedAnnotationScheme)) {
    anno_file <- file.path(ANNO_SCHEME_HOME, paste0(input$selectedAnnotationScheme, ".RData"))
    html_content <- HTML_get_annotation_scheme(anno_file)
    HTML(html_content)
  }
})





