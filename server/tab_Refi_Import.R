output$refi_import_table_datasets <- DT::renderDataTable({
  datasets <- DB_get_datasets()
  DT::datatable(
    datasets
    , selection = 'none'
    , escape = FALSE
    , class = "compact"
    , rownames = FALSE
    , options = list(dom = 't')
  )
})

output$refi_import_table_annotation_scheme <- DT::renderDataTable({
  values$newscheme
  schemes <- IO_get_annotation_schemes()
  num_schemes <- seq(length(schemes))
  btns <- paste0(schemes, '<span data-anno-scheme="', schemes, '"</span>')
  reactive_data <- reactiveValues(
    data = data.frame(Schemes=btns)
  )
  DT::datatable(
    reactive_data$data
    , selection = list(mode = 'single')
    , escape = FALSE
    , class = "compact"
    , rownames = FALSE
    , options = list(dom = 't')
  )
})

output$annotation_scheme_list_import <- renderUI({
  session$sendCustomMessage("getSelectedAnnotationSchemeImport", input$refi_import_table_annotation_scheme_rows_selected)
  if (!is.null(input$selectedAnnotationSchemeImport)) {
    anno_file_name <- list.files(ANNO_SCHEME_HOME, pattern = paste0(input$selectedAnnotationSchemeImport, ".RData"))
    anno_file <- file.path(ANNO_SCHEME_HOME, anno_file_name)
    print(anno_file)
    html_content <- HTML_get_annotation_scheme(anno_file)
    HTML(html_content)
  }
})

observeEvent(input$refi_import_fileInput,{
  dataset <- input$refi_import_dataset_name
  uploaded_file <- input$refi_import_fileInput
  if (is.null(uploaded_file))
    return(NULL)
  
  if (file_ext(uploaded_file$datapath) == "qdpx") {
    # main
    r <- refi_to_collection(uploaded_file, dataset)
    data <- r$data
    xml_document <- r$xmlDoc
    import_directory <- r$importDirectory
    
  } else if (file_ext(uploaded_file) == "qdc") {
    # REFI-QDA Codebook
    tryCatch({
      xml_document <- read_xml(uploaded_file$datapath)
      codebook <- xml_find_first(xml_document, "//d1:CodeBook")
      import_codebook(codebook, name = dataset)
      # invalidate outputs that show available annotation schemes
      values$newscheme<-runif(1,0,1)
      shinyWidgets::sendSweetAlert(session=session,title = "REFI-QDA Codebook", text = paste0("Successfully imported codebook '", dataset,  "."), type = "success")
      # improve this
      return()
    }, error=function(cond){
      shinyWidgets::sendSweetAlert(session=session,title = "REFI-QDA Codebook", text = paste0("Error while importing codebook '", dataset,  "."), type = "error")
    })
    
  } else {
    # wrong file types
    shinyWidgets::sendSweetAlert(session=session,title = "REFI-QDA Standard", text = "No valid REFI datatyp. Please choose filetypes with .qdpx or .qdc fileextension.'", type = "warning")
    
  }
  
  #test if metadata is valid 
  if (length(unique(data[,"id_doc"])) != dim(data)[1]){
    shinyWidgets::sendSweetAlert(session=session,title = "Document id not unique",text = "Please specify id_doc to be unique for every document!",type = "error")
    
  } else if(!(is.numeric(as.numeric(data[,"id_doc"])))){
    shinyWidgets::sendSweetAlert(session=session,title = "Document id not a number",text = "Please specify id_doc to be an integer",type = "error")
    
  } else if (nchar(as.character(data[1,"dataset"])) == 0){
    shinyWidgets::sendSweetAlert(session=session,title = "dataset abbreviation not specified",text = "Please specify a abbreviation for the data",type = "error")
    
  } else if (stringr::str_detect(as.character(data[1,"dataset"]),pattern = "_")){
    shinyWidgets::sendSweetAlert(session=session,title = "'_' not allowed",text = "Please specify a abbreviation without using '_'",type = "error")
    
    
  } else {
    tryCatch({
      process_import(dataset, data, xml_document, import_directory)
      shinyWidgets::sendSweetAlert(session=session,title = "REFI-QDA Project", text = paste0("Successfully started project import '", dataset,  "."), type = "success")
    }, error=function(cond){
      print(cond)
      shinyWidgets::sendSweetAlert(session=session,title = "REFI-QDA Project", text = paste0("Error while importing project '", dataset,  "."),type = "error")
    })
  }
})

process_import <- function(dataset, data, xml_document, import_directory){
  #create meta metadata vector
  meta_metadata<-data.frame(dataset = c(dataset))
  #save needed parameters
  # use most used lagnuage for spacy preprocessing
  lang<-names(table(data[,"language"])[1])
  parameters<-list(data, db = TRUE, lang = lang, "%Y-%m-%d", meta_metadata,save_annotations=T)
  #create process ID
  ID<-get_task_id_counter()+1
  set_task_id_counter(ID)
  #save metadata for process
  process_info<-list(ID,paste("New Data REFI- ",input$refi_import_dataset_name,sep=""),"Create import csv files and write to DB and solr",as.character(Sys.time()))
  #save logfile path
  logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
  #create logfile
  write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
  #save data needed in script execution 
  save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
  #save data needed for saving annotations
  xml_document<-as.character(xml_document)
  save(dataset, xml_document, import_directory,file="collections/tmp/tmp_annotations.RData")
  #start script
  system(paste('Rscript collections/scripts/Import_Script.R','&'))
  
}
