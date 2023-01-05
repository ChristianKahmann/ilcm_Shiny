##########################################################################################################
#                                    common                                                              #
##########################################################################################################
library(cld2)
source("config/sanity_check_config.R")

### import

default_script_decription_import<-'# The vector `result` must be specified in this script
# if you want to use data from the imported csv file or mtf, you can use `input_data`
# example:
#  result<-paste0("Vortrag Nummer:",as.matrix(input_data[,5]))
# or for RegEx
#  result<-str_extract(input_data[,9],regex("(3[01]|[12][0-9]|0?[1-9])\\.(1[012]|0?[1-9])\\.((?:19|20)\\d{2})"))'

#' function to evaluate scripts
#' @input_data: input data for script evaluation
#' @script_text: text input from script
#' @script_label: label of selected script
#' @script_number: number (id) of selected script
#' @import_name: name for the script
eval_script <- function(script_text, input_data, script_label, script_nr, import_name) {
  result<-""
  values[[script_label]][script_nr]<-script_text
  eval(parse(text=script_text))
  if(!is.character(result)) {
    stop("Result needs to be of type string.")
  }
  if(length(result) != length(values[[import_name]])) {
    stop(sprintf("Result not big enough for all %d rows.", length(values[[import_name]])))
  }
  result
}

#' function to handel possible ways to personalize the script
#' @name: name of the script
#' @import_type: import data types
#' @script_nr: number of the script
#' @split_script: should a script be splitted?
script_events <- function(name, import_type, script_nr, split_script = FALSE) {
  input_label <- sprintf("data_%s", import_type)
  script_event_name <- sprintf("Import_script_%s_%s", name, import_type)
  save_event_name <- sprintf("Import_script_save_%s_%s", name, import_type)
  editor_id <- sprintf("script_%s_%s", name, import_type)
  import_name <- sprintf("Import_%s_%s", import_type, name)
  
  script_label_add <- if(split_script) "_split" else ""
  script_label <- sprintf("Import_%s%s_scripts", import_type, script_label_add)
  
  observeEvent(input[[script_event_name]],{
    showModal(
      modalDialog(size = "l",easyClose = T,fade = T,
                  aceEditor(editor_id,theme ="chrome"  ,mode="r", fontSize = "15",
                            showLineNumbers = T, highlightActiveLine = T, autoComplete = "live",
                            value=values[[script_label]][script_nr]),
                  footer = tagList(
                    actionButton(save_event_name, "save")
                  )
      )
    )
  })
  observeEvent(input[[save_event_name]],{
    if (split_script) {
      values[[script_label]] <- input[[editor_id]]
    } else {
      tryCatch({
        values[[import_name]] <- eval_script(input[[editor_id]], values[[input_label]], script_label, script_nr, import_name)
      },
      error=function(e){
        shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
      })
    }
    removeModal()
  })
}

#' options to manipulate the used type
#' @name: name of script to work with
#' @import_type: type to manipulate
type_events <- function(name, import_type) {
  type_event_name <- sprintf("Import_type_%s_%s", name, import_type)
  save_event_name <- sprintf("Import_type_save_%s_%s", name, import_type)
  input_id <- sprintf("Import_type_input_%s_%s", name, import_type)
  import_name <- sprintf("Import_%s_%s", import_type, name)
  data_name <- sprintf("data_%s", import_type)
  
  observeEvent(input[[type_event_name]],{
    showModal(
      modalDialog(size = "l", easyClose = T, fade = T,
                  textInput(inputId = input_id, label = sprintf("%s (one for all)", stringr::str_to_title(name)), placeholder = sprintf("Please type in the %s", name)),
                  actionButton(save_event_name, "save")
      )
    )
  })
  
  observeEvent(input[[save_event_name]],{
    values[[import_name]] <- rep(input[[input_id]],dim(values[[data_name]])[1])
    removeModal()
  })
}
#' function to observe mde
#' @import_type: type of used import
#' @name: name of selected import
observe_mde <- function(name, import_type) {
  import_name <- sprintf("Import_%s_%s", import_type, name)
  data_name <- sprintf("data_%s", import_type)
  
  observe({
    validate(
      need(!is.null(input[[import_name]]), message=FALSE),
      need(input[[import_name]]%in%c(colnames(values[[data_name]]),"not required"),message=FALSE)
    )
    if(input[[import_name]] == "not required"){
      values[[import_name]] <- NULL
    }
    else{
      values[[import_name]]<-as.vector(as.matrix(values[[data_name]][,input[[import_name]]]))
    }
  })
}

### split

default_script_decription_split<-"# You need to specify split_data
# split_data is a list of vectors where every element of the list represents an imported file.
# length(split_data) needs to equal length(selected_data)
# The imported files are in file_data (dataframe).
# The data of the select column is selected_data (array of strings).
# Example for split after x words:
# x <- 1000
# words <- strsplit(selected_data, '\\\\W+')
# splits <- floor(lengths(words)/x)
# for(i in 1:length(words)) {
#   for(j in 0:splits[i]) {
#     max <- min((j+1)*x, length(words[[i]]))
#     split_data[[i]][j+1] <- paste(words[[i]][(j*x+1):max],collapse=' ')
# }}"

#' process to split data
#' @type: type of data 
#' @name: specify name 
process_split <- function(type, name = "split") {
  data_label <- sprintf("data_%s", type)
  column_name <- sprintf("Import_%s_column_name", type)
  method_label <- sprintf("Import_%s_split_method", type)
  editor_id <- sprintf("script_%s_%s", name, type)
  header_label <- sprintf("header_%s", type)
  
  if(input[[method_label]] != 'None') {
    file_data <- values[[data_label]]
    selected_col <- input[[column_name]]
    selected_data <- as.character(file_data[[selected_col]])
    split_data <- perform_split(type, selected_data, file_data, name)
    
    main_ids <- rep(1:length(split_data),lengths(split_data))
    sub_ids <- vector()
    for(split_length in lengths(split_data)) {
      sub_ids <- c(sub_ids,1:split_length)
    }
    ids <- sprintf("%d-%d", main_ids, sub_ids)
    
    file_data[[selected_col]] <- NULL
    file_data <- file_data[main_ids, 1:ncol(file_data)]
    file_data[[selected_col]] <- unlist(split_data)
    file_data$split_id <- ids
    values[[data_label]] <- file_data
    values[[header_label]]<-c(colnames(values[[data_label]]))
  }
}

#' function to performe splittng
#' @type: type of data
#' @selected_data: selected data for splitting
#' @file_data: file data
#' @editor_name: editor name (split)
#' 
#' @return split_data
perform_split <- function(type, selected_data, file_data, editor_name = "split") {
  if (length(selected_data) == 0) {
    return(NULL)
  }
  method_label <- sprintf("Import_%s_split_method", type)
  method_regex_label <- sprintf("Import_%s_split_method_regex", type)
  method_split_number_label <- sprintf("Import_%s_split_method_split_number", type)
  editor_id <- sprintf("script_%s_%s", editor_name, type)
  
  split_data <- vector("list", length = length(selected_data))
  if(input[[method_label]] == 'Regular Expression') {
    split_data <- strsplit(selected_data, input[[method_regex_label]])
  } else if (input[[method_label]] == 'Hard Split') {
    x <- input[[method_split_number_label]]
    splits <- floor(nchar(selected_data)/x)
    for(i in 1:length(selected_data)) {
      split_data[[i]] <- substring(selected_data[[i]], ((0:splits[i])*x+1),((1:(splits[i]+1))*x))
    }
  } else if (input[[method_label]] == 'Script') {
    tryCatch({
      if (is.null(input[[editor_id]])) {
        stop("You didn't set up a script.")
      } else {
        eval(parse(text=input[[editor_id]]))
        if(!is.list(split_data) || !is.character(unlist(split_data))) {
          stop("Result needs to be a list of strings.")
        }
        if(length(split_data) != length(selected_data)) {
          stop("Result needs to be a list of strings.")
        }
      }
    },
    error=function(e){
      shinyalert::shinyalert(title = "error in code",text = as.character(e),type = "error")
    })
  }
  return(split_data)
}

#' test view of splitting process
#' @type: data type
split_test_view <- function(type) {
  data_label <- sprintf("data_%s", type)
  column_name <- sprintf("Import_%s_column_name", type)
  display_condition <- sprintf("input.Import_%s_split_method == 'Regular Expression'", type)
  method_regex_label <- sprintf("Import_%s_split_method_regex", type)
  
  input_data <- values[[data_label]]
  selected_col <- input[[column_name]]
  selected_data <- as.character(input_data[[selected_col]])
  
  values$live_method_regex_label <- method_regex_label
  
  showModal(modalDialog(
    title = "Split Test View",
    size = "l",
    fluidRow(
      column(6,selectInput(inputId = "Import_row_nr", "Row", choices=1:dim(input_data)[1])),
      conditionalPanel(
        condition = display_condition,
        column(6,
               column(9,textInput(inputId = paste0(method_regex_label, "2"),label = "Regular Expression:", value = input[[method_regex_label]])),
               column(3,actionButton("Import_live_split_test", "Test Split", style = "info", block=T))
        )
      )
    ),
    fluidRow(
      box(title = "Original",status = "primary",width = "6",renderText(selected_data[as.integer(input$Import_row_nr)])),
      box(
        title = "Split Result",
        status = "primary",
        renderUI({
          HTML(paste(unlist(perform_split(type, selected_data[as.integer(input$Import_row_nr)])), collapse = "<hr/>"))
        })
      )
    ),
    easyClose = TRUE
  ))
}

#' observe live split test
#' depends on:
#'   input$Import_live_split_test: import information for live split test
#'   values$live_method_regex_label: regex label for live method
observeEvent(input$Import_live_split_test,{
  updateTextInput(session, values$live_method_regex_label, value = input[[paste0(values$live_method_regex_label, "2")]])
})


#' sanity check
#' sanity check for modal
#' @type: type of selected data
#' @data_check_choices: check choices of data
sanity_check_Modal <- function(type, data_check_choices) {
  id_doc_label <- sprintf("Import_%s_id_doc", type)
  date_label <- sprintf("Import_%s_date", type)
  date_format_label <- sprintf("Import_%s_date_format", type)
  
  output$sanity_check_table = DT::renderDataTable({
    regarding_data <- values[[sprintf("Import_%s_%s", type, input$Import_data_id_check)]]
    
    valid_encoding <- if (input$Import_data_id_check == "date") {
      !is.na(as.Date(values[[date_label]],input[[date_format_label]],optional = TRUE))
    } else if(is.numeric(regarding_data)) {
      rep(TRUE,length(regarding_data))
    } else {
      validEnc(regarding_data)
    }
    
    detected_language <- if (is.numeric(regarding_data)) {rep("-",length(regarding_data))} else {
      detect_language(regarding_data)
    }
    
    regex_check <- is.na(str_extract(regarding_data,regex(sanity_check_regex)))
    data.frame(id_doc = values[[id_doc_label]], characters = nchar(regarding_data,allowNA = T), valid_encoding, detected_language, regex_check)
  }, server = FALSE, selection = "single")
  
  output$selected_row = renderText({
    if (is.null(input$sanity_check_table_rows_selected)){
      return("Select a row above, to see its data")
    } else {
      return(values[[sprintf("Import_%s_%s", type, input$Import_data_id_check)]][input$sanity_check_table_rows_selected])
    }
  })
  showModal(
    modalDialog(
      size = "l",easyClose = T,fade = T,
      title = "Sanity Check",
      footer = tagList(modalButton("Cancel")),
      selectInput("Import_data_id_check", "Check Data", choices=data_check_choices),
      renderPlotly({
        ids = paste(values[[id_doc_label]])
        xlab = list(categoryorder = "array", categoryarray = ids)
        plot_ly(y = nchar(values[[sprintf("Import_%s_%s", type, input$Import_data_id_check)]],allowNA = T), x = ids, type = "bar",
                marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 1.5))) %>% layout(title = "Length", xaxis = xlab,yaxis=list(title="number of characters"))
      }),
      p("NOTE: Use unice 'id_doc' to see character length individually - otherwise it gets stacked"),
      hr(),
      DT::dataTableOutput('sanity_check_table'),
      p(tags$b("valid_encoding")," - returns true if no error is found, checks date format if date is selected"),
      p(tags$b("regex_check")," - returns true if nothing is found"),
      p("NOTE: Language detection might fail - especially with short text"),
      hr(),
      h5("Content"),
      textOutput("selected_row")%>%withSpinner()
    )
  )
}

##########################################################################################################
#                                import csv                                                              #
##########################################################################################################
values$Import_csv_title<-""
values$Import_csv_date<-""
values$Import_csv_body<-""
values$Import_csv_mde1<-""
values$Import_csv_mde2<-""
values$Import_csv_mde3<-""
values$Import_csv_mde4<-""
values$Import_csv_mde5<-""
values$Import_csv_mde6<-""
values$Import_csv_mde7<-""
values$Import_csv_mde8<-""
values$Import_csv_mde9<-""
values$Import_csv_language<-""
values$Import_csv_token<-""
values$Import_csv_dataset<-""
values$Import_csv_scripts<-""
values$Import_csv_split_scripts<-""

#' show import of csv files
#' depends on:
#'   values$invalidate_csv_files: check if csv-file is invalide
output$UI_Import_csv_file<-renderUI({
  values$invalidate_csv_files
  validate(
    need(length(list.files("data_import/unprocessed_data/",pattern = ".csv|.xlsx"))>0,message="No CSV/XLSX-Files found in directory: data_import/unprocessed_data")
  )
  return(
    tagList(
      
      shinyWidgets::prettyRadioButtons(inputId = "Import_csv_files",label = "CSV/XLSX Files",
                                       choices = stringr::str_replace_all(string = list.files("data_import/unprocessed_data/",pattern = ".csv|.xlsx"),pattern = ".txt",replacement = ""),
                                       fill=T,animation = "pulse",selected = character(0))
    )
  )
})

#' oberve event of importing a new csv-file
#' depends on:
#'   input$Import_csv_new: new csv-file import
observeEvent(input$Import_csv_new,ignoreInit = T,{
  validate(
    need(
      !is.null(input$Import_csv_new),message=F
    )
  )
  print(input$Import_csv_new)
  if(file.exists(paste0("data_import/unprocessed_data/",input$Import_csv_new$name))){
    shinyWidgets::sendSweetAlert(session=session,title = "Filename already used",text = "Please rename your csv file and then try to upload it again.",type = "warning")
  }
  else{
    file.copy(from = input$Import_csv_new$datapath,to = paste0("data_import/unprocessed_data/",input$Import_csv_new$name))
    values$invalidate_csv_files<-runif(1,0,1)
    shinyWidgets::sendSweetAlert(session=session,title = "File added",text = "You can now select it in the list of files above",type = "success")
  }
})



#' observe loading a new csv-file
#' deoends on:
#'   input$Import_load_csv: load csv-file
#'   input$import_load_csv_seperator: seperator of csv elements
#'   input$Import_load_csv_header: extract header of csv-file
#'   values$header_csv: extracted csv header
#'   values$data_csv: all csv data
#'   values$Import_csv_split_scripts: import csv scripts to split
observeEvent(input$Import_load_csv,{
  withBusyIndicatorServer("Import_load_csv", {
    delim = input$import_load_csv_seperator
    if(delim == "\\t"){
      delim = "\t"
    }
    if(grepl(pattern = ".csv$",x = input$Import_csv_files)){
      values$data_csv<-readr::read_delim(file = paste0("data_import/unprocessed_data/",input$Import_csv_files),col_names = input$Import_load_csv_header,
                                         delim = delim, na = character() )
    }
    if(grepl(pattern = ".xlsx$",x = input$Import_csv_files)){
      values$data_csv<-readxl::read_excel(path = paste0("data_import/unprocessed_data/",input$Import_csv_files), col_names = input$Import_load_csv_header)
    }
    colnames(values$data_csv)<-stringr::str_replace_all(string = colnames(values$data_csv),pattern = "\\.",replacement = " ")
    values$Import_csv_scripts<-rep(default_script_decription_import,13) # 13 for id_doc, title, date, body and 9 mde's
    if(dim(values$data_csv)[1]<2 | dim(values$data_csv)[2]<2){
      text<-paste0("The resulting input dimesions are: ",dim(values$data_csv)[1]," x ",dim(values$data_csv)[2],". Something went wrong during the input. Make sure to specify the csv input parameters correct.")
      shinyWidgets::sendSweetAlert(session=session,title = "Input failed!",text = text,type = "error")
    }
    else{
      values$header_csv<-c(colnames(values$data_csv))
      values$data_load_csv_success<-TRUE
    }
    
    values$Import_csv_split_scripts <- default_script_decription_split
  })
})

#' split
script_events("split", "csv", 1, TRUE)
#' test view of csv-splitting
#' depends on:
#'   input$Import_csv_split_test_view: split test view of csv
observeEvent(input$Import_csv_split_test_view, {
  split_test_view("csv")
})

#' start mapping
#' depends on:
#'   input$Import_start_mapping: import start mapping process
observeEvent(input$Import_start_mapping,{
  if(input$Import_csv_split_method != 'None') {
    showModal(modalDialog(
      title="Are you sure you want to split",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_start_mapping_csv", "Continue", styleclass = "info")
      ),
      "You can't simply undo this split. You would have to reimport your data to go back."
    ))
  } else {
    start_mapping_csv()
  }
})

#' confirm mapping process
#' depends on:
#'   input$confirm_start_mapping_csv: confirm if mapping process should start
observeEvent(input$confirm_start_mapping_csv,{
  start_mapping_csv()
  removeModal()
})

#' initiate mapping of csv
start_mapping_csv <- function() {
  process_split("csv")
  values$start_mapping<-TRUE
}

#' return data if loading the csv was successful
#' depends on:
#'   values$data_load_csv_success: communicate wether the loading process was succesfull
output$data_load_csv_success<-reactive({
  values$data_load_csv_success
})

#' validate if mapping has started
#' depends on:
#'   values$start_mapping: variable to validate if mapping has started
output$start_mapping<-reactive({
  values$start_mapping
})
outputOptions(output, "data_load_csv_success", suspendWhenHidden = FALSE)
outputOptions(output, "start_mapping", suspendWhenHidden = FALSE)

#' render data table to show csv file with header of csv
#' depends on:
#'   values$data_csv: values from csv data
output$Import_head_csv<-DT::renderDataTable({
  data<-values$data_csv
  data<-data[1:min(5,dim(data)[1]),]
  data<-t(apply(data,1,FUN=function(i){apply(as.matrix(i),MARGIN = 1,FUN = function(x){if(is.na(x)){return(x)}else{if(nchar(x)>100){return(substr(x,1,100))}else{return(x)}}})}))
  datatable(data = data,options = list(lengthChange = FALSE,dom="t"),width = "100%")
})

#' check csv-file 
#' depends on:
#'   input$Import_check_csv: variable to confirm that a csv-file needs to be checked
observeEvent(input$Import_check_csv,{
  showModal(
    modalDialog(
      size = "l",
      title = "Imported CSV",easyClose = T,
      tags$div(style="overflow-x:auto; height:70vh;",
               dataTableOutput(outputId = "Import_head_csv")
      ) 
    )
  )
})

#' extract column names
#' depends on:
#'   input$Import_csv_column_name: column names of csv-file
#'   values$header_csv: header of csv-file
output$UI_Import_csv_column_name <- renderUI({
  select_value <- input$Import_csv_column_name
  selectInput(inputId = "Import_csv_column_name", "Column:", choices=values$header_csv, selected = select_value)%>%
    shinyInput_label_embed(
      icon("info") %>%
        bs_embed_tooltip(title = "Select a column with the data you want to split.")
    )
})

#' get csv title for import 
#' depends on:
#'   values$header_csv: header of csv
output$UI_Import_csv_title<-renderUI({
  radioButtons(inputId = "Import_csv_title",label = "Map title",
               choiceNames = 
                 lapply(X = c("automatic",values$header_csv),FUN = function(x){
                   shorten_long_choices_for_radio_buttons(x,16)
                 }),
               choiceValues = c("automatic",values$header_csv),
               selected ="automatic"
  )
})


#' get csv date format automatically
#'  depends on:
#'    values$header_csv: csv header
output$UI_Import_csv_date<-renderUI({
  radioButtons(inputId = "Import_csv_date",label = "Map date",
               choiceNames = 
                 lapply(X = c("automatic",values$header_csv),FUN = function(x){
                   shorten_long_choices_for_radio_buttons(x,16)
                 }),
               choiceValues = c("automatic",values$header_csv),
               selected ="automatic"
  )
})

#' import the csv body 
#' depends on:
#'   values$header_csv: header of csv
output$UI_Import_csv_body<-renderUI({
  radioButtons(inputId = "Import_csv_body",label = "Map body",
               choiceNames = 
                 lapply(X = c("automatic",values$header_csv),FUN = function(x){
                   shorten_long_choices_for_radio_buttons(x,16)
                 }),
               choiceValues = c("automatic",values$header_csv),
               selected ="automatic"
  )
})

#' import csv mde1 
#' depends on:
#'   values$header_csv: header of the csv
output$UI_Import_csv_mde1<-renderUI({
  radioButtons(inputId = "Import_csv_mde1", label = "Map meta 1",
               choiceNames = 
                 lapply(X = c("not required",values$header_csv),FUN = function(x){
                   shorten_long_choices_for_radio_buttons(x,16)
                 }),
               choiceValues = c("not required",values$header_csv),
               selected ="not required"
  )
})

#' check which radio-button was selected to determine the current mde with a matching column from the csv
#' depends on:
#'   input$UI_Import_name_mde1: name of the selected column from csv for mde1
observe({
  updateRadioButtons(session = session,inputId = "Import_csv_mde1",label = paste0("Map ",input$UI_Import_name_mde1))
})

# automatically set name of mde to chosen radio button of corresponding column in csv
observeEvent(input$Import_csv_mde1,{
  if(input$UI_Import_name_mde1=="mde1" & input$Import_csv_mde1!="not required"){
    updateTextInput(session = session, inputId = "UI_Import_name_mde1",value = input$Import_csv_mde1)
  }
})


#' import csv mde2
#' depends on:
#'   values$header_csv: header of the csv
output$UI_Import_csv_mde2<-renderUI({
  radioButtons(inputId = "Import_csv_mde2",label = "Map mde2",
               choiceNames = 
                 lapply(X = c("not required",values$header_csv),FUN = function(x){
                   shorten_long_choices_for_radio_buttons(x,16)
                 }),
               choiceValues = c("not required",values$header_csv),
               selected ="not required"
  )
})

#' check which radio-button was selected to determine the current mde with a matching column from the csv
#' depends on:
#'   input$UI_Import_name_mde2: name of the selected column from csv for mde2
observe({
  updateRadioButtons(session = session,inputId = "Import_csv_mde2",label = paste0("Map ",input$UI_Import_name_mde2))
})

# automatically set name of mde to chosen radio button of corresponding column in csv
observeEvent(input$Import_csv_mde2,{
  if(input$UI_Import_name_mde2=="mde2" & input$Import_csv_mde2!="not required"){
    updateTextInput(session = session, inputId = "UI_Import_name_mde2",value = input$Import_csv_mde2)
  }
})

#' import csv mde3
#' depends on:
#'   values$header_csv: header of the csv
output$UI_Import_csv_mde3<-renderUI({
  radioButtons(inputId = "Import_csv_mde3", label = "Map mde3",
               choiceNames = 
                 lapply(X = c("not required",values$header_csv),FUN = function(x){
                   shorten_long_choices_for_radio_buttons(x,16)
                 }),
               choiceValues = c("not required",values$header_csv),
               selected ="not required"
  )
})
#' check which radio-button was selected to determine the current mde with a matching column from the csv
#' depends on:
#'   input$UI_Import_name_mde3: name of the selected column from csv for mde3
observe({
  updateRadioButtons(session = session,inputId = "Import_csv_mde3",label = paste0("Map ",input$UI_Import_name_mde3))
})

# automatically set name of mde to chosen radio button of corresponding column in csv
observeEvent(input$Import_csv_mde3,{
  if(input$UI_Import_name_mde3=="mde3" & input$Import_csv_mde3!="not required"){
    updateTextInput(session = session, inputId = "UI_Import_name_mde3",value = input$Import_csv_mde3)
  }
})

#' import csv mde4
#' depends on:
#'   values$header_csv: header of the csv
output$UI_Import_csv_mde4<-renderUI({
  radioButtons(inputId = "Import_csv_mde4", label = "Map mde4",
               choiceNames = 
                 lapply(X = c("not required",values$header_csv),FUN = function(x){
                   shorten_long_choices_for_radio_buttons(x,16)
                 }),
               choiceValues = c("not required",values$header_csv),
               selected ="not required"
  )
})

#' check which radio-button was selected to determine the current mde with a matching column from the csv
#' depends on:
#'   input$UI_Import_name_mde4: name of the selected column from csv for mde4
observe({
  updateRadioButtons(session = session,inputId = "Import_csv_mde4",label = paste0("Map ",input$UI_Import_name_mde4))
})
# automatically set name of mde to chosen radio button of corresponding column in csv
observeEvent(input$Import_csv_mde4,{
  if(input$UI_Import_name_mde4=="mde4" & input$Import_csv_mde4!="not required"){
    updateTextInput(session = session, inputId = "UI_Import_name_mde4",value = input$Import_csv_mde4)
  }
})

#' import csv mde5
#' depends on:
#'   values$header_csv: header of the csv
output$UI_Import_csv_mde5<-renderUI({
  radioButtons(inputId = "Import_csv_mde5",label = "Map mde5",
               choiceNames = 
                 lapply(X = c("not required",values$header_csv),FUN = function(x){
                   shorten_long_choices_for_radio_buttons(x,16)
                 }),
               choiceValues = c("not required",values$header_csv),
               selected ="not required"
  )
})
#' check which radio-button was selected to determine the current mde with a matching column from the csv
#' depends on:
#'   input$UI_Import_name_mde5: name of the selected column from csv for mde5
observe({
  updateRadioButtons(session = session,inputId = "Import_csv_mde5",label = paste0("Map ",input$UI_Import_name_mde5))
})
# automatically set name of mde to chosen radio button of corresponding column in csv
observeEvent(input$Import_csv_mde5,{
  if(input$UI_Import_name_mde5=="mde5" & input$Import_csv_mde5!="not required"){
    updateTextInput(session = session, inputId = "UI_Import_name_mde5",value = input$Import_csv_mde5)
  }
})

#' import csv mde6
#' depends on:
#'   values$header_csv: header of the csv
output$UI_Import_csv_mde6<-renderUI({
  radioButtons(inputId = "Import_csv_mde6",label = "Map mde6",
               choiceNames = 
                 lapply(X = c("not required",values$header_csv),FUN = function(x){
                   shorten_long_choices_for_radio_buttons(x,16)
                 }),
               choiceValues = c("not required",values$header_csv),
               selected ="not required"
  )
})

#' check which radio-button was selected to determine the current mde with a matching column from the csv
#' depends on:
#'   input$UI_Import_name_mde6: name of the selected column from csv for mde6
observe({
  updateRadioButtons(session = session,inputId = "Import_csv_mde6",label = paste0("Map ",input$UI_Import_name_mde6))
})

# automatically set name of mde to chosen radio button of corresponding column in csv
observeEvent(input$Import_csv_mde6,{
  if(input$UI_Import_name_mde6=="mde6" & input$Import_csv_mde6!="not required"){
    updateTextInput(session = session, inputId = "UI_Import_name_mde6",value = input$Import_csv_mde6)
  }
})

#' import csv mde7
#' depends on:
#'   values$header_csv: header of the csv
output$UI_Import_csv_mde7<-renderUI({
  radioButtons(inputId = "Import_csv_mde7", label = "Map mde7",
               choiceNames = 
                 lapply(X = c("not required",values$header_csv),FUN = function(x){
                   shorten_long_choices_for_radio_buttons(x,16)
                 }),
               choiceValues = c("not required",values$header_csv),
               selected ="not required"
  )
})

#' check which radio-button was selected to determine the current mde with a matching column from the csv
#' depends on:
#'   input$UI_Import_name_mde7: name of the selected column from csv for mde7
observe({
  updateRadioButtons(session = session,inputId = "Import_csv_mde7",label = paste0("Map ",input$UI_Import_name_mde7))
})

# automatically set name of mde to chosen radio button of corresponding column in csv
observeEvent(input$Import_csv_mde7,{
  if(input$UI_Import_name_mde7=="mde7" & input$Import_csv_mde7!="not required"){
    updateTextInput(session = session, inputId = "UI_Import_name_mde7",value = input$Import_csv_mde7)
  }
})
#' import csv mde8
#' depends on:
#'   values$header_csv: header of the csv
output$UI_Import_csv_mde8<-renderUI({
  radioButtons(inputId = "Import_csv_mde8", label = "Map mde8",
               choiceNames = 
                 lapply(X = c("not required",values$header_csv),FUN = function(x){
                   shorten_long_choices_for_radio_buttons(x,16)
                 }),
               choiceValues = c("not required",values$header_csv),
               selected ="not required"
  )
})
#' check which radio-button was selected to determine the current mde with a matching column from the csv
#' depends on:
#'   input$UI_Import_name_mde8: name of the selected column from csv for mde8
observe({
  updateRadioButtons(session = session,inputId = "Import_csv_mde8",label = paste0("Map ",input$UI_Import_name_mde8))
})

# automatically set name of mde to chosen radio button of corresponding column in csv
observeEvent(input$Import_csv_mde8,{
  if(input$UI_Import_name_mde8=="mde8" & input$Import_csv_mde8!="not required"){
    updateTextInput(session = session, inputId = "UI_Import_name_mde8",value = input$Import_csv_mde8)
  }
})

#' import csv mde9
#' depends on:
#'   values$header_csv: header of the csv
output$UI_Import_csv_mde9<-renderUI({
  radioButtons(inputId = "Import_csv_mde9", label = "Map mde9",
               choiceNames = 
                 lapply(X = c("not required",values$header_csv),FUN = function(x){
                   shorten_long_choices_for_radio_buttons(x,16)
                 }),
               choiceValues = c("not required",values$header_csv),
               selected ="not required"
  )
})
#' check which radio-button was selected to determine the current mde with a matching column from the csv
#' depends on:
#'   input$UI_Import_name_mde9: name of the selected column from csv for mde9
observe({
  updateRadioButtons(session = session,inputId = "Import_csv_mde9",label = paste0("Map ",input$UI_Import_name_mde9))
})
# automatically set name of mde to chosen radio button of corresponding column in csv
observeEvent(input$Import_csv_mde9,{
  if(input$UI_Import_name_mde9=="mde9" & input$Import_csv_mde9!="not required"){
    updateTextInput(session = session, inputId = "UI_Import_name_mde9",value = input$Import_csv_mde9)
  }
})

#' title
script_events("title", "csv", 1)
type_events("title", "csv")

#' set title of csv document
#' depends on:
#'   input$Import_csv_title: import title of csv file
#'   values$data_csv: csv data
observe({
  validate(
    need(!is.null(input$Import_csv_title),message=FALSE),
    need(input$Import_csv_title%in%c(colnames(values$data_csv),"automatic"),message=FALSE)
  )
  if(input$Import_csv_title=="automatic"){
    values$Import_csv_title<-paste("document ",1:dim(values$data_csv)[1],sep="")
  }
  else{
    values$Import_csv_title<-as.vector(as.matrix(values$data_csv[,input$Import_csv_title]))
  }
})




#' body
script_events("body", "csv", 3)
type_events("body", "csv")

#' set body from csv-file
#' depends on:
#'   input$Import_csv_body: import body from csv-file
#'   values$data_csv: data from csv
observe({
  validate(
    need(!is.null(input$Import_csv_body),message=FALSE),
    need(input$Import_csv_body%in%c(colnames(values$data_csv)),message=FALSE)
  )
  values$Import_csv_body<-as.vector(as.matrix(values$data_csv[,input$Import_csv_body]))
})


#' date
script_events("date", "csv", 4)
type_events("date", "csv")

#' set date from csv date
#' depends on:
#'   input$Import_csv_date: import date from csv-file
#'   values$data_csv: data from csv-file
observe({
  validate(
    need(!is.null(input$Import_csv_date),message=FALSE),
    need(input$Import_csv_date%in%c(colnames(values$data_csv),"automatic"),message=FALSE)
  )
  if(input$Import_csv_date=="automatic"){
    values$Import_csv_date<-rep(as.character(Sys.Date()),dim(values$data_csv)[1])
  }
  else{
    values$Import_csv_date<-as.vector(as.matrix(values$data_csv[,input$Import_csv_date]))
  }
})


#' mde1
script_events("mde1", "csv", 5)
type_events("mde1", "csv")
observe_mde("mde1", "csv")

#' mde2
script_events("mde2", "csv", 6)
type_events("mde2", "csv")
observe_mde("mde2", "csv")

#' mde3
script_events("mde3", "csv", 7)
type_events("mde3", "csv")
observe_mde("mde3", "csv")

#' mde4
script_events("mde4", "csv", 8)
type_events("mde4", "csv")
observe_mde("mde4", "csv")

#' mde5
script_events("mde5", "csv", 9)
type_events("mde5", "csv")
observe_mde("mde5", "csv")

#' mde6
script_events("mde6", "csv", 10)
type_events("mde6", "csv")
observe_mde("mde6", "csv")

#' mde7
script_events("mde7", "csv", 11)
type_events("mde7", "csv")
observe_mde("mde7", "csv")

#' mde8
script_events("mde8", "csv", 12)
type_events("mde8", "csv")
observe_mde("mde8", "csv")

#' mde9
script_events("mde9", "csv", 13)
type_events("mde9", "csv")
observe_mde("mde9", "csv")


#' set csv metadata
#' depends on:
#'   values$start_mapping: check if mapping started
#'   input$Import_csv_dataset: import csv-dataset
#'   values$host: selected host
#'   values$db_port: used port for database
#'   values$Import_csv_title: import title from csv-file
#'   values$Import_csv_date: import date from csv-file
#'   values$Import_csv_body: import body from csv-file
#'   values$Import_csv_token: import token from csv-file
#'   input$Import_csv_language: import language from csv-file
#'   values$Import_csv_mde1: import selected row from csv-file for mde1
#'   values$Import_csv_mde2: import selected row from csv-file for mde2
#'   values$Import_csv_mde3: import selected row from csv-file for mde3
#'   values$Import_csv_mde4: import selected row from csv-file for mde3
#'   values$Import_csv_mde5: import selected row from csv-file for mde5
#'   values$Import_csv_mde6: import selected row from csv-file for mde6
#'   values$Import_csv_mde7: import selected row from csv-file for mde7
#'   values$Import_csv_mde8: import selected row from csv-file for mde8
#'   values$Import_csv_mde9: import selected row from csv-file for mde9
#'   input$UI_Import_name_mde1: name for mde1
#'   input$UI_Import_name_mde2: name for mde2
#'   input$UI_Import_name_mde3: name for mde3
#'   input$UI_Import_name_mde4:name for mde4
#'   input$UI_Import_name_mde5: name for mde5
#'   input$UI_Import_name_mde6: name for mde6
#'   input$UI_Import_name_mde7: name for mde7
#'   input$UI_Import_name_mde8: name for mde8
#'   input$UI_Import_name_mde9: name for mde9
output$Import_csv_metadata<-DT::renderDataTable({
  if(values$start_mapping==T){
    dataset<-input$Import_csv_dataset
    # get id_doc automatically by finding an offset in database if abbreviation is  already used
    #check max id_doc in database for specified dataset
    offset=NA
    if(input$Import_csv_dataset!=""){
      mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
      print(paste0("SELECT MAX(id_doc) FROM ilcm.documents where dataset=",input$Import_csv_dataset,";"))
      offset<-RMariaDB::dbGetQuery(mydb,paste0("SELECT MAX(id_doc) FROM ilcm.documents where dataset='",input$Import_csv_dataset,"';"))[1,1]
    }
    if(is.na(offset)){
      offset=0
    }
    id_doc<-(offset+1):(offset+dim(values$data_csv)[1])
    values$Import_csv_id_doc<-id_doc
    
    title<-values$Import_csv_title
    date<-values$Import_csv_date
    body<-values$Import_csv_body
    token<-values$Import_csv_token
    language<-input$Import_csv_language
    
    mde1<-values$Import_csv_mde1
    mde2<-values$Import_csv_mde2
    mde3<-values$Import_csv_mde3
    mde4<-values$Import_csv_mde4
    mde5<-values$Import_csv_mde5
    mde6<-values$Import_csv_mde6
    mde7<-values$Import_csv_mde7
    mde8<-values$Import_csv_mde8
    mde9<-values$Import_csv_mde9
    
    if(is.null(input$Import_csv_dataset)){
      dataset<-""
    }
    
    max_length<-max(length(dataset),length(id_doc),length(mde1),length(title),length(date),length(body),length(mde2),length(token),length(mde3),length(mde4),length(language),length(mde5),length(mde6),length(mde7),length(mde8),length(mde9))
    data<-data.frame(dataset=rep(dataset,max_length))
    data$id_doc<-c(id_doc, rep("", nrow(data)-length(id_doc)))
    data$title<-c(title, rep("", nrow(data)-length(title)))
    # make sure no invalid multibyte code is present
    body<-iconv(body, "UTF-8", "UTF-8",sub='')
    data$body<-c(body, rep("", nrow(data)-length(body)))
    data$date<-c(date, rep("", nrow(data)-length(date)))
    data$token<-c(token, rep("", nrow(data)-length(token)))
    data$language<-rep(language,max_length)
    #free metadata
    data$mde1<-c(mde1, rep("", nrow(data)-length(mde1)))
    data$mde2<-c(mde2, rep("", nrow(data)-length(mde2)))
    data$mde3<-c(mde3, rep("", nrow(data)-length(mde3)))
    data$mde4<-c(mde4, rep("", nrow(data)-length(mde4)))
    data$mde5<-c(mde5, rep("", nrow(data)-length(mde5)))
    data$mde6<-c(mde6, rep("", nrow(data)-length(mde6)))
    data$mde7<-c(mde7, rep("", nrow(data)-length(mde7)))
    data$mde8<-c(mde8, rep("", nrow(data)-length(mde8)))
    data$mde9<-c(mde9, rep("", nrow(data)-length(mde9)))
    
    
    if(length(input$Import_csv_anonymize)>0){
      for(i in 1:length(input$Import_csv_anonymize)){
        data[,input$Import_csv_anonymize[i]]<-anonymizer::anonymize(.x = data[,input$Import_csv_anonymize[i]],.algo = "crc32")
      }
    }
    key.length=8
    if(length(input$Import_csv_pseudonymization)>0){
      lookup_tables<-list()
      for(i in 1:length(input$Import_csv_pseudonymization)){
        aliases <- c(1,1) # Allow the while loop to begin
        while (any(duplicated(aliases))) { # Loop until all keys are unique
          aliases <- replicate(length(unique(data[,input$Import_csv_pseudonymization[i]])), 
                               paste(sample(c(LETTERS, 0:9), key.length, replace = T), collapse = ''))
        }
        lookup.table <- data.frame(id = unique(data[,input$Import_csv_pseudonymization[i]]), key = aliases)
        lookup_tables[[i]]<-lookup.table
        data[,input$Import_csv_pseudonymization[i]]<-  lookup.table[, 'key'][match(data[,input$Import_csv_pseudonymization[i]], lookup.table[, 'id'])]
      }
      values$Import_csv_lookup_tables<-lookup_tables
      showModal(
        shiny::modalDialog(
          tags$h4("Download Lookup Table for chosen columns"),
          tags$h5("Attention! Download the Lookup tables only when all columns to be pseudonymized are selected, since a new encryption is selected in each run."),
          tags$hr(),
          tagList(lapply(1:length(input$Import_csv_pseudonymization),FUN = function(x){
            downloadButton(outputId = paste0("Import_csv_Download_Lookup_",x))
          }))
        )
      ) 
    }
    
    
    values$Import_csv_meta_complete<-data
    colnames(data)[8:16]<-c(input$UI_Import_name_mde1,input$UI_Import_name_mde2,input$UI_Import_name_mde3,input$UI_Import_name_mde4,input$UI_Import_name_mde5,input$UI_Import_name_mde6,
                            input$UI_Import_name_mde7,input$UI_Import_name_mde8,input$UI_Import_name_mde9)
    
    data<-data[1:min(5,dim(data)[1]),]
    data<-t(apply(data,1,FUN=function(i){apply(as.matrix(i),MARGIN = 1,FUN = function(x){if(is.na(x)){return(x)}else{if(nchar(x)>100){return(paste0(substr(x,1,100),"..."))}else{return(x)}}})}))
    
    datatable(data = data,options = list(dom="t",ordering=F),rownames = F)
  }
  else{
    return(NULL)
  }
})


#' download lookup tables
#' depends on:
#'   values$Import_csv_lookup_tables: created lookup tables
#'   input$Import_csv_pseudonymization: selected columns for pseudonymization
observe({
  lapply(1:length(values$Import_csv_lookup_tables),FUN = function(x){
    output[[paste0("Import_csv_Download_Lookup_",x)]]<-downloadHandler(
      filename=function(){
        paste0("Lookup_Table_",input$Import_csv_pseudonymization[x],".csv")
      },
      content = function(file){
        data<-values$Import_csv_lookup_tables[[x]]
        write.csv(data,file)
      }
    )
  })
})


#' import data from csv body
#' depends on:
#'   values$Import_csv_body: import csv-body
#'   values$Import_csv_token: import csv-token
#'   values$Import_csv_mde1: import selected row from csv-file for mde1
#'   values$Import_csv_mde2: import selected row from csv-file for mde2
#'   values$Import_csv_mde3: import selected row from csv-file for mde3
#'   values$Import_csv_mde4: import selected row from csv-file for mde3
#'   values$Import_csv_mde5: import selected row from csv-file for mde5
#'   values$Import_csv_mde6: import selected row from csv-file for mde6
#'   values$Import_csv_mde7: import selected row from csv-file for mde7
#'   values$Import_csv_mde8: import selected row from csv-file for mde8
#'   values$Import_csv_mde9: import selected row from csv-file for mde9
#'   input$UI_Import_name_mde1: name for mde1
#'   input$UI_Import_name_mde2: name for mde2
#'   input$UI_Import_name_mde3: name for mde3
#'   input$UI_Import_name_mde4:name for mde4
#'   input$UI_Import_name_mde5: name for mde5
#'   input$UI_Import_name_mde6: name for mde6
#'   input$UI_Import_name_mde7: name for mde7
#'   input$UI_Import_name_mde8: name for mde8
#'   input$UI_Import_name_mde9: name for mde9
observe({
  body<-values$Import_csv_body
  body<-stringr::str_remove_all(string = body,pattern = "\n")
  body<-stringr::str_squish(string = body)
  values$Import_csv_token<-unlist(lapply(X = body,FUN = function(x){
    length(stringr::str_split(string = x,pattern = " ",simplify = T))}
  ))
})

#' import csv dataset information
#' depends on:
#'   input$Import_csv_dataset: import csv dataset
#'   values$host: selected host
#'   values$db_port: used port to data base
observeEvent(ignoreNULL = T,input$Import_csv_dataset,{
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
  RMariaDB::dbBegin(conn = mydb)
  values$Import_csv_metadatafields<-RMariaDB::dbGetQuery(mydb,paste0("SELECT * from metadata_names where dataset='",input$Import_csv_dataset,"';"))
  RMariaDB::dbCommit(mydb)
  RMariaDB::dbDisconnect(mydb)
})

#' warning for metadata names
#' depends on:
#'   values$Import_csv_meta_complete: import complete metadata names 
#'   values$Import_csv_metadatafields: import metadatafields from csv
output$Import_csv_metadata_names_warning<-renderUI({
  validate(
    need(values$Import_csv_meta_complete[1,"dataset"]!="",message=F),
    #need(any(c(input$Import_csv_mde1,input$Import_csv_mde2,input$Import_csv_mde3,input$Import_csv_mde4,input$Import_csv_mde5,input$Import_csv_mde6,input$Import_csv_mde7,input$Import_csv_mde8,input$Import_csv_mde9)!="not required"),message=F),
    need(!is.null(values$Import_csv_metadatafields),message=F),
    need(dim(values$Import_csv_metadatafields)[1]>0,message = "This dataset is not used yet. Feel free to specify your metadata")
  )
  
  data_db<-values$Import_csv_metadatafields[1,which(!is.na(values$Import_csv_metadatafields))]
  data_db<-data_db[,-1,drop=F]
  data_import<-data.frame(t(c(input$UI_Import_name_mde1,input$UI_Import_name_mde2,input$UI_Import_name_mde3,input$UI_Import_name_mde4,input$UI_Import_name_mde5,input$UI_Import_name_mde6,input$UI_Import_name_mde7,input$UI_Import_name_mde8,input$UI_Import_name_mde9)))
  colnames(data_import)<-c("mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9")
  data_import<-data_import[1,which(c(input$Import_csv_mde1,input$Import_csv_mde2,input$Import_csv_mde3,input$Import_csv_mde4,input$Import_csv_mde5,input$Import_csv_mde6,input$Import_csv_mde7,input$Import_csv_mde8,input$Import_csv_mde9)!="not required"),drop=F]
  data<-plyr::rbind.fill(data_db,data_import)
  colors<-matrix(c(0),dim(data)[1],dim(data)[2])
  #get colors for matching mde's
  if(dim(data)[2]>0){
    for(i in 1:dim(data)[2]){
      mde_names<-data[,i]
      if(any(is.na(mde_names))){
        colors[,i]<-c(1,1)
        next
      }
      if(mde_names[1]== mde_names[2]){
        colors[,i]<-c(0,0)
        next
      }
      else{
        colors[,i]<-c(2,2)
      }
    }
  }
  data<-cbind(data,colors)
  rownames(data)<-c("known","new")
  values$Import_csv_metadatanames_data<-data
  Icon<-tags$p(icon(name = "exclamation","fa-2x"),tags$b("Your current settings do not match those already existing in the database. You can still import your data though."),style="color:#ff8080")
  if(dim(data)[2]==0){
    Icon<-tags$p(icon(name = "check","fa-2x"),tags$b("Your settings match those in the database!"),style="color:#80ff80")
  }
  if(all(data[,c(((ncol(data)/2)+1):ncol(data))]==0)){
    Icon<-tags$p(icon(name = "check","fa-2x"),tags$b("Your settings match those in the database!"),style="color:#80ff80")
  }
  return(tagList(
    tags$div(HTML(paste0("There is already a corpus existing with the abbreviation:",tags$b(isolate(input$Import_csv_dataset)),". If you like to add data to this corpus, be aware of the used mde's:"))
    ),
    DT::dataTableOutput(outputId = "Import_csv_metadatanames_table"),
    tags$br(),
    Icon
  ))
})

#' import metadata names for presenting informations
#' depends on:
#'   values$Import_csv_metadatanames_data: import metadata names from csv-file
output$Import_csv_metadatanames_table<-DT::renderDataTable({
  data =values$Import_csv_metadatanames_data
  validate(
    need(dim(data)[2]>0,message="In the database aswell as in the current setting no mde's are beeing used.")
  )
  table<-DT::datatable( data = data,class = 'cell-border stripe',
                        options=list(dom="t",selection="none",columnDefs=list(list(targets=c(((ncol(data)/2)+1):ncol(data)),visible=F))))%>%
    DT::formatStyle(
      c(1:(ncol(data)/2)), c(((ncol(data)/2)+1):ncol(data)),
      backgroundColor = styleEqual(c(0, 1,2), c('#80ff80', '#ffc04d','#ff8080'))
    )
  return(table)
})

#' start preprocessing the csv-files
#' depends on:
#'   input$Import_csv_start_preprocess: initiate preprocessing
#'   values$Import_csv_meta_complete: import complete meta data from csv-file
#'   input$Import_csv_dataset:
#'   input$UI_Import_name_mde1: import name of mde1
#'   input$UI_Import_name_mde2: import name of mde2
#'   input$UI_Import_name_mde3: import name of mde3
#'   input$UI_Import_name_mde4: import name of mde4
#'   input$UI_Import_name_mde5: import name of mde5
#'   input$UI_Import_name_mde6: import name of mde6
#'   input$UI_Import_name_mde7: import name of mde7
#'   input$UI_Import_name_mde8: import name of mde8
#'   input$UI_Import_name_mde9: import name of mde9
#'   input$Import_csv_date_format: import csv-file date-format 
#'   input$Import_csv_mde1: import selected row for mde1
#'   input$Import_csv_mde2: import selected row for mde2
#'   input$Import_csv_mde3: import selected row for mde3
#'   input$Import_csv_mde4: import selected row for mde4
#'   input$Import_csv_mde5: import selected row for mde5
#'   input$Import_csv_mde6: import selected row for mde6
#'   input$Import_csv_mde7: import selected row for mde7
#'   input$Import_csv_mde8: import selected row for mde8
#'   input$Import_csv_mde9: import selected row for mde9
observeEvent(input$Import_csv_start_preprocess,{
  #test if metadata is valid 
  data<-values$Import_csv_meta_complete
  if(length(unique(data[,"id_doc"]))!=dim(data)[1]){
    shinyWidgets::sendSweetAlert(session=session,title = "Document id not unique",text = "Please specify id_doc to be unique for every document!",type = "error")
  }
  else{
    if(!(is.numeric(as.numeric(data[,"id_doc"])))){
      shinyWidgets::sendSweetAlert(session=session,title = "Document id not a number",text = "Please specify id_doc to be an integer",type = "error")
    }
    else{
      if(nchar(as.character(data[1,"dataset"]))==0){
        shinyWidgets::sendSweetAlert(session=session,title = "dataset abbreviation not specified",text = "Please specify a abbreviation for the data",type = "error")
      }
      else{
        if(stringr::str_detect(as.character(data[1,"dataset"]),pattern = "_")){
          shinyWidgets::sendSweetAlert(session=session,title = "'_' not allowed",text = "Please specify a abbreviation without using '_'",type = "error")
        }
        else{
          if(any(inherits(try({as.Date(data[,"date"],input$Import_csv_date_format)}),"Date")==F)){
            shinyWidgets::sendSweetAlert(session=session,title = "At least one given date can't be imported",text = "Please specify the date and the date format",type = "error")
          }
          else{
            if(any(nchar(data[,"body"],allowNA = T)<=2)){
              #shinyWidgets::sendSweetAlert(session=session,title = "Body is empty for at least one document",type = "warning")
              confirmSweetAlert(
                session = session,
                inputId = "confirm_empty_body_csv_no_db",
                title = NULL,
                type="warning",
                text = tags$b(
                  "There is at least one document with empty body"
                ),
                btn_labels = c("Cancel and change settings", "Continue anyway"),
                html = TRUE
              )
            }
            else{
              # create meta metadata vector
              meta_metadata<-data.frame(t(c(input$Import_csv_dataset,input$UI_Import_name_mde1,input$UI_Import_name_mde2,input$UI_Import_name_mde3,input$UI_Import_name_mde4,input$UI_Import_name_mde5,
                                            input$UI_Import_name_mde6,input$UI_Import_name_mde7,input$UI_Import_name_mde8,input$UI_Import_name_mde9)))
              colnames(meta_metadata)<-c("dataset","mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9")
              if(input$Import_csv_mde1=="not required" && all(data[,"mde1"]=="")){
                meta_metadata[,"mde1"]<-NULL
              }
              if(input$Import_csv_mde2=="not required" && all(data[,"mde2"]=="")){
                meta_metadata[,"mde2"]<-NULL
              }
              if(input$Import_csv_mde3=="not required" && all(data[,"mde3"]=="")){
                meta_metadata[,"mde3"]<-NULL
              }
              if(input$Import_csv_mde4=="not required" && all(data[,"mde4"]=="")){
                meta_metadata[,"mde4"]<-NULL
              }
              if(input$Import_csv_mde5=="not required" && all(data[,"mde5"]=="")){
                meta_metadata[,"mde5"]<-NULL
              }
              if(input$Import_csv_mde6=="not required" && all(data[,"mde6"]=="")){
                meta_metadata[,"mde6"]<-NULL
              }
              if(input$Import_csv_mde7=="not required" && all(data[,"mde7"]=="")){
                meta_metadata[,"mde7"]<-NULL
              }
              if(input$Import_csv_mde8=="not required" && all(data[,"mde8"]=="")){
                meta_metadata[,"mde8"]<-NULL
              }
              if(input$Import_csv_mde9=="not required" && all(data[,"mde9"]=="")){
                meta_metadata[,"mde9"]<-NULL
              }
              #save needed parameters
              slow_mode<-input$Import_csv_slow_mode
              parameters<-list(data,db=FALSE,lang=data[1,"language"],input$Import_csv_date_format,meta_metadata,slow_mode=slow_mode)
              #create process ID
              ID<-get_task_id_counter()+1
              set_task_id_counter(ID)
              #save metadata for process
              process_info<-list(ID,paste("New Data - ",input$Import_csv_dataset,sep=""),"Create import csv files",as.character(Sys.time()))
              #save logfile path
              logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
              #create logfile
              write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
              #save data needed in script execution 
              save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
              #start script
              system(paste('Rscript collections/scripts/Import_Script.R','&'))
              #show modal when process is started
              shinyWidgets::sendSweetAlert(session=session,title = "Started Import Script",type = "success")
            }
          }
        }
      }
    }
  }
})

#' if confirm to continue with empty body is clicked run import script anyway
#' depends on:
#'   input$confirm_empty_body_csv_no_db: confirm is csv body is empty
#'   values$Import_csv_meta_complete: import complete csv meta data 
#'   input$Import_csv_dataset: imported csv dataset
#'   input$UI_Import_name_mde1: chosen name of mde1
#'   input$UI_Import_name_mde2: chosen name of mde2
#'   input$UI_Import_name_mde3: chosen name of mde3
#'   input$UI_Import_name_mde4: chosen name of mde4
#'   input$UI_Import_name_mde5: chosen name of mde5
#'   input$UI_Import_name_mde6: chosen name of mde6
#'   input$UI_Import_name_mde7: chosen name of mde7
#'   input$UI_Import_name_mde8: chosen name of mde8
#'   input$UI_Import_name_mde9: chosen name of mde9
#'   input$Import_csv_date_format: date format from chosen csv-file
observeEvent(ignoreNULL = T,input$confirm_empty_body_csv_no_db,{
  if(input$confirm_empty_body_csv_no_db){
    data<-values$Import_csv_meta_complete
    #create meta metadata vector
    meta_metadata<-data.frame(t(c(input$Import_csv_dataset,input$UI_Import_name_mde1,input$UI_Import_name_mde2,input$UI_Import_name_mde3,input$UI_Import_name_mde4,input$UI_Import_name_mde5,
                                  input$UI_Import_name_mde6,input$UI_Import_name_mde7,input$UI_Import_name_mde8,input$UI_Import_name_mde9)))
    colnames(meta_metadata)<-c("dataset","mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9")
    if(input$Import_csv_mde1=="not required" && all(data[,"mde1"]=="")){
      meta_metadata[,"mde1"]<-NULL
    }
    if(input$Import_csv_mde2=="not required" && all(data[,"mde2"]=="")){
      meta_metadata[,"mde2"]<-NULL
    }
    if(input$Import_csv_mde3=="not required" && all(data[,"mde3"]=="")){
      meta_metadata[,"mde3"]<-NULL
    }
    if(input$Import_csv_mde4=="not required" && all(data[,"mde4"]=="")){
      meta_metadata[,"mde4"]<-NULL
    }
    if(input$Import_csv_mde5=="not required" && all(data[,"mde5"]=="")){
      meta_metadata[,"mde5"]<-NULL
    }
    if(input$Import_csv_mde6=="not required" && all(data[,"mde6"]=="")){
      meta_metadata[,"mde6"]<-NULL
    }
    if(input$Import_csv_mde7=="not required" && all(data[,"mde7"]=="")){
      meta_metadata[,"mde7"]<-NULL
    }
    if(input$Import_csv_mde8=="not required" && all(data[,"mde8"]=="")){
      meta_metadata[,"mde8"]<-NULL
    }
    if(input$Import_csv_mde9=="not required" && all(data[,"mde9"]=="")){
      meta_metadata[,"mde9"]<-NULL
    }
    #save needed parameters
    slow_mode<-input$Import_csv_slow_mode
    parameters<-list(data,db=FALSE,lang=data[1,"language"],input$Import_csv_date_format,meta_metadata,slow_mode=slow_mode)
    #create process ID
    ID<-get_task_id_counter()+1
    set_task_id_counter(ID)
    #save metadata for process
    process_info<-list(ID,paste("New Data - ",input$Import_csv_dataset,sep=""),"Create import csv files",as.character(Sys.time()))
    #save logfile path
    logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
    #create logfile
    write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
    #save data needed in script execution 
    save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
    #start script
    system(paste('Rscript collections/scripts/Import_Script.R','&'))
    #show modal when process is started
    shinyWidgets::sendSweetAlert(session=session,title = "Started Import Script",type = "success")
  }
})

#' observe preprocessing of csv-file
#' depends on:
#'   input$Import_csv_start_preprocess_and_write: initiate start for preprocessing 
#'   values$Import_csv_meta_complete: import complete meta data from csv
#'   input$Import_csv_dataset: import csv dataset
#'   input$UI_Import_name_mde1: import chosen name of mde1
#'   input$UI_Import_name_mde2: import chosen name of mde2
#'   input$UI_Import_name_mde3: import chosen name of mde3
#'   input$UI_Import_name_mde4: import chosen name of mde4
#'   input$UI_Import_name_mde5: import chosen name of mde5
#'   input$UI_Import_name_mde6: import chosen name of mde6
#'   input$UI_Import_name_mde7: import chosen name of mde7
#'   input$UI_Import_name_mde8: import chosen name of mde8
#'   input$UI_Import_name_mde9: import chosen name of mde9
#'   input$Import_csv_date_format: import date format from csv
observeEvent(input$Import_csv_start_preprocess_and_write,{
  #test if metadata is valid 
  data<-values$Import_csv_meta_complete
  if(length(unique(data[,"id_doc"]))!=dim(data)[1]){
    shinyWidgets::sendSweetAlert(session=session,title = "Document id not unique",text = "Please specify id_doc to be unique for every document!",type = "error")
  }
  else{
    if(!(is.numeric(as.numeric(data[,"id_doc"])))){
      shinyWidgets::sendSweetAlert(session=session,title = "Document id not a number",text = "Please specify id_doc to be an integer",type = "error")
    }
    else{
      if(nchar(as.character(data[1,"dataset"]))==0){
        shinyWidgets::sendSweetAlert(session=session,title = "dataset abbreviation not specified",text = "Please specify a abbreviation for the data",type = "error")
      }
      else{
        if(stringr::str_detect(as.character(data[1,"dataset"]),pattern = "_")){
          shinyWidgets::sendSweetAlert(session=session,title = "'_' not allowed",text = "Please specify a abbreviation without using '_'",type = "error")
        }
        else{
          if(any(inherits(try({as.Date(data[,"date"],input$Import_mtf_date_format)}),"Date")==F)){
            shinyWidgets::sendSweetAlert(session=session,title = "At least one given date can't be imported",text = "Please specify the date and the date format",type = "error")
          }
          else{
            if(any(nchar(data[,"body"],allowNA = T)<=2)){
              #shinyWidgets::sendSweetAlert(session=session,title = "Body is empty for at least one document",type = "warning")
              confirmSweetAlert(
                session = session,
                inputId = "confirm_empty_body_csv_db",
                title = NULL,
                type="warning",
                text = tags$b(
                  "There is at least one document with empty body"
                ),
                btn_labels = c("Cancel and change settings", "Continue anyway"),
                html = TRUE
              )
            }
            else{
              #create meta metadata vector
              meta_metadata<-data.frame(t(c(input$Import_csv_dataset,input$UI_Import_name_mde1,input$UI_Import_name_mde2,input$UI_Import_name_mde3,input$UI_Import_name_mde4,input$UI_Import_name_mde5,
                                            input$UI_Import_name_mde6,input$UI_Import_name_mde7,input$UI_Import_name_mde8,input$UI_Import_name_mde9)))
              colnames(meta_metadata)<-c("dataset","mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9")
              if(input$Import_csv_mde1=="not required" && all(data[,"mde1"]=="")){
                meta_metadata[,"mde1"]<-NULL
              }
              if(input$Import_csv_mde2=="not required" && all(data[,"mde2"]=="")){
                meta_metadata[,"mde2"]<-NULL
              }
              if(input$Import_csv_mde3=="not required" && all(data[,"mde3"]=="")){
                meta_metadata[,"mde3"]<-NULL
              }
              if(input$Import_csv_mde4=="not required" && all(data[,"mde4"]=="")){
                meta_metadata[,"mde4"]<-NULL
              }
              if(input$Import_csv_mde5=="not required" && all(data[,"mde5"]=="")){
                meta_metadata[,"mde5"]<-NULL
              }
              if(input$Import_csv_mde6=="not required" && all(data[,"mde6"]=="")){
                meta_metadata[,"mde6"]<-NULL
              }
              if(input$Import_csv_mde7=="not required" && all(data[,"mde7"]=="")){
                meta_metadata[,"mde7"]<-NULL
              }
              if(input$Import_csv_mde8=="not required" && all(data[,"mde8"]=="")){
                meta_metadata[,"mde8"]<-NULL
              }
              if(input$Import_csv_mde9=="not required" && all(data[,"mde9"]=="")){
                meta_metadata[,"mde9"]<-NULL
              }
              #save needed parameters
              slow_mode<-input$Import_csv_slow_mode
              parameters<-list(data,db=TRUE,lang=data[1,"language"],input$Import_csv_date_format,meta_metadata,slow_mode=slow_mode)
              #create process ID
              ID<-get_task_id_counter()+1
              set_task_id_counter(ID)
              #save metadata for process
              process_info<-list(ID,paste("New Data - ",input$Import_csv_dataset,sep=""),"Create import csv files",as.character(Sys.time()))
              #save logfile path
              logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
              #create logfile
              write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
              #save data needed in script execution 
              save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
              #start script
              system(paste('Rscript collections/scripts/Import_Script.R','&'))
              #show modal when process is started
              shinyWidgets::sendSweetAlert(session=session,title = "Started Import Script",type = "success")
            }
          }
        }
      }
    }
  }
})

#' if confirm to continue with empty body is clicked run import script anyway
#' depends on:
#'   input$confirm_empty_body_csv_db: confirm if csv body is empty
#'   values$Import_csv_meta_complete: import complete meta data from csv
#'   input$Import_csv_dataset: import csv dataset
#'   input$UI_Import_name_mde1: import chosen name from mde1
#'   input$UI_Import_name_mde2: import chosen name from mde2
#'   input$UI_Import_name_mde3: import chosen name from mde3
#'   input$UI_Import_name_mde4: import chosen name from mde4
#'   input$UI_Import_name_mde5: import chosen name from mde5
#'   input$UI_Import_name_mde6: import chosen name from mde6
#'   input$UI_Import_name_mde7: import chosen name from mde7
#'   input$UI_Import_name_mde8: import chosen name from mde8
#'   input$UI_Import_name_mde9: import chosen name from mde9
#'   input$Import_csv_date_format: import date format from csv-file
observeEvent(ignoreNULL = T,input$confirm_empty_body_csv_db,{
  if(input$confirm_empty_body_csv_db){
    data<-values$Import_csv_meta_complete
    #create meta metadata vector
    meta_metadata<-data.frame(t(c(input$Import_csv_dataset,input$UI_Import_name_mde1,input$UI_Import_name_mde2,input$UI_Import_name_mde3,input$UI_Import_name_mde4,input$UI_Import_name_mde5,
                                  input$UI_Import_name_mde6,input$UI_Import_name_mde7,input$UI_Import_name_mde8,input$UI_Import_name_mde9)))
    colnames(meta_metadata)<-c("dataset","mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9")
    if(input$Import_csv_mde1=="not required" && all(data[,"mde1"]=="")){
      meta_metadata[,"mde1"]<-NULL
    }
    if(input$Import_csv_mde2=="not required" && all(data[,"mde2"]=="")){
      meta_metadata[,"mde2"]<-NULL
    }
    if(input$Import_csv_mde3=="not required" && all(data[,"mde3"]=="")){
      meta_metadata[,"mde3"]<-NULL
    }
    if(input$Import_csv_mde4=="not required" && all(data[,"mde4"]=="")){
      meta_metadata[,"mde4"]<-NULL
    }
    if(input$Import_csv_mde5=="not required" && all(data[,"mde5"]=="")){
      meta_metadata[,"mde5"]<-NULL
    }
    if(input$Import_csv_mde6=="not required" && all(data[,"mde6"]=="")){
      meta_metadata[,"mde6"]<-NULL
    }
    if(input$Import_csv_mde7=="not required" && all(data[,"mde7"]=="")){
      meta_metadata[,"mde7"]<-NULL
    }
    if(input$Import_csv_mde8=="not required" && all(data[,"mde8"]=="")){
      meta_metadata[,"mde8"]<-NULL
    }
    if(input$Import_csv_mde9=="not required" && all(data[,"mde9"]=="")){
      meta_metadata[,"mde9"]<-NULL
    }
    #save needed parameters
    slow_mode<-input$Import_csv_slow_mode
    parameters<-list(data,db=TRUE,lang=data[1,"language"],input$Import_csv_date_format,meta_metadata,slow_mode=slow_mode)
    #create process ID
    ID<-get_task_id_counter()+1
    set_task_id_counter(ID)
    #save metadata for process
    process_info<-list(ID,paste("New Data - ",input$Import_csv_dataset,sep=""),"Create import csv files",as.character(Sys.time()))
    #save logfile path
    logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
    #create logfile
    write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
    #save data needed in script execution 
    save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
    #start script
    system(paste('Rscript collections/scripts/Import_Script.R','&'))
    #show modal when process is started
    shinyWidgets::sendSweetAlert(session=session,title = "Started Import Script",type = "success")
  }
})
#' sanity check for importet csv-file
#' depends on:
#'   input$Import_csv_sanity_check:  start sanity check for importet csv
#'   input$Import_csv_mde1 : selected mde1 row
#'   input$Import_csv_mde2 : selected mde2 row
#'   input$Import_csv_mde3 : selected mde3 row
#'   input$Import_csv_mde4 : selected mde4 row
#'   input$Import_csv_mde5 : selected mde5 row
#'   input$Import_csv_mde6 : selected mde6 row
#'   input$Import_csv_mde7 : selected mde7 row
#'   input$Import_csv_mde8 : selected mde8 row
#'   input$Import_csv_mde9 : selected mde9 row
observeEvent(input$Import_csv_sanity_check,{
  data_check_choices <- c("body", "id_doc", "title", "date")
  if(input$Import_csv_mde1 != "not required"){
    data_check_choices <- c(data_check_choices, "mde1")
  }
  if(input$Import_csv_mde2 != "not required"){
    data_check_choices <- c(data_check_choices, "mde2")
  }
  if(input$Import_csv_mde3 != "not required"){
    data_check_choices <- c(data_check_choices, "mde3")
  }
  if(input$Import_csv_mde4 != "not required"){
    data_check_choices <- c(data_check_choices, "mde4")
  }
  if(input$Import_csv_mde5 != "not required"){
    data_check_choices <- c(data_check_choices, "mde5")
  }
  if(input$Import_csv_mde6 != "not required"){
    data_check_choices <- c(data_check_choices, "mde6")
  }
  if(input$Import_csv_mde7 != "not required"){
    data_check_choices <- c(data_check_choices, "mde7")
  }
  if(input$Import_csv_mde8!= "not required"){
    data_check_choices <- c(data_check_choices, "mde8")
  }
  if(input$Import_csv_mde9 != "not required"){
    data_check_choices <- c(data_check_choices, "mde9")
  }
  
  sanity_check_Modal("csv", data_check_choices)
})


##########################################################################################################
#                                import multiple text files   MTF                                        #
##########################################################################################################
values$Import_mtf_title<-""
values$Import_mtf_date<-""
values$Import_mtf_body<-""
values$Import_mtf_mde1<-""
values$Import_mtf_mde2<-""
values$Import_mtf_mde3<-""
values$Import_mtf_mde4<-""
values$Import_mtf_mde5<-""
values$Import_mtf_mde6<-""
values$Import_mtf_mde7<-""
values$Import_mtf_mde8<-""
values$Import_mtf_mde9<-""
values$Import_mtf_language<-""
values$Import_mtf_token<-""
values$Import_mtf_dataset<-""
values$Import_mtf_scripts<-""
values$Import_mtf_split_scripts<-""

#' render import of multiple files:
#' depends on:
#'   values$invalidate_mtf_files: invalidate mutiple files
output$UI_Import_mtf_file<-renderUI({
  values$invalidate_mtf_files
  validate(
    need(length(list.dirs("data_import/unprocessed_data/"))>1,message="No directory with text files found in 'data_import/unprocessed_data'")
  )
  return(
    tagList(
      shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_files",label = "Directory",
                                       choices = list.dirs("data_import/unprocessed_data/",recursive = F,full.names = F),
                                       fill=T,animation = "pulse",selected = character(0))
    ))
  
})
#' observe import of new mtf
#' depends on:
#'   input$Import_mtf_new: new mtf to import
observeEvent(input$Import_mtf_new,ignoreInit = T,{
  validate(
    need(
      !is.null(input$Import_mtf_new),message=F
    )
  )
  
  print(input$Import_mtf_new)
  dir_name<-uuid::UUIDgenerate(use.time = T)
  dir.create(recursive = T,path = paste0("data_import/unprocessed_data/",dir_name))
  values$Import_mtf_new_directory_name_uuid<- paste0("data_import/unprocessed_data/",dir_name)
  for(i in 1:dim(input$Import_mtf_new)[1]){
    file.copy(from = input$Import_mtf_new$datapath[i],to = paste0("data_import/unprocessed_data/",dir_name,"/",input$Import_mtf_new$name[i]))
  }
  showModal(modalDialog(size = "s",
                        textInput(inputId = "Import_mtf_new_name",label = "Name new directory",placeholder = "new directory name"),
                        bsButton(inputId = "Import_mtf_new_name_button",label = "Save",icon = icon("save"),style = "success")
  ))
})

#' observ import of new name of mtf when pressing a button
#' depends on:
#'   input$Import_mtf_new_name_button: new name of mtf
#'   values$Import_mtf_new_directory_name_uuid: name uuid
#'    values$invalidate_mtf_files: invalidate files
observeEvent(ignoreInit = T,input$Import_mtf_new_name_button,{
  file.rename(from =values$Import_mtf_new_directory_name_uuid ,to =paste0("data_import/unprocessed_data/",input$Import_mtf_new_name))
  removeModal()
  shinyWidgets::sendSweetAlert(session=session,title = "File added",text = "You can now select it in the list of files above",type = "success")
  values$invalidate_mtf_files<-runif(1,0,1)
})


#' observe event: load mtf
#' depends on:
#'   input$Import_load_mtf: start loading
#'   input$Import_mtf_files: mtf file for import
#'   values$Import_mtf_scripts: mtf scripts
#'   values$header_mtf: header of mtf
#'   values$data_mtf: mtf data
#'   values$data_load_mtf_success: succes if loading mtf data was a success
#'   values$Import_mtf_split_scripts: split scripts from mtf
observeEvent(input$Import_load_mtf,{
  withBusyIndicatorServer("Import_load_mtf", {
    validate(
      need(length(list.dirs("data_import/unprocessed_data/"))>1,message=FALSE)
    )
    file_data <- readtext::readtext(file=list.files(paste0("data_import/unprocessed_data/",input$Import_mtf_files),full.names = T))
    colnames(file_data)[1] <- "file_id"
    data<-cbind(id = row.names(file_data),file_data)
    values$Import_mtf_scripts<-rep(default_script_decription_import,13) # 13 for id_doc, title, date, body and 9 mde's
    
    values$header_mtf<-colnames(data)
    values$data_mtf<-data
    values$data_load_mtf_success<-TRUE
    
    values$Import_mtf_split_scripts <- default_script_decription_split
  })
})
#'meta data csv
#'depends on:
#'  input$Import_mtf_metadata_csv: csv metadata from mtf
#'  input$Import_mtf_metadata_csv_header: header from csv meta data
#'  values$mtf_metadata: metadata from mtf
#'  values$data_mtf: mtf data
#'  values$header_mtf: header from mtf
observeEvent(input$Import_mtf_metadata_csv,{
  values$mtf_metadata<-read.csv(input$Import_mtf_metadata_csv$datapath,header=input$Import_mtf_metadata_csv_header)
  if(dim(values$mtf_metadata)[1]!=dim(isolate(values$data_mtf))[1]){
    shinyWidgets::sendSweetAlert(session=session,title = "Wrong dimensions",text = paste0("Metadata CSV does not match the number of files in the chosen directory. ",
                                                                                          isolate(dim(values$data_mtf)[1])," files were imported The metadata csv has ",isolate(dim(values$mtf_metadata)[1])," rows."),type = "warning")
  }
  else{
    isolate(values$header_mtf<-c(values$header_mtf,colnames(values$mtf_metadata)))
    isolate(values$data_mtf<-cbind(values$data_mtf,values$mtf_metadata))   
  }
})


#' split
script_events("split", "mtf", 1, TRUE)
#' observe: split test view
#' depends on:
#'   input$Import_mtf_split_test_view: mtf test view
observeEvent(input$Import_mtf_split_test_view, {
  split_test_view("mtf")
})
#' observe mapping of mtf
#' depends on:
#'   input$Import_start_mapping_mtf: start mapping process
#'   input$Import_mtf_split_method: split method for mtf
observeEvent(input$Import_start_mapping_mtf,{
  if(input$Import_mtf_split_method != 'None') {
    showModal(modalDialog(
      title="Are you sure you want to split",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_start_mapping_mtf", "Continue", styleclass = "info")
      ),
      "You can't simply undo this split. You would have to reimport your data to go back."
    ))
  } else {
    start_mapping_mtf()
  }
})
#' observe: start of mapping
#' depends on:
#' input$confirm_start_mapping_mtf: initiate start of mtf mapping
observeEvent(input$confirm_start_mapping_mtf,{
  start_mapping_mtf()
  removeModal()
})

#' start mapping mtf
#' @param 
#' @return
start_mapping_mtf <- function() {
  process_split("mtf")
  values$start_mapping_mtf<-TRUE
}

output$data_load_mtf_success<-reactive({
  values$data_load_mtf_success
})

output$start_mapping_mtf<-reactive({
  values$start_mapping_mtf
})
outputOptions(output, "data_load_mtf_success", suspendWhenHidden = FALSE)
outputOptions(output, "start_mapping_mtf", suspendWhenHidden = FALSE)

output$Import_head_mtf<-DT::renderDataTable({
  data<-values$data_mtf
  data<-data[1:min(5,dim(data)[1]),]
  data<-t(apply(data,1,FUN=function(i){apply(as.matrix(i),MARGIN = 1,FUN = function(x){if(nchar(x)>100){return(substr(x,1,100))}else{return(x)}})}))
  datatable(data = data,options = list(lengthChange = FALSE,dom="t"),width = "90%")
})

#' observe import of mtf
#' depends on:
#'  input$Import_check_mtf: check mtf for import
observeEvent(input$Import_check_mtf,{
  showModal(
    modalDialog(
      size = "l",
      title = "Imported Directory",easyClose = T,
      tags$div(style="overflow-x:auto; height:70vh;",
               dataTableOutput(outputId = "Import_head_mtf")
      )
    ) 
  )
})

#' render output of 
#' depends on:
#'   input$Import_mtf_column_name: column name from mtf import 
#'   values$header_mtf: header from mtf
output$UI_Import_mtf_column_name <- renderUI({
  select_value <- input$Import_mtf_column_name
  selectInput(inputId = "Import_mtf_column_name", "Column:", choices=values$header_mtf, selected = select_value)%>%
    shinyInput_label_embed(
      icon("info") %>%
        bs_embed_tooltip(title = "Select a column with the data you want to split.")
    )
})


#' render mtf title 
#'   depends on:
#'     values$header_mtf: mtf from header
output$UI_Import_mtf_title<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_title",label = "Map title",
                                   choices = c("automatic",values$header_mtf),
                                   fill=T,animation = "pulse",selected = "automatic")
})
#' render output of mtf date
output$UI_Import_mtf_date<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_date",label = "Map date",
                                   choices = c("automatic",values$header_mtf),
                                   fill=T,animation = "pulse",selected = "automatic")
})
#' render mtf body
output$UI_Import_mtf_body<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_body",label = "Map body",
                                   choices = values$header_mtf,
                                   fill=T,animation = "pulse")
})


#' import mdf 1
output$UI_Import_mtf_mde1<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_mde1",label = "Map mde1",
                                   choices = c("not required",values$header_mtf),
                                   fill=T,animation = "pulse")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_mtf_mde1",label = paste0("Map ",input$UI_Import_name_mde1_mtf))
})

# automatically set name of mde to chosen radio button of corresponding column in csv
observeEvent(input$Import_mtf_mde1,{
  if(input$UI_Import_name_mde1_mtf=="mde1" & input$Import_mtf_mde1!="not required"){
    updateTextInput(session = session, inputId = "UI_Import_name_mde1_mtf",value = input$Import_mtf_mde1)
  }
})

#' import mdf 2
output$UI_Import_mtf_mde2<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_mde2",label = "Map mde2",
                                   choices = c("not required",values$header_mtf),
                                   fill=T,animation = "pulse")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_mtf_mde2",label = paste0("Map ",input$UI_Import_name_mde2_mtf))
})
# automatically set name of mde to chosen radio button of corresponding column in csv
observeEvent(input$Import_mtf_mde2,{
  if(input$UI_Import_name_mde2_mtf=="mde2" & input$Import_mtf_mde2!="not required"){
    updateTextInput(session = session, inputId = "UI_Import_name_mde2_mtf",value = input$Import_mtf_mde2)
  }
})

#' import mdf 3
output$UI_Import_mtf_mde3<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_mde3",label = "Map mde3",
                                   choices = c("not required",values$header_mtf),
                                   fill=T,animation = "pulse")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_mtf_mde3",label = paste0("Map ",input$UI_Import_name_mde3_mtf))
})
# automatically set name of mde to chosen radio button of corresponding column in csv
observeEvent(input$Import_mtf_mde3,{
  if(input$UI_Import_name_mde3_mtf=="mde3" & input$Import_mtf_mde3!="not required"){
    updateTextInput(session = session, inputId = "UI_Import_name_mde3_mtf",value = input$Import_mtf_mde3)
  }
})
#' import mdf 4
output$UI_Import_mtf_mde4<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_mde4",label = "Map mde4",
                                   choices = c("not required",values$header_mtf),
                                   fill=T,animation = "pulse")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_mtf_mde4",label = paste0("Map ",input$UI_Import_name_mde4_mtf))
})

# automatically set name of mde to chosen radio button of corresponding column in csv
observeEvent(input$Import_mtf_mde4,{
  if(input$UI_Import_name_mde4_mtf=="mde4" & input$Import_mtf_mde4!="not required"){
    updateTextInput(session = session, inputId = "UI_Import_name_mde4_mtf",value = input$Import_mtf_mde4)
  }
})
#' import mdf 5
output$UI_Import_mtf_mde5<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_mde5",label ="Map mde5",
                                   choices = c("not required",values$header_mtf),
                                   fill=T,animation = "pulse")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_mtf_mde5",label = paste0("Map ",input$UI_Import_name_mde5_mtf))
})

# automatically set name of mde to chosen radio button of corresponding column in csv
observeEvent(input$Import_mtf_mde5,{
  if(input$UI_Import_name_mde5_mtf=="mde5" & input$Import_mtf_mde5!="not required"){
    updateTextInput(session = session, inputId = "UI_Import_name_mde5_mtf",value = input$Import_mtf_mde5)
  }
})
#' import mdf 6
output$UI_Import_mtf_mde6<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_mde6",label = "Map mde6",
                                   choices = c("not required",values$header_mtf),
                                   fill=T,animation = "pulse")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_mtf_mde6",label = paste0("Map ",input$UI_Import_name_mde6_mtf))
})

# automatically set name of mde to chosen radio button of corresponding column in csv
observeEvent(input$Import_mtf_mde6,{
  if(input$UI_Import_name_mde6_mtf=="mde6" & input$Import_mtf_mde6!="not required"){
    updateTextInput(session = session, inputId = "UI_Import_name_mde6_mtf",value = input$Import_mtf_mde6)
  }
})

#' import mdf 7
output$UI_Import_mtf_mde7<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_mde7",label ="Map mde7",
                                   choices = c("not required",values$header_mtf),
                                   fill=T,animation = "pulse")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_mtf_mde7",label = paste0("Map ",input$UI_Import_name_mde7_mtf))
})

# automatically set name of mde to chosen radio button of corresponding column in csv
observeEvent(input$Import_mtf_mde7,{
  if(input$UI_Import_name_mde7_mtf=="mde7" & input$Import_mtf_mde7!="not required"){
    updateTextInput(session = session, inputId = "UI_Import_name_mde7_mtf",value = input$Import_mtf_mde7)
  }
})

#' import mdf 8
output$UI_Import_mtf_mde8<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_mde8",label = "Map mde8",
                                   choices = c("not required",values$header_mtf),
                                   fill=T,animation = "pulse")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_mtf_mde8",label = paste0("Map ",input$UI_Import_name_mde8_mtf))
})

# automatically set name of mde to chosen radio button of corresponding column in csv
observeEvent(input$Import_mtf_mde8,{
  if(input$UI_Import_name_mde8_mtf=="mde8" & input$Import_mtf_mde8!="not required"){
    updateTextInput(session = session, inputId = "UI_Import_name_mde8_mtf",value = input$Import_mtf_mde8)
  }
})

#' import mdf 9
output$UI_Import_mtf_mde9<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_mde9",label = "Map mde9",
                                   choices = c("not required",values$header_mtf),
                                   fill=T,animation = "pulse")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_mtf_mde9",label = paste0("Map ",input$UI_Import_name_mde9_mtf))
})

# automatically set name of mde to chosen radio button of corresponding column in csv
observeEvent(input$Import_mtf_mde9,{
  if(input$UI_Import_name_mde9_mtf=="mde9" & input$Import_mtf_mde9!="not required"){
    updateTextInput(session = session, inputId = "UI_Import_name_mde9_mtf",value = input$Import_mtf_mde9)
  }
})

#' title
script_events("title", "mtf", 1)
type_events("title", "mtf")
#' observe title of mtf
#' depends on:
#'   input$Import_mtf_title: mtf title 
#'   values$data_mtf: mtf data
observe({
  validate(
    need(!is.null(input$Import_mtf_title),message=FALSE),
    need(input$Import_mtf_title%in%c(colnames(values$data_mtf),"automatic"),message=FALSE)
  )
  if(input$Import_mtf_title=="automatic"){
    values$Import_mtf_title<-paste("document ",1:dim(values$data_mtf)[1],sep="")
  }
  else{
    values$Import_mtf_title<-as.vector(as.matrix(values$data_mtf[,input$Import_mtf_title]))
  }
})



#' body
script_events("body", "mtf", 3)
type_events("body", "mtf")
#' validate body
#' depends on:
#'   input$Import_mtf_body: import mtf body
#'   values$data_mtf: mtf data
observe({
  validate(
    need(!is.null(input$Import_mtf_body),message=FALSE),
    need(input$Import_mtf_body%in%colnames(values$data_mtf),message=FALSE)
  )
  values$Import_mtf_body<-as.vector(as.matrix(values$data_mtf[,input$Import_mtf_body]))
})


#' date
script_events("date", "mtf", 4)
type_events("date", "mtf")
#' validate mtf date
#' depends on:
#'   input$Import_mtf_date: import mtf date
#'   values$data_mtf: mtf data
observe({
  validate(
    need(!is.null(input$Import_mtf_date),message=FALSE),
    need(input$Import_mtf_date%in%c(colnames(values$data_mtf),"automatic"),message=FALSE)
  )
  if(input$Import_mtf_date=="automatic"){
    values$Import_mtf_date<-rep(as.character(Sys.Date()),dim(values$data_mtf)[1])
  }
  else{
    values$Import_mtf_date<-as.vector(as.matrix(values$data_mtf[,input$Import_mtf_date]))
  }
})


#' mde1
script_events("mde1", "mtf", 5)
type_events("mde1", "mtf")
observe_mde("mde1", "mtf")

#' mde2
script_events("mde2", "mtf", 6)
type_events("mde2", "mtf")
observe_mde("mde2", "mtf")

#' mde3
script_events("mde3", "mtf", 7)
type_events("mde3", "mtf")
observe_mde("mde3", "mtf")

#' mde4
script_events("mde4", "mtf", 8)
type_events("mde4", "mtf")
observe_mde("mde4", "mtf")

#' mde5
script_events("mde5", "mtf", 9)
type_events("mde5", "mtf")
observe_mde("mde5", "mtf")

#' mde6
script_events("mde6", "mtf", 10)
type_events("mde6", "mtf")
observe_mde("mde6", "mtf")

#' mde7
script_events("mde7", "mtf", 11)
type_events("mde7", "mtf")
observe_mde("mde7", "mtf")

#' mde8
script_events("mde8", "mtf", 12)
type_events("mde8", "mtf")
observe_mde("mde8", "mtf")

#' mde9
script_events("mde9", "mtf", 13)
type_events("mde9", "mtf")
observe_mde("mde9", "mtf")



#' import mtf metadata
#' depends on:
#'   values$start_mapping_mtf: start mapping from mtf
#'   input$Import_mtf_dataset: import mtf dataset
#'   values$host: selected host
#'   values$db_port: database port
#'   values$Import_mtf_id_doc:mtf document id
#'   values$Import_mtf_title: mtf title
#'   values$Import_mtf_date: mtf date
#'   values$Import_mtf_body: mtf body
#'   values$Import_mtf_token: mtf token
#'   input$Import_mtf_language: mtf language
#'   values$Import_mtf_mde1: chosen mde1 from mtf
#'   values$Import_mtf_mde2: chosen mde2 from mtf
#'   values$Import_mtf_mde3: chosen mde3 from mtf
#'   values$Import_mtf_mde4: chosen mde4 from mtf
#'   values$Import_mtf_mde5: chosen mde5 from mtf
#'   values$Import_mtf_mde6: chosen mde6 from mtf
#'   values$Import_mtf_mde7: chosen mde7 from mtf
#'   values$Import_mtf_mde8: chosen mde8 from mtf
#'   values$Import_mtf_mde9: chosen mde9 from mtf
#'   input$Import_mtf_anonymize: anonymize mtf
#'   input$Import_mtf_pseudonymization: pseudonymization for mtf
#'   values$Import_mtf_lookup_tables: lookup tables from mtf
#'   values$Import_mtf_meta_complete: complete meta data
output$Import_mtf_metadata<-DT::renderDataTable({
  if(values$start_mapping_mtf==T){
    dataset<-input$Import_mtf_dataset
    
    #check max id_doc in database for specified dataset
    offset=NA
    if(input$Import_mtf_dataset!=""){
      mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
      #print(paste0("SELECT MAX(id_doc) FROM ilcm.documents where dataset=",input$Import_mtf_dataset,";"))
      offset<-RMariaDB::dbGetQuery(mydb,paste0("SELECT MAX(id_doc) FROM ilcm.documents where dataset='",input$Import_mtf_dataset,"';"))[1,1]
    }
    if(is.na(offset)){
      offset=0
    }
    id_doc<-(offset+1):(offset+dim(values$data_mtf)[1])
    values$Import_mtf_id_doc<-id_doc
    
    title<-values$Import_mtf_title
    date<-values$Import_mtf_date
    body<-values$Import_mtf_body
    token<-values$Import_mtf_token
    language<-input$Import_mtf_language
    mde1<-values$Import_mtf_mde1
    mde2<-values$Import_mtf_mde2
    mde3<-values$Import_mtf_mde3
    mde4<-values$Import_mtf_mde4
    mde5<-values$Import_mtf_mde5
    mde6<-values$Import_mtf_mde6
    mde7<-values$Import_mtf_mde7
    mde8<-values$Import_mtf_mde8
    mde9<-values$Import_mtf_mde9
    
    if(is.null(input$Import_mtf_dataset)){
      dataset<-""
    }
    max_length<-max(length(dataset),length(id_doc),length(mde1),length(title),length(date),length(body),length(mde2),
                    length(token),length(mde3),length(mde4),length(language),length(mde5),length(mde6),length(mde7),length(mde8),length(mde9))
    data<-data.frame(dataset=rep(dataset,max_length))
    data$id_doc<-c(id_doc, rep("", nrow(data)-length(id_doc)))
    data$title<-c(title, rep("", nrow(data)-length(title)))
    #make sure there is no invalid multibyte string
    body<-iconv(body, "UTF-8", "UTF-8",sub='')
    data$body<-c(body, rep("", nrow(data)-length(body)))
    data$date<-c(date, rep("", nrow(data)-length(date)))
    data$token<-c(token, rep("", nrow(data)-length(token)))
    data$language<-rep(language,max_length)
    #free metadata
    data$mde1<-c(mde1, rep("", nrow(data)-length(mde1)))
    data$mde2<-c(mde2, rep("", nrow(data)-length(mde2)))
    data$mde3<-c(mde3, rep("", nrow(data)-length(mde3)))
    data$mde4<-c(mde4, rep("", nrow(data)-length(mde4)))
    data$mde5<-c(mde5, rep("", nrow(data)-length(mde5)))
    data$mde6<-c(mde6, rep("", nrow(data)-length(mde6)))
    data$mde7<-c(mde7, rep("", nrow(data)-length(mde7)))
    data$mde8<-c(mde8, rep("", nrow(data)-length(mde8)))
    data$mde9<-c(mde9, rep("", nrow(data)-length(mde9)))
    
    if(length(input$Import_mtf_anonymize)>0){
      for(i in 1:length(input$Import_mtf_anonymize)){
        data[,input$Import_mtf_anonymize[i]]<-anonymizer::anonymize(.x = data[,input$Import_mtf_anonymize[i]],.algo = "crc32")
      }
    }
    if(length(input$Import_mtf_pseudonymization)>0){
      lookup_tables<-list()
      for(i in 1:length(input$Import_mtf_pseudonymization)){
        aliases <- c(1,1) # Allow the while loop to begin
        while (any(duplicated(aliases))) { # Loop until all keys are unique
          aliases <- replicate(length(unique(data[,input$Import_mtf_pseudonymization[i]])), 
                               paste(sample(c(LETTERS, 0:9), key.length, replace = T), collapse = ''))
        }
        lookup.table <- data.frame(id = unique(data[,input$Import_mtf_pseudonymization[i]]), key = aliases)
        lookup_tables[[i]]<-lookup.table
        data[,input$Import_mtf_pseudonymization[i]]<-  lookup.table[, 'key'][match(data[,input$Import_mtf_pseudonymization[i]], lookup.table[, 'id'])]
      }
      values$Import_mtf_lookup_tables<-lookup_tables
      showModal(
        shiny::modalDialog(
          tags$h4("Download Lookup Table for chosen columns"),
          tags$h5("Attention! Download the Lookup tables only when all columns to be pseudonymized are selected, since a new encryption is selected in each run."),
          tags$hr(),
          tagList(lapply(1:length(input$Import_mtf_pseudonymization),FUN = function(x){
            downloadButton(outputId = paste0("Import_mtf_Download_Lookup_",x))
          }))
        )
      ) 
    }
    
    values$Import_mtf_meta_complete<-data
    
    colnames(data)[8:16]<-c(input$UI_Import_name_mde1_mtf,input$UI_Import_name_mde2_mtf,input$UI_Import_name_mde3_mtf,input$UI_Import_name_mde4_mtf,
                            input$UI_Import_name_mde5_mtf,input$UI_Import_name_mde6_mtf,
                            input$UI_Import_name_mde7_mtf,input$UI_Import_name_mde8_mtf,input$UI_Import_name_mde9_mtf)
    
    data<-data[1:min(5,dim(data)[1]),]
    data<-t(apply(data,1,FUN=function(i){apply(as.matrix(i),MARGIN = 1,FUN = function(x){if(nchar(x)>100){return(paste0(substr(x,1,100),"..."))}else{return(x)}})}))
    
    datatable(data = data,options = list(dom="t",ordering=F),rownames = F)
  }
  else{
    return(NULL)
  }
})



#' download lookup tables
#' depends on:
#'   values$Import_mtf_lookup_tables: created lookup tables
#'   input$Import_mtf_pseudonymization: selected columns for pseudonymization
observe({
  lapply(1:length(values$Import_mtf_lookup_tables),FUN = function(x){
    output[[paste0("Import_mtf_Download_Lookup_",x)]]<-downloadHandler(
      filename=function(){
        paste0("Lookup_Table_",input$Import_mtf_pseudonymization[x],".csv")
      },
      content = function(file){
        data<-values$Import_mtf_lookup_tables[[x]]
        write.csv(data,file)
      }
    )
  })
})

#' import mtf body
#' depends on:
#'   values$Import_mtf_body: import mtf body
#'   values$Import_mtf_token: import mtf token
observe({
  body<-values$Import_mtf_body
  body<-stringr::str_remove_all(string = body,pattern = "\n")
  body<-stringr::str_squish(string = body)
  values$Import_mtf_token<-unlist(lapply(X = body,FUN = function(x){
    length(stringr::str_split(string = x,pattern = " ",simplify = T))}
  ))
})
#' observe: import of the mtf dataset
#' depends on:
#'   input$Import_mtf_dataset: import mtf dataset
#'   values$host: selected host
#'   values$db_port: database port
#'   values$Import_mtf_metadatafields: meta data fields
observeEvent(ignoreNULL = T,input$Import_mtf_dataset,{
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
  RMariaDB::dbBegin(conn = mydb)
  values$Import_mtf_metadatafields<-RMariaDB::dbGetQuery(mydb,paste0("SELECT * from metadata_names where dataset='",input$Import_mtf_dataset,"';"))
  RMariaDB::dbCommit(mydb)
  RMariaDB::dbDisconnect(mydb)
})
#' render warning from metadata names
#' depends on:
#'   values$Import_mtf_meta_complete: complete meta data
#'   values$Import_mtf_metadatafields: meta data fields 
#'   values$Import_mtf_metadatanames_data: meta data names
output$Import_mtf_metadata_names_warning<-renderUI({
  validate(
    need(values$Import_mtf_meta_complete[1,"dataset"]!="",message=F),
    #need(any(c(input$Import_csv_mde1,input$Import_csv_mde2,input$Import_csv_mde3,input$Import_csv_mde4,input$Import_csv_mde5,input$Import_csv_mde6,input$Import_csv_mde7,input$Import_csv_mde8,input$Import_csv_mde9)!="not required"),message=F),
    need(!is.null(values$Import_mtf_metadatafields),message=F),
    need(dim(values$Import_mtf_metadatafields)[1]>0,message = "This dataset is not used yet. Feel free to specify your metadata")
  )
  
  data_db<-values$Import_mtf_metadatafields[1,which(!is.na(values$Import_mtf_metadatafields))]
  data_db<-data_db[,-1,drop=F]
  data_import<-data.frame(t(c(input$UI_Import_name_mde1_mtf,input$UI_Import_name_mde2_mtf,input$UI_Import_name_mde3_mtf,input$UI_Import_name_mde4_mtf,input$UI_Import_name_mde5_mtf,input$UI_Import_name_mde6_mtf,input$UI_Import_name_mde7_mtf,input$UI_Import_name_mde8_mtf,input$UI_Import_name_mde9_mtf)))
  colnames(data_import)<-c("mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9")
  data_import<-data_import[1,which(c(input$Import_mtf_mde1,input$Import_mtf_mde2,input$Import_mtf_mde3,input$Import_mtf_mde4,input$Import_mtf_mde5,input$Import_mtf_mde6,input$Import_mtf_mde7,input$Import_mtf_mde8,input$Import_mtf_mde9)!="not required"),drop=F]
  data<-plyr::rbind.fill(data_db,data_import)
  colors<-matrix(c(0),dim(data)[1],dim(data)[2])
  #get colors for matching mde's
  if(dim(data)[2]>0){
    for(i in 1:dim(data)[2]){
      mde_names<-data[,i]
      if(any(is.na(mde_names))){
        colors[,i]<-c(1,1)
        next
      }
      if(mde_names[1]== mde_names[2]){
        colors[,i]<-c(0,0)
        next
      }
      else{
        colors[,i]<-c(2,2)
      }
    }
  }
  data<-cbind(data,colors)
  rownames(data)<-c("known","new")
  values$Import_mtf_metadatanames_data<-data
  Icon<-tags$p(icon(name = "exclamation","fa-2x"),tags$b("Your current settings do not match those already existing in the database. You can still import your data though."),style="color:#ff8080")
  if(dim(data)[2]==0){
    Icon<-tags$p(icon(name = "check","fa-2x"),tags$b("Your settings match those in the database!"),style="color:#80ff80")
  }
  if(all(data[,c(((ncol(data)/2)+1):ncol(data))]==0)){
    Icon<-tags$p(icon(name = "check","fa-2x"),tags$b("Your settings match those in the database!"),style="color:#80ff80")
  }
  return(tagList(
    tags$div(HTML(paste0("There is already a corpus existing with the abbreviation:",tags$b(isolate(input$Import_mtf_dataset)),". If you like to add data to this corpus, be aware of the used mde's:"))
    ),
    DT::dataTableOutput(outputId = "Import_mtf_metadatanames_table"),
    tags$br(),
    Icon
  ))
})
#' render meta data table of mtf
#' depends on:
#'   values$Import_mtf_metadatanames_data: mft metadata names
output$Import_mtf_metadatanames_table<-DT::renderDataTable({
  data =values$Import_mtf_metadatanames_data
  validate(
    need(dim(data)[2]>0,message="In the database aswell in the current setting no mde's are beeing used.")
  )
  table<-DT::datatable( data = data,class = 'cell-border stripe',
                        options=list(dom="t",selection="none",columnDefs=list(list(targets=c(((ncol(data)/2)+1):ncol(data)),visible=F))))%>%
    DT::formatStyle(
      c(1:(ncol(data)/2)), c(((ncol(data)/2)+1):ncol(data)),
      backgroundColor = styleEqual(c(0, 1,2), c('#80ff80', '#ffc04d','#ff8080'))
    )
  return(table)
})




#' import start preprocess of mtf
#' depends on:
#'   input$Import_mtf_start_preprocess: start preprocess of mtf
#'   values$Import_mtf_meta_complete: complete meta data
#'   input$Import_mtf_date_format: formate of date imported from mtf
#'   input$Import_mtf_dataset: dataset from mtf
#'   input$UI_Import_name_mde1_mtf: selected name from mde 1
#'   input$UI_Import_name_mde2_mtf: selected name from mde 2
#'   input$UI_Import_name_mde3_mtf: selected name from mde 3
#'   input$UI_Import_name_mde4_mtf: selected name from mde 4
#'   input$UI_Import_name_mde5_mtf: selected name from mde 5
#'   input$UI_Import_name_mde6_mtf: selected name from mde 6
#'   input$UI_Import_name_mde7_mtf: selected name from mde 7
#'   input$UI_Import_name_mde8_mtf: selected name from mde 8
#'   input$UI_Import_name_mde9_mtf: selected name from mde 9 
#'   input$Import_mtf_slow_mode: slow mode from mtf
#'   input$Import_mtf_date_format: mtf date format
observeEvent(input$Import_mtf_start_preprocess,{
  #test if metadata is valid 
  data<-values$Import_mtf_meta_complete
  if(length(unique(data[,"id_doc"]))!=dim(data)[1]){
    shinyWidgets::sendSweetAlert(session=session,title = "Document id not unique",text = "Please specify id_doc to be unique for every document!",type = "error")
  }
  else{
    if(!(is.numeric(as.numeric(data[,"id_doc"])))){
      shinyWidgets::sendSweetAlert(session=session,title = "Document id not a number",text = "Please specify id_doc to be an integer",type = "error")
    }
    else{
      if(nchar(as.character(data[1,"dataset"]))==0){
        shinyWidgets::sendSweetAlert(session=session,title = "dataset abbreviation not specified",text = "Please specify a abbreviation for the data",type = "error")
      }
      else{
        if(stringr::str_detect(as.character(data[1,"dataset"]),pattern = "_")){
          shinyWidgets::sendSweetAlert(session=session,title = "'_' not allowed",text = "Please specify a abbreviation without using '_'",type = "error")
        }
        else{
          if(nchar(data[1,"dataset"])>50){
            shinyWidgets::sendSweetAlert(session=session,title = "abbreviation too long",text = "Please specify a abbreviation with maximum 50 chars.",type = "error")
          }
          else{
            if(any(inherits(try({as.Date(data[,"date"],input$Import_mtf_date_format)}),"Date")==F)){
              shinyWidgets::sendSweetAlert(session=session,title = "At least one given date can't be imported",text = "Please specify the date and the date format",type = "error")
            }
            else{
              if(any(nchar(data[,"body"],allowNA = T)<=2)){
                #shinyWidgets::sendSweetAlert(session=session,title = "Body is empty for at least one document",type = "warning")
                confirmSweetAlert(
                  session = session,
                  inputId = "confirm_empty_body_mtf_no_db",
                  title = NULL,
                  type="warning",
                  text = tags$b(
                    "There is at least one document with empty body"
                  ),
                  btn_labels = c("Cancel and change settings", "Continue anyway"),
                  html = TRUE
                )
              }
              else{
                #create meta metadata vector
                meta_metadata<-data.frame(t(c(input$Import_mtf_dataset,input$UI_Import_name_mde1_mtf,input$UI_Import_name_mde2_mtf,input$UI_Import_name_mde3_mtf,input$UI_Import_name_mde4_mtf,input$UI_Import_name_mde5_mtf,
                                              input$UI_Import_name_mde6_mtf,input$UI_Import_name_mde7_mtf,input$UI_Import_name_mde8_mtf,input$UI_Import_name_mde9_mtf)))
                colnames(meta_metadata)<-c("dataset","mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9")
                if(input$Import_mtf_mde1=="not required" && all(data[,"mde1"]=="")){
                  meta_metadata[,"mde1"]<-NULL
                }
                if(input$Import_mtf_mde2=="not required" && all(data[,"mde2"]=="")){
                  meta_metadata[,"mde2"]<-NULL
                }
                if(input$Import_mtf_mde3=="not required" && all(data[,"mde3"]=="")){
                  meta_metadata[,"mde3"]<-NULL
                }
                if(input$Import_mtf_mde4=="not required" && all(data[,"mde4"]=="")){
                  meta_metadata[,"mde4"]<-NULL
                }
                if(input$Import_mtf_mde5=="not required" && all(data[,"mde5"]=="")){
                  meta_metadata[,"mde5"]<-NULL
                }
                if(input$Import_mtf_mde6=="not required" && all(data[,"mde6"]=="")){
                  meta_metadata[,"mde6"]<-NULL
                }
                if(input$Import_mtf_mde7=="not required" && all(data[,"mde7"]=="")){
                  meta_metadata[,"mde7"]<-NULL
                }
                if(input$Import_mtf_mde8=="not required" && all(data[,"mde8"]=="")){
                  meta_metadata[,"mde8"]<-NULL
                }
                if(input$Import_mtf_mde9=="not required" && all(data[,"mde9"]=="")){
                  meta_metadata[,"mde9"]<-NULL
                }
                #save needed parameters
                slow_mode<-input$Import_mtf_slow_mode
                parameters<-list(data,db=FALSE,lang=data[1,"language"],input$Import_mtf_date_format,meta_metadata,slow_mode=slow_mode)
                #create process ID
                ID<-get_task_id_counter()+1
                set_task_id_counter(ID)
                #save metadata for process
                process_info<-list(ID,paste("New Data - ",input$Import_mtf_dataset,sep=""),"Create import csv files",as.character(Sys.time()))
                #save logfile path
                logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
                #create logfile
                write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
                #save data needed in script execution 
                save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
                #start script
                system(paste('Rscript collections/scripts/Import_Script.R','&'))
                #show modal when process is started
                shinyWidgets::sendSweetAlert(session=session,title = "Started Import Script",type = "success")
              }
            }
          }
        }
      }
    }
  }
})


#' if confirm to continue with empty body is clicked run import script anyway
#' depends on:
#'   input$confirm_empty_body_mtf_no_db: confirm empty body from mtf 
#'   input$Import_mtf_dataset: mtf datset
#'   input$UI_Import_name_mde1_mtf: selected name of mdf 1
#'   input$UI_Import_name_mde2_mtf: selected name of mdf 2
#'   input$UI_Import_name_mde3_mtf: selected name of mdf 3
#'   input$UI_Import_name_mde4_mtf: selected name of mdf 4
#'   input$UI_Import_name_mde5_mtf: selected name of mdf 5
#'   input$UI_Import_name_mde6_mtf: selected name of mdf 6
#'   input$UI_Import_name_mde7_mtf: selected name of mdf 7
#'   input$UI_Import_name_mde8_mtf: selected name of mdf 8
#'   input$UI_Import_name_mde9_mtf: selected name of mdf 9 
#'   input$Import_mtf_slow_mode: slow mode from mtf
#'   input$Import_mtf_date_format: date format from mtf
observeEvent(ignoreNULL = T,input$confirm_empty_body_mtf_no_db,{
  if(input$confirm_empty_body_mtf_no_db){
    data<-values$Import_mtf_meta_complete #create meta metadata vector
    meta_metadata<-data.frame(t(c(input$Import_mtf_dataset,input$UI_Import_name_mde1_mtf,input$UI_Import_name_mde2_mtf,input$UI_Import_name_mde3_mtf,input$UI_Import_name_mde4_mtf,input$UI_Import_name_mde5_mtf,
                                  input$UI_Import_name_mde6_mtf,input$UI_Import_name_mde7_mtf,input$UI_Import_name_mde8_mtf,input$UI_Import_name_mde9_mtf)))
    colnames(meta_metadata)<-c("dataset","mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9")
    if(input$Import_mtf_mde1=="not required"){
      meta_metadata[,"mde1"]<-NULL
    }
    if(input$Import_mtf_mde2=="not required"){
      meta_metadata[,"mde2"]<-NULL
    }
    if(input$Import_mtf_mde3=="not required"){
      meta_metadata[,"mde3"]<-NULL
    }
    if(input$Import_mtf_mde4=="not required"){
      meta_metadata[,"mde4"]<-NULL
    }
    if(input$Import_mtf_mde5=="not required"){
      meta_metadata[,"mde5"]<-NULL
    }
    if(input$Import_mtf_mde6=="not required"){
      meta_metadata[,"mde6"]<-NULL
    }
    if(input$Import_mtf_mde7=="not required"){
      meta_metadata[,"mde7"]<-NULL
    }
    if(input$Import_mtf_mde8=="not required"){
      meta_metadata[,"mde8"]<-NULL
    }
    if(input$Import_mtf_mde9=="not required"){
      meta_metadata[,"mde9"]<-NULL
    }
    #save needed parameters
    slow_mode<-input$Import_mtf_slow_mode
    parameters<-list(data,db=FALSE,lang=data[1,"language"],input$Import_mtf_date_format,meta_metadata,slow_mode=slow_mode)
    #create process ID
    ID<-get_task_id_counter()+1
    set_task_id_counter(ID)
    #save metadata for process
    process_info<-list(ID,paste("New Data - ",input$Import_mtf_dataset,sep=""),"Create import csv files",as.character(Sys.time()))
    #save logfile path
    logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
    #create logfile
    write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
    #save data needed in script execution 
    save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
    #start script
    system(paste('Rscript collections/scripts/Import_Script.R','&'))
    #show modal when process is started
    shinyWidgets::sendSweetAlert(session=session,title = "Started Import Script",type = "success")
  }
})

#' start preprocess and writing
#' depends on:
#'   input$Import_mtf_start_preprocess_and_write: initiate start of preprocess and writting
#'   values$Import_mtf_meta_complete: import complete meta data
#'   input$Import_mtf_dataset: import mtf dataset
#'   input$UI_Import_name_mde1_mtf: selected name of mde 1
#'   input$UI_Import_name_mde2_mtf: selected name of mde 2
#'   input$UI_Import_name_mde3_mtf: selected name of mde 3
#'   input$UI_Import_name_mde4_mtf: selected name of mde 4
#'   input$UI_Import_name_mde5_mtf: selected name of mde 5
#'   input$UI_Import_name_mde6_mtf: selected name of mde 6
#'   input$UI_Import_name_mde7_mtf: selected name of mde 7
#'   input$UI_Import_name_mde8_mtf: selected name of mde 8
#'   input$UI_Import_name_mde9_mtf: selected name of mde 9 
#'   input$Import_mtf_slow_mode: slow mode from mtf
#'   input$Import_mtf_date_format: mtf date format
#'   
observeEvent(input$Import_mtf_start_preprocess_and_write,{
  #test if metadata is valid 
  data<-values$Import_mtf_meta_complete
  if(length(unique(data[,"id_doc"]))!=dim(data)[1]){
    shinyWidgets::sendSweetAlert(session=session,title = "Document id not unique",text = "Please specify id_doc to be unique for every document!",type = "error")
  }
  else{
    if(!(is.numeric(as.numeric(data[,"id_doc"])))){
      shinyWidgets::sendSweetAlert(session=session,title = "Document id not a number",text = "Please specify id_doc to be an integer",type = "error")
    }
    else{
      if(nchar(as.character(data[1,"dataset"]))==0){
        shinyWidgets::sendSweetAlert(session=session,title = "dataset abbreviation not specified",text = "Please specify a abbreviation for the data",type = "error")
      }
      else{
        if(stringr::str_detect(as.character(data[1,"dataset"]),pattern = "_")){
          shinyWidgets::sendSweetAlert(session=session,title = "'_' not allowed",text = "Please specify a abbreviation without using '_'",type = "error")
        }
        else{
          if(nchar(data[1,"dataset"])>50){
            shinyWidgets::sendSweetAlert(session=session,title = "abbreviation too long",text = "Please specify a abbreviation with maximum 50 chars.",type = "error")
          }
          else{
            if(any(inherits(try({as.Date(data[,"date"],input$Import_mtf_date_format)}),"Date")==F)){
              shinyWidgets::sendSweetAlert(session=session,title = "At least one given date can't be imported",text = "Please specify the date and the date format or if you are not intrested in using dates, just use the 'autoamtic'-option",type = "error")
            }
            else{
              if(any(nchar(data[,"body"],allowNA = T)<=2)){
                #shinyWidgets::sendSweetAlert(session=session,title = "Body is empty for at least one document",type = "warning")
                confirmSweetAlert(
                  session = session,
                  inputId = "confirm_empty_body_mtf_db",
                  title = NULL,
                  type="warning",
                  text = tags$b(
                    "There is at least one document with empty body"
                  ),
                  btn_labels = c("Cancel and change settings", "Continue anyway"),
                  html = TRUE
                )
              }
              else{
                #create meta metadata vector
                meta_metadata<-data.frame(t(c(input$Import_mtf_dataset,input$UI_Import_name_mde1_mtf,input$UI_Import_name_mde2_mtf,input$UI_Import_name_mde3_mtf,input$UI_Import_name_mde4_mtf,input$UI_Import_name_mde5_mtf,
                                              input$UI_Import_name_mde6_mtf,input$UI_Import_name_mde7_mtf,input$UI_Import_name_mde8_mtf,input$UI_Import_name_mde9_mtf)))
                colnames(meta_metadata)<-c("dataset","mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9")
                if(input$Import_mtf_mde1=="not required" && all(data[,"mde1"]=="")){
                  meta_metadata[,"mde1"]<-NULL
                }
                if(input$Import_mtf_mde2=="not required" && all(data[,"mde2"]=="")){
                  meta_metadata[,"mde2"]<-NULL
                }
                if(input$Import_mtf_mde3=="not required" && all(data[,"mde3"]=="")){
                  meta_metadata[,"mde3"]<-NULL
                }
                if(input$Import_mtf_mde4=="not required" && all(data[,"mde4"]=="")){
                  meta_metadata[,"mde4"]<-NULL
                }
                if(input$Import_mtf_mde5=="not required" && all(data[,"mde5"]=="")){
                  meta_metadata[,"mde5"]<-NULL
                }
                if(input$Import_mtf_mde6=="not required" && all(data[,"mde6"]=="")){
                  meta_metadata[,"mde6"]<-NULL
                }
                if(input$Import_mtf_mde7=="not required" && all(data[,"mde7"]=="")){
                  meta_metadata[,"mde7"]<-NULL
                }
                if(input$Import_mtf_mde8=="not required" && all(data[,"mde8"]=="")){
                  meta_metadata[,"mde8"]<-NULL
                }
                if(input$Import_mtf_mde9=="not required" && all(data[,"mde9"]=="")){
                  meta_metadata[,"mde9"]<-NULL
                }
                #save needed parameters
                slow_mode<-input$Import_mtf_slow_mode
                parameters<-list(data,db=TRUE,lang=data[1,"language"],input$Import_mtf_date_format,meta_metadata,slow_mode=slow_mode)
                #create process ID
                ID<-get_task_id_counter()+1
                set_task_id_counter(ID)
                #save metadata for process
                process_info<-list(ID,paste("New Data - ",input$Import_mtf_dataset,sep=""),"Create import csv files and write to DB and solr",as.character(Sys.time()))
                #save logfile path
                logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
                #create logfile
                write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
                #save data needed in script execution 
                save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
                #start script
                system(paste('Rscript collections/scripts/Import_Script.R','&'))
                #show modal when process is started
                shinyWidgets::sendSweetAlert(session=session,title = "Started Import Script",type = "success")
              }
            }
          }
        }
      }
    }
  }
})

#' if confirm to continue with empty body is clicked run import script anyway
#' depends on:
#'   input$confirm_empty_body_mtf_db: confirmation that the script has an empty body and is loading to the database anyway
#'   values$Import_mtf_meta_complete: complete meta data from script
#'   input$Import_mtf_dataset: import instruction for mtf dataset
#'   input$UI_Import_name_mde1_mtf: set name of mde1
#'   input$UI_Import_name_mde2_mtf: set name of mde2
#'   input$UI_Import_name_mde3_mtf: set name of mde3
#'   input$UI_Import_name_mde4_mtf: set name of mde4
#'   input$UI_Import_name_mde5_mtf: set name of mde5
#'   input$UI_Import_name_mde6_mtf: set name of mde6
#'   input$UI_Import_name_mde7_mtf: set name of mde7
#'   input$UI_Import_name_mde8_mtf: set name of mde8
#'   input$UI_Import_name_mde9_mtf: set name of mde9
#'   input$Import_mtf_slow_mode: checks if slow mode for mtf is activated
#'   input$Import_mtf_date_format: date format from mtf
observeEvent(ignoreNULL = T,input$confirm_empty_body_mtf_db,{
  if(input$confirm_empty_body_mtf_db){
    data<-values$Import_mtf_meta_complete
    #create meta metadata vector
    meta_metadata<-data.frame(t(c(input$Import_mtf_dataset,input$UI_Import_name_mde1_mtf,input$UI_Import_name_mde2_mtf,input$UI_Import_name_mde3_mtf,input$UI_Import_name_mde4_mtf,input$UI_Import_name_mde5_mtf,
                                  input$UI_Import_name_mde6_mtf,input$UI_Import_name_mde7_mtf,input$UI_Import_name_mde8_mtf,input$UI_Import_name_mde9_mtf)))
    colnames(meta_metadata)<-c("dataset","mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9")
    if(input$Import_mtf_mde1=="not required" && all(data[,"mde1"]=="")){
      meta_metadata[,"mde1"]<-NULL
    }
    if(input$Import_mtf_mde2=="not required" && all(data[,"mde2"]=="")){
      meta_metadata[,"mde2"]<-NULL
    }
    if(input$Import_mtf_mde3=="not required" && all(data[,"mde3"]=="")){
      meta_metadata[,"mde3"]<-NULL
    }
    if(input$Import_mtf_mde4=="not required" && all(data[,"mde4"]=="")){
      meta_metadata[,"mde4"]<-NULL
    }
    if(input$Import_mtf_mde5=="not required" && all(data[,"mde5"]=="")){
      meta_metadata[,"mde5"]<-NULL
    }
    if(input$Import_mtf_mde6=="not required" && all(data[,"mde6"]=="")){
      meta_metadata[,"mde6"]<-NULL
    }
    if(input$Import_mtf_mde7=="not required" && all(data[,"mde7"]=="")){
      meta_metadata[,"mde7"]<-NULL
    }
    if(input$Import_mtf_mde8=="not required" && all(data[,"mde8"]=="")){
      meta_metadata[,"mde8"]<-NULL
    }
    if(input$Import_mtf_mde9=="not required" && all(data[,"mde9"]=="")){
      meta_metadata[,"mde9"]<-NULL
    }
    #save needed parameters
    slow_mode<-input$Import_mtf_slow_mode
    parameters<-list(data,db=TRUE,lang=data[1,"language"],input$Import_mtf_date_format,meta_metadata,slow_mode=slow_mode)
    #create process ID
    ID<-get_task_id_counter()+1
    set_task_id_counter(ID)
    #save metadata for process
    process_info<-list(ID,paste("New Data - ",input$Import_mtf_dataset,sep=""),"Create import csv files and write to DB and solr",as.character(Sys.time()))
    #save logfile path
    logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
    #create logfile
    write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
    #save data needed in script execution 
    save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
    #start script
    system(paste('Rscript collections/scripts/Import_Script.R','&'))
    #show modal when process is started
    shinyWidgets::sendSweetAlert(session=session,title = "Started Import Script",type = "success")
  }
})
#' sanity check for the imported script
#' depends on: 
#'   input$Import_mtf_sanity_check: sanity check order to initiate checking
#'   input$Import_mtf_mde1: import mtf column mde1
#'   input$Import_mtf_mde2: import mtf column mde2
#'   input$Import_mtf_mde3: import mtf column mde3
#'   input$Import_mtf_mde4: import mtf column mde4
#'   input$Import_mtf_mde5: import mtf column mde5
#'   input$Import_mtf_mde6: import mtf column mde6
#'   input$Import_mtf_mde7: import mtf column mde7
#'   input$Import_mtf_mde8: import mtf column mde8
#'   input$Import_mtf_mde9: import mtf column mde9
observeEvent(input$Import_mtf_sanity_check,{
  data_check_choices <- c("body", "id_doc", "title", "date")
  if(input$Import_mtf_mde1 != "not required"){
    data_check_choices <- c(data_check_choices, "mde1")
  }
  if(input$Import_mtf_mde2 != "not required"){
    data_check_choices <- c(data_check_choices, "mde2")
  }
  if(input$Import_mtf_mde3 != "not required"){
    data_check_choices <- c(data_check_choices, "mde3")
  }
  if(input$Import_mtf_mde4 != "not required"){
    data_check_choices <- c(data_check_choices, "mde4")
  }
  if(input$Import_mtf_mde5 != "not required"){
    data_check_choices <- c(data_check_choices, "mde5")
  }
  if(input$Import_mtf_mde6 != "not required"){
    data_check_choices <- c(data_check_choices, "mde6")
  }
  if(input$Import_mtf_mde7 != "not required"){
    data_check_choices <- c(data_check_choices, "mde7")
  }
  if(input$Import_mtf_mde8!= "not required"){
    data_check_choices <- c(data_check_choices, "mde8")
  }
  if(input$Import_mtf_mde9 != "not required"){
    data_check_choices <- c(data_check_choices, "mde9")
  }
  
  sanity_check_Modal("mtf", data_check_choices)
})



autoInvalidate_normal <- reactiveTimer(500)
observe({
  autoInvalidate_normal()
  values$import_files_changed<-length(list.files("data_import/processed_data/"))
})

########################################
#            DB & SOLR                 #
########################################
#' render visulization to import files 
#' depends on:
#'   values$import_files_changed: import files that changes
output$Import_Files_UI<-renderUI({
  values$import_files_changed
  return(
    shinyWidgets::prettyRadioButtons(
      inputId = "Import_Files",
      label = "available data for upload",
      choices = unique(
        stringr::str_replace(
          string = stringr::str_replace_all(
            string = list.files("data_import/processed_data/"),
            pattern = ".csv",
            replacement = ""
          ),
          pattern = "[a-zA-Z]+\\_",
          replacement = ""
        )
      ),
      fill = F,
      animation = "tada",
      selected = character(0),
      shape="curve",
      inline=T,outline = T,plain = T,bigger = T
    )
  )
})



#' observing the process of loading the data into the database
#' depends on:
#'   input$Upload_Data: data to upload
#'   input$Import_Files: files to import
#'   values$host: selected host for database connection
#'   values$db_port: selected database port
#'   values$update_datasets_avaiable: update list of avaiable datasets
observeEvent(input$Upload_Data,{
  #check if already imported
  withBusyIndicatorServer("Upload_Data", {
    if(is.null(input$Import_Files)){
      shinyWidgets::sendSweetAlert(session=session,title = "no import file specified",text = "please specify a file you want to import!",type = "warning")
    }
    else{
      meta_metadata<-readr::read_csv(file=paste0("data_import/processed_data/metameta_",input$Import_Files,".csv"))
      mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
      a<-readr::read_csv(file = paste0("data_import/processed_data/meta_",input$Import_Files,".csv"),col_names = FALSE)[1,c(1,2)]
      b<-RMariaDB::dbGetQuery(mydb,paste0("Select title from documents where id_doc=",a[1,2]," and dataset='",a[1,1],"' limit 1;"))
      if(dim(b)[1]!=0){
        shinyWidgets::sendSweetAlert(type = "warning",session = session,title =  "Data seems to be uploaded already")
      }
      else{
        if(dim(meta_metadata)[2]==1){
          query<-paste0("LOAD DATA LOCAL INFILE '","data_import/processed_data/meta_",input$Import_Files,".csv","' INTO TABLE ilcm.documents  CHARACTER SET utf8mb4 FIELDS TERMINATED BY ',' ENCLOSED BY '\"' LINES TERMINATED BY '","\n","' 
                    (dataset,id_doc,title,body,date,token,language",",entities) ;")
          rs<- dbSendQuery(mydb, query)
        }
        else{
          query<-paste0("LOAD DATA LOCAL INFILE '","data_import/processed_data/meta_",input$Import_Files,".csv","' INTO TABLE ilcm.documents CHARACTER SET utf8mb4 FIELDS TERMINATED BY ',' ENCLOSED BY '\"' LINES TERMINATED BY '","\n","' 
                    (dataset,id_doc,title,body,date,token,language,",paste(colnames(meta_metadata)[2:dim(meta_metadata)[2]],collapse=","),",entities) ;")
          rs<- dbSendQuery(mydb, query)
        }
        query<-paste0("LOAD DATA LOCAL INFILE '","data_import/processed_data/token_",input$Import_Files,".csv","' INTO TABLE ilcm.token CHARACTER SET utf8mb4 FIELDS TERMINATED BY ',' ENCLOSED BY '\"' LINES TERMINATED BY '","\n","';")
        rs<- RMariaDB::dbSendStatement(mydb, query)
        try({
          if(dim(meta_metadata)[2]==1){
            query<-paste0("LOAD DATA LOCAL INFILE '","data_import/processed_data/metameta_",input$Import_Files,".csv","' INTO TABLE ilcm.metadata_names CHARACTER SET utf8mb4 FIELDS TERMINATED BY ',' ENCLOSED BY '\"' LINES TERMINATED BY '","\n","' IGNORE 1 LINES (dataset",");")
            rs<- dbSendQuery(mydb, query)
          }
          else{
            query<-paste0("LOAD DATA LOCAL INFILE '","data_import/processed_data/metameta_",input$Import_Files,".csv","' INTO TABLE ilcm.metadata_names CHARACTER SET utf8mb4 FIELDS TERMINATED BY ',' ENCLOSED BY '\"' LINES TERMINATED BY '","\n","' IGNORE 1 LINES (dataset,",paste(colnames(meta_metadata)[2:dim(meta_metadata)[2]],collapse=","),");")
            rs<- dbSendQuery(mydb, query)
          }
        })
        
        
        #update meta tables in database
        #data<-data.frame(readtext::readtext(file =paste0("data_import/processed_data/meta_",input$Import_Files,".csv") ),stringsAsFactors = F)
        data<-data.frame(readr::read_delim(file = paste0("data_import/processed_data/meta_",input$Import_Files,".csv"), delim=',',
                                           escape_double=FALSE, escape_backslash=TRUE, quote='"',col_names = F,na=character()),
                         stringsAsFactors = F)
        data<-cbind(rep(paste0("data_import/processed_data/meta_",input$Import_Files,".csv"),nrow(data)),data)%>%
          mutate_all(as.character)
        #remove entities table from data
        data<-data[,1:(ncol(data)-1)]
        #date
        dates<-unique(data[,6])
        dates<-cbind(rep(data[1,2],length(dates)),dates)
        rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_date (dataset, date) values ",paste(sprintf("('%s', '%s')", dates[,1], dates[,2]), collapse=', ') ,";"))
        #token
        token<-unique(data[,7])
        token<-cbind(rep(data[1,2],length(token)),token)
        rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_token (dataset, token) values ",paste(sprintf("('%s', %s)", token[,1], token[,2]), collapse=', ') ,";"))
        #mde1 
        
        try({
          mde1<-unique(data[,9])
          mde1<-cbind(rep(data[1,2],length(mde1)),mde1)
          #check if only NA
          if(any(!is.na(mde1[,2]))){
            mde1<-mde1[which(!is.na(mde1[,2])),,drop=F]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde1 (dataset, mde1) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde1[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde1[,2])), collapse=', ') ,";"))
          }
        })
        #mde2
        try({
          mde2<-unique(data[,10])
          mde2<-cbind(rep(data[1,2],length(mde2)),mde2)
          #check if only NA
          if(any(!is.na(mde2[,2]))){
            mde2<-mde2[which(!is.na(mde2[,2])),,drop=F]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde2 (dataset, mde2) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde2[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde2[,2])), collapse=', ') ,";"))
          }
        })
        #mde3
        try({
          mde3<-unique(data[,11])
          mde3<-cbind(rep(data[1,2],length(mde3)),mde3)
          #check if only NA
          if(any(!is.na(mde3[,2]))){
            mde3<-mde3[which(!is.na(mde3[,2])),,drop=F]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde3 (dataset, mde3) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde3[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde3[,2])), collapse=', ') ,";"))
          }
        })
        #mde4
        try({
          mde4<-unique(data[,12])
          mde4<-cbind(rep(data[1,2],length(mde4)),mde4)
          #check if only NA
          if(any(!is.na(mde4[,2]))){
            mde4<-mde4[which(!is.na(mde4[,2])),,drop=F]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde4 (dataset, mde4) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde4[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde4[,2])), collapse=', ') ,";"))
          }
        })
        #mde5
        try({
          mde5<-unique(data[,13])
          mde5<-cbind(rep(data[1,2],length(mde5)),mde5)
          #check if only NA
          if(any(!is.na(mde5[,2]))){
            mde5<-mde5[which(!is.na(mde5[,2])),,drop=F]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde5 (dataset, mde5) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde5[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde5[,2])), collapse=', ') ,";"))
          }
        })
        #mde6
        try({
          mde6<-unique(data[,14])
          mde6<-cbind(rep(data[1,2],length(mde6)),mde6)
          #check if only NA
          if(any(!is.na(mde6[,2]))){
            mde6<-mde6[which(!is.na(mde6[,2])),,drop=F]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde6 (dataset, mde6) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde6[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde6[,2])), collapse=', ') ,";"))
          }
        })
        #mde7
        try({
          mde7<-unique(data[,15])
          mde7<-cbind(rep(data[1,2],length(mde7)),mde7)
          #check if only NA
          if(any(!is.na(mde7[,2]))){
            mde7<-mde7[which(!is.na(mde7[,2])),,drop=F]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde7 (dataset, mde7) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde7[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde7[,2])), collapse=', ') ,";"))
          }
        })
        #mde8
        try({
          mde8<-unique(data[,16])
          mde8<-cbind(rep(data[1,2],length(mde8)),mde8)
          #check if only NA
          if(any(!is.na(mde8[,2]))){
            mde8<-mde8[which(!is.na(mde8[,2])),,drop=F]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde8 (dataset, mde8) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde8[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde8[,2])), collapse=', ') ,";"))
          }
        })
        #mde9
        try({
          mde9<-unique(data[,17])
          mde9<-cbind(rep(data[1,2],length(mde9)),mde9)
          #check if only NA
          if(any(!is.na(mde9[,2]))){
            mde9<-mde9[which(!is.na(mde9[,2])),,drop=F]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde9 (dataset, mde9) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde9[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde9[,2])), collapse=', ') ,";"))
          }
        })
        rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
        
        shinyWidgets::sendSweetAlert(type = "success",session = session,title =  "successfully imported data to database")
        values$update_datasets_avaiable<-runif(1,0,1)
      }
      RMariaDB::dbDisconnect(mydb)
    }
  })
})

#' load data to solr query
#' depends on:
#'   input$Import_to_solr: start command to load query to solr query
#'   values$solr_url: used solr url
observeEvent(input$Import_to_solr,{
  #import data to solr
  withBusyIndicatorServer("Import_to_solr", {
    url<-stringr::str_replace(string = values$solr_url,pattern = "select/",replacement = "")
    z<-RCurl::getURL(
      paste0(url,"dataimport?command=delta-import"),followlocation=TRUE
    )
    #initiate suggest
    z<-RCurl::getURL(
      paste0(url,"suggest?suggest.build=true"),followlocation=TRUE
    )
    shinyWidgets::sendSweetAlert(type = "success",session = session,title =  "successfully started solr delta import and solr suggest")
  })
})

#' upload the data to the database and solr query
#' depends on:
#'   input$Upload_Data_DB_and_Solr: initiate the upload
#'   input$Import_Files: files to import
#'   values$host: selected host
#'   values$db_port: selected database port
#'   values$update_datasets_avaiable: update list of available datasets 
observeEvent(input$Upload_Data_DB_and_Solr,{
  withBusyIndicatorServer("Upload_Data_DB_and_Solr",{
    if(is.null(input$Import_Files)){
      shinyWidgets::sendSweetAlert(session=session,title = "no import file specified",text = "please specify a file you want to import!",type = "warning")
    }
    else{
      meta_metadata<-readr::read_csv(file=paste0("data_import/processed_data/metameta_",input$Import_Files,".csv"))
      mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
      a<-readr::read_csv(file = paste0("data_import/processed_data/meta_",input$Import_Files,".csv"),col_names = FALSE)[1,c(1,2)]
      b<-RMariaDB::dbGetQuery(mydb,paste0("Select title from documents where id_doc=",a[1,2]," and dataset='",a[1,1],"' limit 1;"))
      if(dim(b)[1]!=0){
        shinyWidgets::sendSweetAlert(type = "warning",session = session,title =  "Data seems to be uploaded already")
      }
      else{
        if(dim(meta_metadata)[2]==1){
          query<-paste0("LOAD DATA LOCAL INFILE '","data_import/processed_data/meta_",input$Import_Files,".csv","' INTO TABLE ilcm.documents  CHARACTER SET utf8mb4 FIELDS TERMINATED BY ',' ENCLOSED BY '\"' LINES TERMINATED BY '","\n","' 
                    (dataset,id_doc,title,body,date,token,language",",entities) ;")
          rs<- dbSendQuery(mydb, query)
        }
        else{
          query<-paste0("LOAD DATA LOCAL INFILE '","data_import/processed_data/meta_",input$Import_Files,".csv","' INTO TABLE ilcm.documents CHARACTER SET utf8mb4 FIELDS TERMINATED BY ',' ENCLOSED BY '\"' LINES TERMINATED BY '","\n","' 
                    (dataset,id_doc,title,body,date,token,language,",paste(colnames(meta_metadata)[2:dim(meta_metadata)[2]],collapse=","),",entities) ;")
          rs<- dbSendQuery(mydb, query)
        }
        query<-paste0("LOAD DATA LOCAL INFILE '","data_import/processed_data/token_",input$Import_Files,".csv","' INTO TABLE ilcm.token CHARACTER SET utf8mb4 FIELDS TERMINATED BY ',' ENCLOSED BY '\"' LINES TERMINATED BY '","\n","';")
        rs<- RMariaDB::dbSendStatement(mydb, query)
        try({
          if(dim(meta_metadata)[2]==1){
            query<-paste0("LOAD DATA LOCAL INFILE '","data_import/processed_data/metameta_",input$Import_Files,".csv","' INTO TABLE ilcm.metadata_names CHARACTER SET utf8mb4 FIELDS TERMINATED BY ',' ENCLOSED BY '\"' LINES TERMINATED BY '","\n","' IGNORE 1 LINES (dataset",");")
            rs<- dbSendQuery(mydb, query)
          }
          else{
            query<-paste0("LOAD DATA LOCAL INFILE '","data_import/processed_data/metameta_",input$Import_Files,".csv","' INTO TABLE ilcm.metadata_names CHARACTER SET utf8mb4 FIELDS TERMINATED BY ',' ENCLOSED BY '\"' LINES TERMINATED BY '","\n","' IGNORE 1 LINES (dataset,",paste(colnames(meta_metadata)[2:dim(meta_metadata)[2]],collapse=","),");")
            rs<- dbSendQuery(mydb, query)
          }
        })
        
        
        #update meta tables in database
        #data<-data.frame(readtext::readtext(file =paste0("data_import/processed_data/meta_",input$Import_Files,".csv") ),stringsAsFactors = F)
        data<-data.frame(readr::read_delim(file = paste0("data_import/processed_data/meta_",input$Import_Files,".csv"), delim=',',
                                           escape_double=FALSE, escape_backslash=TRUE, quote='"',col_names = F,na=character()),
                         stringsAsFactors = F)
        data<-cbind(rep(paste0("data_import/processed_data/meta_",input$Import_Files,".csv"),nrow(data)),data)%>%
          mutate_all(as.character)
        #remove entities table from data
        data<-data[,1:(ncol(data)-1)]
        #date
        dates<-unique(data[,6])
        dates<-cbind(rep(data[1,2],length(dates)),dates)
        rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_date (dataset, date) values ",paste(sprintf("('%s', '%s')", dates[,1], dates[,2]), collapse=', ') ,";"))
        #token
        token<-unique(data[,7])
        token<-as.numeric(token)
        token<-token[which(!is.na(token))]
        token<-cbind(rep(data[1,2],length(token)),token)
        rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_token (dataset, token) values ",paste(sprintf("('%s', %s)", token[,1], token[,2]), collapse=', ') ,";"))
        #mde1 
        
        try({
          mde1<-unique(data[,9])
          mde1<-cbind(rep(data[1,2],length(mde1)),mde1)
          #check if only NA
          if(any(!is.na(mde1[,2]))){
            mde1<-mde1[which(!is.na(mde1[,2])),,drop=F]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde1 (dataset, mde1) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde1[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde1[,2])), collapse=', ') ,";"))
          }
        })
        #mde2
        try({
          mde2<-unique(data[,10])
          mde2<-cbind(rep(data[1,2],length(mde2)),mde2)
          #check if only NA
          if(any(!is.na(mde2[,2]))){
            mde2<-mde2[which(!is.na(mde2[,2])),,drop=F]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde2 (dataset, mde2) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde2[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde2[,2])), collapse=', ') ,";"))
          }
        })
        #mde3
        try({
          mde3<-unique(data[,11])
          mde3<-cbind(rep(data[1,2],length(mde3)),mde3)
          #check if only NA
          if(any(!is.na(mde3[,2]))){
            mde3<-mde3[which(!is.na(mde3[,2])),,drop=F]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde3 (dataset, mde3) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde3[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde3[,2])), collapse=', ') ,";"))
          }
        })
        #mde4
        try({
          mde4<-unique(data[,12])
          mde4<-cbind(rep(data[1,2],length(mde4)),mde4)
          #check if only NA
          if(any(!is.na(mde4[,2]))){
            mde4<-mde4[which(!is.na(mde4[,2])),,drop=F]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde4 (dataset, mde4) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde4[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde4[,2])), collapse=', ') ,";"))
          }
        })
        #mde5
        try({
          mde5<-unique(data[,13])
          mde5<-cbind(rep(data[1,2],length(mde5)),mde5)
          #check if only NA
          if(any(!is.na(mde5[,2]))){
            mde5<-mde5[which(!is.na(mde5[,2])),,drop=F]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde5 (dataset, mde5) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde5[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde5[,2])), collapse=', ') ,";"))
          }
        })
        #mde6
        try({
          mde6<-unique(data[,14])
          mde6<-cbind(rep(data[1,2],length(mde6)),mde6)
          #check if only NA
          if(any(!is.na(mde6[,2]))){
            mde6<-mde6[which(!is.na(mde6[,2])),,drop=F]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde6 (dataset, mde6) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde6[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde6[,2])), collapse=', ') ,";"))
          }
        })
        #mde7
        try({
          mde7<-unique(data[,15])
          mde7<-cbind(rep(data[1,2],length(mde7)),mde7)
          #check if only NA
          if(any(!is.na(mde7[,2]))){
            mde7<-mde7[which(!is.na(mde7[,2])),,drop=F]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde7 (dataset, mde7) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde7[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde7[,2])), collapse=', ') ,";"))
          }
        })
        #mde8
        try({
          mde8<-unique(data[,16])
          mde8<-cbind(rep(data[1,2],length(mde8)),mde8)
          #check if only NA
          if(any(!is.na(mde8[,2]))){
            mde8<-mde8[which(!is.na(mde8[,2])),,drop=F]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde8 (dataset, mde8) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde8[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde8[,2])), collapse=', ') ,";"))
          }
        })
        #mde9
        try({
          mde9<-unique(data[,17])
          mde9<-cbind(rep(data[1,2],length(mde9)),mde9)
          #check if only NA
          if(any(!is.na(mde9[,2]))){
            mde9<-mde9[which(!is.na(mde9[,2])),,drop=F]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde9 (dataset, mde9) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde9[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde9[,2])), collapse=', ') ,";"))
          }
        })
        rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
        values$update_datasets_avaiable<-runif(1,0,1)
      }
      RMariaDB::dbDisconnect(mydb)
    }
    
    #import to solr
    url<-stringr::str_replace(string = values$solr_url,pattern = "select/",replacement = "")
    z<-RCurl::getURL(
      paste0(url,"dataimport?command=delta-import"),followlocation=TRUE
    )
    #initiate suggest
    z<-RCurl::getURL(
      paste0(url,"suggest?suggest.build=true"),followlocation=TRUE
    )
    shinyWidgets::sendSweetAlert(type = "success",session = session,title =  "successfully imported data to database and started solr delta import and solr suggest")
    
    
    
  })
})



#' observe if user wants to delete the selected import files
#' depends on:
#'   input$Import_delete: delete command for imported files 
observeEvent(input$Import_delete,{
  shinyWidgets::confirmSweetAlert(session = session,inputId = "confirm_delete_import",type = "warning",title = "Are you sure you want to delete the selected input files",danger_mode = T)
})
#' cofirm to delete the import files
#' depends on: 
#'   input$confirm_delete_import: user confirms to delete the import files
observeEvent(input$confirm_delete_import,{
  if(isTRUE(input$confirm_delete_import)){
    file.remove(list.files(path = "data_import/processed_data/",pattern = input$Import_Files, full.names = T))
  }
}
)












#######################################################################################
# wortschatz Import  #
#######################################################################################
output$UI_Import_Wortschatz<-renderUI({
  return(tagList(
    tags$div(icon("info"))%>%
      bs_embed_popover(
        title ="You can download Wortschatz datasets from https://wortschatz.uni-leipzig.de/de/download/German#deu_news_2020", placement = "right"
      ),
    fileInput(inputId = "Import_Wortschatz_new",label = "Upload new Wortschatz dataset",multiple = F,accept = ".tar.gz",width = "50%"),
    tags$br(),
    tags$hr(),
    uiOutput(outputId = "Import_Wortschatz_Dataset_Information"),
    tags$br(),
    uiOutput(outputId = "Import_Wortschatz_Transfer_to_CSV_UI"),
    tags$br(),
    uiOutput(outputId = "Import_Wortschatz_Dataset_ready_UI")
    
    
  )
  )
})

output$Import_Wortschatz_Dataset_ready_UI<-renderUI({
  validate(
    need(!is.null(values$Import_Wortschatz_sentences),FALSE)
  )
  return(tagList(
    tags$h5("Number of Documents/Sentences:"),
    tags$span(nrow(values$Import_Wortschatz_sentences)),
    tags$br(),
    shiny::actionButton(inputId = "Import_Wortschatz_Dataset_ready_more_details",label = "More Details",icon = icon("search"),styleclass = "default"),
    tags$hr(),
    textInput(inputId="Import_Wortschatz_Dataset_ready_csv_name",label="CSV Name:",value=stringr::str_replace(string = input$Import_Wortschatz_new$name,pattern = ".tar.gz",replacement = ""),width="30%"),
    shiny::actionButton(inputId = "Import_Wortschatz_Dataset_ready_csv_save",label = "Save CSV File",styleclass = "primary",icon = icon("save"))
  ))
})






observeEvent(input$Import_Wortschatz_Dataset_ready_more_details,{
  showModal(
    modalDialog(title = "Excerpt of prepared Dataset",easyClose = T,size = "l",
                DT::dataTableOutput(outputId = "Import_Wortschatz_Dataset_ready_table")        
    )
  )
})


output$Import_Wortschatz_Dataset_ready_table<-DT::renderDataTable({
  data<-values$Import_Wortschatz_sentences
  data<-data[order(as.numeric(data[,1])),]
  data<-data[1:min(nrow(data),5),]
  data[,2]<-paste0(substr(data[,2],1,100),"...")
  datatable(data,rownames=F)
})


output$Import_Wortschatz_Transfer_to_CSV_UI<-renderUI({
  validate(
    need(!is.null(input$Import_Wortschatz_new),message=F)
  )
  tagList(
    checkboxInput(inputId="Input_Wortschatz_recreate_documents",label="Recreate Documents using Bag of Sentences Approach",value=T,width="27%")%>%
      shinyInput_label_embed(
        shiny_iconlink() %>%
          bs_embed_popover(
            title = "Sentences inside the Wortschatz-datasets are shuffeld due to copyright reasons. Therefor documents can only be reconstrcuted in a bag of sentences approach.", placement = "right"
          )
      ),
    actionButton(inputId = "Import_Wortschatz_Transfer_to_CSV",label = "Prepare this Wortschatz-dataset",size = "large",styleclass = "primary")
  )
})

observeEvent(input$Import_Wortschatz_new,ignoreInit = T,{
  validate(
    need(
      !is.null(input$Import_Wortschatz_new),message=F
    )
  )
  values$Import_Wortschatz_sentences<-NULL
  file.remove(list.files(path = "collections/tmp/Wortschatz/",full.names =  T,recursive = T,include.dirs = T))
  file.copy(from = input$Import_Wortschatz_new$datapath,to = paste0("collections/tmp/Wortschatz/",input$Import_Wortschatz_new$name))
  values$Wortschatz_current_path<-paste0("collections/tmp/Wortschatz/",input$Import_Wortschatz_new$name)
})  


observeEvent(input$Import_Wortschatz_Dataset_ready_csv_save,{
  file_already_exisiting<-file.exists(paste0("data_import/unprocessed_data/",input$Import_Wortschatz_Dataset_ready_csv_name,".csv"))
  if(file_already_exisiting==FALSE){
    data<-values$Import_Wortschatz_sentences
    #file.remove(list.files(path = "collections/tmp/Wortschatz/",full.names =  T,recursive = T,include.dirs = T))
    write.csv(x = data,file = paste0("data_import/unprocessed_data/",input$Import_Wortschatz_Dataset_ready_csv_name,".csv"),row.names = F)
    values$invalidate_csv_files<-runif(1,0,1)
    shinyWidgets::sendSweetAlert(session=session,title = "New CSV File created",text = HTML(paste0("Please go to CSV Input Mode, select the created File:<b>",
                                                                                                   stringr::str_replace(string = input$Import_Wortschatz_new$name,pattern = ".tar.gz",replacement = "")," </b> and follow the regular input steps.")),type = "success",html = T)
    
  }
  else{
    shinyWidgets::confirmSweetAlert(session = session,inputId = "Import_Wortschatz_Confirm_Overwrite",type = "warning",
                                    title = "A file with the specified name already exists",text = "Are you sure you want to overwrite the exisiting file?",danger_mode = T,btn_labels = c("Change Name","Overwrite"))
  }
})

observeEvent(input$Import_Wortschatz_Confirm_Overwrite,{
  if(isTRUE(input$Import_Wortschatz_Confirm_Overwrite)){
    data<-values$Import_Wortschatz_sentences
    #file.remove(list.files(path = "collections/tmp/Wortschatz/",full.names =  T,recursive = T,include.dirs = T))
    write.csv(x = data,file = paste0("data_import/unprocessed_data/",input$Import_Wortschatz_Dataset_ready_csv_name,".csv"),row.names = F)
    values$invalidate_csv_files<-runif(1,0,1)
    shinyWidgets::sendSweetAlert(session=session,title = "New CSV File created",text = HTML(paste0("Please go to CSV Input Mode, select the created File:<b>",stringr::str_replace(string = input$Import_Wortschatz_new$name,pattern = ".tar.gz",replacement = ""),
                                                                                                   " </b> and follow the regular input steps.")),type = "success",html = T)
  }
  else{
    shinyWidgets::closeSweetAlert(session = session)    
  }
})


observeEvent(input$Import_Wortschatz_Transfer_to_CSV,{
  validate(need(
    !is.null(values$Wortschatz_current_path),message="You need to upload a Wortschatz dataset before transfering it into CSV Format"
  ))
  withProgress(value = 0,message = "Unzipping Wortschatz Dataset" ,expr = {
    path<-values$Wortschatz_current_path
    untar(tarfile = path,exdir = "collections/tmp/Wortschatz")
    
    incProgress(amount = 0.1,message = "Finished Unzipping")
    file_name<-stringr::str_replace(string = input$Import_Wortschatz_new$name,pattern = ".tar.gz",replacement = "")
    all_files<-list.files(path = "collections/tmp/Wortschatz",full.names = T,recursive = T)
    
    sentences_path<-all_files[grepl(pattern = "-sentences.txt",x = all_files)]
    sources_path<-all_files[grepl(pattern = "-sources.txt",x = all_files)]
    inv_source_path<-all_files[grepl(pattern = "-inv_so.txt",x = all_files)]
    
    incProgress(amount = 0.2,message = "Importing Sentences.txt")
    sentences<-readLines(con = sentences_path)
    sentences<-stringr::str_split(sentences,"\t",simplify = T)
    
    incProgress(amount = 0.2,message = "Importing Sources.txt")
    sources<-readLines(con = sources_path)
    sources<-stringr::str_split(sources,"\t",simplify = T)
    
    incProgress(amount = 0.2,message = "Importing inv_so.txt")
    inv_sources<-readLines(con = inv_source_path)
    inv_sources<-stringr::str_split(inv_sources,"\t",simplify = T)
    
    incProgress(amount = 0.1,message = "Merge Sentences and Sources")
    inv_sources<-readLines(con = inv_source_path)
    inv_sources<-stringr::str_split(inv_sources,"\t",simplify = T)
    
    sentences<-merge(x = sentences,y = inv_sources,by.x="V1",by.y="V2")
    sentences<-merge(x=sentences,y=sources,by.x="V1.y",by.y="V1")
    
    sentences<-sentences[,-2]
    colnames(sentences)<-c("ID_Source","Text","Source","Date")
    sentences<-data.frame(sentences,stringsAsFactors = F)
    
    if(input$Input_Wortschatz_recreate_documents==TRUE){
      incProgress(amount = 0.1,message = "Recreate Documents")
      sentences<-cbind(aggregate(sentences$Text,by = sentences['ID_Source'],paste,collapse=" "),aggregate(sentences$Source,by = sentences['ID_Source'],first)[,2],aggregate(sentences$Date,by = sentences['ID_Source'],first)[,2])
      colnames(sentences)<-c("ID_Source","Text","Source","Date")
      values$Import_Wortschatz_sentences<-sentences
      incProgress(amount = 0.1,message = "Finished")
    }
    else{
      incProgress(amount = 0.2,message = "Finished")
      values$Import_Wortschatz_sentences<-sentences
    }
    shinyWidgets::sendSweetAlert(session=session,title = "Wortschatz-dataset ready",text = HTML(paste0("The Wortschatz-dataset:<b> ",file_name,"</b> is ready to be tranfered to a CSV.")),type = "success",html = T)
  })
})



output$Import_Wortschatz_Dataset_Information<-renderUI({
  validate(
    need(!is.null(input$Import_Wortschatz_new),message=F)
  )
  tagList(tags$h4("Filename:"),
          tags$div(input$Import_Wortschatz_new$name),
          tags$h4("Filesize:"),
          tags$div(paste0(as.numeric(input$Import_Wortschatz_new$size/1000000)," MB")))
})






########################################################################
# OHD Import #
########################################################################

output$UI_Import_OHD<-renderUI({
  return(tagList(tags$div(style="height:79vh; overflow-y:auto;",
                          fluidRow(style="margin-left:0px;margin-right:0px",
                                   uiOutput(outputId = "Import_ohd_avail_files")
                          ),
                          fluidRow(style="margin-left:0px;margin-right:0px",
                                   column(4,
                                          fileInput(inputId = "Import_ohd_new",label = "Upload new OHD datasets",multiple = F,accept = "csv|zip")
                                   ),
                                   column(4,offset = 3,
                                          fileInput(inputId = "Import_ohd_new_meta",label = "Upload new OHD Metadata",multiple = F,accept = "csv")
                                   )
                          ),
                          tags$hr(),
                          conditionalPanel(condition='input.Import_ohd_files!= null | input.Import_ohd_files_multiple!= null',
                                           withBusyIndicatorUI(
                                             shinyBS::bsButton(inputId = "Import_load_ohd",label = "use selected CSV/JSON",icon = icon("upload"),style = "info")
                                           ),            
                                           conditionalPanel(condition='output.data_load_ohd_success==true',
                                                            fluidRow(style="margin-left:0px;margin-right:0px",
                                                                     column(2,
                                                                            shinyBS::bsButton(inputId = "Import_check_ohd",label = "Check imported data",icon = icon("search"),style = "primary")
                                                                     ),
                                                                     column(1,
                                                                            textInput(inputId="Import_ohd_corpus_name","Dataset Abbreviation")
                                                                     ),
                                                                     column(1,
                                                                            switchInput(inputId = "Import_ohd_add_meta_to_data",label = "Add Metadata",value = F,onStatus = "success",offStatus = "primary")
                                                                     ),
                                                                     column(8,
                                                                            conditionalPanel(condition='input.Import_ohd_add_meta_to_data==true',
                                                                                             uiOutput(outputId = "Import_ohd_meta_columns_UI")
                                                                            )
                                                                     ),
                                                            ),
                                                            fluidRow(style="margin-left:0px;margin-right:0px",
                                                                     shinyBS::bsButton(inputId = "Import_start_ohd_mapping",label = "Apply Standard OHD Mapping",icon = icon("play"),style="info")
                                                                     
                                                            ),
                                                            tabsetPanel(id = "Resulting Mappings",
                                                                        tabPanel(title = "iLCM Document Format",
                                                                                 tags$div(style = 'overflow-x: auto',
                                                                                          DT::dataTableOutput(outputId = "Import_ohd_data_documents_table")
                                                                                 )
                                                                        ),
                                                                        tabPanel(title="Interview Data format",
                                                                                 tags$div(style = 'overflow-x: auto',
                                                                                          DT::dataTableOutput(outputId = "Import_ohd_data_interview_table")
                                                                                 )
                                                                        )
                                                            ),
                                                            tags$hr(),
                                                            uiOutput("Import_ohd_start_import_UI")
                                           )
                          )
                          
                          
  )
  )
  )
})


output$Import_ohd_start_import_UI<-renderUI({
  validate(
    need(!is.null( values$import_ohd_documents_after_mapping),message=F)
  )
  shinyBS::bsButton(inputId = "Import_ohd_start_import",label = "Start Import to iLCM",icon = icon("run"),style = "success")
})


observeEvent(input$Import_ohd_start_import,{
  data_documents <- values$import_ohd_documents_after_mapping
  data_interviews <- values$import_ohd_interview_after_mapping
  meta_metadata <- values$import_ohd_meta_meta_after_mapping
  # do something
  if(data_documents$dataset[1]==""){
    shinyWidgets::sendSweetAlert(session=session,title = "No Dataset Abbreviation",text = "Please use an existing Dataset you want to add these interviews to or specify a new one.",type = "warning")
  }
  else{
    if(stringr::str_detect(as.character(data_documents$dataset[1]),pattern = "_")){
      shinyWidgets::sendSweetAlert(session=session,title = "'_' not allowed",text = "Please specify a abbreviation without using '_'",type = "error")
    }
    else{
      slow_mode<-F
      parameters<-list(data=data_documents,db=TRUE,lang="de","%Y-%m-%d",meta_metadata,slow_mode=slow_mode,data_interviews,is_interview=T)
      #create process ID
      ID<-get_task_id_counter()+1
      set_task_id_counter(ID)
      #save metadata for process
      process_info<-list(ID,paste("New Data - ",data_documents$dataset[1],sep=""),"Create import interview files",as.character(Sys.time()))
      #save logfile path
      logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
      #create logfile
      write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
      #save data needed in script execution 
      save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
      #start script
      system(paste('Rscript collections/scripts/Import_Script.R','&'))
      #show modal when process is started
      shinyWidgets::sendSweetAlert(session=session,title = "Started Import Script",type = "success")
      
      
    }
  }
  
})





output$Import_ohd_meta_columns_UI<-renderUI({
  validate(
    need(input$Import_ohd_meta_file!="",message="Please specify a Meta File")
  )
  meta <- readr::read_delim(file = paste0("data_import/ohd_meta_files/",input$Import_ohd_meta_file),
                            delim = "\t", na = character() )
  values$import_ohd_meta <- meta
  checkboxGroupButtons(inputId = "Import_ohd_meta_columns",label = "Which meta fields to import?",selected = "",
                       choices = setdiff(colnames(meta),"Interview-ID"),direction = "horizontal",checkIcon = list(yes=icon("check")),individual = T)
})





#' show import of csv files
#' depends on:
#'   values$invalidate_csv_files: check if csv-file is invalide
output$Import_ohd_avail_files<-renderUI({
  values$invalidate_ohd_files
  validate(
    need(length(list.files("data_import/ohd/",pattern = ".csv|.json"))>0,message="No CSV/Zip-Files found in directory: data_import/ohd")
  )
  return(
    tagList(
      column(3,
             shinyWidgets::prettyRadioButtons(inputId = "Import_ohd_files",label = "Available OHD Single Files",
                                              choices = stringr::str_replace_all(string = list.files("data_import/ohd/",pattern = ".csv|.json"),pattern = ".txt",replacement = ""),
                                              fill=F,animation = "pulse",selected = "")
      ),
      column(3,
             shinyWidgets::prettyRadioButtons(inputId = "Import_ohd_files_multiple",label = "Available OHD Multiple Files",
                                              choices = setdiff(list.dirs("data_import/ohd/",full.names = F),""),
                                              fill=F,animation = "pulse",selected = "")     
      ),
      column(offset = 1,2,
             shinyWidgets::prettyRadioButtons(inputId = "Import_ohd_meta_file",label = "Available OHD Metadata Files",
                                              choices = stringr::str_replace_all(string = list.files("data_import/ohd_meta_files/",pattern = ".csv"),pattern = ".txt",replacement = ""),
                                              fill=F,animation = "pulse")  
      ),
      column(2,
             conditionalPanel(condition='input.Import_ohd_meta_file!=""',
                              bsButton(inputId = "Import_ohd_meta_show_details",label = "Check Meta Table",icon = icon("search"),style = "primary")
             )
      )
    )
  )
})

observeEvent(input$Import_ohd_meta_show_details,{
  validate(
    need(input$Import_ohd_meta_file!="",message=F)
  )
  values$import_ohd_data_meta_show <- readr::read_delim(file = paste0("data_import/ohd_meta_files/",input$Import_ohd_meta_file),
                                                        delim = "\t", na = character() )
  showModal(
    modalDialog(title = "Metadata Details",size = "l",
                tags$div(style="overflow-x: auto;",
                         DT::dataTableOutput(outputId = "Import_ohd_meta_details_table")
                )
    )
  )
})

output$Import_ohd_meta_details_table<-DT::renderDataTable({
  data <- values$import_ohd_data_meta_show
  datatable(data, options=list(pageLength=5,lengthMenu = c(5,10,25)))
})


#' oberve event of importing a new csv-file
#' depends on:
#'   input$Import_ohd_new: new csv-file import
observeEvent(input$Import_ohd_new,ignoreInit = T,{
  validate(
    need(
      !is.null(input$Import_ohd_new),message=F
    )
  )
  print(input$Import_ohd_new)
  if(file.exists(paste0("data_import/ohd/",input$Import_ohd_new$name))){
    shinyWidgets::sendSweetAlert(session=session,title = "Filename already used",text = "Please rename your file and then try to upload it again.",type = "warning")
  }
  else{
    if(grepl(x = input$Import_ohd_new$name,pattern = ".zip$")){
      if(dir.exists(stringr::str_remove(paste0("data_import/ohd/",input$Import_ohd_new$name),".zip"))){
        shinyWidgets::sendSweetAlert(session=session,title = "Zip Name already present",text = "Please rename your Zip File before Uploading",type = "warning")
      }else{
        unzip(zipfile = input$Import_ohd_new$datapath,exdir = stringr::str_remove(paste0("data_import/ohd/",input$Import_ohd_new$name),".zip"))
        values$invalidate_ohd_files<-runif(1,0,1)        
        shinyWidgets::sendSweetAlert(session=session,title = "Files added",text = "The Zip File was unpacked. You can now select the directory in the multiple files tab.",type = "success")
      }
    }
    else{
      file.copy(from = input$Import_ohd_new$datapath,to = paste0("data_import/ohd/",input$Import_ohd_new$name))
      values$invalidate_ohd_files<-runif(1,0,1)
      shinyWidgets::sendSweetAlert(session=session,title = "File added",text = "You can now select it in the list of files above",type = "success")
    }
  }
})


observeEvent(input$Import_ohd_new_meta,ignoreInit = T,{
  validate(
    need(
      !is.null(input$Import_ohd_new_meta),message=F
    )
  )
  if(file.exists(paste0("data_import/ohd_meta_files/",input$Import_ohd_new_meta$name))){
    shinyWidgets::sendSweetAlert(session=session,title = "Filename already used",text = "Please rename your file and then try to upload it again.",type = "warning")
  }
  else{
    file.copy(from = input$Import_ohd_new_meta$datapath,to = paste0("data_import/ohd_meta_files/",input$Import_ohd_new_meta$name))
    values$invalidate_ohd_files<-runif(1,0,1)
    shinyWidgets::sendSweetAlert(session=session,title = "Meta File added",text = "You can now select it in the list of files above",type = "success")
  }
  
})


#' observe loading a new csv-file
#' deoends on:
#'   input$Import_load_ohd: load csv-file
#'   values$header_ohd: extracted csv header
#'   values$data_ohd: all csv data
observeEvent(input$Import_load_ohd,{
  if(is.null(input$Import_ohd_files)){
    withBusyIndicatorServer("Import_load_ohd", {
      files = list.files(paste0("data_import/ohd/",input$Import_ohd_files_multiple),full.names = T)
      data=NULL
      for(i in 1:length(files)){
        data_i <- readr::read_delim(file = files[i],
                                    delim = "\t", na = character() )
        file_names <- rep(list.files(paste0("data_import/ohd/",input$Import_ohd_files_multiple),full.names = F)[i],nrow(data_i))
        data_i <- cbind(file_names,data_i)
        data <- rbind(data,data_i)
      }
      values$data_ohd <- data
      values$header_ohd<-c(colnames(values$data_ohd))
      values$import_ohd_documents_after_mapping <- NULL
      values$import_ohd_interview_after_mapping <- NULL
      values$import_ohd_meta_meta_after_mapping <- NULL
      values$data_load_ohd_success<-TRUE
    })
  }
  else{
    withBusyIndicatorServer("Import_load_ohd", {
      if(grepl(pattern = ".csv$",x = input$Import_ohd_files)){
        data_i <- readr::read_delim(file = paste0("data_import/ohd/",input$Import_ohd_files),
                                    delim = "\t", na = character() )
        file_names <- rep(input$Import_ohd_files,nrow(data_i))
        data_i <- cbind(file_names,data_i)
        values$data_ohd<-data_i
      }
      if(grepl(pattern = ".json$",x = input$Import_ohd_files)){
        # TODO
      }
      colnames(values$data_ohd)<-stringr::str_replace_all(string = colnames(values$data_ohd),pattern = "\\.",replacement = " ")
      if(dim(values$data_ohd)[1]<2 | dim(values$data_ohd)[2]<2){
        text<-paste0("The resulting input dimesions are: ",dim(values$data_ohd)[1]," x ",dim(values$data_ohd)[2],". Something went wrong during the input. Make sure to specify the csv input parameters correct.")
        shinyWidgets::sendSweetAlert(session=session,title = "Input failed!",text = text,type = "error")
      }
      else{
        values$header_ohd<-c(colnames(values$data_ohd))
        values$data_load_ohd_success<-TRUE
      }
    })
  }
})




#' return data if loading the ohd data was successful
#' depends on:
#'   values$data_load_ohd_success: communicate wether the loading process was succesfull
output$data_load_ohd_success<-reactive({
  print("update ohd success")
  values$data_load_ohd_success
})
outputOptions(output, "data_load_ohd_success", suspendWhenHidden = FALSE)

#' check csv-file 
#' depends on:
#'   input$Import_check_csv: variable to confirm that a csv-file needs to be checked
observeEvent(input$Import_check_ohd,{
  showModal(
    modalDialog(
      size = "l",
      title = "Imported OHD Data",easyClose = T,
      tags$div(style="overflow-x:auto; height:70vh;",
               dataTableOutput(outputId = "Import_head_ohd")
      ) 
    )
  )
})


#' render data table to show csv file with header of csv
#' depends on:
#'   values$data_csv: values from csv data
output$Import_head_ohd<-DT::renderDataTable({
  data<-values$data_ohd
  data<-data[1:min(5,dim(data)[1]),]
  data<-t(apply(data,1,FUN=function(i){apply(as.matrix(i),MARGIN = 1,FUN = function(x){if(is.na(x)){return(x)}else{if(nchar(x)>100){return(substr(x,1,100))}else{return(x)}}})}))
  datatable(data = data,options = list(lengthChange = FALSE,dom="t"),width = "100%")
})




observeEvent(input$Import_start_ohd_mapping,{
  validate(
    need(!is.null(values$data_ohd),message=F)
  )
  data_interview <- values$data_ohd
  data_documents <- data_interview[,c("file_names","Transkript")]
  data_documents <- aggregate(data=data_documents,Transkript ~ file_names, paste0, collapse="_-_-_-_-_-_-_-_-_")
  
  if(input$Import_ohd_add_meta_to_data==TRUE){
    if(length(input$Import_ohd_meta_file)!=1){
      shinyWidgets::sendSweetAlert(session=session,title = "No Metadata found",text = "Please choose a Metadata file or upload a new one.",type = "warning")
      return(NULL)
    }
    if(length(input$Import_ohd_meta_columns)<1){
      shinyWidgets::sendSweetAlert(session=session,title = "No Metadata Columns selected",text = "Please add at least one Metadata Column you want to import.",type = "warning")
      return(NULL)
    }
    else{
      meta <- readr::read_delim(file = paste0("data_import/ohd_meta_files/",input$Import_ohd_meta_file),
                                delim = "\t", na = character() )
      interview_id <- substr(data_documents$file_names,1,7)
      data_documents<-cbind(interview_id,data_documents)
      #merge
      data_documents <- merge(x = data_documents,y = meta,by.x = "interview_id",by.y = "Interview-ID",all.x=T)
      # metadata to use
      data_documents <- data_documents[,c("interview_id","file_names","Transkript",      input$Import_ohd_meta_columns)]
      # id doc
      offset=NA
      if(input$Import_ohd_corpus_name!=""){
        mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
        #print(paste0("SELECT MAX(id_doc) FROM ilcm.documents where dataset=",input$Import_mtf_dataset,";"))
        offset<-RMariaDB::dbGetQuery(mydb,paste0("SELECT MAX(id_doc) FROM ilcm.documents where dataset='",input$Import_ohd_corpus_name,"';"))[1,1]
      }
      if(is.na(offset)){
        offset=0
      }
      id_doc<-(offset+1):(offset+dim(data_documents)[1])
      data_documents<-cbind(id_doc,data_documents)
      # dataset
      data_documents <- cbind(rep(input$Import_ohd_corpus_name,nrow(data_documents)),data_documents)
      colnames(data_documents)[1] <- "dataset"
      is_interview <- rep(1,nrow(data_documents))
      data_documents <- cbind(data_documents,is_interview)
      ###############################
      # create standard data format #
      ###############################
      data_clean <- data_documents[,1:3]
      data_clean <- data.frame(data_clean, title = paste0("Interview: ",substr(data_documents$file_names,1,7)),
                               body = data_documents$Transkript,
                               date = as.character(Sys.Date()),
                               token = unlist(lapply(X = data_documents$Transkript,FUN = function(x){
                                 length(stringr::str_split(string = x,pattern = " ",simplify = T))}
                               )),
                               language = "de_core_news_sm(3.2.0)",
                               mde1 = "",
                               mde2 = "",
                               mde3 = "",
                               mde4 = "",
                               mde5 = "",
                               mde6 = "",
                               mde7 = "",
                               mde8 = "",
                               mde9 = "",
                               is_interview = 1)
      data_clean[,9:(8+length(input$Import_ohd_meta_columns))] <- data_documents[,6:(ncol(data_documents)-1)]
      #colnames(data_clean)[9:(8+length(input$Import_ohd_meta_columns))]<-colnames(data_documents[,6:(ncol(data_documents)-1)])
      data_meta_meta <- data.frame(dataset=input$Import_ohd_corpus_name)
      a = lapply(1:length(input$Import_ohd_meta_columns),function(x){
        y = data.frame(input$Import_ohd_meta_columns[x])
        colnames(y) = paste0("mde",x)
        return(y)
      })
      data_meta_meta <- data.frame(data_meta_meta, t(data.frame(unlist(a))))
      rownames(data_meta_meta) <- NULL
      values$import_ohd_meta_meta_after_mapping <- data_meta_meta
      data_clean$language <- "de_core_news_sm(3.2.0)"
      values$import_ohd_documents_after_mapping <- data_clean
    }
  }
  else{
    # id doc
    offset=NA
    if(input$Import_ohd_corpus_name!=""){
      mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
      #print(paste0("SELECT MAX(id_doc) FROM ilcm.documents where dataset=",input$Import_mtf_dataset,";"))
      offset<-RMariaDB::dbGetQuery(mydb,paste0("SELECT MAX(id_doc) FROM ilcm.documents where dataset='",input$Import_ohd_corpus_name,"';"))[1,1]
    }
    if(is.na(offset)){
      offset=0
    }
    # interview id
    interview_id <- substr(data_documents$file_names,1,7)
    data_documents<-cbind(interview_id,data_documents)
    # id doc
    id_doc<-(offset+1):(offset+dim(data_documents)[1])
    data_documents<-cbind(id_doc,data_documents)
    # dataset
    data_documents <- cbind(rep(input$Import_ohd_corpus_name,nrow(data_documents)),data_documents)
    colnames(data_documents)[1] <- "dataset"
    is_interview <- rep(1,nrow(data_documents))
    data_documents <- cbind(data_documents,is_interview)
    ###############################
    # create standard data format #
    ###############################
    data_clean <- data_documents[,1:3]
    data_clean <- data.frame(data_clean, title = paste0("Interview: ",substr(data_documents$file_names,1,7)),
                             body = data_documents$Transkript,
                             date = as.cahracter(Sys.Date()),
                             token = unlist(lapply(X = data_documents$Transkript,FUN = function(x){
                               length(stringr::str_split(string = x,pattern = " ",simplify = T))}
                             )),
                             language = "de_core_news_sm(3.2.0)",
                             mde1 = "",
                             mde2 = "",
                             mde3 = "",
                             mde4 = "",
                             mde5 = "",
                             mde6 = "",
                             mde7 = "",
                             mde8 = "",
                             mde9 = "",
                             is_interview = 1)
    data_clean$language <- "de_core_news_sm(3.2.0)"
    values$import_ohd_documents_after_mapping <- data_clean
    data_meta_meta <- data.frame(dataset=input$Import_ohd_corpus_name)
    values$import_ohd_meta_meta_after_mapping <- data_meta_meta
  }
  # add interview ids and counter for row per interview
  interview_id <- substr(data_interview$file_names,1,7)
  data_interview <- cbind(interview_id, data_interview)
  interview_row_id <- NULL
  for(i in 1:length(unique(data_interview$interview_id))){
    interview_row_id <- c(interview_row_id, 1:length(which(data_interview$interview_id==unique(data_interview$interview_id)[i])))
  }
  data_interview <- cbind(interview_row_id, data_interview)
  data_interview <- cbind(rep(input$Import_ohd_corpus_name,nrow(data_interview)),data_interview)
  colnames(data_interview)[1] <- "dataset"
  id_doc_merge_table <- unique(data_documents[,c("file_names","id_doc")])
  data_interview <- merge(x=data_interview,y=id_doc_merge_table,by="file_names",all.x =T)
  values$import_ohd_interview_after_mapping <- data_interview
}
)


output$Import_ohd_data_interview_table<-DT::renderDataTable({
  validate(
    need(
      !is.null(values$import_ohd_interview_after_mapping),message=F
    )
  )
  data = values$import_ohd_interview_after_mapping[1:5,]
  datatable(data)
})


output$Import_ohd_data_documents_table <- DT::renderDataTable({
  validate(
    need(
      !is.null(values$import_ohd_documents_after_mapping),message=F
    )
  )
  data = values$import_ohd_documents_after_mapping[1:min(5,nrow(values$import_ohd_documents_after_mapping)),]
  data$body<-paste0(substr(data$body,1,50),"...")
  datatable(data)
})

#ensure not more than 9 metadata fields are selected
observe({
  if(length(input$Import_ohd_meta_columns) > 9){
    print("update selected")
    shinyWidgets::updateCheckboxGroupButtons(session, "Import_ohd_meta_columns", selected= tail(input$Import_ohd_meta_columns,9))
  }
})



observeEvent(input$Import_ohd_files_multiple,{
  if(length(input$Import_ohd_files_multiple)>0){
    updatePrettyRadioButtons(session = session,inputId = "Import_ohd_files",selected = "",
                             choices = stringr::str_replace_all(string = list.files("data_import/ohd/",pattern = ".csv|.json"),pattern = ".txt",replacement = ""),
                             prettyOptions = list(fill=F,animation="pulse",shape="round")) 
  }
})
observeEvent(input$Import_ohd_files,{
  if(length(input$Import_ohd_files)>0){
    updatePrettyRadioButtons(session = session,inputId = "Import_ohd_files_multiple",selected = "",
                             choices = setdiff(list.dirs("data_import/ohd/",full.names = F),""),
                             prettyOptions = list(fill=F,animation="pulse",shape="round")) 
  }
})