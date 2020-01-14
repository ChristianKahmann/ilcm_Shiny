tabPanel("Importer",
         tabBox(
           id = "tabBox_Import",
           width = 12,
           tabPanel(
             "Preprocess Data",
             tags$div(style="height:79vh; overflow-y:auto;",
                      shinyWidgets::prettyRadioButtons(inputId = "Import_Type",label = "Which type of Import?",
                                                       choices = c("csv","multiple text files"),
                                                       fill=F,animation = "pulse",selected = character(0)),
                      conditionalPanel(condition='input.Import_Type=="csv"',
                                       uiOutput(outputId = "UI_Import_csv_file"),
                                       fileInput(inputId = "Import_csv_new",label = "Upload new CSV",multiple = F,accept = ".csv",width = "50%"),
                                       conditionalPanel(condition='input.Import_csv_files!= null',
                                                        box(width=2,title = tags$div("CSV-Import Parameters",style="color:white;"),status = "primary",collapsible = T,solidHeader = T,
                                                            prettyCheckbox(inputId = "Import_load_csv_header",label = "header?",value = TRUE,status = "primary",shape = "curve"),
                                                            textInput(inputId = "import_load_csv_seperator",label = "seperator:",value = ",")
                                                        ),
                                                        withBusyIndicatorUI(
                                                          shinyBS::bsButton(inputId = "Import_load_csv",label = "use selected csv",icon = icon("upload"),style = "info")
                                                        ),
                                                        
                                                        conditionalPanel(condition='output.data_load_csv_success==true',
                                                                         #tags$img(src="success.svg"),
                                                                         #tags$br(),
                                                                         tags$br(),
                                                                         fluidRow(style="margin-left:0px;margin-right:0px",
                                                                                  shinyBS::bsButton(inputId = "Import_check_csv",label = "check csv",icon = icon("search"),style = "primary"),
                                                                                  shinyBS::bsButton(inputId = "Import_start_mapping",label = "start mapping",icon = icon("play"),style="info")
                                                                         ),
                                                                         conditionalPanel(condition='output.start_mapping==true',
                                                                                          tags$br(),
                                                                                          box(title = tags$h3("Mapping",style="color:white;"),solidHeader = T,width=12,status = "primary",collapsible = T,
                                                                                              tags$div(style = 'overflow-x: auto',
                                                                                                       fluidRow(style="margin-left:0px; margin-right:0px;",
                                                                                                                column(1,offset=3,
                                                                                                                       textInput(inputId = "UI_Import_name_mde1",label = "Name of Metadata Field",value = "mde1")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       textInput(inputId = "UI_Import_name_mde2",label = "Name of Metadata Field",value = "mde2")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       textInput(inputId = "UI_Import_name_mde3",label = "Name of Metadata Field",value = "mde3")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       textInput(inputId = "UI_Import_name_mde4",label = "Name of Metadata Field",value = "mde4")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       textInput(inputId = "UI_Import_name_mde5",label = "Name of Metadata Field",value = "mde5")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       textInput(inputId = "UI_Import_name_mde6",label = "Name of Metadata Field",value = "mde6")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       textInput(inputId = "UI_Import_name_mde7",label = "Name of Metadata Field",value = "mde7")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       textInput(inputId = "UI_Import_name_mde8",label = "Name of Metadata Field",value = "mde8")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       textInput(inputId = "UI_Import_name_mde9",label = "Name of Metadata Field",value = "mde9")
                                                                                                                )
                                                                                                       ),
                                                                                                       fluidRow(style="margin-left:0px; margin-right:0px;",
                                                                                                                column(3,
                                                                                                                       column(3,
                                                                                                                              uiOutput(outputId = "UI_Import_csv_id_doc")
                                                                                                                       ),
                                                                                                                       column(3,
                                                                                                                              uiOutput(outputId = "UI_Import_csv_title")
                                                                                                                       ),
                                                                                                                       column(3,
                                                                                                                              uiOutput(outputId = "UI_Import_csv_date")
                                                                                                                       ),
                                                                                                                       column(3,
                                                                                                                              uiOutput(outputId = "UI_Import_csv_body")
                                                                                                                       )
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       uiOutput(outputId = "UI_Import_csv_mde1")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       uiOutput(outputId = "UI_Import_csv_mde2")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       uiOutput(outputId = "UI_Import_csv_mde3")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       uiOutput(outputId = "UI_Import_csv_mde4")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       uiOutput(outputId = "UI_Import_csv_mde5")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       uiOutput(outputId = "UI_Import_csv_mde6")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       uiOutput(outputId = "UI_Import_csv_mde7")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       uiOutput(outputId = "UI_Import_csv_mde8")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       uiOutput(outputId = "UI_Import_csv_mde9")
                                                                                                                )
                                                                                                       ),
                                                                                                       fluidRow(style="margin-left:0px; margin-right:0px;",
                                                                                                                column(3,
                                                                                                                       column(3,
                                                                                                                              shinyBS::bsButton("Import_script_id_doc",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                              tags$br(),
                                                                                                                              shinyBS::bsButton("Import_type_id_doc",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                       ),
                                                                                                                       column(3,
                                                                                                                              shinyBS::bsButton("Import_script_title",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                              tags$br(),
                                                                                                                              shinyBS::bsButton("Import_type_title",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                       ),
                                                                                                                       column(3,
                                                                                                                              shinyBS::bsButton("Import_script_date",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                              tags$br(),
                                                                                                                              shinyBS::bsButton("Import_type_date",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                              
                                                                                                                       ),
                                                                                                                       column(3,
                                                                                                                              shinyBS::bsButton("Import_script_body",label = "Script",style = "info",icon=icon("terminal"),block=T,title = "Use an R-Script for creating the metadata"),
                                                                                                                              tags$br(),
                                                                                                                              shinyBS::bsButton("Import_type_body",label = "Type",style="info",icon=icon("edit"),block=T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                       )
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde1",label = "Script",style = "info",icon=icon("terminal"),block=T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde1",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde2",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde2",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde3",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde3",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde4",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde4",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde5",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde5",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde6",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde6",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde7",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde7",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde8",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde8",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde9",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde9",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                )
                                                                                                       ),
                                                                                                       tags$br(),
                                                                                                       tags$br(),
                                                                                                       fluidRow(style="margin-left:0px; margin-right:0px;",
                                                                                                                column(1,
                                                                                                                       selectInput(inputId = "Import_csv_language",label = "Language",choices = stringr::str_split(
                                                                                                                         stringr::str_replace_all(
                                                                                                                           stringr::str_replace_all(string = system(command = "python -m spacy info",intern = T)[8],pattern = "Models           ",replacement = ""),
                                                                                                                           pattern=" ",replacement=""),
                                                                                                                         pattern=",",simplify = T)[1,],multiple = F,selected = character(0))
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       textInput(inputId = "Import_csv_dataset",label = "dataset abbreviation",placeholder = "e.g. GU for Guardian")
                                                                                                                ),
                                                                                                                column(1,  
                                                                                                                       selectizeInput(inputId = "Import_csv_date_format",label = "date format",choices = c("%Y-%m-%d","%d-%m-%Y"),multiple=F,options=list(create=T))
                                                                                                                ),
                                                                                                                column(9,
                                                                                                                       uiOutput("Import_csv_metadata_names_warning")
                                                                                                                )
                                                                                                                
                                                                                                       )
                                                                                              )
                                                                                          ),
                                                                                          tags$br(),
                                                                                          box(title = tags$h3("Resulting Metadata Import File",style="color:white;"),solidHeader = T,width=12,status = "primary",collapsible = T,
                                                                                              tags$div(style = 'overflow-x: auto',
                                                                                                       DT::dataTableOutput(outputId = "Import_csv_metadata")
                                                                                              ),
                                                                                              tags$hr()
                                                                                          ),
                                                                                          
                                                                                          shinyBS::bsButton("Import_csv_start_preprocess",label = "Start Preprocessing and save csv-files",style="info",icon=icon("tags")),
                                                                                          shinyBS::bsButton("Import_csv_start_preprocess_and_write",label = "Start Preprocessing and directly wirte to DB",style="info",icon=icon("upload"))
                                                                         )
                                                        )
                                                        
                                       )
                      ),
                      conditionalPanel(condition='input.Import_Type=="multiple text files"',
                                       uiOutput(outputId = "UI_Import_mtf_file"),
                                       fileInput(inputId = "Import_mtf_new",label = "Upload new text files",multiple = T,width = "50%"),
                                       conditionalPanel(condition='input.Import_mtf_files!= null',
                                                        #conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                        #                 p("Uploading..."),
                                                        #                 tags$img(src="busy.gif")),
                                                        withBusyIndicatorUI(
                                                          shinyBS::bsButton(inputId = "Import_load_mtf",label = "import directory",icon = icon("upload"),style = "info")
                                                        ),
                                                        conditionalPanel(condition='output.data_load_mtf_success==true',
                                                                         #tags$img(src="success.svg"),
                                                                         #tags$br(),
                                                                         fluidRow(style="margin-left:0px;margin-right:0px",
                                                                                  box(width=2,title = tags$div("Metadata CSV-Import Parameters",style="color:white;"),status = "primary",collapsible = T,solidHeader = T,
                                                                                      fileInput(inputId = "Import_mtf_metadata_csv", label = "Choose CSV File containing metadata",
                                                                                                accept = c(
                                                                                                  "text/csv",
                                                                                                  "text/comma-separated-values,text/plain",
                                                                                                  ".csv")
                                                                                      )%>%
                                                                                        shinyInput_label_embed(
                                                                                          icon("info") %>%
                                                                                            bs_embed_tooltip(title = "The files are imported in alphabetical order based on their filenames. Please make sure, that the order of the rows in the metadata csv corresponds to this chronology.")
                                                                                        ),
                                                                                      prettyCheckbox(inputId = "Import_mtf_metadata_csv_header",label = "header?",value = TRUE,status = "primary",shape = "curve"),
                                                                                      textInput(inputId = "import_load_mtf_seperator",label = "seperator:",value = ","),
                                                                                      textInput(inputId = "import_load_mtf_encoding",label = "encoding:",value = "UTF-8")
                                                                                  ),
                                                                                  shinyBS::bsButton(inputId = "Import_check_mtf",label = "check import",icon = icon("search"),style = "primary"),
                                                                                  shinyBS::bsButton(inputId = "Import_start_mapping_mtf",label = "start mapping",icon = icon("play"),style="info")
                                                                         ),
                                                                         conditionalPanel(condition='output.start_mapping_mtf==true',
                                                                                          tags$br(),
                                                                                          box(title = tags$h3("Mapping",style="color:white;"),solidHeader = T,width=12,status = "primary",collapsible = T,
                                                                                              tags$div(style = 'overflow-x: auto',
                                                                                                       fluidRow(style="margin-left:0px; margin-right:0px;",
                                                                                                                column(1,offset=3,
                                                                                                                       textInput(inputId = "UI_Import_name_mde1_mtf",label = "Name of Metadata Field",value = "mde1")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       textInput(inputId = "UI_Import_name_mde2_mtf",label = "Name of Metadata Field",value = "mde2")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       textInput(inputId = "UI_Import_name_mde3_mtf",label = "Name of Metadata Field",value = "mde3")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       textInput(inputId = "UI_Import_name_mde4_mtf",label = "Name of Metadata Field",value = "mde4")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       textInput(inputId = "UI_Import_name_mde5_mtf",label = "Name of Metadata Field",value = "mde5")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       textInput(inputId = "UI_Import_name_mde6_mtf",label = "Name of Metadata Field",value = "mde6")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       textInput(inputId = "UI_Import_name_mde7_mtf",label = "Name of Metadata Field",value = "mde7")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       textInput(inputId = "UI_Import_name_mde8_mtf",label = "Name of Metadata Field",value = "mde8")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       textInput(inputId = "UI_Import_name_mde9_mtf",label = "Name of Metadata Field",value = "mde9")
                                                                                                                )
                                                                                                       ),
                                                                                                       # column(3,tags$br()),
                                                                                                       fluidRow(style="margin-left:0px; margin-right:0px;",
                                                                                                                column(3,
                                                                                                                       column(3,
                                                                                                                              uiOutput(outputId = "UI_Import_mtf_id_doc")
                                                                                                                       ),
                                                                                                                       column(3,
                                                                                                                              uiOutput(outputId = "UI_Import_mtf_title")
                                                                                                                       ),
                                                                                                                       column(3,
                                                                                                                              uiOutput(outputId = "UI_Import_mtf_date")
                                                                                                                       ),
                                                                                                                       column(3,
                                                                                                                              uiOutput(outputId = "UI_Import_mtf_body")
                                                                                                                       )
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       uiOutput(outputId = "UI_Import_mtf_mde1")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       uiOutput(outputId = "UI_Import_mtf_mde2")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       uiOutput(outputId = "UI_Import_mtf_mde3")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       uiOutput(outputId = "UI_Import_mtf_mde4")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       uiOutput(outputId = "UI_Import_mtf_mde5")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       uiOutput(outputId = "UI_Import_mtf_mde6")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       uiOutput(outputId = "UI_Import_mtf_mde7")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       uiOutput(outputId = "UI_Import_mtf_mde8")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       uiOutput(outputId = "UI_Import_mtf_mde9")
                                                                                                                )
                                                                                                       ),
                                                                                                       fluidRow(style="margin-left:0px; margin-right:0px;", 
                                                                                                                column(3,
                                                                                                                       column(3,
                                                                                                                              shinyBS::bsButton("Import_script_id_doc_mtf",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                              tags$br(),
                                                                                                                              shinyBS::bsButton("Import_type_id_doc_mtf",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                       ),
                                                                                                                       column(3,
                                                                                                                              shinyBS::bsButton("Import_script_title_mtf",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                              tags$br(),
                                                                                                                              shinyBS::bsButton("Import_type_title_mtf",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                       ),
                                                                                                                       column(3,
                                                                                                                              shinyBS::bsButton("Import_script_date_mtf",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                              tags$br(),
                                                                                                                              shinyBS::bsButton("Import_type_date_mtf",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                       ),
                                                                                                                       column(3,
                                                                                                                              shinyBS::bsButton("Import_script_body_mtf",label = "Script",style = "info",icon=icon("terminal"),block=T,title = "Use an R-Script for creating the metadata"),
                                                                                                                              tags$br(),
                                                                                                                              shinyBS::bsButton("Import_type_body_mtf",label = "Type",style="info",icon=icon("edit"),block=T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                       )
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde1_mtf",label = "Script",style = "info",icon=icon("terminal"),block=T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde1_mtf",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde2_mtf",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde2_mtf",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde3_mtf",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde3_mtf",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde4_mtf",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde4_mtf",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde5_mtf",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde5_mtf",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde6_mtf",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde6_mtf",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde7_mtf",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde7_mtf",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde8_mtf",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde8_mtf",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde9_mtf",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde9_mtf",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                )
                                                                                                       ),
                                                                                                       tags$br(),
                                                                                                       tags$br(),
                                                                                                       fluidRow(style="margin-left:0px; margin-right:0px;",
                                                                                                                column(1,
                                                                                                                       selectInput(inputId = "Import_mtf_language",label = "Language",choices =stringr::str_split(
                                                                                                                         stringr::str_replace_all(
                                                                                                                           stringr::str_replace_all(string = system(command = "python -m spacy info",intern = T)[8],pattern = "Models           ",replacement = ""),
                                                                                                                           pattern=" ",replacement=""),
                                                                                                                         pattern=",",simplify = T)[1,],multiple = F,selected = character(0))
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       textInput(inputId = "Import_mtf_dataset",label = "dataset abbreviation",placeholder = "e.g. GU for Guardian")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       selectizeInput(inputId = "Import_mtf_date_format",label = "date format",choices = c("%Y-%m-%d","%d-%m-%Y"),multiple=F,options=list(create=T))
                                                                                                                ),
                                                                                                                column(9,
                                                                                                                       uiOutput("Import_mtf_metadata_names_warning")
                                                                                                                )
                                                                                                                
                                                                                                       )
                                                                                              )
                                                                                          ),
                                                                                          tags$br(),
                                                                                          box(title = tags$h3("Resulting Metadata Import File",style="color:white;"),solidHeader = T,width=12,status = "primary",collapsible = T,
                                                                                              tags$div(style = 'overflow-x: auto',
                                                                                                       DT::dataTableOutput(outputId = "Import_mtf_metadata")%>% withSpinner(color="#0dc5c1")
                                                                                              ),
                                                                                              tags$hr()
                                                                                          ),
                                                                                          
                                                                                          shinyBS::bsButton("Import_mtf_start_preprocess",label = "Start Preprocessing and save csv-files",style="info",icon=icon("tags")),
                                                                                          shinyBS::bsButton("Import_mtf_start_preprocess_and_write",label = "Start Preprocessing and directly wirte to DB",style="info",icon=icon("upload"))
                                                                         )
                                                        )
                                       )
                      )
             )
           ),
           tabPanel(
             "Upload Data to DB and Solr",
             uiOutput("Import_Files_UI"),
             tags$br(),
             withBusyIndicatorUI(
               shinyBS::bsButton(
                 inputId = "Upload_Data",
                 label = "Upload selected data to DB",
                 icon = icon("database"),
                 style = "default"
               )
             ),
             withBusyIndicatorUI(
               shinyBS::bsButton(
                 inputId = "Import_to_solr",
                 label = "Import data to solr",
                 icon = icon("upload"),
                 style = "default"
               )
             ),
             shinyBS::bsButton(
               inputId = "Import_delete",
               label = "Delete",
               icon = icon("remove"),
               style = "danger"
             )
           )
         ))
