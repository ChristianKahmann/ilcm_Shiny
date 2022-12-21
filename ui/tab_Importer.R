#' panel to import data 
tabPanel("Importer",
         tabBox(
           id = "tabBox_Import",
           width = 12,
           tabPanel(
             "Preprocess Data",
             tags$div(style="height:79vh; overflow-y:auto;",
                      shinyWidgets::prettyRadioButtons(inputId = "Import_Type",label = "Which type of Import?",
                                                       choices = c("CSV/XLSX","multiple text files","Wortschatz"),
                                                       fill=F,animation = "pulse",selected = character(0)),
                      conditionalPanel(condition='input.Import_Type=="CSV/XLSX"',
                                       uiOutput(outputId = "UI_Import_csv_file"),
                                       fileInput(inputId = "Import_csv_new",label = "Upload new CSV",multiple = F,accept = c(".xlsx",".csv"),width = "50%"),
                                       conditionalPanel(condition='input.Import_csv_files!= null',
                                                        box(width=2,title = tags$div("CSV-Import Parameters",style="color:white;"),status = "primary",collapsible = T,solidHeader = T,
                                                            prettyCheckbox(inputId = "Import_load_csv_header",label = "header?",value = TRUE,status = "primary",shape = "curve"),
                                                            textInput(inputId = "import_load_csv_seperator",label = "seperator:",value = ",")
                                                        ),
                                                        withBusyIndicatorUI(
                                                          shinyBS::bsButton(inputId = "Import_load_csv",label = "use selected CSV/XLSX",icon = icon("upload"),style = "info")
                                                        ),
                                                        
                                                        conditionalPanel(condition='output.data_load_csv_success==true',
                                                                         #tags$img(src="success.svg"),
                                                                         #tags$br(),
                                                                         tags$br(),
                                                                         box(width=2,title = "Split Method",status = "primary",collapsible = T,
                                                                             selectInput(inputId = "Import_csv_split_method", "Method:",
                                                                                         choices=c("None", "Regular Expression", "Hard Split", "Script")
                                                                             )%>%
                                                                               shinyInput_label_embed(
                                                                                 icon("info") %>%
                                                                                   bs_embed_tooltip(title = "This Method will split the text of the imported files.")
                                                                               ),
                                                                             conditionalPanel(
                                                                               condition = "input.Import_csv_split_method != 'None'",
                                                                               uiOutput("UI_Import_csv_column_name")
                                                                             ),
                                                                             conditionalPanel(
                                                                               condition = "input.Import_csv_split_method == 'Regular Expression'",
                                                                               textInput(inputId = "Import_csv_split_method_regex",label = "Regular Expression:", value = "\\n\\n")
                                                                             ),
                                                                             conditionalPanel(
                                                                               condition = "input.Import_csv_split_method == 'Hard Split'",
                                                                               numericInput(inputId = "Import_csv_split_method_split_number",label = "Split after x Characters:", value = 2000, min = 1)
                                                                             ),
                                                                             conditionalPanel(
                                                                               condition = "input.Import_csv_split_method == 'Script'",
                                                                               shinyBS::bsButton("Import_script_split_csv",label = "Script",style = "info",icon=icon("terminal"),block=T,title = "Use an R-Script to split text in imported files")
                                                                             ),
                                                                             conditionalPanel(
                                                                               condition = "input.Import_csv_split_method != 'None'",
                                                                               tags$br(),
                                                                               actionButton("Import_csv_split_test_view", "Test Split", style = "info", block=T)
                                                                             )
                                                                         ),
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
                                                                                                                       column(4,
                                                                                                                              uiOutput(outputId = "UI_Import_csv_title")
                                                                                                                       ),
                                                                                                                       column(4,
                                                                                                                              uiOutput(outputId = "UI_Import_csv_date")
                                                                                                                       ),
                                                                                                                       column(4,
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
                                                                                                                       column(4,
                                                                                                                              shinyBS::bsButton("Import_script_title_csv",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                              tags$br(),
                                                                                                                              shinyBS::bsButton("Import_type_title_csv",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                       ),
                                                                                                                       column(4,
                                                                                                                              shinyBS::bsButton("Import_script_date_csv",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                              tags$br(),
                                                                                                                              shinyBS::bsButton("Import_type_date_csv",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                              
                                                                                                                       ),
                                                                                                                       column(4,
                                                                                                                              shinyBS::bsButton("Import_script_body_csv",label = "Script",style = "info",icon=icon("terminal"),block=T,title = "Use an R-Script for creating the metadata"),
                                                                                                                              tags$br(),
                                                                                                                              shinyBS::bsButton("Import_type_body_csv",label = "Type",style="info",icon=icon("edit"),block=T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                       )
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde1_csv",label = "Script",style = "info",icon=icon("terminal"),block=T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde1_csv",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde2_csv",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde2_csv",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde3_csv",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde3_csv",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde4_csv",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde4_csv",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde5_csv",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde5_csv",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde6_csv",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde6_csv",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde7_csv",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde7_csv",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde8_csv",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde8_csv",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       shinyBS::bsButton("Import_script_mde9_csv",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                       tags$br(),
                                                                                                                       shinyBS::bsButton("Import_type_mde9_csv",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                )
                                                                                                       ),
                                                                                                       tags$br(),
                                                                                                       tags$br(),
                                                                                                       tags$head(tags$style(HTML('#Import_csv_language+ div>.selectize-dropdown{bottom: 100% !important; top:auto!important;}'))),
                                                                                                       fluidRow(style="margin-left:0px; margin-right:0px;",
                                                                                                                column(3,
                                                                                                                       selectInput(inputId = "Import_csv_language",label = "Model",choices = stringr::str_split(
                                                                                                                         stringr::str_replace_all(
                                                                                                                           stringr::str_replace_all(string = system(command = "python -m spacy info",intern = T)[8],pattern = "Pipelines[ ]+",replacement = ""),
                                                                                                                           pattern=" ",replacement=""),
                                                                                                                         pattern=",",simplify = T)[1,],multiple = F,selected = character(0))
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       textInput(inputId = "Import_csv_dataset",label = "dataset abbreviation",placeholder = "e.g. GU for Guardian")
                                                                                                                ),
                                                                                                                column(1,  
                                                                                                                       selectizeInput(inputId = "Import_csv_date_format",label = "date format",choices = c("%Y-%m-%d","%d-%m-%Y"),multiple=F,options=list(create=T))
                                                                                                                ),
                                                                                                                column(7,
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
                                                                                          fluidRow(style="margin-left:0px;margin-right:0px;padding-right:0px;",
                                                                                                   column(2,
                                                                                                          checkboxInput(inputId = "Import_csv_slow_mode",label = "use mode for slow/old PC's",value = FALSE)%>%
                                                                                                            shinyInput_label_embed(
                                                                                                              shiny_iconlink() %>%
                                                                                                                bs_embed_popover(
                                                                                                                  title = "If you have a huge data set or a pc with less than 8GB of RAM you can activate this option. This way the import will take longer but it needs less memory.", placement = "right"
                                                                                                                )
                                                                                                            )
                                                                                                   ),
                                                                                                   column(2,
                                                                                                          selectInput(inputId="Import_csv_anonymize",selected=character(0),label="anonymize your data?",
                                                                                                                      choices=c("title","mde1","mde2","mde3","mde4","mde5","mde6","med7","mde8","mde9"),multiple=T) %>%
                                                                                                            shinyInput_label_embed(
                                                                                                              shiny_iconlink() %>%
                                                                                                                bs_embed_popover(
                                                                                                                  title = "If you have certain column which you would like to anonymize, please select them here.", placement = "right"
                                                                                                                )
                                                                                                            )
                                                                                                   ),
                                                                                                   column(2,
                                                                                                          selectInput(inputId="Import_csv_pseudonymization",selected=character(0),label="pseudonymize your data?",
                                                                                                                      choices=c("title","mde1","mde2","mde3","mde4","mde5","mde6","med7","mde8","mde9"),multiple=T) %>%
                                                                                                            shinyInput_label_embed(
                                                                                                              shiny_iconlink() %>%
                                                                                                                bs_embed_popover(
                                                                                                                  title = "If you have certain column which you would like to apply pseudonymization to, please select them here.", placement = "right"
                                                                                                                )
                                                                                                            )
                                                                                                   )
                                                                                          ),
                                                                                          shinyBS::bsButton("Import_csv_start_preprocess",label = "Start Preprocessing and save csv-files",style="info",icon=icon("tags")),
                                                                                          shinyBS::bsButton("Import_csv_start_preprocess_and_write",label = "Start Preprocessing and directly write to DB",style="info",icon=icon("upload")),
                                                                                          shinyBS::bsButton("Import_csv_sanity_check",label = "Sanity Check",style="info",icon=icon("search"))
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
                                                                                  box(width=2,title = "Split Method",status = "primary",collapsible = T,
                                                                                      selectInput(inputId = "Import_mtf_split_method", "Method:",
                                                                                                  choices=c("None", "Regular Expression", "Hard Split", "Script")
                                                                                      )%>%
                                                                                        shinyInput_label_embed(
                                                                                          icon("info") %>%
                                                                                            bs_embed_tooltip(title = "This Method will split the text of the imported files.")
                                                                                        ),
                                                                                      conditionalPanel(
                                                                                        condition = "input.Import_mtf_split_method != 'None'",
                                                                                        uiOutput("UI_Import_mtf_column_name")
                                                                                      ),
                                                                                      conditionalPanel(
                                                                                        condition = "input.Import_mtf_split_method == 'Regular Expression'",
                                                                                        textInput(inputId = "Import_mtf_split_method_regex",label = "Regular Expression:", value = "\\n\\n")
                                                                                      ),
                                                                                      conditionalPanel(
                                                                                        condition = "input.Import_mtf_split_method == 'Hard Split'",
                                                                                        numericInput(inputId = "Import_mtf_split_method_split_number",label = "Split after x Characters:", value = 2000, min = 1)
                                                                                      ),
                                                                                      conditionalPanel(
                                                                                        condition = "input.Import_mtf_split_method == 'Script'",
                                                                                        shinyBS::bsButton("Import_script_split_mtf",label = "Script",style = "info",icon=icon("terminal"),block=T,title = "Use an R-Script to split text in imported files")
                                                                                      ),
                                                                                      conditionalPanel(
                                                                                        condition = "input.Import_mtf_split_method != 'None'",
                                                                                        tags$br(),
                                                                                        actionButton("Import_mtf_split_test_view", "Test Split", style = "info", block=T)
                                                                                      )
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
                                                                                                                       column(4,
                                                                                                                              uiOutput(outputId = "UI_Import_mtf_title")
                                                                                                                       ),
                                                                                                                       column(4,
                                                                                                                              uiOutput(outputId = "UI_Import_mtf_date")
                                                                                                                       ),
                                                                                                                       column(4,
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
                                                                                                                       column(4,
                                                                                                                              shinyBS::bsButton("Import_script_title_mtf",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                              tags$br(),
                                                                                                                              shinyBS::bsButton("Import_type_title_mtf",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                       ),
                                                                                                                       column(4,
                                                                                                                              shinyBS::bsButton("Import_script_date_mtf",label = "Script",style = "info",icon=icon("terminal"),block = T,title = "Use an R-Script for creating the metadata"),
                                                                                                                              tags$br(),
                                                                                                                              shinyBS::bsButton("Import_type_date_mtf",label = "Type",style="info",icon=icon("edit"),block = T,title = "Type in the attrbute you want to use for all documents")
                                                                                                                       ),
                                                                                                                       column(4,
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
                                                                                                       tags$head(tags$style(HTML('#Import_mtf_language+ div>.selectize-dropdown{bottom: 100% !important; top:auto!important;}'))),
                                                                                                       fluidRow(style="margin-left:0px; margin-right:0px;",
                                                                                                                column(3,
                                                                                                                       selectInput(inputId = "Import_mtf_language",label = "Model",choices =stringr::str_split(
                                                                                                                         stringr::str_replace_all(
                                                                                                                           stringr::str_replace_all(string = system(command = "python -m spacy info",intern = T)[8],pattern = "Pipelines[ ]+",replacement = ""),
                                                                                                                           pattern=" ",replacement=""),
                                                                                                                         pattern=",",simplify = T)[1,],multiple = F,selected = character(0))
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       textInput(inputId = "Import_mtf_dataset",label = "dataset abbreviation",placeholder = "e.g. GU for Guardian")
                                                                                                                ),
                                                                                                                column(1,
                                                                                                                       selectizeInput(inputId = "Import_mtf_date_format",label = "date format",choices = c("%Y-%m-%d","%d-%m-%Y"),multiple=F,options=list(create=T))
                                                                                                                ),
                                                                                                                column(7,
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
                                                                                          fluidRow(style="margin-left:0px;margin-right:0px;padding-right:0px;",
                                                                                                   column(2,
                                                                                                          checkboxInput(inputId = "Import_mtf_slow_mode",label = "use mode for slow/old PC's",value = FALSE)%>%
                                                                                                            shinyInput_label_embed(
                                                                                                              shiny_iconlink() %>%
                                                                                                                bs_embed_popover(
                                                                                                                  title = "If you have a huge data set or a pc with less than 8GB of RAM you can activate this option. This way the import will take longer but it needs less memory.", placement = "right"
                                                                                                                )
                                                                                                            )
                                                                                                   ),
                                                                                                   column(2,
                                                                                                          selectInput(inputId="Import_mtf_anonymize",selected=character(0),label="anonymize your data?",
                                                                                                                      choices=c("title","mde1","mde2","mde3","mde4","mde5","mde6","med7","mde8","mde9"),multiple=T) %>%
                                                                                                            shinyInput_label_embed(
                                                                                                              shiny_iconlink() %>%
                                                                                                                bs_embed_popover(
                                                                                                                  title = "If you have certain column which you would like to anonymize, please select them here.", placement = "right"
                                                                                                                )
                                                                                                            )
                                                                                                   ),
                                                                                                   column(2,
                                                                                                          selectInput(inputId="Import_mtf_pseudonymization",selected=character(0),label="pseudonymize your data?",
                                                                                                                      choices=c("title","mde1","mde2","mde3","mde4","mde5","mde6","med7","mde8","mde9"),multiple=T) %>%
                                                                                                            shinyInput_label_embed(
                                                                                                              shiny_iconlink() %>%
                                                                                                                bs_embed_popover(
                                                                                                                  title = "If you have certain column which you would like to apply pseudonymization to, please select them here.", placement = "right"
                                                                                                                )
                                                                                                            )
                                                                                                   )
                                                                                          ),
                                                                                          shinyBS::bsButton("Import_mtf_start_preprocess",label = "Start Preprocessing and save csv-files",style="info",icon=icon("tags")),
                                                                                          shinyBS::bsButton("Import_mtf_start_preprocess_and_write",label = "Start Preprocessing and directly write to DB",style="info",icon=icon("upload")),
                                                                                          shinyBS::bsButton("Import_mtf_sanity_check",label = "Sanity Check",style="info",icon=icon("search"))
                                                                         )
                                                        )
                                       )
                      ),
                      conditionalPanel(condition='input.Import_Type=="Wortschatz"',
                                       uiOutput(outputId = "UI_Import_Wortschatz")
                      )
             )
           ),
           tabPanel(
             "Upload Data to DB and Solr",
             uiOutput("Import_Files_UI"),
             tags$br(),
             withBusyIndicatorUI(
               shinyBS::bsButton(
                 inputId = "Upload_Data_DB_and_Solr",
                 label = "Upload selected data to DB and import to Solr",
                 icon = icon("upload"),
                 style = "default"
               )
             ),
             # withBusyIndicatorUI(
             #   shinyBS::bsButton(
             #     inputId = "Upload_Data",
             #     label = "Upload selected data to DB",
             #     icon = icon("database"),
             #     style = "default"
             #   )
             # ),
             # withBusyIndicatorUI(
             #   shinyBS::bsButton(
             #     inputId = "Import_to_solr",
             #     label = "Import data to solr",
             #     icon = icon("upload"),
             #     style = "default"
             #   )
             # ),
             shinyBS::bsButton(
               inputId = "Import_delete",
               label = "Delete",
               icon = icon("trash"),
               style = "danger"
             )
           ),
           tabPanel(
             "REFI-Import",
             tags$br(),
             tags$br(),
             box(
               id = "refi_import_box1"
               , width = 4
               , tags$div(
                 style=""
                 , textInput(
                   inputId = "refi_import_dataset_name"
                   , label = "Choose a name for the dataset or annotation scheme"
                   , value = "REFI"
                   , placeholder = "refi-import"
                 )
                 , verbatimTextOutput(
                   outputId = "refi_import_validate_dataset"
                 )
                 , fileInput(
                   inputId = "refi_import_fileInput"
                   , label = "Choose a REFI-QDA Project file (.qdpx) or a REFI-QDA Codebook file (.qdc)."
                   , accept = c(".xml", ".qdpx", ".qdc", ".qde")
                   , buttonLabel = "Start import"
                 )
                 # , infoBox(
                 #   title = "Naming convention"
                 #   , value = "New text sources are saved as a dataset and as a collection.\nIn case of an CodeBook the dataset, collection and the annotation system will have the same name."
                 # )
               )
             ),
             box(
               id = "refi_import_box2"
               , width = 4
               , tags$div(
                 style=""
                 , DT::dataTableOutput(outputId = "refi_import_table_datasets")
               )
             ),
             box(
               id = "refi_import_box3"
               , width = 4
               , panel(
                 tags$div(
                   DT::dataTableOutput(outputId = "refi_import_table_annotation_scheme")
                 )
               )
               , panel(
                 tags$div(
                   conditionalPanel(
                     condition = "input.selectedAnnotationSchemeImport != ''"
                     , uiOutput("annotation_scheme_list_import")
                   )
                 )
               )
             )
           ),
           tabPanel("OHD Import",
                    uiOutput(outputId = "UI_Import_OHD")
           )
         )
)
