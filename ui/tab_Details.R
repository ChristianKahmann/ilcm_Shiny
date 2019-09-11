tabPanel(
  "Details",
  shinyalert::useShinyalert(),
  conditionalPanel(
    condition = 'output.Details_FA_selected!=true',
    box(
      title = tags$b("Parameters", style = "color:white"),
      solidHeader = T,
      width = 2,
      collapsible = F,
      status = "primary",
      div(
        style =  'height: 75vh;overflow-y: auto;',
        uiOutput(outputId = "details_parameter") %>% withSpinner(type = 1)
      )
    ),
    box(
      solidHeader = T,
      width = 10,
      status = "primary",
      div(
        style = 'height: 79vh; overflow-y: auto;',
        uiOutput(outputId = "details_visu") %>% withSpinner(type = 1)
      )
    )
  )
  # ,
  # conditionalPanel(
  #   condition = 'output.Details_FA_selected==true',
  #   tabsetPanel(id = "facto_what",
  #               tabPanel(title = "CA",
  #                        uiOutput(outputId="facto_meta_ca_ui"),
  #                        uiOutput(outputId = "Details_Facto_CA_UI")
  #               ),
  #               tabPanel(title="FAMD",
  #                        uiOutput(outputId="facto_meta_famd_ui"),
  #                        uiOutput(outputId = "Details_Facto_FAMD_UI")
  #               ),
  #               tabPanel(title="HCPC",
  #                        uiOutput(outputId="facto_meta_hcpc_ui"),
  #                        uiOutput(outputId = "Details_Facto_HCPC_UI")
  #               ),
  #               tabPanel(title="PCA",
  #                        uiOutput(outputId="facto_meta_pca_ui"),
  #                        uiOutput(outputId = "Details_Facto_PCA_UI")
  #               )
  #   )
  # )
)