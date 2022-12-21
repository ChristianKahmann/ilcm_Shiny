tagList(
  switchInput(inputId="enrichment_upload_new_csv",label = "Upload new Transcript or use existing Interviews",value = T,onLabel = "Upload",offLabel = "Existing Data"),
  conditionalPanel(condition='input.enrichment_upload_new_csv==true',
                   fileInput(inputId = "Enrichment_new_transcript",label = "Upload new Transcript")
  ),
  tags$hr(),
  column(4,
  selectInput(inputId="enrichment_topic","Choose Topic Model:",choices = list.files(path = "collections/results/topic-model/"))
  ),
  column(8,
         box(title = "Check chosen Topic Model",solidHeader = T,status = "primary",
             uiOutput("enrichment_topic_control_UI")
             )
         )
)