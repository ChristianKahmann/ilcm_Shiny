#' panel for facets from meta data 
tabPanel("Facets",
         div(style = 'height: 80vh; overflow-y: auto;',
             uiOutput(outputId = "Fac_meta_UI")
             
         )
)
