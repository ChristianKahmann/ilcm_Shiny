output$script_custom_script_options_UI<-renderUI({
  if(input$analysis_selected_script==""){
    shinyalert::shinyalert(title = "Please select an analysis",text = "Please choose a type of analysis first.",type = "warning")
    return(NULL)
  }
  else{
    choices<-list.files(paste0("collections/scripts/",input$analysis_selected_script,"/"))
    if(length(choices)==0){
      shinyalert::shinyalert(title = "no custom script found!",text = "Save an updated version of standard script.",type = "warning")
      return(NULL)
    }
    else{
      return(
        selectInput(inputId = "script_custom_script_options",label = "available custom scripts",choices = choices)
      )
    }
  }
})


output$script_UI<-renderUI({
  if(input$script_use_custom_script==TRUE){
    if(file.exists( paste0("collections/scripts/",input$analysis_selected_script,"/",input$script_custom_script_options))){
      return(tagList(
        tags$h4(input$script_custom_script_options),
        aceEditor(outputId = "R_Script_custom",showLineNumbers = T,mode = "r",fontSize = 14,theme = input$script_theme,autoComplete = "enabled",value = readChar(con = paste0("collections/scripts/",input$analysis_selected_script,"/",input$script_custom_script_options), file.info(paste0("collections/scripts/",input$analysis_selected_script,"/",input$script_custom_script_options))$size)),        
        bsButton("Save_Script_custom","Save Script",icon=icon("save"),style = "primary")
      ))
    }
  }
  else{
    return(tagList(
      conditionalPanel(condition = "input.analysis_selected_script=='Cooccurrence_Analysis'",
                       tags$h4("Cooccurrence Analysis Script"),
                       aceEditor(outputId = "R_Script_CA",showLineNumbers = T,mode = "r",fontSize = 14,theme = input$script_theme,autoComplete = "enabled",value = readChar(con = "collections/scripts/Cooccurrence_Analysis_Script.R", file.info("collections/scripts/Cooccurrence_Analysis_Script.R")$size)),        
                       bsButton("Save_Script_CA","Save Script",icon=icon("save"),style = "primary")
      ),
      conditionalPanel(condition = "input.analysis_selected_script=='Frequency_Extraction'",
                       tags$h4("Frequency Extraction Script"),
                       aceEditor(outputId = "R_Script_FE",showLineNumbers = T,mode = "r",fontSize = 14,theme = input$script_theme,autoComplete = "enabled",value = readChar(con = "collections/scripts/Frequency_Extraction_Script.R", file.info("collections/scripts/Frequency_Extraction_Script.R")$size)),        
                       bsButton("Save_Script_FE","Save Script",icon=icon("save"),style = "primary")
      ),
      conditionalPanel(condition = "input.analysis_selected_script=='Volatility_Analysis'",
                       tags$h4("Volatility Analysis Script"),
                       aceEditor(outputId = "R_Script_VA",showLineNumbers = T,mode = "r",fontSize = 14,theme = input$script_theme,autoComplete = "enabled",value = readChar(con = "collections/scripts/Volatility_Analysis_Script.R", file.info("collections/scripts/Volatility_Analysis_Script.R")$size)),        
                       bsButton("Save_Script_VA","Save Script",icon=icon("save"),style = "primary")
      ),
      conditionalPanel(condition = "input.analysis_selected_script=='Topic_Model'",
                       tags$h4("Topic Model Script"),
                       aceEditor(outputId = "R_Script_TM",showLineNumbers = T,mode = "r",fontSize = 14,theme = input$script_theme,autoComplete = "enabled",value = readChar(con = "collections/scripts/Topic_Model_Script.R", file.info("collections/scripts/Topic_Model_Script.R")$size)),        
                       bsButton("Save_Script_TM","Save Script",icon=icon("save"),style = "primary")
      ),
      conditionalPanel(condition = "input.analysis_selected_script=='Dictionary_Extraction'",
                       tags$h4("Dictionary Extraction Script"),
                       aceEditor(outputId = "R_Script_DE",showLineNumbers = T,mode = "r",fontSize = 14,theme = input$script_theme,autoComplete = "enabled",value = readChar(con = "collections/scripts/Dictionary_Extraction_Script.R", file.info("collections/scripts/Dictionary_Extraction_Script.R")$size)),        
                       bsButton("Save_Script_DE","Save Script",icon=icon("save"),style = "primary")
      ),
      conditionalPanel(condition = "input.analysis_selected_script=='Classification'",
                       tags$h4("Classification Script"),
                       aceEditor(outputId = "R_Script_CL",showLineNumbers = T,mode = "r",fontSize = 14,theme = input$script_theme,autoComplete = "enabled",value = readChar(con = "collections/scripts/Classification_Script.R", file.info("collections/scripts/Classification_Script.R")$size)),        
                       bsButton("Save_Script_CL","Save Script",icon=icon("save"),style = "primary")
      ),
      conditionalPanel(condition = "input.analysis_selected_script=='Sentiment_Analysis'",
                       tags$h4("Sentiment Analysis Script"),
                       aceEditor(outputId = "R_Script_SA",showLineNumbers = T,mode = "r",fontSize = 14,theme = input$script_theme,autoComplete = "enabled",value = readChar(con = "collections/scripts/Sentiment_Analysis_Script.R", file.info("collections/scripts/Sentiment_Analysis_Script.R")$size)),        
                       bsButton("Save_Script_SA","Save Script",icon=icon("save"),style = "primary")
      ),
      #conditionalPanel(condition = "input.analysis_selected=='Input'",
      #                 tags$h4("Input Script"),
      #                  aceEditor(outputId = "R_Script_Im",showLineNumbers = T,mode = "r",fontSize = 14,theme = "monokai",autoComplete = "enabled",value = readChar(con = "collections/scripts/Import_Script.R", file.info("collections/scripts/Import_Script.R")$size)),        
      #                   bsButton("Save_Script_Im","Save Script",icon=icon("save"),style = "primary")
      # ),
      conditionalPanel(condition = "input.analysis_selected_script=='Factorial_Analysis'",
                       tags$h4("Factorial Analysis Script"),
                       aceEditor(outputId = "R_Script_FA",showLineNumbers = T,mode = "r",fontSize = 14,theme = input$script_theme,autoComplete = "enabled",value = readChar(con = "collections/scripts/Factorial_Analysis_Script.R", file.info("collections/scripts/Factorial_Analysis_Script.R")$size)),        
                       bsButton("Save_Script_Im","Save Script",icon=icon("save"),style = "primary")
      )
    )
    )
  }
})



observe({
  if(input$analysis_selected%in%c("Cooccurrence_Analysis","Frequency_Extraction",
                                  "Volatility_Analysis","Topic_Model","Dictionary_Extraction","Classification","Sentiment_Analysis","Factorial_Analysis")){
    
    updateSelectInput(session = session,inputId = "analysis_selected_script",selected = input$analysis_selected)
    updateCheckboxInput(session = session,inputId = "script_use_custom_script",value = input$use_custom_script)
    updateSelectInput(session = session,inputId = "script_custom_script_options",selected =input$custom_script_options)
  }
  
})



observeEvent(input$Save_Script_CA,{
  showModal(
    modalDialog(title = "Name your Script",easyClose = F,
                textInput(inputId = "Script_Name_CA",label = "Name",value = "Cooccurrence_Analysis_")%>%
                  shinyInput_label_embed(
                    shiny_iconlink() %>%
                      bs_embed_popover(
                        title = "suffix '_Script.R' will be added automatically", placement = "left"
                      )
                  ),
                bsButton(inputId = "Script_Name_save_CA",label = "save",icon = icon("save"),style = "primary")
    )
  )
})

observeEvent(input$Script_Name_save_CA,{
  text=input$R_Script_CA
  name<-paste0(input$Script_Name_CA,"_Script.R")
  if(name%in%list.files(path = "collections/scripts/Cooccurrence_Analysis/",full.names = F)){
    shinyalert::shinyalert(title = "File Name already used",text = "specify an other name.",type = "warning") 
  }
  else{
    removeModal()
    write(text,file = paste0("collections/scripts/Cooccurrence_Analysis/",name))
    shinyalert::shinyalert(title = "File saved",type = "success") 
  }
})




observeEvent(input$Save_Script_FE,{
  showModal(
    modalDialog(title = "Name your Script",easyClose = F,
                textInput(inputId = "Script_Name_FE",label = "Name",value = "Frequency_Extraction_")%>%
                  shinyInput_label_embed(
                    shiny_iconlink() %>%
                      bs_embed_popover(
                        title = "suffix '_Script.R' will be added automatically", placement = "left"
                      )
                  ),
                bsButton(inputId = "Script_Name_save_FE",label = "save",icon = icon("save"),style = "primary")
    )
  )
})
observeEvent(input$Script_Name_save_FE,{
  text=input$R_Script_FE
  name<-paste0(input$Script_Name_FE,"_Script.R")
  if(name%in%list.files(path = "collections/scripts/Frequency_Extraction/",full.names = F)){
    shinyalert::shinyalert(title = "File Name already used",text = "specify an other name.",type = "warning") 
  }
  else{
    removeModal()
    write(text,file = paste0("collections/scripts/Frequency_Extraction/",name))
    shinyalert::shinyalert(title = "File saved",type = "success") 
  }
})




observeEvent(input$Save_Script_VA,{
  showModal(
    modalDialog(title = "Name your Script",easyClose = F,
                textInput(inputId = "Script_Name_VA",label = "Name",value = "Volatility_Analysis_")%>%
                  shinyInput_label_embed(
                    shiny_iconlink() %>%
                      bs_embed_popover(
                        title = "suffix '_Script.R' will be added automatically", placement = "left"
                      )
                  ),
                bsButton(inputId = "Script_Name_save_VA",label = "save",icon = icon("save"),style = "primary")
    )
  )
})
observeEvent(input$Script_Name_save_VA,{
  text=input$R_Script_VA
  name<-paste0(input$Script_Name_VA,"_Script.R")
  if(name%in%list.files(path = "collections/scripts/Volatility_Analysis/",full.names = F)){
    shinyalert::shinyalert(title = "File Name already used",text = "specify an other name.",type = "warning") 
  }
  else{
    removeModal()
    write(text,file = paste0("collections/scripts/Volatility_Analysis/",name))
    shinyalert::shinyalert(title = "File saved",type = "success") 
  }
})




observeEvent(input$Save_Script_TM,{
  showModal(
    modalDialog(title = "Name your Script",easyClose = F,
                textInput(inputId = "Script_Name_TM",label = "Name",value = "Topic_Model_")%>%
                  shinyInput_label_embed(
                    shiny_iconlink() %>%
                      bs_embed_popover(
                        title = "suffix '_Script.R' will be added automatically", placement = "left"
                      )
                  ),
                bsButton(inputId = "Script_Name_save_TM",label = "save",icon = icon("save"),style = "primary")
    )
  )
})
observeEvent(input$Script_Name_save_TM,{
  text=input$R_Script_TM
  name<-paste0(input$Script_Name_TM,"_Script.R")
  if(name%in%list.files(path = "collections/scripts/Topic_Model/",full.names = F)){
    shinyalert::shinyalert(title = "File Name already used",text = "specify an other name.",type = "warning") 
  }
  else{
    removeModal()
    write(text,file = paste0("collections/scripts/Topic_Model/",name))
    shinyalert::shinyalert(title = "File saved",type = "success") 
  }
})



observeEvent(input$Save_Script_DE,{
  showModal(
    modalDialog(title = "Name your Script",easyClose = F,
                textInput(inputId = "Script_Name_DE",label = "Name",value = "Dictionary_Extraction_")%>%
                  shinyInput_label_embed(
                    shiny_iconlink() %>%
                      bs_embed_popover(
                        title = "suffix '_Script.R' will be added automatically", placement = "left"
                      )
                  ),
                bsButton(inputId = "Script_Name_save_DE",label = "save",icon = icon("save"),style = "primary")
    )
  )
})
observeEvent(input$Script_Name_save_DE,{
  text=input$R_Script_DE
  name<-paste0(input$Script_Name_DE,"_Script.R")
  if(name%in%list.files(path = "collections/scripts/Dictionary_Extraction/",full.names = F)){
    shinyalert::shinyalert(title = "File Name already used",text = "specify an other name.",type = "warning") 
  }
  else{
    removeModal()
    write(text,file = paste0("collections/scripts/Dictionary_Extraction/",name))
    shinyalert::shinyalert(title = "File saved",type = "success") 
  }
})



observeEvent(input$Save_Script_CL,{
  showModal(
    modalDialog(title = "Name your Script",easyClose = F,
                textInput(inputId = "Script_Name_CL",label = "Name",value = "Classification_")%>%
                  shinyInput_label_embed(
                    shiny_iconlink() %>%
                      bs_embed_popover(
                        title = "suffix '_Script.R' will be added automatically", placement = "left"
                      )
                  ),
                bsButton(inputId = "Script_Name_save_CL",label = "save",icon = icon("save"),style = "primary")
    )
  )
})
observeEvent(input$Script_Name_save_CL,{
  text=input$R_Script_CL
  name<-paste0(input$Script_Name_CL,"_Script.R")
  if(name%in%list.files(path = "collections/scripts/Classification/",full.names = F)){
    shinyalert::shinyalert(title = "File Name already used",text = "specify an other name.",type = "warning") 
  }
  else{
    removeModal()
    write(text,file = paste0("collections/scripts/Classification/",name))
    shinyalert::shinyalert(title = "File saved",type = "success") 
  }
})



observeEvent(input$Script_Name_save_FA,{
  text=input$R_Script_FA
  name<-paste0(input$Script_Name_FA,"_Script.R")
  if(name%in%list.files(path = "collections/scripts/Factorial_Analysis/",full.names = F)){
    shinyalert::shinyalert(title = "File Name already used",text = "specify an other name.",type = "warning") 
  }
  else{
    removeModal()
    write(text,file = paste0("collections/scripts/Factorial_Analysis/",name))
    shinyalert::shinyalert(title = "File saved",type = "success") 
  }
})
observeEvent(input$Save_Script_FA,{
  showModal(
    modalDialog(title = "Name your Script",easyClose = F,
                textInput(inputId = "Script_Name_FA",label = "Name",value = "Factorial_Analysis_") %>%
                  shinyInput_label_embed(
                    shiny_iconlink() %>%
                      bs_embed_popover(
                        title = "suffix '_Script.R' will be added automatically", placement = "left"
                      )
                  ),
                bsButton(inputId = "Script_Name_save_FA",label = "save",icon = icon("save"),style = "primary")
    )
  )
})


observeEvent(input$Script_Name_save_SA,{
  text=input$R_Script_SA
  name<-paste0(input$Script_Name_SA,"_Script.R")
  if(name%in%list.files(path = "collections/scripts/Sentiment_Analysis/",full.names = F)){
    shinyalert::shinyalert(title = "File Name already used",text = "specify an other name.",type = "warning") 
  }
  else{
    removeModal()
    write(text,file = paste0("collections/scripts/Sentiment_Analysis/",name))
    shinyalert::shinyalert(title = "File saved",type = "success") 
  }
})
observeEvent(input$Save_Script_SA,{
  showModal(
    modalDialog(title = "Name your Script",easyClose = F,
                textInput(inputId = "Script_Name_SA",label = "Name",value = "Sentiment_Analysis_") %>%
                  shinyInput_label_embed(
                    shiny_iconlink() %>%
                      bs_embed_popover(
                        title = "suffix '_Script.R' will be added automatically", placement = "left"
                      )
                  ),
                bsButton(inputId = "Script_Name_save_SA",label = "save",icon = icon("save"),style = "primary")
    )
  )
})

observeEvent(input$Save_Script_custom,{
  showModal(
    modalDialog(title = "Name your Script",easyClose = F,
                textInput(inputId = "Script_Name_custom",label = "Name",value = input$script_custom_script_options ) %>%
                  shinyInput_label_embed(
                    shiny_iconlink() %>%
                      bs_embed_popover(
                        title = "Please type the full name, including '.R' at the end.", placement = "left"
                      )
                  ),
                bsButton(inputId = "Script_Name_save_custom",label = "save",icon = icon("save"),style = "primary")
    )
  )
})

observeEvent(input$Script_Name_save_custom,{
  text=input$R_Script_custom
  name<-input$Script_Name_custom
  if(name%in%list.files(path = paste0("collections/scripts/",input$analysis_selected_script,"/",input$script_custom_script_options),full.names = F)){
    shinyalert::shinyalert(title = "File Name already used",text = "specify an other name.",type = "warning") 
  }
  else{
    removeModal()
    write(text,file = paste0("collections/scripts/",input$analysis_selected_script,"/",name))
    shinyalert::shinyalert(title = "File saved",type = "success") 
    values$new_script_saved<-runif(n = 1,0,1)
  }
})

