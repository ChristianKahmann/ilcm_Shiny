
observe({
  validate(
    need(!is.null(values$Det_SP_annotations),message=F),
    need(!is.null(input$Det_SP_baseform),message=F)
  )
  annos<-values$Det_SP_annotations
  annos$sentence_id<-paste(annos$doc_id,annos$sentence_id,sep="_")
  sentences_to_keep<-NULL
  if(input$Det_SP_baseform==TRUE){
    if(!is.null(input$Det_SP_subject)){
      if(nchar(input$Det_SP_subject[1])>0){
        sentences_to_keep<-unique(annos$sentence_id[intersect(which(annos$lemma%in%input$Det_SP_subject),which(annos$dep_rel=="nsubj"))])
      }
    }
    
    if(!is.null(input$Det_SP_predicate)){
      if(nchar(input$Det_SP_predicate[1])>0){
        if(!is.null(sentences_to_keep)){
          #sentences_to_keep<-intersect(sentences_to_keep,unique(annos$sentence_id[intersect(which(annos$lemma%in%input$Det_SP_predicate),union(which(annos$dep_rel=="aux"),which(annos$dep_rel=="cop")))]))
          sentences_to_keep<-intersect(sentences_to_keep,unique(annos$sentence_id[intersect(which(annos$lemma%in%input$Det_SP_predicate),which(annos$upos=="VERB"))]))
        }
        else{
          #sentences_to_keep<-unique(annos$sentence_id[intersect(which(annos$lemma%in%input$Det_SP_predicate),union(which(annos$dep_rel=="aux"),which(annos$dep_rel=="cop")))])
          sentences_to_keep<-unique(annos$sentence_id[intersect(which(annos$lemma%in%input$Det_SP_predicate),which(annos$upos=="VERB"))])
        }
      }
    }
    if(!is.null(input$Det_SP_object)){
      if(nchar(input$Det_SP_object[1])>0){
        if(!is.null(sentences_to_keep)){
          sentences_to_keep<-intersect(sentences_to_keep,unique(annos$sentence_id[intersect(which(annos$lemma%in%input$Det_SP_object),which(annos$dep_rel=="obj"))]))
        }
        else{
          sentences_to_keep<-unique(annos$sentence_id[intersect(which(annos$lemma%in%input$Det_SP_object),which(annos$dep_rel=="obj"))])
          
        }
      }
    }
    if(!is.null(sentences_to_keep)){
      values$Det_SP_data<-annos[which(annos$sentence_id%in% sentences_to_keep),]
    }
    else{
      values$Det_SP_data<-annos
    }
  }
  else{
    if(!is.null(input$Det_SP_subject)){
      if(nchar(input$Det_SP_subject[1])>0){
        sentences_to_keep<-unique(annos$sentence_id[intersect(which(annos$token%in%input$Det_SP_subject),which(annos$dep_rel=="nsubj"))])
      }
    }
    
    if(!is.null(input$Det_SP_predicate)){
      if(nchar(input$Det_SP_predicate[1])>0){
        if(!is.null(sentences_to_keep)){
          #sentences_to_keep<-intersect(sentences_to_keep,unique(annos$sentence_id[intersect(which(annos$token%in%input$Det_SP_predicate),union(which(annos$dep_rel=="aux"),which(annos$dep_rel=="cop")))]))
          sentences_to_keep<-intersect(sentences_to_keep,unique(annos$sentence_id[intersect(which(annos$token%in%input$Det_SP_predicate),which(annos$upos=="VERB"))]))
        }
        else{
          #sentences_to_keep<-unique(annos$sentence_id[intersect(which(annos$token%in%input$Det_SP_predicate),union(which(annos$dep_rel=="aux"),which(annos$dep_rel=="cop")))])
          sentences_to_keep<-unique(annos$sentence_id[intersect(which(annos$token%in%input$Det_SP_predicate),which(annos$upos=="VERB"))])
        }
      }
    }
    if(!is.null(input$Det_SP_object)){
      if(nchar(input$Det_SP_object[1])>0){
        if(!is.null(sentences_to_keep)){
          sentences_to_keep<-intersect(sentences_to_keep,unique(annos$sentence_id[intersect(which(annos$token%in%input$Det_SP_object),which(annos$dep_rel=="obj"))]))
        }
        else{
          sentences_to_keep<-unique(annos$sentence_id[intersect(which(annos$token%in%input$Det_SP_object),which(annos$dep_rel=="obj"))])
          
        }
      }
    }
    if(!is.null(sentences_to_keep)){
      values$Det_SP_data<-annos[which(annos$sentence_id%in% sentences_to_keep),]
    }
    else{
      values$Det_SP_data<-annos
    }
  }
})


output$Det_SP_slider_ui<-renderUI({
  validate(
    need(
      dim(values$Det_SP_data)[1]>0,message=F
    )
  )
  max<-ceiling(length(unique(values$Det_SP_data$sentence_id))/10)
  sliderInput(inputId = "Det_SP_slider",label = "",min = 1,max = max,value = 1,step = 1)
})


output$Det_SP_sentences<-renderUI({
  validate(
    need(!is.null(values$Det_SP_data),message=F),
    need(dim(values$Det_SP_data)[1]>0,message="No matching sentences found!"),
    need(!is.null(input$Det_SP_slider),message=F)
  )
  sentence_ids<-unique(values$Det_SP_data$sentence_id)[((10*(input$Det_SP_slider-1))+1):(10*input$Det_SP_slider)]
  sentence_ids<-sentence_ids[!is.na(sentence_ids)]
  count=0
  offset=10*(input$Det_SP_slider-1)
  sentences<-lapply(1:length(sentence_ids),FUN = function(x){
    annos<-values$Det_SP_data[which(values$Det_SP_data$sentence_id==sentence_ids[x]),]
    if(any(grepl(annos$token_id,pattern = "-"))){
      to_del<-stringr::str_split(string = annos$token_id[which(grepl(annos$token_id,pattern = "-"))],pattern = "-",simplify = T)
      annos<-annos[-which(annos$token_id%in%to_del),]
    }
    tags$div(
      bsButton(inputId = paste0("syntax_tree_",x),label = "",style = "primary",size = "s",icon = icon("tree"),onclick='Shiny.onInputChange(\"show_syntax_button\",  this.id)'),
      tags$b(paste0(" Sentence ",(offset+x)),":"),
      paste(annos$token,collapse=" "))
  })
  
  return(tagList(sentences))
})


observeEvent(ignoreInit = T,input$show_syntax_button,{
  id<-as.numeric(stringr::str_split(string = input$show_syntax_button,pattern = "_",simplify = T)[1,3])
  if(id>0){
    shinyjs::runjs('Shiny.onInputChange(\"show_syntax_button\",  "syntax_tree_0")')
    sentence_id<-unique(values$Det_SP_data$sentence_id)[((((10*(input$Det_SP_slider-1))+1):(10*input$Det_SP_slider))[id])]
    keep<-which(values$Det_SP_data$sentence_id==sentence_id)
    tree_data<-values$Det_SP_data[keep,]
    added<-which(grepl(tree_data$token_id,pattern = "-"))
    if(length(added)>0){
      values$Det_SP_data_without_addings<-tree_data[-added,]
    }
    else{
      values$Det_SP_data_without_addings<-tree_data
    }
    original<-tree_data[grepl(tree_data$token_id,pattern = "-"),"token_id",drop=F]
    if(nchar(original)>1){
      to_del<-stringr::str_split(string = original,pattern = "-",simplify = T)
      tree_data_original<-tree_data[-which(tree_data$token_id%in%to_del),]
    }
    else{
      tree_data_original<-tree_data
    }
    showModal(
      modalDialog(title = "Syntax Tree",size="l",easyClose = T,
                  tags$b("Sentence: ",paste(tree_data_original$token,collapse=" ")),
                  visNetworkOutput(outputId = "Det_SP_tree")
      )
    )
  }
})


output$Det_SP_tree<-renderVisNetwork({
  
  x<-values$Det_SP_data_without_addings
  
  nodes<-data.frame(id=x$token_id,label=x$token,shape="box",group=x$upos)
  edges<-data.frame(from=x$token_id,to=x$head_token_id,label=x$dep_rel,shadow=TRUE)
  
  visNetwork(nodes,edges)%>%
    visEdges(arrows = "from") %>% 
    visHierarchicalLayout() %>%
    visGroups(groupname = "ADJ", color = c25[1]) %>%
    visGroups(groupname = "ADP", color = c25[2]) %>%
    visGroups(groupname = "ADV", color = c25[3]) %>%
    visGroups(groupname = "AUX", color = c25[4]) %>%
    visGroups(groupname = "CCONJ", color = c25[5]) %>%
    visGroups(groupname = "DET", color = c25[6]) %>%
    visGroups(groupname = "INTJ", color = c25[7]) %>%
    visGroups(groupname = "NOUN", color = c25[8]) %>%
    visGroups(groupname = "NUM", color = c25[9]) %>%
    visGroups(groupname = "PART", color = c25[10]) %>%
    visGroups(groupname = "PRON", color = c25[11]) %>%
    visGroups(groupname = "PROPN", color = c25[12]) %>%
    visGroups(groupname = "PUNCT", color = c25[13]) %>%
    visGroups(groupname = "SCONJ", color = c25[14]) %>%
    visGroups(groupname = "SYM", color = c25[15]) %>%
    visGroups(groupname = "VERB", color = c25[16]) %>%
    visGroups(groupname = "X", color = c25[17]) %>%
    visLegend(width = 0.18, position = "right", main = "Group")
  
})


# output$Det_SP_search_graph<-visNetwork::renderVisNetwork({
#   validate(
#     need(
#       !is.null(values$Det_SP_data),message=F
#     )
#   )
#   x<-values$Det_SP_data[1:min(200,dim(values$Det_SP_data)[1]),]
#   x$token_id<-paste(x$sentence_id,x$token_id,sep="_")
#   x$head_token_id<-paste(x$sentence_id,x$head_token_id,sep="_")
#   #browser()
#   nodes<-data.frame(id=x$token_id,label=x$token,shape="box",group=x$upos)
#   edges<-data.frame(from=x$token_id,to=x$head_token_id,label=x$dep_rel,shadow=TRUE)
#   
#   visNetwork(nodes,edges)%>%
#     visEdges(arrows = "from") %>% 
#     visHierarchicalLayout() %>%
#     visLegend(width = 0.18, position = "right", main = "Group")
# })
# 
# 
# 
# output$Det_SP_search_table<-DT::renderDataTable({
#   data<-values$Det_SP_data
#   subj<-as.data.frame(table(data$token[which(data$dep_rel=="nsubj")]))
#   
#   data<-matrix(c(1,2,3,4),nrow = 2)
#   datatable(data)
# })


output$Det_SP_number_of_results<-renderUI({
  text<-paste0(length(unique(values$Det_SP_data$sentence_id))," sentences could have been found matching the sepcified input!")
  return(tags$b(text))
})