remove_existing_checkboxes<-function(ids){
  for(i in ids){
    shiny::removeUI(immediate = T,selector = paste0("div#dcbox_",i))
  }
}  

remove_existing_checkboxes_Doc<-function(ids){
  for(i in ids){
    shiny::removeUI(immediate = T,selector = paste0("div#Doccbox_",i))
  }
}

remove_delete_buttons<-function(ids){
  for( i in ids){
    shiny::removeUI(immediate = T,selector = paste0("#logs_button_",i))
  }
}

remove_tm_add_buttons<-function(ids){
  for( i in ids){
    shiny::removeUI(immediate = T,selector = paste0("#tm_ac_",i))
  }
}

remove_existing_material<-function(ids){
  for(i in ids){
    shiny::removeUI(immediate = T,selector = paste0("div#material_",i))
  }
}  


remove_existing_active_learning_settings<-function(ids){
  for(i in ids){
    shiny::removeUI(immediate = T,selector = paste0("#Class_eval_radio_buttons_",i,"_deny"))
  }
}


remove_existing_radio_buttons_classification_whole_document<-function(ids){
  for(i in ids){
    shiny::removeUI(immediate = T,selector = paste0("#Class_correct_annotation_",i))
    shiny::removeUI(immediate = T,selector = paste0("#Class_correct_annotation_",i,"_deny"))
  }
}