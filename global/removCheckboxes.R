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