shorten_long_choices_for_radio_buttons<-function(choice,char_split = 16){
  if(nchar(choice)<=char_split){
    return(choice)
  }
  else{
    choice_split <- paste0(substr(choice,1,(char_split-3)),"...")
    tag <- HTML(paste0("<span title='",choice,"'>",choice_split,"</span>"))
    return(tag)
  }
  
  
}