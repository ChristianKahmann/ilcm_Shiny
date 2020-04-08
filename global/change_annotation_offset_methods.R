transform_word_offset_to_character_offset <- function(token, annotations){
  # convert word annotations to character annotations
  for(i in 1:nrow(annotations)){
    token_single_doc <- token[which(token$id==paste0(annotations[i,"dataset"],"_",annotations[i,"id"])),]
    annotations[i,"from"] <- token_single_doc[as.numeric(annotations[i,"from"]),"idx"]
    annotations[i,"to"] <- (token_single_doc[as.numeric(annotations[i,"to"]),"idx"]+nchar(token_single_doc[as.numeric(annotations[i,"to"]),"word"])-1)
  }
  return(annotations)
}



transform_character_offset_to_word_offset <- function(token, annotations){
  # convert character annotations to word annotations
  for( i in 1:nrow(annotations)){
    annotations[i,"from"] <- max(which(token$idx<=as.numeric(annotations[i,"from"])))
    annotations[i,"to"] <- max(which(token$idx<=as.numeric(annotations[i,"to"])))
  }
  return(annotations)
}