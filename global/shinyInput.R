shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}


shinyInput_big <- function(FUN, len, id, ...) {
  inputs <- character(len)
  inputs[1]<-as.character(FUN(paste0(id, 1), ...))
  if(len>1){
    for (i in 2:len) {
      inputs[i] <- stringr::str_replace_all(string = inputs[1],pattern ="_1",replacement = paste0("_",i) )
    }
  }
  inputs
}
