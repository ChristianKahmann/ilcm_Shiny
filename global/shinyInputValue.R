shinyInput_checkbox = function(FUN, len, id, values,...) { 
  inputs = character(len) 
  for (i in seq_len(len)) { 
    inputs[i] = as.character(tags$div(id=paste0("dcbox_",i),FUN(paste0(id, i),value=values[i],...))) 
  } 
  inputs 
} 


shinyInput_checkbox_Doc = function(FUN, len, id, values,...) { 
  inputs = character(len) 
  for (i in seq_len(len)) { 
    inputs[i] = as.character(tags$div(id=paste0("Doccbox_",i),FUN(paste0(id, i),value=values[i],...))) 
  } 
  inputs 
} 


shinyValue = function(id, len) { 
  unlist(lapply(seq_len(len), function(i) { 
    value = input[[paste0(id, i)]] 
    if (is.null(value)) NA else value 
  }
  )
  )
}

shinyInput_material = function(FUN, len, id, values,status,...) { 
  inputs = character(len) 
  for (i in seq_len(len)) { 
    inputs[i] = as.character(tags$div(id=paste0("material_",i),FUN(paste0(id, i),value=values[i],status=status,...))) 
  } 
  inputs 
} 


