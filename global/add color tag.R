#' add_colortag
#' @param text
#' @param color
#' 
#' @return marked_text
#' 
#' @export
#' @example 
add_colortag<-function(text,color){
  marked_text<-paste("<u style='color:",color,"'>",text,"</u>",sep = "")
  return(marked_text)  
}

#' add_marktag
#' @param text
#' 
#' @return marker tag-name
#' 
#' @export
#' @example 
add_marktag<-function(text){
  return(paste("<mark>",text,"</mark>",sep = ""))
}


#' add_Entitytag
#' @param text
#' @param tag
#' 
#' @return highlighted entities 
add_Entitytag<-function(text,tag){
  return(paste0("<span style='background-color:#7FDBFF;'>",text,"</span>"))
}




#' add_Entitytag_alt
#' @param text
#' @param tag
#' 
#' @return 
#' @export
#' @example  
add_Entitytag_alt<-function(text,tag){
  switch(tag,
         PERSON={return(paste0("<span style='background-color:#7FDBFF;'>",text,"</span>"))
         },
         NORP={return(paste0("<span style='background-color:#7FDBFF;'>",text,"</span>"))
         },
         FACILITY={return(paste0("<span style='background-color:#7FDBFF;'>",text,"</span>"))
         },
         ORG={return(paste0("<span style='background-color:#7FDBFF;'>",text,"</span>"))
         },
         GPE={return(paste0("<span style='background-color:#7FDBFF;'>",text,"</span>"))
         },
         PRODUCT={return(paste0("<span style='background-color:#7FDBFF;'>",text,"</span>"))
         },
         EVENT={return(paste0("<span style='background-color:#7FDBFF;'>",text,"</span>"))
         },
         WORK_OF_ART={return(paste0("<span style='background-color:#7FDBFF;'>",text,"</span>"))
         },
         LAW={return(paste0("<span style='background-color:#7FDBFF;'>",text,"</span>"))
         },
         LANGUAGE={return(paste0("<span style='background-color:#7FDBFF;'>",text,"</span>"))
         },
         DATE={return(paste0("<span style='background-color:#7FDBFF;'>",text,"</span>"))
         },
         TIME={return(paste0("<span style='background-color:#7FDBFF;'>",text,"</span>"))
         },
         PERCENT={return(paste0("<span style='background-color:#7FDBFF;'>",text,"</span>"))
         },
         QUANTITY={return(paste0("<span style='background-color:#7FDBFF;'>",text,"</span>"))
         },
         MONEY={return(paste0("<span style='background-color:#7FDBFF;'>",text,"</span>"))
         },
         ORDINAL={return(paste0("<span style='background-color:#7FDBFF;'>",text,"</span>"))
         },
         CARDINAL={return(paste0("<span style='background-color:#7FDBFF;'>",text,"</span>"))
         }
  )
}

#' add_tags
#' @param x
#' @param annos
#' @param pos
#' @param ner
#' @param space
#' @param anno_highlight
#' 
#' @return x (visualization of annotations)
add_tags<-function(x,annos,pos,ner,space,anno_highlight){
  count=0
  if(dim(annos)[1]>0){
    for(i in as.numeric(annos[,1])){
      count<-count+1
      x[i]<-paste0('<b style="border-bottom: 2px solid ',annos[count,4],';" title="',annos[count,2],' (annotated by ',annos[count,3],')"','>',x[i],'</b>')
    }
  }
  for(i in pos){
    x[i]<-paste0('<mark>',x[i],'</mark>')
  }
  
  for(i in ner){
    x[i]<-paste0('<font style="background-color:#7FDBFF;">',x[i],'</font>')
  }
  for(i in space){
    x[i]<-paste0(x[i],'<br>')
  }
  for(i in 1:length(anno_highlight[,1])){
    x[as.numeric(anno_highlight[i,1])]<-paste0('<font style="background-color:',anno_highlight[i,2],';">',x[as.numeric(anno_highlight[i,1])],'</font>')
  }
  return(x)
  
}

