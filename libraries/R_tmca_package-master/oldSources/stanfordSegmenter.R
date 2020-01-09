#Process data with tika
# TIKA Skript
.libPaths(c("C:/_TOOLS/R_LIB"))

stanfordSegmenter <- function(file, sourceFolder, targetFolder){
  
  # Usage: "segment.bat|.sh [-k] ctb|pku filename encoding kBest"
  # -k   : keep whitespaces
  # ctb  : use Chinese Treebank segmentation
  # pku  : Beijing University segmentation
  # kBest: print kBest best segmenations; 0 means kBest mode is off.
  # 
  # Example: segment.bat ctb test.simp.utf8 UTF-8 0
  # Example: segment.bat pku test.simp.utf8 UTF-8 0
  
  if(file.exists(paste0(targetFolder, file, ".txt")))
    return()
  
  
  
  myFile <- paste0("\"",normalizePath(paste0("./",sourceFolder,"/",file)),"\"")
  cat("Segmenting from ", file, "...\n")
  #command <- paste("./stanfordSegmenter/segment.bat", "ctb", myFile,"UTF-8","0")
  #cat(command,"\n")
  
  
  
  if(.Platform$OS.type == "windows") {
    system2("./stanfordSegmenter/segment.bat",args=paste("ctb", myFile,"UTF-8","0"),stdout = paste0(targetFolder, file, ".txt"))
  } else {
    system2("./stanfordSegmenter/segment.sh",args=paste("ctb", myFile,"UTF-8","0"),stdout = paste0(targetFolder, file, ".txt"))
  }
  
  #system2("./stanfordSegmenter/segment.bat",args=paste("ctb", myFile,"UTF-8","0"),stdout = paste0(targetFolder, file, ".txt"))
  #output <- system(command, intern = T)
  #output <- iconv(output, to = "UTF-8")
  
  # Korrektur
  #dir.create(paste0(targetFolder, unlist(strsplit(file[1],split = "/"))[1]),recursive = T)
  #fileConn<-file(paste0(targetFolder, file, ".txt"), encoding = "UTF-8")
  #writeLines(output, fileConn)
  #close(fileConn)
  
}


