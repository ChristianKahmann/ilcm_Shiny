#' Read in textual data
#'
#' @param path a path to a file or directory
#' @param datatype string to specify just certain file-formats, which should be imported
#' @return a list of data.tables containing the doc_id, text and date of the files
read_data<-function(path,datatype="*"){
  #check weather path contains a file or a directory
  directory<-FALSE
  if(substr(path,nchar(path),nchar(path))=="/"){
    directory<-TRUE
  }
  #file:
  if (directory==FALSE){
    #for csv and txt use data.table::fread because it can guess the delimiters
    if(tools::file_ext(path)=="csv"|tools::file_ext(path)=="txt"){
      data<-data.table::fread(path)
    }
    else{
    data<-(readtext::readtext(path))
    }
    date<-file.mtime(path)
    return(data.table::data.table(cbind(data,date)))
  }
  #directory:
  else{
    data_direct<-list()
    files<-paste(path,list.files(path,pattern = paste(".",datatype,"$",sep="")),sep = "/")
    count=0
    for (file in files){
      count=count+1
      if(tools::file_ext(file)=="csv"|tools::file_ext(file)=="txt"){
        data<-data.table::fread(file)
        date<-file.info(file)$mtime
        data_direct[[count]]<-data.table::data.table(cbind(data,date))
      }
      else{
        data<-(readtext::readtext(file))
        date<-file.info(file)$mtime
        data_direct[[count]]<-data.table::data.table(cbind(data,date))
      }
    }
    return(data_direct) 
  }  
}  