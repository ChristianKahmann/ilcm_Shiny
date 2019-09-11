read_data<-function(path,directory=FALSE,datatype="*"){

  if (directory==FALSE){
    if(tools::file_ext(path)=="csv"|tools::file_ext(path)=="txt")
    {
      data<-data.table::fread(path)
    }
    else{
    data<-(readtext::readtext(path))
    }
    date<-file.mtime(path)
    return(data.table(cbind(data,date)))
  }
  else{
    
    data_direct<-list()
    files<-paste(path,list.files(path,pattern = paste(".",datatype,"$",sep="")),sep = "/")
    count=0
    for (file in files){
      count=count+1
      if(tools::file_ext(file)=="csv"|tools::file_ext(file)=="txt")
      {
        data<-data.table::fread(file)
        date<-file.info(file)$mtime
        data_direct[[count]]<-data.table(cbind(data,date))
      }
      else{
        data<-(readtext::readtext(file))
        date<-file.info(file)$mtime
        data.table(cbind(data,date))
        data_direct[[count]]<-data.table(cbind(data,date))
      }
      
    }
    return(data_direct) 
  }  
}   