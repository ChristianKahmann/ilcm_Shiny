
#library(GuardianR)
#library(stringr)
#library(xml2)


crawl_guardian<-function(key,from,to,keyword=""){

  results<-get_guardian(keywords = keyword,from.date = from,to.date = to,api.key = key)
  Text<-vector(mode = "list",length=dim(results)[1])
  data<-matrix(c(0),dim(results)[1],6)
  colnames(data)<-c("title","section","date","url","id","text")
  for (i in 1:dim(results)[1])
  {
    Text[[i]]<-(paste(levels(results[,"webTitle"])[i],levels(results[i,"trailText"])[i],results[i,"body"],sep = ". "))
    
    data[i,1]<-as.character(results[i,"webTitle"])
    data[i,2]<-as.character(results[i,"sectionName"])
    data[i,3]<-as.character(results[i,"webPublicationDate"])
    data[i,4]<-as.character(results[i,"webUrl"])
    data[i,5]<-as.character(results[i,"id"])
    data[i,6]<-as.character(Text[[i]])
    data[i,6]<-xml2::xml_text(xml2::read_html(iconv(paste("<p>",data[i,6],"</p>"),"ISO-8859-1","UTF-8")))
  }
  return(data)
}














