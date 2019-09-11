load("~/Schreibtisch/import GuardiantoMariaDB.RData")
dim(data)

data<-paragraphs
colnames(data)
d<-rep("WP",dim(data)[1])
data[1,]
data[1,4]
data[1,5]
d2<-1:dim(data)[1]
d3<-data[,"section"]
d4<-data[,"name"]
d5<-data[,"date"]
#d6<-data[,"body"]
d6<-books[,3]
#d6<-data[,"text"]
d7<-rep("0",dim(data)[1])
d8<-nchar(d6)
d9<-rep("0",dim(data)[1])
d10<-rep("book",dim(data)[1])
d11<-rep("eng",dim(data)[1])

data_g<-cbind(d,d2)
data_g<-cbind(data_g,d3)
data_g<-cbind(data_g,d4)
data_g<-cbind(data_g,d5)
data_g<-cbind(data_g,d6)
data_g<-cbind(data_g,d7)
data_g<-cbind(data_g,d8)
data_g<-cbind(data_g,d9)
data_g<-cbind(data_g,d10)
data_g<-cbind(data_g,d11)

write.table(x = data_g,file = "documents_WP.csv",row.names = F,col.names = F,sep = ",")

token_g<-token
token_g<-cbind(rep("WP",dim(token)[1]),token_g)
token_g<-cbind(token_g,1:dim(token_g)[1])

write.table(x = token_g,file = "token_WP.csv",row.names = F,col.names = F,sep = ",")






cleanNLP::init_spaCy()
 load("~/R/Shiny-iLCM/collections/data/Harry_Potter.RData")
 text<-books[,3]
 dataset="HP"
 dataset_file="HP"
 offset=0
 metadata<-paragraphs

books[,3]<-stringr::str_replace_all(string = books[,3],pattern = "\n",replacement = " ")

cleanNLP::init_spaCy()


add_data_to_DB<-function(text,metadata,dataset,dataset_file,date_format="%Y-%m-%d",offset=0){
print("preprocess text")  
#token<-cleanNLP::get_token(cleanNLP::run_annotators(input = text,as_strings = TRUE,doc_id_offset = offset))
  token<-spacyr::spacy_parse(text[1],pos = T,tag = F,lemma = T,entity = T,dependency = F)
  token[,1]<-stringr::str_replace_all(string = token[,1],pattern = "text",replacement = "")
  token[,1]<-as.numeric(token[,1])+offset
split<-split(2:length(text), ceiling(seq_along(2:length(text))/100))
count=0
for(i in split){
  count=count+1
  toks<-spacyr::spacy_parse(text[i],pos = T,tag = F,lemma = T,entity = T,dependency = F)
  toks[,1]<-stringr::str_replace_all(string = toks[,1],pattern = "text",replacement = "")
  toks[,1]<-as.numeric(toks[,1])+offset+1+((count-1)*100)
  token<-rbind(token,toks)
  print(paste(count,"of:",length(split)))
  if(count%%10==0)gc()
}  
  
#token<-spacyr::spacy_parse(text[1],pos = T,tag = T,lemma = T,entity = T,dependency = T)

#for(j in 2:length(text)){
#token<-rbind(token,spacyr::spacy_parse(text[j],pos = T,tag = T,lemma = T,entity = T,dependency = T))
#print(j)
#}
token<-token[,c("doc_id","sentence_id","token_id","token","lemma","pos","entity")]
token[which(token[,6]=="SPACE"),4:5]<-""
#token[,1]<-stringr::str_replace_all(string = token[,1],pattern = "text",replacement = "")
#token[,1]<-as.numeric(token[,1])+offset
token<-cbind(rep(dataset,dim(token)[1]),token)
#token<-cbind(token,rep(0,dim(token)[1]))
#token<-cbind(token,rep(0,dim(token)[1]))
token<-cbind(token,1:dim(token)[1])
print("text preprocessed")
print("preparing metadata")
meta<-matrix(c(0),dim(metadata)[1],11)
meta[,1]<-dataset
#meta[,2]<-metadata[,"id"]
meta[,2]<-(offset+1):(offset+length(text))
meta[,3]<-metadata[,"section"]
meta[,4]<-metadata[,"title"]
print("converting dates")
meta[,5]<-as.character(as.Date(metadata[,"date"],format ="%Y-%m-%d" ))
print(meta[1,5])
meta[,6]<-text
meta<-cbind(meta,rep(0,dim(meta)[1]))
count=0
for(j in unique(token[,2])){
  count=count+1
  toks<-token[which(token[,2]==j),c("doc_id"  ,    "sentence_id", "token_id"  ,  "token"    ,   "lemma"     ,  "pos"    ,     "entity")]
  class(toks)<-c("spacyr_parsed","data.frame")
  entities<-unique(spacyr::entity_extract(toks,type = "named")[,3])
  entities<-stringr::str_replace_all(string = entities,pattern = " ",replacement = "_")
  entities<-paste(entities,collapse=" ")
  meta[count,dim(meta)[2]]<-entities
}
#meta[,7]<-metadata[,"publisher"]

meta[,8]<-unlist(lapply(X = text,FUN = function(x){length(stringr::str_split(string = x,pattern = " ",simplify = T))}))
#meta[,9]<-metadata[,"author"]

#meta[,10]<-metadata[,"type"]
meta[,10]<-"article"
#meta[,11]<-metadata[,"language"]
meta[,11]<-"eng"
print("metadata finished")
print("save as csv")
print(max(token[,2]))
write.table(x = meta,file = paste("meta_",dataset_file,".csv",sep=""),row.names = F,col.names = F,sep = ",")
write.table(x = token,file = paste("token_",dataset_file,".csv",sep=""),row.names = F,col.names = F,sep = ",")
print("save data to db")
return(max(token[,2]))
}


load("~/R/Crawler/guardian_data/data_GU1999_1.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "1999_1_GU",offset = 0)

load("~/R/Crawler/guardian_data/data_GU1999_2.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "1999_2_GU",offset = offs)

load("~/R/Crawler/guardian_data/data_GU2000_1.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2000_1_GU",offset = offs)

load("~/R/Crawler/guardian_data/data_GU2000_2.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2000_2_GU",offset = offs)
print(offs)
gc()
load("~/R/Crawler/guardian_data/data_GU2001_1.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2001_1_GU",offset = offs)
print(offs)
gc()
load("~/R/Crawler/guardian_data/data_GU2001_2.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2001_2_GU",offset = offs)
print(offs)
gc()


load("~/R/Crawler/guardian_data/data_GU2002_1.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2002_1_GU",offset = offs)
print(offs)
gc()
load("~/R/Crawler/guardian_data/data_GU2002_2.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2002_2_GU",offset = offs)
print(offs)
gc()


load("~/R/Crawler/guardian_data/data_GU2003_1.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2003_1_GU",offset = offs)
print(offs)
gc()
load("~/R/Crawler/guardian_data/data_GU2003_2.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2003_2_GU",offset = offs)
print(offs)
gc()


load("~/R/Crawler/guardian_data/data_GU2004_1.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2004_1_GU",offset = offs)
print(offs)

load("~/R/Crawler/guardian_data/data_GU2004_2.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2004_2_GU",offset = offs)
print(offs)

gc()

load("~/R/Crawler/guardian_data/data_GU2005_1.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2005_1_GU",offset = offs)
print(offs)

load("~/R/Crawler/guardian_data/data_GU2005_2.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2005_2_GU",offset = offs)
print(offs)

gc()

load("~/R/Crawler/guardian_data/data_GU2006_1.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2006_1_GU",offset = offs)
print(offs)

load("~/R/Crawler/guardian_data/data_GU2006_2.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2006_2_GU",offset = offs)
print(offs)

gc()

load("~/R/Crawler/guardian_data/data_GU2007_1.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2007_1_GU",offset = offs)
print(offs)

load("~/R/Crawler/guardian_data/data_GU2007_2.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2007_2_GU",offset = offs)
print(offs)



load("~/R/Crawler/guardian_data/data_GU2008_1.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2008_1_GU",offset = offs)
print(offs)

load("~/R/Crawler/guardian_data/data_GU2008_2.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2008_2_GU",offset = offs)
print(offs)

gc()

load("~/R/Crawler/guardian_data/data_GU2011_1.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2011_1_GU",offset = offs)
print(offs)

load("~/R/Crawler/guardian_data/data_GU2011_2.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2011_2_GU",offset = offs)
print(offs)

gc()
load("~/R/Crawler/guardian_data/data_GU2012_1.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2012_1_GU",offset = offs)
print(offs)

load("~/R/Crawler/guardian_data/data_GU2012_2.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2012_2_GU",offset = offs)
print(offs)
gc()

load("~/R/Crawler/guardian_data/data_GU2013_1.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2013_1_GU",offset = offs)
print(offs)

load("~/R/Crawler/guardian_data/data_GU2013_2.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2013_2_GU",offset = offs)
print(offs)
gc()

load("~/R/Crawler/guardian_data/data_GU2014_1.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2014_1_GU",offset = offs)
print(offs)

load("~/R/Crawler/guardian_data/data_GU2014_2.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2014_2_GU",offset = offs)
print(offs)

gc()
load("~/R/Crawler/guardian_data/data_GU2015_1.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2015_1_GU",offset = offs)
print(offs)

load("~/R/Crawler/guardian_data/data_GU2015_2.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2015_2_GU",offset = offs)
print(offs)

gc()
load("~/R/Crawler/guardian_data/data_GU2016_1.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2016_1_GU",offset = offs)
print(offs)

load("~/R/Crawler/guardian_data/data_GU2016_2.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2016_2_GU",offset = offs)
print(offs)


load("~/R/Crawler/guardian_data/data_GU2017_1.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2017_1_GU",offset = offs)
print(offs)
gc()
load("~/R/Crawler/guardian_data/data_GU2017_2.RData")
offs<-add_data_to_DB(text = data[,6],metadata = data,dataset = "GU",dataset_file = "2017_2_GU",offset = offs)
print(offs)
1363935

