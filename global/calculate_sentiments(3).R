#laden der deutschen Listen
SWS_neg <- read.delim("SentiWS_v1.8c_Negative.txt", header=FALSE, stringsAsFactors=FALSE)
SWS_pos <- read.delim("SentiWS_v1.8c_Positive.txt", header=FALSE, stringsAsFactors=FALSE)
#laden der engliśchen Listen
senti_eng <- read.delim("AFINN-111.txt", header=FALSE, stringsAsFactors=FALSE)
senti_eng[,2]<-senti_eng[,2]/5
senti_pos<-senti_eng[which(senti_eng[,2]>0),]
senti_neg<-senti_eng[which(senti_eng[,2]<0),]


#Pos tag information entfernen
SWS_neg[,1]<-stringr::str_replace_all(string = SWS_neg[,1],pattern = "\\|[A-Za-z]+",replacement = "")
SWS_pos[,1]<-stringr::str_replace_all(string = SWS_pos[,1],pattern = "\\|[A-Za-z]+",replacement = "")
#prepare sentiWS data for sentiment_score function
for(i in 1:dim(SWS_neg)[1]){
  Flektionen<-as.character(SWS_neg[i,3])
  if(nchar(Flektionen)>0){
    Worte<-stringr::str_split(string = Flektionen,pattern = ",",simplify = T)
    add_matrix<-matrix(c(""),length(Worte),3)
    add_matrix[,1]<-Worte
    add_matrix[,2]<-SWS_neg[i,2]
    SWS_neg<-rbind(SWS_neg,add_matrix)
  }
}
SWS_neg<-SWS_neg[,1:2]


for(i in 1:dim(SWS_pos)[1]){
  Flektionen<-as.character(SWS_pos[i,3])
  if(nchar(Flektionen)>0){
    Worte<-stringr::str_split(string = Flektionen,pattern = ",",simplify = T)
    add_matrix<-matrix(c(""),length(Worte),3)
    add_matrix[,1]<-Worte
    add_matrix[,2]<-SWS_pos[i,2]
    SWS_pos<-rbind(SWS_pos,add_matrix)
  }
}
SWS_pos<-SWS_pos[,1:2]

#hilfsvariablen löschen
rm(add_matrix)
rm(Worte)
rm(Flektionen)

calcuate_sentiment_score<-function(text, pos_dict, neg_dict){
  #split text
  text<- stringr::str_remove_all(string = text,pattern = "[,.:;!?]")
  worte<-stringr::str_split(string = text,pattern = " ",simplify = T)
  idx_pos<-which(pos_dict[,1]%in%worte)
  idx_neg<-which(neg_dict[,1]%in%worte)
  scores_pos<-as.numeric(pos_dict[idx_pos,2])
  scores_neg<-as.numeric(neg_dict[idx_neg,2])
  all_scores<-c(scores_pos,scores_neg)
  result<-(sum(all_scores))/length(all_scores)
  return(result)
}


text1<-"Christian Kahmann findet die schöne Akademie super spitze. Es ist eine rundum gelungene Veranstaltung."
text2<-"Arbeiten am Nachmittag ist bei der Musik auf der Terasse eine absolute Katastrophe."


calcuate_sentiment_score(text = text1,pos_dict = SWS_pos,neg_dict = SWS_neg)

calcuate_sentiment_score(text = text2,pos_dict = SWS_pos,neg_dict = SWS_neg)





#bezüglich des datums aufsplitten

dates<-substring(as.character(meta[,"date"]),1,7)
unique_dates<-unique(dates)
results<-matrix(c(0),length(unique_dates),2)

for(i in 1:length(unique_dates)){
  idx<-which(dates==unique_dates[i])
  results_for_date<-list()
  for(j in 1:length(idx)){
    results_for_date[[j]]<-calcuate_sentiment_score(text = meta[idx[j],"body"],pos_dict = senti_pos,neg_dict = senti_neg)
  }
  results[i,1]<-mean(na.omit(unlist(results_for_date)))
  results[i,2]<-length(idx)
  print(i)
}


p<-plot_ly(x=unique_dates,y=results[,1],type="scatter",mode="markers",name="avg. sentiment scores",marker = list(
  color = 'forestgreen',
  size = 12,
  line = list(
    color = 'black',
    width = 2
  )))
p<-plotly::add_trace(p = p,x=unique_dates,y=results[,2],mode="markers",name="number of documents found",yaxis="y2",marker = list(
  color = 'maroon',
  size = 12,
  line = list(
    color = 'red',
    width = 2
  )
))
p<-layout(p = p,legend=list(orientation="h",yanchor="bottom",xanchor="center",x=0.5,y=1),margin=list(r=50,b=150),yaxis2=list(rangemode="tozero",tickfont = list(color = "red"),overlaying = "y",side = "right",title = "number of documents found"),
          yaxis=list(rangemode = "tozero",title="Sentiment Score",type="linear",showgrid=T,showgrid = TRUE,showline = FALSE,showticklabels = TRUE,tickcolor = 'rgb(127,127,127)', ticks = 'outside', zeroline = T))
p


