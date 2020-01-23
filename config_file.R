##no docker##
url<-'http://0.0.0.0:8983/solr/iLCM/select/'
update_solr_url<-'0.0.0.0'
update_solr_port<-'8983'
host<-'0.0.0.0'
version<-"0.993"
db_port='3306'
max_upload_file_size=35
#docker##
#url<-"http://tmca_solr-1_1:8081/solr/iLCM/select/"
#host = 'tmca_db_1'
#update_solr_url<-"tmca_solr-1_1"
#update_solr_port<-"8081"
#appearance: fancy or scientific
look<-"fancy"
plotly_colors<-c('#1f77b4','#ff7f0e','#2ca02c','#d62728','#9467bd','#8c564b','#e377c2','#7f7f7f','#bcbd22','#17becf')

c25 <- rainbow(25)
# url<-"http://localhost:3851/solr/iLCM/select/"
# update_solr_url<-"localhost"
# update_solr_port<-"3851"
# host<-'0.0.0.0'
# version<-"0.9.7"
# db_port="3852"


#set python path
Sys.setenv(PATH="/home/rstudio/miniconda3/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games")

#login credentials
library(sodium)
credentials = data.frame(
  username_id = c("rstudio"),
  passod   = sapply(c("rstudio"),password_store),
  permission  = c("basic"), 
  stringsAsFactors = F
)

# show Login Page?!
# specify output object thats need to rendered before loading page gets hidden
hide_login=TRUE
if(hide_login==TRUE){
  waiter_wait_object<-"datasets_avaiable"
}else{
  waiter_wait_object<-"body_UI"
}

