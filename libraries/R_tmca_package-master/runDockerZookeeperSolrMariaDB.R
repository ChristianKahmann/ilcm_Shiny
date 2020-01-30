ZK_CLUSTER_SIZE=1
SOLRCLOUD_CLUSTER_SIZE=1

Sys.setenv(HOME = "C:/Users/aniekler")
Sys.getenv("HOME")

DOCKER_BIN="docker"
DOCKER_COMPOSE_BIN="docker-compose"

SZD_DATA_DIR <- paste0(Sys.getenv("HOME"),"/.iLCM/","data")

APP= basename(getwd()) %>% stringr::str_to_lower() %>% stringr::str_replace_all(pattern="_",replacement="") #IMPORTANT

m_container_name="mariadb"
m_container_version="latest"

library(dplyr)
script = system2(command = DOCKER_BIN, args = "images",stdout = T)
there <- script %>% grep(pattern = paste0(m_container_name," "),value = T) %>% grepl(pattern = paste0(m_container_version," "))

if(length(there)<1)
  there <- F

if(!there)
  system2(command = DOCKER_BIN, args = paste0("pull"," ",m_container_name,":",m_container_version),stdout = T)

#CREATE Script/conf directory 
HOST_DATA=paste0(SZD_DATA_DIR,"/mariadb")
dir.create(paste0(HOST_DATA,"/myconfig"),recursive = T)
dir.create(paste0(HOST_DATA,"/docker-entrypoint-initdb.d"),recursive = T)

#Copy prepared configuration there
file.copy("./db/init_iLCM.sql",paste0(HOST_DATA,"/docker-entrypoint-initdb.d"),recursive = T)
f <- list.files(HOST_DATA, all.files = TRUE, full.names = TRUE, recursive = TRUE)
d <- list.dirs(HOST_DATA,recursive = T,full.names = T)
Sys.chmod(c(f,d), mode = "0777", use_umask = TRUE)

s_container_name="solr"
s_container_version="latest"

library(dplyr)
script = system2(command = DOCKER_BIN, args = "images",stdout = T)
there <- script %>% grep(pattern = paste0(s_container_name," "),value = T) %>% grepl(pattern = paste0(s_container_version," "))

if(length(there)<1)
  there <- F

if(!there)
  system2(command = DOCKER_BIN, args = paste0("pull"," ",s_container_name,":",s_container_version),stdout = T)

z_container_name="zookeeper"
z_container_version="latest"

script = system2(command = DOCKER_BIN, args = "images",stdout = T)
there <- script %>% grep(pattern = paste0(z_container_name," "),value = T) %>% grepl(pattern = paste0(z_container_version," "))

if(length(there)<1)
  there <- F

if(!there)
  system2(command = DOCKER_BIN, args = paste0("pull"," ",z_container_name,":",z_container_version),stdout = T)


# Need a volume to read the config from
conf_prefix <- "zoo-"
conf_container=paste0(conf_prefix,1)
cluster_size=ZK_CLUSTER_SIZE

for (i in 1:cluster_size)
{
  HOST_DATA=paste0(SZD_DATA_DIR,"/",conf_prefix,i)
  dir.create(paste0(HOST_DATA,"/logs"),recursive = T)
  dir.create(paste0(HOST_DATA,"/data"),recursive = T)
}

SOLR_HEAP=""
#SOLR_JAVA_MEM=$SOLRCLOUD_JVMFLAGS

# Start the solrcloud containers
HOST_PREFIX=paste0(s_container_name,"-")
  
for (i in 1:SOLRCLOUD_CLUSTER_SIZE)
{
  SOLR_HOSTNAME=paste0(HOST_PREFIX,i)
  HOST_DATA_DIR=paste0(SZD_DATA_DIR,"/",SOLR_HOSTNAME)
  
  dir.create(paste0(HOST_DATA_DIR,"/logs"),recursive = T)
  dir.create(paste0(HOST_DATA_DIR,"/store/solr"),recursive = T)
  dir.create(paste0(HOST_DATA_DIR,"/store/sql-driver"),recursive = T)
  dir.create(paste0(HOST_DATA_DIR,"/store/shared-lib"),recursive = T)
  
  file.copy("./solr/solr.xml",paste0(HOST_DATA_DIR,"/store/solr"),recursive = T)
  file.copy("./solr/docker-entrypoint-initdb.d",HOST_DATA_DIR,recursive = T)
  file.copy("./solr/mariadb-java-client-2.1.0.jar",paste0(HOST_DATA_DIR,"/store/sql-driver"),recursive = T)
  file.copy("./solr/config",paste0(HOST_DATA_DIR,"/store/solr"),recursive = T)
  
  Sys.chmod(HOST_DATA_DIR, mode = "0666", use_umask = TRUE)
  
}

NETWORK_NAME=paste0(APP,"_default")

script = system2(command = DOCKER_BIN, args = "network ls",stdout = T)
there <- any(script %>% grepl(pattern = NETWORK_NAME))

if(length(there)<1)
  there <- F

if(!there)
  system2(command = DOCKER_BIN, args = paste0("network create"," ",NETWORK_NAME),stdout = T)

system2(command = DOCKER_COMPOSE_BIN, args = "-f docker-compose.yml create",stdout = T)
print(system2(command = DOCKER_COMPOSE_BIN, args = "-f docker-compose.yml start",stdout = T))

# initial default zoo.cfg
ZKCLIENT_PORT=2181

zkhost=""
conf_prefix=paste0(".iLCM","_zoo-")
  
print("ZOO_SERVERS:")
for (i in 1:cluster_size)
{
  ZKCLIENT_PORT <- ZKCLIENT_PORT + 1
  print(paste0("localhost:",ZKCLIENT_PORT))
}

SOLR_PORT=8080
print("SOLR_SERVERS:")
for (i in 1:cluster_size)
{
  SOLR_PORT <- SOLR_PORT + 1
  print(paste0("localhost: ",paste0(".iLCM_",HOST_PREFIX,i,"_1")))
}

print("try connecting to http://localhost:8081/solr")


#UPLOAD NEW CONFIGURATION TO SOLR/CUSTOMIZE TO CONTAINER NAMES JUST CREATED
cat(system2(command = DOCKER_BIN, args = paste0("exec"," -it --user=solr rtmcapackage_solr-1_1 bin/solr zk upconfig -n iLCM -d /store/solr/config/iLCM"),stdout = T))

library(RCurl)
#Add Collection

#USE SOLR API TO CREATE COLLECTION
h = getCurlHandle()
#http://localhost:8081/solr/admin/collections?_=1502875245230&action=CREATE&collection.configName=iLCM&maxShardsPerNode=1&name=iLCM&numShards=1&replicationFactor=1&router.name=compositeId&routerName=compositeId&wt=json
z <- getURL(
paste0("http://localhost:8081/solr/admin/collections?action=CREATE&collection.configName=iLCM&maxShardsPerNode=1&name=iLCM&numShards=1&replicationFactor=1&router.name=compositeId&routerName=compositeId&wt=json"), 
followlocation=TRUE, curl=h)

#MORE STUFF FOR MORE CORES - JUST REPEAT EVERYTHING According to https://github.com/freedev/solrcloud-zookeeper-docker

print(system2(command = DOCKER_COMPOSE_BIN, args = "-f docker-compose.yml stop",stdout = T))
system2(command = DOCKER_COMPOSE_BIN, args = "-f docker-compose.yml down",stdout = T)
