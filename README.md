[![Binder](https://notebooks.gesis.org/binder/badge_logo.svg)](https://notebooks.gesis.org/binder/v2/gh/ChristianKahmann/LCM_Binderhub/master)

# Note
This is just the R-Source Code for the Shiny-App. If you want to install the iLCM on your system please use the dockerized version. 

# Install
There is a standalone iLCM version avaiable at https://hub.docker.com/r/ckahmann/ilcm.

To use this version locally one has to install docker before (https://docs.docker.com/install/).
Having done this, the command 

`docker run -it -d -p 3838:3838 -p 8787:8787 -p 8983:8983 ckahmann/ilcm:latest` 

does pull the image from dockerhub and starts a container hosting the needed services.

One can set port mappings to diffrent ports on their local system.
Per default the 3 services are available on this ports:
* **shiny-server**: http://localhost:3838 
* **rstudio-server**: http://localhost:8787 
* **solr-dashboard**: http://localhost:8983 
