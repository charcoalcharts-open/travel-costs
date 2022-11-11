library(httr)
library(caTools)
setwd("/cloud/project/travel-costs")


tt=function(x) format(x,"%Y-%m-%dT%H:%M:%S"); uu=function(x) format(x,"%Y-%m-%dT%H:%M")

pid="chipchristensenanalyticscollege"
key="lZbX9DMgM3UmFxiL7c7SOJbjujy7wlbL"

d=read.table("https://raw.githubusercontent.com/charcoalcharts-open/travel-costs/main/data/upcoming-tickets.csv",sep="|",quote="",header=T,fill=T)
d$date=as.Date(d$date); p=list(); p$lat=40; p$long=-83
as.num=function(x) as.numeric(gsub("\\$|,","",x)); dol=function(x) paste0("$",format(x,big.mark=","))
km=function(lat1,long1,lat2,long2) {mk=6371*acos(sin(lat1*pi/180)*sin(lat2*pi/180)+cos(lat1*pi/180)*cos(lat2*pi/180)*cos(abs(long1-long2)*pi/180)); mk[is.na(mk)]=1e9; mk}
mi=function(lat1,long1,lat2,long2) 0.621371*km(lat1,long1,lat2,long2)

d$mi=mi(p$lat,p$long,d$lat,d$long)


flight=function(latx1,longx1,latx2,longx2,d1=Sys.Date()+2,d2=Sys.Date()+4,id=1,sort="Luxury"){
  from=paste0(round(latx1,1),"-",round(longx1,1),"-100km")
  to=paste0(round(latx2,1),"-",round(longx2,1),"-100km")
  
  sx=""; if(sort=="Travel") sx="&sort=duration"; if(sort=="Luxury") sx="&sort=quality"
  url=paste0("https://api.skypicker.com/flights?partner=",pid,"&fly_from=",from,"&fly_to=",to,sx,"&arrive_after=",uu(d1),"&arrive_before=",uu(d1+1),"&rt_depart_after=",uu(d2),"&rt_depart_before=",uu(d2+1))
  h0=GET(url); h1=content(h0); if(length(h1$data)>0){ h=h1$data[[1]]; hr=h$route
  leg=0:length(hr); stops=length(hr)
  loc=c(paste0(as.character(sapply(hr,"[","cityFrom"))," (",as.character(sapply(hr,"[","flyFrom")),")")[1]
        ,paste0(as.character(sapply(hr,"[","cityTo"))," (",as.character(sapply(hr,"[","flyTo")),")"))
  lat=c(as.numeric(as.character(sapply(hr,"[","latFrom")))[1],as.numeric(as.character(sapply(hr,"[","latTo"))))
  long=c(as.numeric(as.character(sapply(hr,"[","lngFrom")))[1],as.numeric(as.character(sapply(hr,"[","lngTo"))))
  dest=loc==paste0(h$cityTo," (",h$flyTo,")")
  data.frame(id,leg,stops,loc,lat,long,price=h$price,dest)
  
  }}


f=data.frame(id=NA,leg=NA,stops=NA,loc=NA,lat=NA,long=NA,price=NA,dest=NA)[-1,]; w=which(d$mi>250)
for(j in w) {f=rbind(f,flight(p$lat,p$long,d$lat[j],d$long[j],d$date[j]-1,d$date[j]+1,d$id[j],"Luxury"))
print(paste(j,"out of",max(w))); flush.console()}


nx=paste(names(f),collapse="|")
bx=apply(f, 1, paste, collapse="|")
dx=paste(c(nx,bx),collapse="\n")

tok="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"
gh="https://api.github.com/repos/charcoalcharts-open/travel-costs/contents/data/upcoming-columbus-flights-by-luxury.csv"
sha=content(GET(gh))$sha
content=base64encode(dx)
body=paste0("{\"message\":\"Tickets\",\"content\":\"",content,"\",\"sha\":\"",sha,"\"}")
PUT(gh,body=body,add_headers(Authorization=paste("Bearer",tok)))

