library(httr)
library(caTools)
setwd("/cloud/project/travel-costs")

tix_id="MTA4MzkxMTZ8MTY2NjY0MDc1Mi40ODQ4NTEx"
tix_secret="d1495b37273ded287a979ee285e9a561a527fd4bba0534118aed3ce95e85f4fa"

omaha=c(41.25203632458333, -96.03731122497379)
usa=c(53.382808, 24.521208, -66.945392, -124.736342)

event_url=paste0("https://api.seatgeek.com/2/events?client_id=",tix_id,"&client_secret=",tix_secret,"&taxonomies.name=sports&sort=score.desc&per_page=5000&lat=",omaha[1],"&lon=",omaha[2],"&range=2500km")
venue_url=paste0("https://api.seatgeek.com/2/venues?client_id=",tix_id,"&client_secret=",tix_secret,"&taxonomies.name=sports&sort=score.desc&per_page=5000&lat=",omaha[1],"&lon=",omaha[2],"&range=2500km")
tax_url=paste0("https://api.seatgeek.com/2/taxonomies?client_id=",tix_id,"&client_secret=",tix_secret)

km=function(lat1,long1,lat2,long2) {mk=6371*acos(sin(lat1*pi/180)*sin(lat2*pi/180)+cos(lat1*pi/180)*cos(lat2*pi/180)*cos(abs(long1-long2)*pi/180)); mk[is.na(mk)]=1e9; mk}
mi=function(lat1,long1,lat2,long2) 0.621371*km(lat1,long1,lat2,long2)

tax1=GET(tax_url); tax2=content(tax1); name=as.character(sapply(tax2$taxonomies,"[","name")); short=as.character(sapply(tax2$taxonomies,"[","short_name")); short[short=="NULL"]=NA
id=as.numeric(sapply(tax2$taxonomies,"[","id"))
slug=as.character(sapply(tax2$taxonomies,"[","slug"))
parent=as.numeric(as.character(sapply(tax2$taxonomies,"[","parent_id")))
visible=as.logical(sapply(tax2$taxonomies,"[","is_visible"))
sports=1000000; for(i in 1:3) sports=unique(c(sports,id[parent%in%sports])); name=ifelse(parent==1000000,paste0("#",name),name)
tax=data.frame(id,name,slug,parent,short)[id%in%sports[-1] & visible,]
tax$major=tax$slug%in%c("mlb","nfl","nba","nhl","mls","ncaa_football","ncaa_basketball")

tt=function(x) format(x,"%Y-%m-%dT%H:%M:%S"); uu=function(x) format(x,"%Y-%m-%dT%H:%M")


tids=paste(tax$id[tax$major],collapse=",")
url=paste0(event_url,"&datetime_local.gt=",tt(Sys.Date()),"&datetime_local.lt=",tt(Sys.Date()+40),"&taxonomies.id=",tids)
d1=GET(url); d2=content(d1); d3=d2$events

id=unlist(sapply(d3,"[","id"))
time1=strptime(as.character(sapply(d3,"[","datetime_local")), "%Y-%m-%dT%H:%M:%S",tz="UTC")
date=as.Date(time1); time=format(time1,"%I:%M %P")
who=sapply(d3,"[","performers"); tm=function(i){xi=sapply(who,"[",i); xi[sapply(xi,is.null)]=NA; as.character(sapply(xi,"[","name"))}
t1=tm(1); t2=tm(2); t3=tm(3); t4=tm(4)
league=as.character(sapply(d3,"[","type"))
full=as.character(sapply(d3,"[","title"))
title=as.character(sapply(d3,"[","short_title"))
ven=sapply(d3,"[","venue"); venue=as.character(sapply(ven,"[","name")); ven_id=as.numeric(sapply(ven,"[","id"))
loc=sapply(ven,"[","location"); lat=as.numeric(sapply(loc,"[","lat")); long=as.numeric(sapply(loc,"[","lon"))
score=as.numeric(sapply(d3,"[","score"))
city=as.character(sapply(ven,"[","display_location"))
stats=sapply(d3,"[","stats"); low=as.numeric(as.character(sapply(stats,"[","lowest_price")))
med=as.numeric(as.character(sapply(stats,"[","median_price"))); high=as.numeric(as.character(sapply(stats,"[","highest_price")))

d=data.frame(id,time,date,city,league,title,full,venue,ven_id,score,lat,long,t1,t2,t3,t4,low,med,high)[!is.na(med),]
d=merge(d,data.frame(league=slug,name=ifelse(is.na(short),name,short),xname=name),sort=F)
d=d[d$score>0 & !grepl("SUITE|VIP",d$title),]

nx=paste(names(d),collapse="|")
bx=apply(d, 1, paste, collapse="|")
dx=paste(c(nx,bx),collapse="\n")

tok="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"
gh="https://api.github.com/repos/charcoalcharts-open/travel-costs/contents/data/upcoming-tickets.csv"
sha=content(GET(gh))$sha
content=base64encode(dx)
body=paste0("{\"message\":\"Tickets\",\"content\":\"",content,"\",\"sha\":\"",sha,"\"}")
PUT(gh,body=body,add_headers(Authorization=paste("Bearer",tok)))





