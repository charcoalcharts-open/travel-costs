library(httr)
library(caTools)
setwd("/cloud/project/travel-costs")


d=read.table("https://raw.githubusercontent.com/charcoalcharts-open/travel-costs/main/data/upcoming-tickets.csv",sep="|",quote="",header=T,fill=T)
d$date=as.Date(d$date,"%Y-%m-%d")

km=function(lat1,long1,lat2,long2) {mk=6371*acos(sin(lat1*pi/180)*sin(lat2*pi/180)+cos(lat1*pi/180)*cos(lat2*pi/180)*cos(abs(long1-long2)*pi/180)); mk[is.na(mk)]=1e9; mk}
mi=function(lat1,long1,lat2,long2) 0.621371*km(lat1,long1,lat2,long2)

           
url="https://api.apify.com/v2/acts/jupri~kayak-hotels/run-sync-get-dataset-items?token=apify_api_RbyI2ymW77ddian3oLDzWkObu2BmQo4fV2KY&timeout=600"
           

hotel=function(i){
  
  di=dd[i,]

  b=paste0("
{
    \"check_in\": \"",di$date-1,"\",
    \"check_out\": \"",di$date+1,"\",
    \"limit\": 100,
    \"location\": \"",di$city,"\",
    \"price_mode\": \"total\",
    \"sort\": \"distance\"
}"); b=gsub("\n","",b)
             
             z0=POST(url,content_type_json(),body=b); z=content(z0)
             lx=sapply(z,"[","resultPoint")
             lat=unlist(sapply(lx,"[","lat")); long=unlist(sapply(lx,"[","lng"))
             name=unlist(sapply(z,"[","name")); price=unlist(sapply(z,"[","price"))
             #rating=unlist(sapply(sapply(z,"[","userRating"),"[","rating"))
 if(length(price)==100){   
    pfilt=rank(price)<=10; tfilt=rank(mi(di$lat,di$long,lat,long))<=10; lfilt=rank(-price)<=10
    data.frame(id=di$id,lat,long,name,price,pfilt,tfilt,lfilt)[pfilt|tfilt|lfilt,]}
}

dates=sort(unique(d$date))
for(k in 1:length(dates)){
dt=dates[k]; dd=d[d$date==dt,]

h=data.frame(id=NA,lat=NA,long=NA,name=NA,price=NA,pfilt=NA,tfilt=NA,lfilt=NA)[-1,]; n=nrow(dd)
for(j in 1:n) h=rbind(h,hotel(j))


nx=paste(names(h),collapse="|")
bx=apply(h, 1, paste, collapse="|")
dx=paste(c(nx,bx),collapse="\n")

tok="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"
gh=paste0("https://api.github.com/repos/charcoalcharts-open/travel-costs/contents/data/hotels-",dt,".csv")
sha=content(GET(gh))$sha
content=base64encode(dx)
body=paste0("{\"message\":\"Tickets\",\"content\":\"",content,"\",\"sha\":\"",sha,"\"}")
PUT(gh,body=body,add_headers(Authorization=paste("Bearer",tok)))

print(dt); flush.console()}

