library(rvest)
library(jsonlite)
library(httr)
library(caTools)
library(magick)
library(rsvg)
library(dplyr)
library(showtext)
library(base64enc)

tmp=tempfile()
download.file("https://www.1001fonts.com/download/typo-grotesk-rounded.zip",tmp)
unzip(tmp,exdir="fonts")

font_add("B","fonts/Typo Grotesk Rounded Demo.otf")
showtext_auto()

a="https://www.olympics.com/en/olympic-games" %>% read_html %>% html_nodes("script[type='application/json']") %>% html_text %>% fromJSON

game=data.frame(
name=a$props$pageProps$olympicGamesNoYog$name,
year=a$props$pageProps$olympicGamesNoYog$year,
country=a$props$pageProps$olympicGamesNoYog$location,
season=a$props$pageProps$olympicGamesNoYog$season,
slug=a$props$pageProps$olympicGamesNoYog$meta$slug)
game$sport=NA; class(game$sport)="list"

for(j in 1:nrow(game)){ og=game$slug[j]
b=og %>% paste0("https://www.olympics.com/en/olympic-games/",.,"/results") %>% read_html %>% html_nodes("script[type='application/json']") %>% html_text %>% fromJSON
game$sport[[j]]=data.frame(gid=j,
name=b$props$pageProps$olympicGame$disciplines$title,
slug=b$props$pageProps$olympicGame$disciplines$slug,
id=b$props$pageProps$olympicGame$disciplines$sportDisciplineId)
game$sport[[j]]$gold=NA; class(game$sport[[j]]$gold)="list"

for(k in 1:nrow(game$sport[[j]])){ dx=game$sport[[j]]$slug[k]
d=paste0("https://www.olympics.com/en/olympic-games/",og,"/results/",dx) %>% read_html %>% html_nodes("script[type='application/json']") %>% html_text %>% fromJSON
game$sport[[j]]$gold[[k]]=d$props$pageProps$gameDiscipline$events$awards %>% sapply(function(x) x$participant$country$triLetterCode[1])
}; print(game$name[j]); flush.console()}

json=game %>% toJSON %>% as.character


tok=paste0("ghp_",gsub("@","",creds[4]))
gh="https://api.github.com/repos/charcoalcharts-open/travel-costs/contents/olympics/games.json"
sha=content(GET(gh))$sha
content=caTools::base64encode(json)
body=paste0("{\"message\":\"",nrow(game)," games\",\"content\":\"",content,"\",\"sha\":\"",sha,"\"}")
PUT(gh,body=body,add_headers(Authorization=paste("Bearer",tok)))

sp=game$sport %>% do.call(rbind,.); sp$id=sp$id %>% strsplit("\\$|-") %>% sapply("[",2) %>% tolower; sp$id[sp$id=="sof"]="bsb"
sz="cma,msp,eva,mbo,jdp,raq,roq" %>% strsplit(",") %>% unlist


"https://www.olympics.com/images/static/b2p-images/logo_color.svg" %>% rsvg_raw(width=1000) %>% image_read %>% image_colorize(100,gray(.4)) %>% image_composite(image_blank(1024,1024),.,operator="Add",gravity="Center") %>% image_background(gray(.5)) %>% image_write("rings.png")
files = list(file=upload_file("rings.png"))
res <- httr::POST(url = "https://photomania.net/upload/file", body = files, encode = "multipart")
p=content(res) %>% fromJSON
data = list(
  photoId = p$id,
  effectId = "520fe4df92237beb771617d8" #retro black
)
res <- httr::POST(url = "https://photomania.net/render", body = data, encode = "form")
rings=content(res)$url_secure %>% image_read


n=length(unique(sp$id))-length(sz)
u1=1; for(u in u1:n){
si=sp$id %>% unique %>% .[!.%in%sz] %>% .[u]
ti=sp$name[sp$id==si][1] %>% gsub("Artistic Swimming","Synchronized Swimming",.) %>% gsub(" Sevens","",.) %>% gsub("/"," & ",.) %>% gsub(" skat"," Skat",.)
if(ti=="Snowboard") ti="Snowboarding"; if(ti=="Hockey") ti="Field Hockey"
sm=paste0("https://gstatic.olympics.com/s1/t_1-1_64/static/light/pictograms/2022/",si,".svg") %>% rsvg_raw(width=400) %>% image_read
sg=sp$gold[sp$id==si] %>% unlist %>% table; if(length(sg)>1) sg=sg %>% sample
sy=sp$gid[sp$id==si] %>% unique %>% game$season[.] %>% table %>% rev
#s0="https://www.olympics.com/en/sports/" %>% read_html %>% html_nodes("react-component[data-sporttype='all']") %>% html_attr("data-sport-list") %>% fromJSON

######################### Plot Flags

b=ceiling(30*sg/max(sg))
a=b*3/2
xmax=201
ymax=201

g=expand.grid(1:xmax,1:ymax); names(g)=c("x","y")

g$rk=0; g$u=paste(g$x,g$y)
cx=mean(g$x); cy=mean(g$y)

legal=function(x,y,a,b) x+ceiling(a/2)<=xmax & x-ceiling(a/2)>=1 & y+ceiling(b/2)<=ymax & y-ceiling(b/2)>=1
outline=function(x,y,a,b){
  m1=aggregate(y,list(x),max); m2=aggregate(y,list(x),min); m3=aggregate(x,list(y),max); m4=aggregate(x,list(y),min)
  data.frame(x=c(m1[,1],m2[,1],m3[,2]+1+ceiling(a/2),m4[,2]-1-ceiling(a/2)),y=c(m1[,2]+1+ceiling(b/2),m2[,2]-1-ceiling(b/2),m3[,1],m4[,1]))}
free1=function(x,y,a,b){
  h=expand.grid(x--ceiling(a/2):ceiling(a/2),y--ceiling(b/2):ceiling(b/2)); names(h)=c("x","y")
  h$u=paste(h$x,h$y); !any(h$u%in%g$u[g$rk!=0])}
free=function(x,y,a,b,short=T){s=rep(NA,length(x))
if(exists("gu2") & short) if(length(gu2)>3) {s[paste(x,y)%in%gu2]=T; dz=(x-cx)**2+(y-cy)**2; m=min(dz[s],na.rm=T); s[dz>m & is.na(s)]=F} 
for(i in which(is.na(s))) s[i]=free1(x[i],y[i],a,b); s}
zmin=function(a,b,rk,short=T){ t0=Sys.time()
if(rk==1) go=data.frame(x=cx,y=cy) else go=outline(g$x[g$rk>0],g$y[g$rk>0],a,b); t1=Sys.time()
gl=go #lz=legal(go$x,go$y,a,b); gl=go[lz,]; t2=Sys.time()
fz=free(gl$x,gl$y,a,b,short); gf=gl[fz,]; t3=Sys.time()
dz=(gf$x-cx)**2+(gf$y-cy)**2
wz=which.min(dz); xz=gf$x[wz]; yz=gf$y[wz]
rz=g$x%in%(xz--ceiling(a/2):ceiling(a/2)) & g$y%in%(yz--ceiling(b/2):ceiling(b/2))
rz2=gf$x%in%(xz--ceiling(a):ceiling(a)) & gf$y%in%(yz--ceiling(b):ceiling(b))
gu2<<-paste(gf$x,gf$y)[!rz2]
if(sum(rz)>0) g$rk[rz]<<-rk; if(sum(rz)==0) stop<<-T} #; t4=Sys.time(); print(t4-t0)}


i=1; stop=F; while(i<=length(a) & !stop) {zmin(a[i],b[i],i,ifelse(i==1,T,a[i]<=a[i-1])); print(paste(i,"/",length(a),length(gu2))); flush.console(); i=i+1}

overlap=0
v=data.frame(x0=NA,y0=NA,x1=NA,y1=NA)[-1,]
for(i in 1:length(a)){
  h=g[g$rk==i,]
  v=rbind(v,data.frame(x0=mean(h$x)-a[i]/2-overlap,y0=mean(h$y)-b[i]/2-overlap,x1=mean(h$x)+a[i]/2+overlap,y1=mean(h$y)+b[i]/2+overlap))}

png(paste0("osports/",ti,".png"),1024,1024)
par(mar=rep(0,4),bg=gray(.5))
plot(c(1,xmax),c(1,ymax),type="n",xaxt="n",yaxt="n",xaxs="i",yaxs="i")
rings %>% rasterImage(1,1,xmax,ymax)
for(i in 1:nrow(v)) paste0("https://gstatic.olympics.com/s3/noc/oly/3x2/",names(sg)[i],".png") %>% image_read %>% rasterImage(v[i,1],v[i,2],v[i,3],v[i,4]) %>% try(silent=T)
par(mar=rep(0,4),fig=c(0,1,0,1),new=T)
plot(0:1,0:1,type="n",xaxt="n",yaxt="n",xaxs="i",yaxs="i")
sm %>% image_negate %>% rasterImage(0,.8,.2,1)
text(.99,.99,ti %>% gsub(" ","\n",.) %>% gsub("Short\nTrack","Short Track",.),cex=6,col="white",adj=c(1,1),family="B")
text(.01,.03,"All-Time Gold Medals",cex=2,col="white",adj=c(0,1),family="B")
text(.01,.055,sum(sg) %>% format(big.mark=","),cex=2,col="gold",adj=c(0,1),family="B")
w=names(sy)[1]=="Winter"
text(.99,.08,sy[1],adj=c(1,1),cex=2,col="white",family="B")
text(.99,.055,names(sy)[1],adj=c(1,1),cex=2,col=ifelse(w,"blue","red"),family="B")
text(.99,.03,"Games",adj=c(1,1),cex=2,col="white",family="B")
if(length(sy)==2){
  text(.99,.16,sy[2],adj=c(1,1),cex=2,col="white",family="B")
  text(.99,.135,names(sy)[2],adj=c(1,1),cex=2,col="red",family="B")
  text(.99,.11,"Games",adj=c(1,1),cex=2,col="white",family="B")
  lines(c(.99,.91),rep(.086,2))
}
dev.off()
}

############# Push

fi=list.files("osports")

for(i in 1:length(fi)){
  gh=paste0("https://api.github.com/repos/charcoalcharts-open/travel-costs/contents/olympics/sports/",fi[i]) %>% URLencode
  sha=content(GET(gh))$sha
  content=base64enc::base64encode(paste0("osports/",fi[i]))
  body=paste0("{\"message\":\"","sportbot"," games\",\"content\":\"",content,"\",\"sha\":\"",sha,"\"}")
  PUT(gh,body=body,add_headers(Authorization=paste("Bearer",tok)))
}
