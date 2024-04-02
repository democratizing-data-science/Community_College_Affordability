### Code prepared to identify neighbors
packages = c("ncf", "GWmodel", "Boruta", "spdep", "spatialreg", "stringr", "plyr", "tigris", "igraph", "geosphere", "leaflet", "viridis", "stargazer", "tmap")#, "missMDA")
## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
options(tigris_use_cache = TRUE)


a<-getwd()
url18 <- paste('http://nces.ed.gov/ipeds/datacenter/data/HD2018.zip')
download.file(url18, destfile = paste(a,"HD2018.zip",sep="/"))
url18 <- paste('http://nces.ed.gov/ipeds/datacenter/data/IC2018.zip')
download.file(url18, destfile = paste(a,"IC2018.zip",sep="/"))
url18 <- paste('https://nces.ed.gov/ipeds/datacenter/data/SFA1819.zip')
download.file(url18, destfile = paste(a,"SFA1819.zip",sep="/"))
url18 <- paste('https://nces.ed.gov/ipeds/datacenter/data/IC2018_AY.zip')
download.file(url18, destfile = paste(a,"IC2018_AY.zip",sep="/"))
url18 <- paste('https://ed-public-download.app.cloud.gov/downloads/CollegeScorecard_Raw_Data_09262023.zip')#check name for updates at https://collegescorecard.ed.gov/data/
download.file(url18, destfile = paste(a,"CollegeScorecard_Raw_Data_09262023.zip",sep="/"))

#Researchers can use the files from College Score Card (CSC) or the ones stored in our server
#Given potential updates of CSC, we rely on our own files, but minimal code modifications are expected.
# sct0 <- read.csv(unz("CollegeScorecard_Raw_Data_01122021.zip", "Raw Data Files/MERGED2017_18_PP.csv"))
# sc <- read.csv(unz("CollegeScorecard_Raw_Data_01122021.zip", "Raw Data Files/MERGED2018_19_PP.csv"))

sct0 <- read.csv("https://raw.githubusercontent.com/democratizing-data-science/Community_College_Affordability/main/MERGED2017_18_PP.csv")

sc <- read.csv("https://raw.githubusercontent.com/democratizing-data-science/Community_College_Affordability/main/MERGED2018_19_PP.csv")

phu <- read.csv("https://raw.githubusercontent.com/democratizing-data-science/Community_College_Affordability/main/ruca_phudcfily.csv")

m1P <- read.csv("https://raw.githubusercontent.com/democratizing-data-science/Community_College_Affordability/main/m1p%2BPHUDCFILY.csv")

data1 <- read.csv(unz("IC2018.zip", "ic2018.csv"))#Inst characteristics
data2 <- read.csv(unz("SFA1819.zip", "sfa1819.csv"))#Inst AID

d18<- read.csv(unz("HD2018.zip", "hd2018.csv"))
d18<-d18[c(d18$SECTOR==2|d18$SECTOR==1|d18$SECTOR==3|d18$SECTOR==4), c("UNITID", "INSTNM", "COUNTYCD", "STABBR", "ZIP", "LONGITUD", "LATITUDE", "SECTOR", "CARNEGIE", "OBEREG", "OPEFLAG")]

"%ni%" <- Negate("%in%")
d18 <-d18[d18$STABBR  %ni% c('AK','GU', 'AS', 'FM','MH','MP','PW','VI','PR','HI'),] 

map_data<-d18
map_data<-map_data[map_data$OPEFLAG<5,] #remove no title IV
map_data$OPEFLAG<-NULL

dim(map_data)
table(map_data$SECTOR)

map_data <- merge(map_data, data1[,c("UNITID","OPENADMP")], by='UNITID',all.x=TRUE)
names(map_data)
#doctoral 6
map_data$DoctoralDummy<-0
map_data[map_data$CARNEGIE==15|map_data$CARNEGIE==16,]$DoctoralDummy<-1
table(map_data$DoctoralDummy)

#Open Admission
map_data <- map_data[!is.na(map_data$OPENADMP),]
map_data$OpenDoor<-0
map_data[map_data$OPENADMP==1|map_data$OPENADMP==-2,]$OpenDoor<-1
table(map_data$OpenDoor)

#Reducing zip to 5
summary(nchar(map_data$ZIP))
map_data$ZIP2<- as.character(substr(map_data$ZIP, 1, 5))
summary(nchar(map_data$ZIP2))

#Identifying rural areas using USAER
phu$FIPS<-str_pad(phu$FIPS, 5, pad = "0")
phu$Rural<-0
phu$Rural[phu$RUCA>=10] <- 1 #7 Urban population of 2,500 to 19,999, not adjacent to a metro area 
table(phu$Rural)
summary(nchar(map_data$COUNTYCD))
map_data$COUNTYCD<-str_pad(map_data$COUNTYCD, 5, pad = "0")

map_data$Rural_i<- phu$Rural[match(map_data$COUNTYCD,phu$FIPS)]
head(map_data[is.na(map_data$Rural_i),])
map_data$Rural_i[map_data$UNITID==219277]<-0 #Manually looked and found it is not rural

#Check for UNITID in college score data
map_data$net_price_pub <- as.numeric(sct0$NPT4_PUB[match(map_data$UNITID, sct0$UNITID)])
map_data$net_price_priv <- as.numeric(sct0$NPT4_PRIV[match(map_data$UNITID, sct0$UNITID)])
map_data$net_price_t0 <- ifelse(!is.na(map_data$net_price_pub), map_data$net_price_pub, map_data$net_price_priv)

###Current academic year net prices (2018-2019)
map_data$net_price_pub <- as.numeric(sc$NPT4_PUB[match(map_data$UNITID, sc$UNITID)])
map_data$net_price_priv <- as.numeric(sc$NPT4_PRIV[match(map_data$UNITID, sc$UNITID)])

map_data$net_price_t0 <- ifelse(is.na(map_data$net_price_t0)&(map_data$SECTOR==1|map_data$SECTOR==4), map_data$net_price_pub, map_data$net_price_t0)

map_data$net_price_t0 <- ifelse(is.na(map_data$net_price_t0)&(map_data$SECTOR==2|map_data$SECTOR==3), map_data$net_price_priv, map_data$net_price_t0)

map_data$prop_Pell_grant_i <- as.numeric(data2$PGRNT_P[match(map_data$UNITID, data2$UNITID)])
map_data<- map_data[!is.na(map_data$net_price_t0)&!is.na(map_data$prop_Pell_grant_i),]  
table(map_data$SECTOR, is.na(map_data$net_price_t0))
map_data$prop_Pell_grant_i<-NULL

#Saving original copy so that we restore later
map_data2<-map_data

###Replication of Figure 1 begins
map_data_het <- map_data
#Get map
states <- states(cb = TRUE)
#Only focusing on the contiguous USA
"%ni%" <- Negate("%in%")
states<-states[states$NAME %ni% c('Alaska','American Samoa','Commonwealth of the Northern Mariana Islands','Guam','Hawaii','United States Virgin Islands','Puerto Rico'),]

map_data_het$net_tuit_reven_FTE <- as.numeric(sc$TUITFTE[match(map_data_het$UNITID,sc$UNITID)])
#Dots PHUDCFILY
ccdf <- st_as_sf(x = map_data_het[map_data_het$SECTOR==4,],                         
           coords = c("LONGITUD", "LATITUDE"),
           crs = st_crs(states))
ccdf$id_cc <- paste("Average net price: $", ccdf$net_price_t0, " for ", ccdf$INSTNM, " located in ", ccdf$STABBR, sep="")
Encoding(ccdf$id_cc)<- "latin1"

ccdf$id_cc_t <- paste("Net tuition revenue: $", ccdf$net_tuit_reven_FTE, " for ", ccdf$INSTNM, " located in ", ccdf$STABBR, sep="")
Encoding(ccdf$id_cc)<- "latin1"

###Interactive mapping
tmap_mode("view")#change "view" to "plot" if interested in static form
w1 <- tm_shape(states) +
  tm_borders() +
tm_shape(ccdf) + tm_symbols(col = "net_price_t0", size = "net_price_t0", style = "quantile", n = 9, palette = "inferno", title.col = "Net Price", colorNA = "grey81", id = "id_cc", alpha = .5) +
tm_layout(bg.color = "grey11",
title = "Net Price Heterogeneity among Community Colleges",
title.position = c("right", "top"), title.size = 1.1, title.color = "white",
legend.position = c("right", "bottom"), legend.text.size = 0.85,
legend.width = 0.25, legend.text.color = "white", legend.title.color="white") +
tm_credits("Data source: College ScoreCard, IPEDS, Tiger/Line Shapefiles",
position = c(0.002, 0.002), size = .75, col="white")+
# tm_borders(col=rgb(31,31,31,max=250,250/3)) +
  tm_view(view.legend.position = c("right", "bottom"))

#For tuition revenue
w2 <- tm_shape(states) +
  tm_borders() +
tm_shape(ccdf) + tm_symbols(col = "net_tuit_reven_FTE", size = "net_tuit_reven_FTE", style = "quantile", n = 9, palette = "inferno", title.col = "Net Tuition Revenue FTE", colorNA = "grey81", id = "id_cc_t", alpha = .5) +
tm_layout(bg.color = "grey11",
title = "Net Tuition Revenue Heterogeneity among Community Colleges",
title.position = c("right", "top"), title.size = 1.1, title.color = "white",
legend.position = c("right", "bottom"), legend.text.size = 0.85,
legend.width = 0.25, legend.text.color = "white", legend.title.color="white") +
tm_credits("Data source: College ScoreCard, IPEDS, Tiger/Line Shapefiles",
position = c(0.002, 0.002), size = .75, col="white")+
# tm_borders(col=rgb(31,31,31,max=250,250/3)) +
  tm_view(view.legend.position = c("right", "bottom")) 
  tmap_arrange(w1, w2, nrow=1)
###Replication of Figure 1 ends

#The following codes allow for the
# (b) identification of heterogeneous neighboring structures, and 
# (c) Modeling procedures

#Visualizing non-rural structures

map_data<-map_data2
map_data <- map_data[map_data$Rural==0,]
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
af <- map_data
coordinates(af) <- c("LONGITUD", "LATITUDE")
proj4string(af) <- CRS(projcrs)
class(af)
DiMa <- gw.dist(coordinates(af), longlat = TRUE)

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==4, DiMa[ncol(DiMa),]==4]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==1, DiMa[ncol(DiMa),]==1]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==1, DiMa[ncol(DiMa),]==2]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==1, DiMa[ncol(DiMa),]==3]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==1, DiMa[ncol(DiMa),]==4]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

### 2 PHUDCFILY
DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==2, DiMa[ncol(DiMa),]==1]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==2, DiMa[ncol(DiMa),]==2]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==2, DiMa[ncol(DiMa),]==3]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==2, DiMa[ncol(DiMa),]==4]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

### 3 PHUDCFILY
DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==3, DiMa[ncol(DiMa),]==1]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==3, DiMa[ncol(DiMa),]==2]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==3, DiMa[ncol(DiMa),]==3]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==3, DiMa[ncol(DiMa),]==4]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa[DiMa>32.1869]<-0 #20 miles = 32.1869
DiMa[DiMa>0]<-1
summary(rowSums(DiMa))

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
DI<-DiMa[DiMa[,nrow(DiMa)]==4, DiMa[ncol(DiMa),]!=4]
DI<-DI[,-ncol(DI)]
dim(DI)
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]
summary(rowSums(DI))

#Calculating average prices all neighbors PHUDCFILY this matches our previous approach
dima<-DiMa
dima<-dima/rowSums(dima)
dima[is.na(dima)]<-0
test.listwU<-mat2listw(dima)  
head(test.listwU, zero.policy=TRUE)
summary(test.listwU, zero.policy=TRUE)
n_price_U <- data.frame(UNITID=map_data$UNITID, mean_cost_all_i=lag.listw(test.listwU, map_data$net_price_t0))
summary(n_price_U)
head(n_price_U[!is.na(n_price_U$mean_cost_all_i),])

table(map_data$SECTOR)
diag(DiMa)<-1
colnames(DiMa)<-map_data$UNITID
g<-graph.adjacency(DiMa)
g

g1<-as.data.frame(get.edgelist(g))
head(g1)
g1$SECTOR <- map_data$SECTOR[match(g1$V1, map_data$UNITID)]
g1<-g1[g1$SECTOR==4,]
dim(g1)
links2<-g1
links2$longitudecc<-map_data$LONGITUD[match(links2$V1, map_data$UNITID)]
links2$latitudecc <-map_data$LATITUDE[match(links2$V1, map_data$UNITID)]
links2$longitude4c<-map_data$LONGITUD[match(links2$V2, map_data$UNITID)]
links2$latitude4c <-map_data$LATITUDE[match(links2$V2, map_data$UNITID)]


links3<-data.frame(v1=c(links2$V1,links2$V2),v2=c(links2$V2,links2$V1))
dim(links3)
head(links3)

links3[c(1:10,(nrow(links2)+1):(nrow(links2)+10)),]

links3$group <- c(paste(links2$V1,links2$V2, sep=''),paste(links2$V1,links2$V2, sep=''))
links3$group<-as.factor(links3$group)

links3$lat<-c(links2$latitudecc,links2$latitude4c)
links3$lon<-c(links2$longitudecc,links2$longitude4c)
links3$end_lat<-c(links2$latitude4c,links2$latitudecc)
links3$end_lon<-c(links2$longitude4c,links2$longitudecc)

library(geosphere)
#calculating distances for descriptives PHUDCFILY
links2$distance <- distVincentyEllipsoid(cbind(links2$longitudecc, links2$latitudecc), cbind(links2$longitude4c, links2$latitude4c)) #result in meters, to change to miles divide by 1609
links2$distance<-links2$distance/1609
# links2$id<-phu$V1

links2$sector4c <- map_data$SECTOR[match(links2$V2, map_data$UNITID)]
head(links23)
links23 <- links2[links2$sector4c==4,]
links2 <- links2[links2$sector4c!=4,]
links2$sector4yr<-ifelse(links2$sector4c==1, "4-yr public", ifelse(links2$sector4c==2, "4-yr priv. No", "4-yr priv. Profit"))
links2$sector2yr<-"CC $rightarrow$ "
links2$gPHU<-paste(links2$sector2yr, links2$sector4yr, sep="")
table(links2$gPHU)
head(links2)

links3$distance <- distVincentyEllipsoid(cbind(links3$lon, links3$lat), cbind(links3$end_lon, links3$end_lat)) #result in meters, to change to miles divide by 1609
links3$distance<-links3$distance/1609

# links3$id<-phu$V1
head(links3)


dots1<-(links3[!duplicated(links3$v1),c("v1","lat","lon")])
head(dots1)
dots2<-links3[!duplicated(links3$v2),c("v2","end_lat","end_lon")]
head(dots2)
names(dots2)<-c("v1","lat","lon")
dots<-rbind(dots1,dots2)
dots<-dots[!duplicated(dots$v1),]
dim(dots)
head(dots)


dots$name <-map_data$INSTNM[match(dots$v1,map_data$UNITID)]  
dots$sector <-map_data$SECTOR[match(dots$v1,map_data$UNITID)]
dots$net_price <-final$net_price_i[match(dots$v1,final$UNITID)]
dots$net_price_t0 <-map_data$net_price_t0[match(dots$v1,map_data$UNITID)]
dots$net_price <- ifelse(!is.na(dots$net_price), dots$net_price, dots$net_price_t0)
#This requires execution of model (1) in Table 3.
dots$net_priceP <- round(m1P$trend[match(dots$v1,m1P$UNITID)],3)
dots$net_priceP <- ifelse(is.na(dots$net_priceP)&dots$sector==4, "no neighbors", ifelse(is.na(dots$net_priceP)&dots$sector!=4, "not a CC", dots$net_priceP))

dots$net_tuit_reven_FTE <- sc$TUITFTE[match(dots$v1,sc$UNITID)]
dots$pct_Pell_FTE 		<- data2$PGRNT_P[match(dots$v1,data2$UNITID)]
dots$pct_inst_aid_FTE 	<- data2$IGRNT_P[match(dots$v1,data2$UNITID)]

head(dots)
links2$number<-1
con<-aggregate(links2$number, by=list(links2$V1), FUN=sum, na.rm=TRUE) 
con2<-aggregate(links2$number, by=list(links2$V2), FUN=sum, na.rm=TRUE) 
head(con)
head(con2)
supercon<-rbind(con, con2)
con<-aggregate(supercon$x, by=list(supercon$Group.1), FUN=sum, na.rm=TRUE) 
dim(con)
head(con)

dots$number_neighbors <-con$x[match(dots$v1,con$Group.1)] 
dots$number_neighbors[is.na(dots$number_neighbors)]<-0
summary(dots)
dim(dots)

dots$sector_nm<-ifelse(dots$sector==1, "4-yr public", ifelse(dots$sector==2, "4-yr priv. Not-profit", ifelse(dots$sector==3, "4-yr priv. profit", "Community College"))) #PHUDCFILY
table(dots$sector_nm)

#Label
links2$group <- paste(links2$V1,links2$V2, sep='')
links2$group<-as.factor(links2$group)
links3$number<-links2$number[match(links3$group, links2$group)]
links3$num_fPHU<-(links3$number/max(links3$number))*3
# labels <- 
  # paste0(
    # "Zip Code: ",
    # co@data$NAME) %>%
  # lapply(htmltools::HTML)


labelschool <- 
  paste0(
    "School name: ",
    dots$name, "<br/>",
	"Sector: ",
    dots$sector_nm, "<br/>",
    "Net Price: ",
    dots$net_price, "<br/>",
    "Net Tuition Rev/FTE: ",
    dots$net_tuit_reven_FTE, "<br/>",
    "Pct. Pell/FTE: ",
    dots$pct_Pell_FTE, "<br/>",
    "Pct. Inst Aid/FTE: ",
    dots$pct_inst_aid_FTE, "<br/>",
    "Number of neighbors: ",
    dots$number_neighbors, "<br/>",
    "<b>Predicted net price:</b> ",
    dots$net_priceP) %>%
  lapply(htmltools::HTML)
css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML

labellink <- 
  paste0(
    "Distance in Miles: ",
    round(links3$distance,1)) %>%
  lapply(htmltools::HTML)
  
pal <- colorNumeric(
  palette= rev(inferno(256)),
  # palette = "YlGnBu",
  domain = links3$distance
)

states <- states(cb = TRUE, class="sp")
"%ni%" <- Negate("%in%")
states<-states[states$NAME %ni% c('Alaska','American Samoa','Commonwealth of the Northern Mariana Islands','Guam','Hawaii','United States Virgin Islands','Puerto Rico'),]
co<-states
names(co)

co@data$state_color <- ifelse(co@data$NAME=="Pennsylvania", "#2E2E2E", "#2E2E2E")

mymap<-co %>% 
  leaflet %>% 
  # add base map
  addProviderTiles("CartoDB") %>% #CartoDB.DarkMatter dark PHUDCFILY
  # add zip codes
addPolygons(fillColor = ~state_color,
weight = 1,
              opacity = .25,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(weight = 2,
                                           color = "black",
                                           dashArray = "",
                                           fillOpacity = 0.7,
                                           bringToFront = FALSE))%>%
			addControl("<b>Four-year neighboring structures in non-rural zones</b><br>Select school indicator by clicking on a given dot.<br>Lines identify neighbors within 20 miles<br>Source: College Score Card https://collegescorecard.ed.gov/ and<br>NCES's IPEDS https://nces.ed.gov/ipeds/datacenter/<br>
			CC= gold, Public = Cyan, Not-profit = grey, Profit = red", position = "topright")
  
for(group in levels(links3$group)){
      mymap = addPolylines(mymap,
                           lng= ~ lon,
                           lat= ~ lat,
                           data = links3[links3$group==group,],
                           color= ~ pal(links3$distance[links3$group==group]),
                           weight = links3$num_fPHU[links3$group==group], label = labellink[links3$group==group]) 
    }

css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML

dots$sectorc<-NA
dots$sectorc[dots$sector==4]<-"#EEC900"
dots$sectorc[dots$sector==2]<-"#CCCCCC"
dots$sectorc[dots$sector==1]<-"#00FFFF"
dots$sectorc[dots$sector==3]<-"#ff2200"

table(dots$sectorc,dots$sector)
dots$num_wgt<-log(dots$number_neighbors+2)/max(log(dots$number_neighbors+2))*34


mymap %>%
addCircleMarkers(data = dots,
                    lng = ~lon, lat = ~lat, radius = ~num_wgt, color = ~sectorc, label = labelschool,
  weight = 2.5, opacity = 0.5, fill = TRUE, fillColor = ~sectorc,
                    labelOptions = labelOptions(interactive = TRUE, direction = 'top', textOnly = F))%>%
htmlwidgets::prependContent(html_fix) 

dotsR<-dots #For final visualization in Appendix

#Visualizing Rural areas
map_data<-map_data2
map_data <- map_data[map_data$Rural==1,]
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
af <- map_data
coordinates(af) <- c("LONGITUD", "LATITUDE")
proj4string(af) <- CRS(projcrs)
class(af)
DiMa <- gw.dist(coordinates(af), longlat = TRUE)

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==4, DiMa[ncol(DiMa),]==4]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==1, DiMa[ncol(DiMa),]==1]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==1, DiMa[ncol(DiMa),]==2]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==1, DiMa[ncol(DiMa),]==3]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==1, DiMa[ncol(DiMa),]==4]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

### 2 PHUDCFILY
DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==2, DiMa[ncol(DiMa),]==1]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==2, DiMa[ncol(DiMa),]==2]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==2, DiMa[ncol(DiMa),]==3]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==2, DiMa[ncol(DiMa),]==4]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

### 3 PHUDCFILY
DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==3, DiMa[ncol(DiMa),]==1]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==3, DiMa[ncol(DiMa),]==2]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==3, DiMa[ncol(DiMa),]==3]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
DiMa[DiMa[,nrow(DiMa)]==3, DiMa[ncol(DiMa),]==4]<-0
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]

DiMa[DiMa>64.3738]<-0 # 64.3738 = 40 miles
DiMa[DiMa>0]<-1
summary(rowSums(DiMa))

DiMa<-cbind(DiMa,map_data$SECTOR)
DiMa<-rbind(DiMa,map_data$SECTOR)
DI<-DiMa[DiMa[,nrow(DiMa)]==4, DiMa[ncol(DiMa),]!=4]
DI<-DI[,-ncol(DI)]
dim(DI)
DiMa<-DiMa[-nrow(DiMa),-ncol(DiMa)]
summary(rowSums(DI))

#Calculating average prices all neighbors PHUDCFILY this matches our previous approach
dima<-DiMa
dima<-dima/rowSums(dima)
dima[is.na(dima)]<-0
test.listwU<-mat2listw(dima)  
head(test.listwU, zero.policy=TRUE)
summary(test.listwU, zero.policy=TRUE)
n_price_R <- data.frame(UNITID=map_data$UNITID, mean_cost_all_i=lag.listw(test.listwU, map_data$net_price_t0))
summary(n_price_R)
head(n_price_R[!is.na(n_price_R$mean_cost_all_i),])
#

table(map_data$SECTOR)
diag(DiMa)<-1
colnames(DiMa)<-map_data$UNITID
g<-graph.adjacency(DiMa)
g

g1<-as.data.frame(get.edgelist(g))
head(g1)
g1$SECTOR <- map_data$SECTOR[match(g1$V1, map_data$UNITID)]
g1<-g1[g1$SECTOR==4,]
dim(g1)
links2<-g1
links2$longitudecc<-map_data$LONGITUD[match(links2$V1, map_data$UNITID)]
links2$latitudecc <-map_data$LATITUDE[match(links2$V1, map_data$UNITID)]
links2$longitude4c<-map_data$LONGITUD[match(links2$V2, map_data$UNITID)]
links2$latitude4c <-map_data$LATITUDE[match(links2$V2, map_data$UNITID)]


links3<-data.frame(v1=c(links2$V1,links2$V2),v2=c(links2$V2,links2$V1))
dim(links3)
head(links3)

links3[c(1:10,(nrow(links2)+1):(nrow(links2)+10)),]

links3$group <- c(paste(links2$V1,links2$V2, sep=''),paste(links2$V1,links2$V2, sep=''))
links3$group<-as.factor(links3$group)

links3$lat<-c(links2$latitudecc,links2$latitude4c)
links3$lon<-c(links2$longitudecc,links2$longitude4c)
links3$end_lat<-c(links2$latitude4c,links2$latitudecc)
links3$end_lon<-c(links2$longitude4c,links2$longitudecc)

library(geosphere)
#calculating distances for descriptives PHUDCFILY
links2$distance <- distVincentyEllipsoid(cbind(links2$longitudecc, links2$latitudecc), cbind(links2$longitude4c, links2$latitude4c)) #result in meters, to change to miles divide by 1609
links2$distance<-links2$distance/1609
# links2$id<-phu$V1

links2$sector4c <- map_data$SECTOR[match(links2$V2, map_data$UNITID)]
head(links23)
links23 <- links2[links2$sector4c==4,]
links2 <- links2[links2$sector4c!=4,]
links2$sector4yr<-ifelse(links2$sector4c==1, "4-yr public", ifelse(links2$sector4c==2, "4-yr priv. No", "4-yr priv. Profit"))
links2$sector2yr<-"CC $rightarrow$ "
links2$gPHU<-paste(links2$sector2yr, links2$sector4yr, sep="")
table(links2$gPHU)
head(links2)

links3$distance <- distVincentyEllipsoid(cbind(links3$lon, links3$lat), cbind(links3$end_lon, links3$end_lat)) #result in meters, to change to miles divide by 1609
links3$distance<-links3$distance/1609

# links3$id<-phu$V1
head(links3)


dots1<-(links3[!duplicated(links3$v1),c("v1","lat","lon")])
head(dots1)
dots2<-links3[!duplicated(links3$v2),c("v2","end_lat","end_lon")]
head(dots2)
names(dots2)<-c("v1","lat","lon")
dots<-rbind(dots1,dots2)
dots<-dots[!duplicated(dots$v1),]
dim(dots)
head(dots)


dots$name <-map_data$INSTNM[match(dots$v1,map_data$UNITID)]  
dots$sector <-map_data$SECTOR[match(dots$v1,map_data$UNITID)]
dots$net_price <-final$net_price_i[match(dots$v1,final$UNITID)]
dots$net_price_t0 <-map_data$net_price_t0[match(dots$v1,map_data$UNITID)]
dots$net_price <- ifelse(!is.na(dots$net_price), dots$net_price, dots$net_price_t0)
#This requires execution of model (1) in Table 3.
dots$net_priceP <- round(m1P$trend[match(dots$v1,m1P$UNITID)],3)
dots$net_priceP <- ifelse(is.na(dots$net_priceP)&dots$sector==4, "no neighbors", ifelse(is.na(dots$net_priceP)&dots$sector!=4, "not a CC", dots$net_priceP))

dots$net_tuit_reven_FTE <- sc$TUITFTE[match(dots$v1,sc$UNITID)]
dots$pct_Pell_FTE 		<- data2$PGRNT_P[match(dots$v1,data2$UNITID)]
dots$pct_inst_aid_FTE 	<- data2$IGRNT_P[match(dots$v1,data2$UNITID)]

head(dots)
links2$number<-1
con<-aggregate(links2$number, by=list(links2$V1), FUN=sum, na.rm=TRUE) 
con2<-aggregate(links2$number, by=list(links2$V2), FUN=sum, na.rm=TRUE) 
head(con)
head(con2)
supercon<-rbind(con, con2)
con<-aggregate(supercon$x, by=list(supercon$Group.1), FUN=sum, na.rm=TRUE) 
dim(con)
head(con)

dots$number_neighbors <-con$x[match(dots$v1,con$Group.1)] 
dots$number_neighbors[is.na(dots$number_neighbors)]<-0
summary(dots)
dim(dots)

dots$sector_nm<-ifelse(dots$sector==1, "4-yr public", ifelse(dots$sector==2, "4-yr priv. Not-profit", ifelse(dots$sector==3, "4-yr priv. profit", "Community College"))) #PHUDCFILY
table(dots$sector_nm)

#Label
links2$group <- paste(links2$V1,links2$V2, sep='')
links2$group<-as.factor(links2$group)
links3$number<-links2$number[match(links3$group, links2$group)]
links3$num_fPHU<-(links3$number/max(links3$number))*3
# labels <- 
  # paste0(
    # "Zip Code: ",
    # co@data$NAME) %>%
  # lapply(htmltools::HTML)


labelschool <- 
  paste0(
    "School name: ",
    dots$name, "<br/>",
	"Sector: ",
    dots$sector_nm, "<br/>",
    "Net Price: ",
    dots$net_price, "<br/>",
    "Net Tuition Rev/FTE: ",
    dots$net_tuit_reven_FTE, "<br/>",
    "Pct. Pell/FTE: ",
    dots$pct_Pell_FTE, "<br/>",
    "Pct. Inst Aid/FTE: ",
    dots$pct_inst_aid_FTE, "<br/>",
    "Number of neighbors: ",
    dots$number_neighbors, "<br/>",
    "<b>Predicted net price:</b> ",
    dots$net_priceP) %>%
  lapply(htmltools::HTML)
css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML

labellink <- 
  paste0(
    # "Number of agreements: ",
    # links3$number, "<br/>",
    "Distance in Miles: ",
    round(links3$distance,1)) %>%
  lapply(htmltools::HTML)
  
pal <- colorNumeric(
  palette= rev(inferno(256)),
  # palette = "YlGnBu",
  domain = links3$distance
)

states <- states(cb = TRUE, class="sp")
"%ni%" <- Negate("%in%")
states<-states[states$NAME %ni% c('Alaska','American Samoa','Commonwealth of the Northern Mariana Islands','Guam','Hawaii','United States Virgin Islands','Puerto Rico'),]
co<-states
names(co)

co@data$state_color <- ifelse(co@data$NAME=="Pennsylvania", "#2E2E2E", "#2E2E2E")

mymap<-co %>% 
  leaflet %>% 
  # add base map
  addProviderTiles("CartoDB") %>% #CartoDB.DarkMatter dark PHUDCFILY
  # add zip codes
addPolygons(fillColor = ~state_color,
weight = 1,
              opacity = .25,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(weight = 2,
                                           color = "black",
                                           dashArray = "",
                                           fillOpacity = 0.7,
                                           bringToFront = FALSE))%>%
			addControl("<b>Four-year neighboring structures in rural zones</b><br>Select school indicator by clicking on a given dot.<br>Lines identify neighbors within 40 miles<br>Source: College Score Card https://collegescorecard.ed.gov/ and<br>NCES's IPEDS https://nces.ed.gov/ipeds/datacenter/<br>
			CC= gold, Public = Cyan, Not-profit = grey, Profit = red", position = "topright" )
  
for(group in levels(links3$group)){
      mymap = addPolylines(mymap,
                           lng= ~ lon,
                           lat= ~ lat,
                           data = links3[links3$group==group,],
                           color= ~ pal(links3$distance[links3$group==group]),
                           weight = links3$num_fPHU[links3$group==group], label = labellink[links3$group==group]) 
    }

css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML

dots$sectorc<-NA
dots$sectorc[dots$sector==4]<-"#EEC900" #gold
dots$sectorc[dots$sector==2]<-"#CCCCCC"
dots$sectorc[dots$sector==1]<-"#00FFFF" #cyan
dots$sectorc[dots$sector==3]<-"#ff2200"

table(dots$sectorc,dots$sector)
dots$num_wgt<-log(dots$number_neighbors+2)/max(log(dots$number_neighbors+2))*20


mymap %>%
addCircleMarkers(data = dots,
                    lng = ~lon, lat = ~lat, radius = ~num_wgt, color = ~sectorc, label = labelschool,
  weight = 2.5, opacity = 0.5, fill = TRUE, fillColor = ~sectorc,
                    labelOptions = labelOptions(interactive = TRUE, direction = 'top', textOnly = F))%>%
htmlwidgets::prependContent(html_fix) 
	

########################################
  ########Final Map App###############
###Predictions Final
co<-states
dotsf<-rbind(dotsR,dots)
dotsf<-dotsf[dotsf$number_neighbors>0,]
dotsf<-dotsf[dotsf$sector==4,]
dotsf$net_priceP <- as.numeric(dotsf$net_priceP)
qpal <- colorQuantile("inferno", dotsf$net_priceP, n = 10)

dotsf$STUSPS<-map_data2$STABBR[match(dotsf$v1, map_data2$UNITID)]
ct<-aggregate(v1 ~ STUSPS, data = dotsf, FUN = length)
head(ct)
stag<-aggregate(net_priceP ~ STUSPS, data = dotsf, FUN = mean)
head(stag)
stag$ct<-ct$v1[match(stag$STUSPS, ct$STUSPS)]
co <- geo_join(co, stag, "STUSPS", "STUSPS")
qpals <- colorQuantile("Greys", co@data$net_priceP, n = 9, na.color = "#ff2200")

labelst <- 
  paste0(
    "Number CCs w/ neighbors: ",
    co@data$ct, "<br/>",
	"Average Predicted Price: ",
    round(co@data$net_priceP,2)) %>%
  lapply(htmltools::HTML)

labelschool <- 
  paste0(
    "School name: ",
    dotsf$name, "<br/>",
	"Sector: ",
    dotsf$sector_nm, "<br/>",
    "Net Price: ",
    dotsf$net_price, "<br/>",
    "Net Tuition Rev/FTE: ",
    dotsf$net_tuit_reven_FTE, "<br/>",
    "Pct. Pell/FTE: ",
    dotsf$pct_Pell_FTE, "<br/>",
    "Pct. Inst Aid/FTE: ",
    dotsf$pct_inst_aid_FTE, "<br/>",
    "Number of neighbors: ",
    dotsf$number_neighbors, "<br/>",
    "<b>Predicted net price:</b> ",
    dotsf$net_priceP) %>%
  lapply(htmltools::HTML)


mymap<-co %>% 
  leaflet %>% 
  # add base map
  addProviderTiles("CartoDB.DarkMatter") %>% #CartoDB.DarkMatter dark PHUDCFILY
  # add zip codes
addPolygons(fillColor = ~qpals(net_priceP),
				weight = 1,
				opacity = .25,
				color = "white",
				dashArray = "3",
				fillOpacity = 0.7, 
				label = labelst,
				highlight = highlightOptions(weight = 2,
                                           color = "black",
                                           dashArray = "",
                                           fillOpacity = 0.7,
                                           bringToFront = FALSE))%>%
			addControl("<b>The Heterogeneous Landscape of College Affordability</b><br>Select school indicator by clicking on a given dot.<br>Message box shows Avg and predicted net prices<br>Source: College Score Card https://collegescorecard.ed.gov/ and<br>NCES's IPEDS https://nces.ed.gov/ipeds/datacenter/", 
			position = "topright" )
mymap %>%
addCircleMarkers(data = dotsf,
                    lng = ~lon, lat = ~lat, radius = ~num_wgt, color = ~qpal(net_priceP), label = labelschool,
  weight = 2.5, opacity = 1, fill = TRUE, 
                    labelOptions = labelOptions(interactive = TRUE, direction = 'top', textOnly = F))%>%
  addLegend(pal = qpals, values = ~round(co@data$net_priceP,2), opacity = 1, title = "Average Net Price", na.label = "No CC w/ neighbor")%>%
  addLegend(pal = qpal, values = ~round(dotsf$net_priceP,2), opacity = 1, title = "Predicted Net Price")%>%
htmlwidgets::prependContent(html_fix)   

##########CLOSING Visualization###############
   ########################################

########################################
   ########NEW SECTION###############
#Identifying heterogeneous neighbor structures 

### Urban regions PHUDCFILy
map_data <- map_data2
map_data <- map_data[map_data$Rural==0 & (map_data$SECTOR==1|map_data$SECTOR==4),]
table(map_data$SECTOR)
coords<-cbind(map_data$LONGITUD,map_data$LATITUDE)

#Retrieve neighbors
test.nb<-dnearneigh(coords, 0, 32.1869, row.names = map_data$UNITID,
					longlat = TRUE) # 12 miles = 19.32, 20 miles = 32.1869

#Transform to matrix
matr <- nb2mat (test.nb, zero.policy=TRUE)
colnames(matr)<-row.names(matr)
dim(matr)

# Public four-year procedures
#Add sector as a final row and column PHUDCFILY
matr<-cbind(matr,map_data$SECTOR)
matr<-rbind(matr,map_data$SECTOR)
#All selections of rows that are 4-year pub. to columns that are CC ==0
matr[matr[,nrow(matr)]==1, matr[ncol(matr),]==4]<-0
matr<-matr[-nrow(matr),-ncol(matr)] #this process altered the addition of sector so remove

matr<-cbind(matr,map_data$SECTOR)
matr<-rbind(matr,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
matr[matr[,nrow(matr)]==4, matr[ncol(matr),]==4]<-0
matr<-matr[-nrow(matr),-ncol(matr)]

matr<-cbind(matr,map_data$SECTOR)
matr<-rbind(matr,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
matr[matr[,nrow(matr)]==1, matr[ncol(matr),]==1]<-0
matr<-matr[-nrow(matr),-ncol(matr)]
dim(matr)

# {
matfin1 <- matr

#Replace connections with 1
matfin1 [matfin1>0]<- 1
dim(matfin1)
matfin1<-matfin1/rowSums(matfin1)
matfin1[is.na(matfin1)]<-0
test.listwU<-mat2listw(matfin1)  
head(test.listwU, zero.policy=TRUE)
summary(test.listwU, zero.policy=TRUE)
plot(test.listwU, coords)

matfin1 [matfin1>0]<- 1
matfin1 <- data.frame(UNITID=row.names(matfin1), treat=rowSums(matfin1))
summary(matfin1)

map_data$treat_pu4_i <- matfin1$treat[match(map_data$UNITID, matfin1$UNITID)]
summary(map_data)

map_data$lag.OpenDoor_pu4_i  <- lag.listw(test.listwU, map_data$OpenDoor, zero.policy=NULL) #this takes lag means or mean of 

map_data$lag.Doct_Res_pu4_i <- lag.listw(test.listwU, map_data$Doctoral) #this takes lag means or mean of 

map_data$lag.net_price_pu4_i <- lag.listw(test.listwU, map_data$net_price_t0) #this takes lag means or mean of 

head(matfin1)
matfin1<-merge(matfin1,map_data[,c("treat_pu4_i", "lag.OpenDoor_pu4_i", "lag.Doct_Res_pu4_i", "lag.net_price_pu4_i", "UNITID", "SECTOR")])
summary(matfin1)
matfin1<-matfin1[matfin1$SECTOR==4,]
table(matfin1$treat_pu4_i)
matfin1$treat<-NULL
matfin1$SECTOR<-NULL

map_data<-map_data2 #restore PHUDCFILy
###Preparing identification PHUDCFILY
### Urban regions PHUDCFILy
map_data <- map_data[map_data$Rural==0 & (map_data$SECTOR==2|map_data$SECTOR==4),]
table(map_data$SECTOR)
coords<-cbind(map_data$LONGITUD,map_data$LATITUDE)

#Retrieve neighbors
test.nb<-dnearneigh(coords, 0, 32.1869, row.names = map_data$UNITID,
					longlat = TRUE) # 12 miles

#Transform to matrix
matr <- nb2mat (test.nb, zero.policy=TRUE)
colnames(matr)<-row.names(matr)
dim(matr)

# Public four-year procedures
#Add sector as a final row and column PHUDCFILY
matr<-cbind(matr,map_data$SECTOR)
matr<-rbind(matr,map_data$SECTOR)
#All selections of rows that are 4-year pub. to columns that are CC ==0
matr[matr[,nrow(matr)]==2, matr[ncol(matr),]==4]<-0
matr<-matr[-nrow(matr),-ncol(matr)] #this process altered the addition of sector so remove

matr<-cbind(matr,map_data$SECTOR)
matr<-rbind(matr,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
matr[matr[,nrow(matr)]==4, matr[ncol(matr),]==4]<-0
matr<-matr[-nrow(matr),-ncol(matr)]

matr<-cbind(matr,map_data$SECTOR)
matr<-rbind(matr,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
matr[matr[,nrow(matr)]==2, matr[ncol(matr),]==2]<-0
matr<-matr[-nrow(matr),-ncol(matr)]
dim(matr)

# {
matfin2 <- matr

#Replace connections with 1
matfin2 [matfin2>0]<- 1
dim(matfin2)
matfin2<-matfin2/rowSums(matfin2)
matfin2[is.na(matfin2)]<-0
test.listwU<-mat2listw(matfin2)  
head(test.listwU, zero.policy=TRUE)
summary(test.listwU, zero.policy=TRUE)
plot(test.listwU, coords)

matfin2 [matfin2>0]<- 1
matfin2 <- data.frame(UNITID=row.names(matfin2), treat=rowSums(matfin2))
summary(matfin2)

map_data$treat_priv4_i <- matfin2$treat[match(map_data$UNITID, matfin2$UNITID)]
summary(map_data)

map_data$lag.OpenDoor_priv4_i  <- lag.listw(test.listwU, map_data$OpenDoor, zero.policy=NULL) #this takes lag means or mean of 

map_data$lag.Doct_Res_priv4_i <- lag.listw(test.listwU, map_data$Doctoral) #this takes lag means or mean of 

map_data$lag.net_price_priv4_i <- lag.listw(test.listwU, map_data$net_price_t0) #this takes lag means or mean of 

head(matfin2)
matfin2<-merge(matfin2,map_data[,c("treat_priv4_i", "lag.OpenDoor_priv4_i", "lag.Doct_Res_priv4_i", "lag.net_price_priv4_i", "UNITID", "SECTOR")])
summary(matfin2)
matfin2<-matfin2[matfin2$SECTOR==4,]
table(matfin2$treat_priv4_i)
matfin2$treat<-NULL
matfin2$SECTOR<-NULL

map_data<-map_data2 #restore PHUDCFILy
###Preparing identification PHUDCFILY
### Urban regions PHUDCFILy
map_data <- map_data[map_data$Rural==0 & (map_data$SECTOR==3|map_data$SECTOR==4),]
table(map_data$SECTOR)
coords<-cbind(map_data$LONGITUD,map_data$LATITUDE)

#Retrieve neighbors
test.nb<-dnearneigh(coords, 0, 32.1869, row.names = map_data$UNITID,
					longlat = TRUE) # 12 miles

#Transform to matrix
matr <- nb2mat (test.nb, zero.policy=TRUE)
colnames(matr)<-row.names(matr)
dim(matr)

# Public four-year procedures
#Add sector as a final row and column PHUDCFILY
matr<-cbind(matr,map_data$SECTOR)
matr<-rbind(matr,map_data$SECTOR)
#All selections of rows that are 4-year pub. to columns that are CC ==0
matr[matr[,nrow(matr)]==3, matr[ncol(matr),]==4]<-0
matr<-matr[-nrow(matr),-ncol(matr)] #this process altered the addition of sector so remove

matr<-cbind(matr,map_data$SECTOR)
matr<-rbind(matr,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
matr[matr[,nrow(matr)]==4, matr[ncol(matr),]==4]<-0
matr<-matr[-nrow(matr),-ncol(matr)]

matr<-cbind(matr,map_data$SECTOR)
matr<-rbind(matr,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
matr[matr[,nrow(matr)]==3, matr[ncol(matr),]==3]<-0
matr<-matr[-nrow(matr),-ncol(matr)]
dim(matr)

# {
matfin3 <- matr

#Replace connections with 1
matfin3 [matfin3>0]<- 1
dim(matfin3)
matfin3<-matfin3/rowSums(matfin3)
matfin3[is.na(matfin3)]<-0
test.listwU<-mat2listw(matfin3)  
head(test.listwU, zero.policy=TRUE)
summary(test.listwU, zero.policy=TRUE)
plot(test.listwU, coords)

matfin3 [matfin3>0]<- 1
matfin3 <- data.frame(UNITID=row.names(matfin3), treat=rowSums(matfin3))
summary(matfin3)

map_data$treat_profit4_i <- matfin3$treat[match(map_data$UNITID, matfin3$UNITID)]
summary(map_data)

map_data$lag.OpenDoor_profit4_i  <- lag.listw(test.listwU, map_data$OpenDoor, zero.policy=NULL) #this takes lag means or mean of 

map_data$lag.Doct_Res_profit4_i <- lag.listw(test.listwU, map_data$Doctoral) #this takes lag means or mean of 

map_data$lag.net_price_profit4_i <- lag.listw(test.listwU, map_data$net_price_t0) #this takes lag means or mean of 

head(matfin3)
matfin3<-merge(matfin3,map_data[,c("treat_profit4_i", "lag.OpenDoor_profit4_i", "lag.Doct_Res_profit4_i", "lag.net_price_profit4_i", "UNITID", "SECTOR")])
summary(matfin3)
matfin3<-matfin3[matfin3$SECTOR==4,]
table(matfin3$treat_profit4_i)
matfin3$treat<-NULL
matfin3$SECTOR<-NULL


###################
####RURAL ZONES####
###################

###Preparing identification PHUDCFILY
### Urban regions PHUDCFILy
map_data<-map_data2
map_data <- map_data[map_data$Rural==1 & (map_data$SECTOR==1|map_data$SECTOR==4),]
table(map_data$SECTOR)
coords<-cbind(map_data$LONGITUD,map_data$LATITUDE)

#Retrieve neighbors
test.nb<-dnearneigh(coords, 0, 64.3738, row.names = map_data$UNITID,
					longlat = TRUE) # 24 miles = 38.6243 40 miles = 

#Transform to matrix
matr <- nb2mat (test.nb, zero.policy=TRUE)
colnames(matr)<-row.names(matr)
dim(matr)

# Public four-year procedures
#Add sector as a final row and column PHUDCFILY
matr<-cbind(matr,map_data$SECTOR)
matr<-rbind(matr,map_data$SECTOR)
#All selections of rows that are 4-year pub. to columns that are CC ==0
matr[matr[,nrow(matr)]==1, matr[ncol(matr),]==4]<-0
matr<-matr[-nrow(matr),-ncol(matr)] #this process altered the addition of sector so remove

matr<-cbind(matr,map_data$SECTOR)
matr<-rbind(matr,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
matr[matr[,nrow(matr)]==4, matr[ncol(matr),]==4]<-0
matr<-matr[-nrow(matr),-ncol(matr)]

matr<-cbind(matr,map_data$SECTOR)
matr<-rbind(matr,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
matr[matr[,nrow(matr)]==1, matr[ncol(matr),]==1]<-0
matr<-matr[-nrow(matr),-ncol(matr)]
dim(matr)

# {
matfin1R <- matr

#Replace connections with 1
matfin1R [matfin1R>0]<- 1
dim(matfin1R)
matfin1R<-matfin1R/rowSums(matfin1R)
matfin1R[is.na(matfin1R)]<-0
test.listwU<-mat2listw(matfin1R)  
head(test.listwU, zero.policy=TRUE)
summary(test.listwU, zero.policy=TRUE)
plot(test.listwU, coords)

matfin1R [matfin1R>0]<- 1
matfin1R <- data.frame(UNITID=row.names(matfin1R), treat=rowSums(matfin1R))
summary(matfin1R)

map_data$treat_pu4_i <- matfin1R$treat[match(map_data$UNITID, matfin1R$UNITID)]
summary(map_data)

map_data$lag.OpenDoor_pu4_i  <- lag.listw(test.listwU, map_data$OpenDoor, zero.policy=NULL) #this takes lag means or mean of 

map_data$lag.Doct_Res_pu4_i <- lag.listw(test.listwU, map_data$Doctoral) #this takes lag means or mean of 

map_data$lag.net_price_pu4_i <- lag.listw(test.listwU, map_data$net_price_t0) #this takes lag means or mean of 

head(matfin1R)
matfin1R<-merge(matfin1R,map_data[,c("treat_pu4_i", "lag.OpenDoor_pu4_i", "lag.Doct_Res_pu4_i", "lag.net_price_pu4_i", "UNITID", "SECTOR")])
summary(matfin1R)
matfin1R<-matfin1R[matfin1R$SECTOR==4,]
table(matfin1R$treat_pu4_i)
matfin1R$treat<-NULL
matfin1R$SECTOR<-NULL

###PHUDCFILY private not-profit

map_data<-map_data2 #restore PHUDCFILy
###Preparing identification PHUDCFILY
### Urban regions PHUDCFILy
map_data <- map_data[map_data$Rural==1 & (map_data$SECTOR==2|map_data$SECTOR==4),]
table(map_data$SECTOR)
coords<-cbind(map_data$LONGITUD,map_data$LATITUDE)

#Retrieve neighbors
test.nb<-dnearneigh(coords, 0, 64.3738, row.names = map_data$UNITID,
					longlat = TRUE) # 24 miles

#Transform to matrix
matr <- nb2mat (test.nb, zero.policy=TRUE)
colnames(matr)<-row.names(matr)
dim(matr)

# Public four-year procedures
#Add sector as a final row and column PHUDCFILY
matr<-cbind(matr,map_data$SECTOR)
matr<-rbind(matr,map_data$SECTOR)
#All selections of rows that are 4-year pub. to columns that are CC ==0
matr[matr[,nrow(matr)]==2, matr[ncol(matr),]==4]<-0
matr<-matr[-nrow(matr),-ncol(matr)] #this process altered the addition of sector so remove

matr<-cbind(matr,map_data$SECTOR)
matr<-rbind(matr,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
matr[matr[,nrow(matr)]==4, matr[ncol(matr),]==4]<-0
matr<-matr[-nrow(matr),-ncol(matr)]

matr<-cbind(matr,map_data$SECTOR)
matr<-rbind(matr,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
matr[matr[,nrow(matr)]==2, matr[ncol(matr),]==2]<-0
matr<-matr[-nrow(matr),-ncol(matr)]
dim(matr)

# {
matfin2R <- matr

#Replace connections with 1
matfin2R [matfin2R>0]<- 1
dim(matfin2R)
matfin2R<-matfin2R/rowSums(matfin2R)
matfin2R[is.na(matfin2R)]<-0
test.listwU<-mat2listw(matfin2R)  
head(test.listwU, zero.policy=TRUE)
summary(test.listwU, zero.policy=TRUE)
plot(test.listwU, coords)

matfin2R [matfin2R>0]<- 1
matfin2R <- data.frame(UNITID=row.names(matfin2R), treat=rowSums(matfin2R))
summary(matfin2R)

map_data$treat_priv4_i <- matfin2R$treat[match(map_data$UNITID, matfin2R$UNITID)]
summary(map_data)

map_data$lag.OpenDoor_priv4_i  <- lag.listw(test.listwU, map_data$OpenDoor, zero.policy=NULL) #this takes lag means or mean of 

map_data$lag.Doct_Res_priv4_i <- lag.listw(test.listwU, map_data$Doctoral) #this takes lag means or mean of 

map_data$lag.net_price_priv4_i <- lag.listw(test.listwU, map_data$net_price_t0) #this takes lag means or mean of 

head(matfin2R)
matfin2R<-merge(matfin2R,map_data[,c("treat_priv4_i", "lag.OpenDoor_priv4_i", "lag.Doct_Res_priv4_i", "lag.net_price_priv4_i", "UNITID", "SECTOR")])
summary(matfin2R)
matfin2R<-matfin2R[matfin2R$SECTOR==4,]
table(matfin2R$treat_priv4_i)
matfin2R$treat<-NULL
matfin2R$SECTOR<-NULL

###PHUDCFILY private not-profit

map_data<-map_data2 #restore PHUDCFILy
###Preparing identification PHUDCFILY
### Urban regions PHUDCFILy
map_data <- map_data[map_data$Rural==1 & (map_data$SECTOR==3|map_data$SECTOR==4),]
table(map_data$SECTOR)
coords<-cbind(map_data$LONGITUD,map_data$LATITUDE)

#Retrieve neighbors
test.nb<-dnearneigh(coords, 0, 64.3738, row.names = map_data$UNITID,
					longlat = TRUE) # 12 miles

#Transform to matrix
matr <- nb2mat (test.nb, zero.policy=TRUE)
colnames(matr)<-row.names(matr)
dim(matr)

# Public four-year procedures
#Add sector as a final row and column PHUDCFILY
matr<-cbind(matr,map_data$SECTOR)
matr<-rbind(matr,map_data$SECTOR)
#All selections of rows that are 4-year pub. to columns that are CC ==0
matr[matr[,nrow(matr)]==3, matr[ncol(matr),]==4]<-0
matr<-matr[-nrow(matr),-ncol(matr)] #this process altered the addition of sector so remove

matr<-cbind(matr,map_data$SECTOR)
matr<-rbind(matr,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
matr[matr[,nrow(matr)]==4, matr[ncol(matr),]==4]<-0
matr<-matr[-nrow(matr),-ncol(matr)]

matr<-cbind(matr,map_data$SECTOR)
matr<-rbind(matr,map_data$SECTOR)
#All selections of rows that are CC to columns that are CC ==0
matr[matr[,nrow(matr)]==3, matr[ncol(matr),]==3]<-0
matr<-matr[-nrow(matr),-ncol(matr)]
dim(matr)

# {
matfin3R <- matr

#Replace connections with 1
matfin3R [matfin3R>0]<- 1
dim(matfin3R)
matfin3R<-matfin3R/rowSums(matfin3R)
matfin3R[is.na(matfin3R)]<-0
test.listwU<-mat2listw(matfin3R)  
head(test.listwU, zero.policy=TRUE)
summary(test.listwU, zero.policy=TRUE)
plot(test.listwU, coords)

matfin3R [matfin3R>0]<- 1
matfin3R <- data.frame(UNITID=row.names(matfin3R), treat=rowSums(matfin3R))
summary(matfin3R)

map_data$treat_profit4_i <- matfin3R$treat[match(map_data$UNITID, matfin3R$UNITID)]
summary(map_data)

map_data$lag.OpenDoor_profit4_i  <- lag.listw(test.listwU, map_data$OpenDoor, zero.policy=NULL) #this takes lag means or mean of 

map_data$lag.Doct_Res_profit4_i <- lag.listw(test.listwU, map_data$Doctoral) #this takes lag means or mean of 

map_data$lag.net_price_profit4_i <- lag.listw(test.listwU, map_data$net_price_t0) #this takes lag means or mean of 

head(matfin3R)
matfin3R<-merge(matfin3R,map_data[,c("treat_profit4_i", "lag.OpenDoor_profit4_i", "lag.Doct_Res_profit4_i", "lag.net_price_profit4_i", "UNITID", "SECTOR")])
summary(matfin3R)
# matfin3R[is.na(matfin3R)]<-0
matfin3R<-matfin3R[matfin3R$SECTOR==4,]
table(matfin3R$treat_profit4_i)
matfin3R$treat<-NULL
matfin3R$SECTOR<-NULL


# Bringing datasets together PHUDCFILY

matfin_U <- join_all(list(matfin1, matfin2, matfin3), by = 'UNITID', type = 'left')
matfin_R <- join_all(list(matfin1R, matfin2R, matfin3R), by = 'UNITID', type = 'left')

#Users can merge the following dataset with other institutional characteristics
final<-rbind(matfin_U, matfin_R)
dim(final)
summary(final)

final$treat_i <- final$treat_pu4_i + final$treat_priv4_i + final$treat_profit4_i
table((final$treat_i))
final$mean_cost_all_i <- rowMeans(as.matrix(final[ ,c("lag.net_price_profit4_i", "lag.net_price_priv4_i", "lag.net_price_pu4_i")]), na.rm = TRUE)
final$prop_pub_neigh <- final$treat_pu4_i/final$treat_i

##########CLOSING SECTION###############
########################################

###Final dataset as used in the paper (including zip, county, state, indicators) 
# Data sources as depicted in paper, original code available
#For information about how to compile the zip, county, state, indicators contact us
final <- read.csv("https://raw.githubusercontent.com/democratizing-data-science/Community_College_Affordability/main/spillovers_18_19_including_profit_PHUDCFILY.csv")

#######################################################
	##################BORUTA######################
	############ Net Tuition Revenue #############
set.seed(47)
varnames <- c("treat_i", "undup_count_i", "non_resident_alien_i", "prop_part_time_i", "prop_loan_i", "pct_living_on_campus_i", "transf_rate_FTE_i", "White_i", "Black_i", "Hispanic_i", "Asian_i", "race_other_i", "Northeast", "Midwest", "South", "West", "Rural_i", "prop_EITC_z", "Gini_z", "med_income_z", "med_rent_z", "rent_income_ratio_z", "single_mother_hshld_z", "citzn_born_abroad_z", "prop_crime_z", "White_z", "Black_z", "Hispanic_z", "Asian_z", "race_other_z", "prop_adlts_up_HS_c", "unemplymnt_rate_c", "prop_poverty_c", "death_rate_c", "prop_net_migration_c", "State.Gross.Domestic.Product_s", "Absolute.Domestic.Migration_s", "Non.Farm.Payroll_s", "state_sup_per_inhabi_s", "Need_based_s", "Nonneed_based_s", "Nongrant_Aid_s") #"prop_pub_neigh_i",  transf_rate_FTE_i instruct_expendts_i "fac_salary_month_i", "prop_fac_fulltime_i", "prop_Pell_grant_i", "Bach_or_more_z",

boruta_output <- Boruta(net_tuition_rev_i ~ ., data=final[,c("net_tuition_rev_i",varnames)], doTrace=2, maxRuns=1000)
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  

#If you are not sure about the tentative variables being selected for granted, you can choose a TentativeRoughFix on boruta_output.

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

#There you go. Boruta has decided on the ‘Tentative’ variables on our behalf. Let’s find out the importance scores of these variables.

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

#Let’s plot it to see the importances of these variables.

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  

df<-as.data.frame(t(boruta_output)[2])
head(df)
# install.packages("robustbase")
library(robustbase)
df<-do.call(data.frame,lapply(df, function(x) replace(x, is.infinite(x),NA)))
mns <- colMedians(as.matrix(df), na.rm=T, hasNA=T)
df <- df[,order(mns)]
names(df)
# write.csv(df, "borutaPHUDCFILY.csv",row.names=F)
# df<-read.csv("borutaPHUDCFILY.csv")

mns<-as.data.frame(mns)
mns$decision<-imps$decision[match(row.names(mns),row.names(imps))]
mns$variable<-row.names(mns)
mns

library(reshape)
df$ID<-rownames(df)
df <- melt(as.data.frame(df), id='ID')
df <- merge(df,mns[,c("variable","mns","decision")], by="variable")
head(df)
df$type <- ifelse((df$variable=="shadowMax"|df$variable=="shadowMean"|df$variable=="shadowMin"),"Shadow","Confirmed")
df$type <- ifelse(is.na(df$decision),"Shadow",as.character(df$decision))
table(df$type)
library(RColorBrewer)
library(plotly)
brewer.pal(n = 8, name = "Dark2")

plot_ly(df, x = ~variable, y = ~value, color = ~type, type = "box", colors= c("#E7298A", "#E6AB02", "#66A61E"),line = list(color = 'rgb(74,74,74,max=255,255/2)')) %>% 
         layout(xaxis = list(title='Candidate features compared against their shadows'), 
         yaxis = list(title='Variable Importance'),
		 title = "<b>Feature selection, Boruta procedure (random forest classification)</b><br>Outcome: Net Tuition Revenue")
#Rejected: "Rural_i", "race_other_z", 
#Omitted groups: "White_z", "West", "White_i"

###############################
########Net Price##############
varnames <- c("treat_i", "undup_count_i", "non_resident_alien_i", "prop_part_time_i", "prop_loan_i", "pct_living_on_campus_i", "transf_rate_FTE_i", "White_i", "Black_i", "Hispanic_i", "Asian_i", "race_other_i", "Northeast", "Midwest", "South", "West", "Rural_i", "prop_EITC_z", "Gini_z", "med_income_z", "med_rent_z", "rent_income_ratio_z", "single_mother_hshld_z", "citzn_born_abroad_z", "prop_crime_z", "White_z", "Black_z", "Hispanic_z", "Asian_z", "race_other_z", "prop_adlts_up_HS_c", "unemplymnt_rate_c", "prop_poverty_c", "death_rate_c", "prop_net_migration_c", "State.Gross.Domestic.Product_s", "Absolute.Domestic.Migration_s", "Non.Farm.Payroll_s", "state_sup_per_inhabi_s", "Need_based_s", "Nonneed_based_s", "Nongrant_Aid_s") #"prop_pub_neigh_i",  transf_rate_FTE_i instruct_expendts_i "fac_salary_month_i", "prop_fac_fulltime_i", "prop_Pell_grant_i", "Bach_or_more_z",
set.seed(47)
boruta_output <- Boruta(net_price_i ~ ., data=final[,c("net_price_i",varnames)], doTrace=2, maxRuns=1000)
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  

#If you are not sure about the tentative variables being selected for granted, you can choose a TentativeRoughFix on boruta_output.

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

#There you go. Boruta has decided on the ‘Tentative’ variables on our behalf. Let’s find out the importance scores of these variables.

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

#Let’s plot it to see the importances of these variables.

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  

df<-as.data.frame(t(boruta_output)[2])
head(df)
# install.packages("robustbase")
library(robustbase)
df<-do.call(data.frame,lapply(df, function(x) replace(x, is.infinite(x),NA)))
mns <- colMedians(as.matrix(df), na.rm=T, hasNA=T)
df <- df[,order(mns)]
names(df)
# write.csv(df, "borutaPHUDCFILY.csv",row.names=F)
# df<-read.csv("borutaPHUDCFILY.csv")

mns<-as.data.frame(mns)
mns$decision<-imps$decision[match(row.names(mns),row.names(imps))]
mns$variable<-row.names(mns)
mns

library(reshape)
df$ID<-rownames(df)
df <- melt(as.data.frame(df), id='ID')
df <- merge(df,mns[,c("variable","mns","decision")], by="variable")
head(df)
df$type <- ifelse((df$variable=="shadowMax"|df$variable=="shadowMean"|df$variable=="shadowMin"),"Shadow","Confirmed")
df$type <- ifelse(is.na(df$decision),"Shadow",as.character(df$decision))
table(df$type)
library(RColorBrewer)
library(plotly)
brewer.pal(n = 8, name = "Dark2")

plot_ly(df, x = ~variable, y = ~value, color = ~type, type = "box", colors= c("#E7298A", "#E6AB02", "#66A61E"),line = list(color = 'rgb(74,74,74,max=255,255/2)')) %>% 
         layout(xaxis = list(title='Candidate features compared against their shadows'), 
         yaxis = list(title='Variable Importance'),
		 title = "<b>Feature selection, Boruta procedure (random forest classification)</b><br>Outcome: Net Price of Attendance")
		 varnames <- c("treat_i", "undup_count_i", "non_resident_alien_i", "prop_part_time_i", "prop_loan_i", "pct_living_on_campus_i", "transf_rate_FTE_i", "White_i", "Black_i", "Hispanic_i", "Asian_i", "race_other_i", "Northeast", "Midwest", "South", "West", "Rural_i", "prop_EITC_z", "Gini_z", "med_income_z", "med_rent_z", "rent_income_ratio_z", "single_mother_hshld_z", "citzn_born_abroad_z", "prop_crime_z", "White_z", "Black_z", "Hispanic_z", "Asian_z", "race_other_z", "prop_adlts_up_HS_c", "unemplymnt_rate_c", "prop_poverty_c", "death_rate_c", "prop_net_migration_c", "State.Gross.Domestic.Product_s", "Absolute.Domestic.Migration_s", "Non.Farm.Payroll_s", "state_sup_per_inhabi_s", "Need_based_s", "Nonneed_based_s", "Nongrant_Aid_s") #"prop_pub_neigh_i",  transf_rate_FTE_i instruct_expendts_i "fac_salary_month_i", "prop_fac_fulltime_i", "prop_Pell_grant_i", "Bach_or_more_z",
set.seed(47)
boruta_output <- Boruta(net_price_i ~ ., data=final[,c("net_price_i",varnames)], doTrace=2, maxRuns=1000)
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  

#If you are not sure about the tentative variables being selected for granted, you can choose a TentativeRoughFix on boruta_output.

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

#There you go. Boruta has decided on the ‘Tentative’ variables on our behalf. Let’s find out the importance scores of these variables.

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

#Let’s plot it to see the importances of these variables.

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  

df<-as.data.frame(t(boruta_output)[2])
head(df)
# install.packages("robustbase")
library(robustbase)
df<-do.call(data.frame,lapply(df, function(x) replace(x, is.infinite(x),NA)))
mns <- colMedians(as.matrix(df), na.rm=T, hasNA=T)
df <- df[,order(mns)]
names(df)
# write.csv(df, "borutaPHUDCFILY.csv",row.names=F)
# df<-read.csv("borutaPHUDCFILY.csv")

mns<-as.data.frame(mns)
mns$decision<-imps$decision[match(row.names(mns),row.names(imps))]
mns$variable<-row.names(mns)
mns

library(reshape)
df$ID<-rownames(df)
df <- melt(as.data.frame(df), id='ID')
df <- merge(df,mns[,c("variable","mns","decision")], by="variable")
head(df)
df$type <- ifelse((df$variable=="shadowMax"|df$variable=="shadowMean"|df$variable=="shadowMin"),"Shadow","Confirmed")
df$type <- ifelse(is.na(df$decision),"Shadow",as.character(df$decision))
table(df$type)
library(RColorBrewer)
library(plotly)
brewer.pal(n = 8, name = "Dark2")

plot_ly(df, x = ~variable, y = ~value, color = ~type, type = "box", colors= c("#E7298A", "#E6AB02", "#66A61E"),line = list(color = 'rgb(74,74,74,max=255,255/2)')) %>% 
         layout(xaxis = list(title='Candidate features compared against their shadows'), 
         yaxis = list(title='Variable Importance'),
		 title = "<b>Feature selection, Boruta procedure (random forest classification)</b><br>Outcome: Net Price of Attendance")
		 varnames <- c("treat_i", "undup_count_i", "non_resident_alien_i", "prop_part_time_i", "prop_loan_i", "pct_living_on_campus_i", "transf_rate_FTE_i", "White_i", "Black_i", "Hispanic_i", "Asian_i", "race_other_i", "Northeast", "Midwest", "South", "West", "Rural_i", "prop_EITC_z", "Gini_z", "med_income_z", "med_rent_z", "rent_income_ratio_z", "single_mother_hshld_z", "citzn_born_abroad_z", "prop_crime_z", "White_z", "Black_z", "Hispanic_z", "Asian_z", "race_other_z", "prop_adlts_up_HS_c", "unemplymnt_rate_c", "prop_poverty_c", "death_rate_c", "prop_net_migration_c", "State.Gross.Domestic.Product_s", "Absolute.Domestic.Migration_s", "Non.Farm.Payroll_s", "state_sup_per_inhabi_s", "Need_based_s", "Nonneed_based_s", "Nongrant_Aid_s") #"prop_pub_neigh_i",  transf_rate_FTE_i instruct_expendts_i "fac_salary_month_i", "prop_fac_fulltime_i", "prop_Pell_grant_i", "Bach_or_more_z",
set.seed(47)
boruta_output <- Boruta(net_price_i ~ ., data=final[,c("net_price_i",varnames)], doTrace=2, maxRuns=1000)
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  

#If you are not sure about the tentative variables being selected for granted, you can choose a TentativeRoughFix on boruta_output.

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

#There you go. Boruta has decided on the ‘Tentative’ variables on our behalf. Let’s find out the importance scores of these variables.

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

#Let’s plot it to see the importances of these variables.

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  

df<-as.data.frame(t(boruta_output)[2])
head(df)
# install.packages("robustbase")
library(robustbase)
df<-do.call(data.frame,lapply(df, function(x) replace(x, is.infinite(x),NA)))
mns <- colMedians(as.matrix(df), na.rm=T, hasNA=T)
df <- df[,order(mns)]
names(df)
# write.csv(df, "borutaPHUDCFILY.csv",row.names=F)
# df<-read.csv("borutaPHUDCFILY.csv")

mns<-as.data.frame(mns)
mns$decision<-imps$decision[match(row.names(mns),row.names(imps))]
mns$variable<-row.names(mns)
mns

library(reshape)
df$ID<-rownames(df)
df <- melt(as.data.frame(df), id='ID')
df <- merge(df,mns[,c("variable","mns","decision")], by="variable")
head(df)
df$type <- ifelse((df$variable=="shadowMax"|df$variable=="shadowMean"|df$variable=="shadowMin"),"Shadow","Confirmed")
df$type <- ifelse(is.na(df$decision),"Shadow",as.character(df$decision))
table(df$type)
library(RColorBrewer)
library(plotly)
brewer.pal(n = 8, name = "Dark2")

plot_ly(df, x = ~variable, y = ~value, color = ~type, type = "box", colors= c("#E7298A", "#E6AB02", "#66A61E"),line = list(color = 'rgb(74,74,74,max=255,255/2)')) %>% 
         layout(xaxis = list(title='Candidate features compared against their shadows'), 
         yaxis = list(title='Variable Importance'),
		 title = "<b>Feature selection, Boruta procedure (random forest classification)</b><br>Outcome: Net Price of Attendance")
		 varnames <- c("treat_i", "undup_count_i", "non_resident_alien_i", "prop_part_time_i", "prop_loan_i", "pct_living_on_campus_i", "transf_rate_FTE_i", "White_i", "Black_i", "Hispanic_i", "Asian_i", "race_other_i", "Northeast", "Midwest", "South", "West", "Rural_i", "prop_EITC_z", "Gini_z", "med_income_z", "med_rent_z", "rent_income_ratio_z", "single_mother_hshld_z", "citzn_born_abroad_z", "prop_crime_z", "White_z", "Black_z", "Hispanic_z", "Asian_z", "race_other_z", "prop_adlts_up_HS_c", "unemplymnt_rate_c", "prop_poverty_c", "death_rate_c", "prop_net_migration_c", "State.Gross.Domestic.Product_s", "Absolute.Domestic.Migration_s", "Non.Farm.Payroll_s", "state_sup_per_inhabi_s", "Need_based_s", "Nonneed_based_s", "Nongrant_Aid_s") #"prop_pub_neigh_i",  transf_rate_FTE_i instruct_expendts_i "fac_salary_month_i", "prop_fac_fulltime_i", "prop_Pell_grant_i", "Bach_or_more_z",
set.seed(47)
boruta_output <- Boruta(net_price_i ~ ., data=final[,c("net_price_i",varnames)], doTrace=2, maxRuns=1000)
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  

#If you are not sure about the tentative variables being selected for granted, you can choose a TentativeRoughFix on boruta_output.

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

#There you go. Boruta has decided on the ‘Tentative’ variables on our behalf. Let’s find out the importance scores of these variables.

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

#Let’s plot it to see the importances of these variables.

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  

df<-as.data.frame(t(boruta_output)[2])
head(df)
# install.packages("robustbase")
library(robustbase)
df<-do.call(data.frame,lapply(df, function(x) replace(x, is.infinite(x),NA)))
mns <- colMedians(as.matrix(df), na.rm=T, hasNA=T)
df <- df[,order(mns)]
names(df)
# write.csv(df, "borutaPHUDCFILY.csv",row.names=F)
# df<-read.csv("borutaPHUDCFILY.csv")

mns<-as.data.frame(mns)
mns$decision<-imps$decision[match(row.names(mns),row.names(imps))]
mns$variable<-row.names(mns)
mns

library(reshape)
df$ID<-rownames(df)
df <- melt(as.data.frame(df), id='ID')
df <- merge(df,mns[,c("variable","mns","decision")], by="variable")
head(df)
df$type <- ifelse((df$variable=="shadowMax"|df$variable=="shadowMean"|df$variable=="shadowMin"),"Shadow","Confirmed")
df$type <- ifelse(is.na(df$decision),"Shadow",as.character(df$decision))
table(df$type)
library(RColorBrewer)
library(plotly)
brewer.pal(n = 8, name = "Dark2")

plot_ly(df, x = ~variable, y = ~value, color = ~type, type = "box", colors= c("#E7298A", "#E6AB02", "#66A61E"),line = list(color = 'rgb(74,74,74,max=255,255/2)')) %>% 
         layout(xaxis = list(title='Candidate features compared against their shadows'), 
         yaxis = list(title='Variable Importance'),
		 title = "<b>Feature selection, Boruta procedure (random forest classification)</b><br>Outcome: Net Price of Attendance")
#Rejected: "rent_income_ratio_z", "West", "single_mother_hshld_z", "non_resident_alien_i", 
#Omitted groups: "White_z", "West", "White_i"

###############################
######Institutional Aid########

varnames <- c("treat_i", "undup_count_i", "non_resident_alien_i", "prop_part_time_i", "prop_loan_i", "pct_living_on_campus_i", "transf_rate_FTE_i", "White_i", "Black_i", "Hispanic_i", "Asian_i", "race_other_i", "Northeast", "Midwest", "South", "West", "Rural_i", "prop_EITC_z", "Gini_z", "med_income_z", "med_rent_z", "rent_income_ratio_z", "single_mother_hshld_z", "citzn_born_abroad_z", "prop_crime_z", "White_z", "Black_z", "Hispanic_z", "Asian_z", "race_other_z", "prop_adlts_up_HS_c", "unemplymnt_rate_c", "prop_poverty_c", "death_rate_c", "prop_net_migration_c", "State.Gross.Domestic.Product_s", "Absolute.Domestic.Migration_s", "Non.Farm.Payroll_s", "state_sup_per_inhabi_s", "Need_based_s", "Nonneed_based_s", "Nongrant_Aid_s") #"prop_pub_neigh_i",  transf_rate_FTE_i instruct_expendts_i "fac_salary_month_i", "prop_fac_fulltime_i", "prop_Pell_grant_i", "Bach_or_more_z",
set.seed(47)
boruta_output <- Boruta(prop_inst_aid_i ~ ., data=final[,c("prop_inst_aid_i",varnames)], doTrace=2, maxRuns=1000)
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  

#If you are not sure about the tentative variables being selected for granted, you can choose a TentativeRoughFix on boruta_output.

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

#There you go. Boruta has decided on the ‘Tentative’ variables on our behalf. Let’s find out the importance scores of these variables.

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

#Let’s plot it to see the importances of these variables.

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  

df<-as.data.frame(t(boruta_output)[2])
head(df)
# install.packages("robustbase")
library(robustbase)
df<-do.call(data.frame,lapply(df, function(x) replace(x, is.infinite(x),NA)))
mns <- colMedians(as.matrix(df), na.rm=T, hasNA=T)
df <- df[,order(mns)]
names(df)
# write.csv(df, "borutaPHUDCFILY.csv",row.names=F)
# df<-read.csv("borutaPHUDCFILY.csv")

mns<-as.data.frame(mns)
mns$decision<-imps$decision[match(row.names(mns),row.names(imps))]
mns$variable<-row.names(mns)
mns

library(reshape)
df$ID<-rownames(df)
df <- melt(as.data.frame(df), id='ID')
df <- merge(df,mns[,c("variable","mns","decision")], by="variable")
head(df)
df$type <- ifelse((df$variable=="shadowMax"|df$variable=="shadowMean"|df$variable=="shadowMin"),"Shadow","Confirmed")
df$type <- ifelse(is.na(df$decision),"Shadow",as.character(df$decision))
table(df$type)
library(RColorBrewer)
library(plotly)
brewer.pal(n = 8, name = "Dark2")

plot_ly(df, x = ~variable, y = ~value, color = ~type, type = "box", colors= c("#E7298A", "#E6AB02", "#66A61E"),line = list(color = 'rgb(74,74,74,max=255,255/2)')) %>% 
         layout(xaxis = list(title='Candidate features compared against their shadows'), 
         yaxis = list(title='Variable Importance'),
		 title = "<b>Feature selection, Boruta procedure (random forest classification)</b><br>Outcome: Proportion Receiving Institutional Grant")
# Rejected: None
#Omitted groups: "White_z", "West", "White_i"

###############################
######Prop Pell Grant##########

varnames <- c("treat_i", "undup_count_i", "non_resident_alien_i", "prop_part_time_i", "prop_loan_i", "pct_living_on_campus_i", "transf_rate_FTE_i", "White_i", "Black_i", "Hispanic_i", "Asian_i", "race_other_i", "Northeast", "Midwest", "South", "West", "Rural_i", "prop_EITC_z", "Gini_z", "med_income_z", "med_rent_z", "rent_income_ratio_z", "single_mother_hshld_z", "citzn_born_abroad_z", "prop_crime_z", "White_z", "Black_z", "Hispanic_z", "Asian_z", "race_other_z", "prop_adlts_up_HS_c", "unemplymnt_rate_c", "prop_poverty_c", "death_rate_c", "prop_net_migration_c", "State.Gross.Domestic.Product_s", "Absolute.Domestic.Migration_s", "Non.Farm.Payroll_s", "state_sup_per_inhabi_s", "Need_based_s", "Nonneed_based_s", "Nongrant_Aid_s") #"prop_pub_neigh_i",  transf_rate_FTE_i instruct_expendts_i "fac_salary_month_i", "prop_fac_fulltime_i", "prop_Pell_grant_i", "Bach_or_more_z",
set.seed(47)
boruta_output <- Boruta(prop_Pell_grant_i ~ ., data=final[,c("prop_Pell_grant_i",varnames)], doTrace=2, maxRuns=1000)
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  

#If you are not sure about the tentative variables being selected for granted, you can choose a TentativeRoughFix on boruta_output.

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

#There you go. Boruta has decided on the ‘Tentative’ variables on our behalf. Let’s find out the importance scores of these variables.

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

#Let’s plot it to see the importances of these variables.

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  

df<-as.data.frame(t(boruta_output)[2])
head(df)
# install.packages("robustbase")
library(robustbase)
df<-do.call(data.frame,lapply(df, function(x) replace(x, is.infinite(x),NA)))
mns <- colMedians(as.matrix(df), na.rm=T, hasNA=T)
df <- df[,order(mns)]
names(df)
# write.csv(df, "borutaPHUDCFILY.csv",row.names=F)
# df<-read.csv("borutaPHUDCFILY.csv")

mns<-as.data.frame(mns)
mns$decision<-imps$decision[match(row.names(mns),row.names(imps))]
mns$variable<-row.names(mns)
mns

library(reshape)
df$ID<-rownames(df)
df <- melt(as.data.frame(df), id='ID')
df <- merge(df,mns[,c("variable","mns","decision")], by="variable")
head(df)
df$type <- ifelse((df$variable=="shadowMax"|df$variable=="shadowMean"|df$variable=="shadowMin"),"Shadow","Confirmed")
df$type <- ifelse(is.na(df$decision),"Shadow",as.character(df$decision))
table(df$type)
library(RColorBrewer)
library(plotly)
brewer.pal(n = 8, name = "Dark2")

plot_ly(df, x = ~variable, y = ~value, color = ~type, type = "box", colors= c("#E7298A", "#E6AB02", "#66A61E"),line = list(color = 'rgb(74,74,74,max=255,255/2)')) %>% 
         layout(xaxis = list(title='Candidate features compared against their shadows'), 
         yaxis = list(title='Variable Importance'),
		 title = "<b>Feature selection, Boruta procedure (random forest classification)</b><br>Outcome: Proportion Receiving Pell Grant")
#Rejected: "Rural_i", "rent_income_ratio_z", "pct_living_on_campus_i",
#Omitted groups: "White_z", "West", "White_i"
#######################################################

###############################
  ######Moran's I############
birdI <- spline.correlog(x= final$LONGITUD,
                         y= final$LATITUDE,
                         z= final$net_tuition_rev_i,
                         resamp= 100,
                         xmax= FALSE,
                         quiet= TRUE,
						 latlon = TRUE)

birdII <- spline.correlog(x= final$LONGITUD,
                         y= final$LATITUDE,
                         z= final$net_price_i,
                         resamp= 100,
                         xmax= FALSE,
                         quiet= TRUE,
						 latlon = TRUE)

birdIII <- spline.correlog(x= final$LONGITUD,
                         y= final$LATITUDE,
                         z= final$prop_inst_aid_i,
                         resamp= 100,
                         xmax= FALSE,
                         quiet= TRUE,
						 latlon = TRUE)

birdIIII <- spline.correlog(x= final$LONGITUD,
                         y= final$LATITUDE,
                         z= final$prop_Pell_grant_i,
                         resamp= 100,
                         xmax= FALSE,
                         quiet= TRUE,
						 latlon = TRUE)
par(mfrow=c(2,2))
plot(birdII, ylab="Moran's I", xlab="Distance in Kilometers")
title(main="A. spline correlogram with 95% CIs: Net Price (distance bandwidth = 807.88 Km)")
plot(birdI, ylab="Moran's I", xlab="Distance in Kilometers")
title(main="B. spline correlogram with 95% CIs: Tuition Revenue (distance bandwidth = 862.16 Km)")
plot(birdIII, ylab="Moran's I", xlab="Distance in Kilometers")
title(main="C. spline correlogram with 95% CIs: Prop. Inst. Grant (distance bandwidth = 1234.46 Km)")
plot(birdIIII, ylab="Moran's I", xlab="Distance in Kilometers")
title(main="D. spline correlogram with 95% CIs: Prop. Pell (distance bandwidth = 757.41 Km)")
#=###############################
coords<-cbind(final$LONGITUD,final$LATITUDE)

## Specific Moran's I values with thresholds depicted in Figure X
nb.dist.band <- dnearneigh(coords, 0, unlist(birdI$real[1])/1, longlat = TRUE)
distances <- nbdists(nb.dist.band,coords, longlat = TRUE)
max(unlist(nbdists(nb.dist.band,coords, longlat=T)))
invd1a <- lapply(distances, function(x) (1/(x/10)))
invd1a[1]
invd.weightsI <- nb2listw(nb.dist.band, glist = invd1a, style = "B")
moran.test(final$net_tuition_rev_i, listw=invd.weightsI, zero.policy=T, na.action=na.omit)

nb.dist.band <- dnearneigh(coords, 0, unlist(birdII$real[1])/1, longlat = TRUE)
distances <- nbdists(nb.dist.band,coords, longlat = TRUE)
max(unlist(nbdists(nb.dist.band,coords, longlat=T)))
invd1a <- lapply(distances, function(x) (1/(x/10)))
invd1a[1]
invd.weightsII <- nb2listw(nb.dist.band, glist = invd1a, style = "B")
moran.test(final$net_price_i, listw=invd.weightsII, zero.policy=T, na.action=na.omit)

nb.dist.band <- dnearneigh(coords, 0, unlist(birdIII$real[1])/1, longlat = TRUE)
distances <- nbdists(nb.dist.band,coords, longlat = TRUE)
max(unlist(nbdists(nb.dist.band,coords, longlat=T)))
invd1a <- lapply(distances, function(x) (1/(x/10)))
invd1a[1]
invd.weightsIII <- nb2listw(nb.dist.band, glist = invd1a, style = "B")
moran.test(final$prop_inst_aid_i, listw=invd.weightsIII, zero.policy=T, na.action=na.omit)

nb.dist.band <- dnearneigh(coords, 0, unlist(birdIIII$real[1])/1, longlat = TRUE)
distances <- nbdists(nb.dist.band,coords, longlat = TRUE)
max(unlist(nbdists(nb.dist.band,coords, longlat=T)))
invd1a <- lapply(distances, function(x) (1/(x/10)))
invd1a[1]
invd.weightsIIII <- nb2listw(nb.dist.band, glist = invd1a, style = "B")
moran.test(final$prop_Pell_grant_i, listw=invd.weightsIIII, zero.policy=T, na.action=na.omit)

###############################
######Summary Stats############
library("reporttools")
attach(final)
stats <- list("n", "mean", "s", "min", "max", "q1", "median", "q3")
stats <- list("mean", "s", "median", "max")

vars1 <- final[, c("net_tuition_rev_i", "net_price_i", "prop_inst_aid_i", "prop_Pell_grant_i", "undup_count_i", "non_resident_alien_i", "prop_part_time_i", "prop_loan_i", "pct_living_on_campus_i", "transf_rate_FTE_i", "Black_i", "White_i", "Hispanic_i", "Asian_i", "race_other_i", "Northeast", "Midwest", "South", "West", "Rural_i", "prop_EITC_z", "Gini_z", "med_income_z", "med_rent_z", "rent_income_ratio_z", "single_mother_hshld_z", "citzn_born_abroad_z", "prop_crime_z", "Black_z", "White_z",  "Hispanic_z", "Asian_z", "race_other_z", "prop_adlts_up_HS_c", "unemplymnt_rate_c", "prop_poverty_c", "death_rate_c", "prop_net_migration_c", "State.Gross.Domestic.Product_s", "Absolute.Domestic.Migration_s", "Non.Farm.Payroll_s", "state_sup_per_inhabi_s", "Need_based_s", "Nonneed_based_s", "Nongrant_Aid_s", "treat_pu4_i", "treat_priv4_i", "treat_profit4_i", "lag.OpenDoor_pu4_i", "lag.Doct_Res_pu4_i", "lag.net_price_pu4_i", "lag.OpenDoor_priv4_i", "lag.Doct_Res_priv4_i", "lag.net_price_priv4_i", "lag.OpenDoor_profit4_i", "lag.Doct_Res_profit4_i", "lag.net_price_profit4_i", "mean_cost_all_i", "prop_pub_neigh_i", "prop_pri_NoProf_neigh_i", "prop_Profit_neigh_i", "treat_i")]  
#Omitted groups: "White_z", "West", "White_i"
cap1 <- "Summary statistics of institution and place-based indicators by neighbor status"
tableContinuous (vars = vars1, group = neigh_i, cap = cap1, stats = stats, print.pval = "anova", lab = "tab:nominal1", longtable = T, prec=2)

###############################
  ######SAR Models###########
#Table 2

final$neigh_pu4_i     <-ifelse(final$treat_pu4_i>0, 1, 0)
final$neigh_priv4_i   <-ifelse(final$treat_priv4_i>0, 1, 0)
final$neigh_profit4_i <-ifelse(final$treat_profit4_i>0, 1, 0)

library(stargazer)
library(spatialreg)
m1 <- errorsarlm(formula = net_price_i ~ neigh_i + prop_part_time_i + prop_loan_i + pct_living_on_campus_i + transf_rate_FTE_i + Black_i + Hispanic_i + Asian_i + race_other_i + Northeast + Midwest + South + Rural_i + prop_EITC_z + Gini_z + med_income_z + med_rent_z + citzn_born_abroad_z + prop_crime_z + Black_z + Hispanic_z + Asian_z + race_other_z + prop_adlts_up_HS_c + unemplymnt_rate_c + prop_poverty_c + death_rate_c + prop_net_migration_c + State.Gross.Domestic.Product_s + Absolute.Domestic.Migration_s + Non.Farm.Payroll_s + state_sup_per_inhabi_s + Need_based_s + Nonneed_based_s + Nongrant_Aid_s, data = final, listw = invd.weightsII, weights = undup_count_i) 
summary(m1)
m1$lambda
hNULL<-residuals(m1)
moran.test(hNULL,invd.weightsII, zero.policy=TRUE)

m12 <- errorsarlm(formula = net_price_i ~ treat_i + prop_part_time_i + prop_loan_i + pct_living_on_campus_i + transf_rate_FTE_i + Black_i + Hispanic_i + Asian_i + race_other_i + Northeast + Midwest + South + Rural_i + prop_EITC_z + Gini_z + med_income_z + med_rent_z + citzn_born_abroad_z + prop_crime_z + Black_z + Hispanic_z + Asian_z + race_other_z + prop_adlts_up_HS_c + unemplymnt_rate_c + prop_poverty_c + death_rate_c + prop_net_migration_c + State.Gross.Domestic.Product_s + Absolute.Domestic.Migration_s + Non.Farm.Payroll_s + state_sup_per_inhabi_s + Need_based_s + Nonneed_based_s + Nongrant_Aid_s, data = final, listw = invd.weightsII, weights = undup_count_i) 
m12$lambda
hNULL<-residuals(m12)
moran.test(hNULL,invd.weightsII, zero.policy=TRUE)

m14 <- errorsarlm(formula = net_price_i ~ treat_pu4_i + treat_priv4_i + treat_profit4_i + prop_part_time_i + prop_loan_i + pct_living_on_campus_i + transf_rate_FTE_i + Black_i + Hispanic_i + Asian_i + race_other_i + Northeast + Midwest + South + Rural_i + prop_EITC_z + Gini_z + med_income_z + med_rent_z + citzn_born_abroad_z + prop_crime_z + Black_z + Hispanic_z + Asian_z + race_other_z + prop_adlts_up_HS_c + unemplymnt_rate_c + prop_poverty_c + death_rate_c + prop_net_migration_c + State.Gross.Domestic.Product_s + Absolute.Domestic.Migration_s + Non.Farm.Payroll_s + state_sup_per_inhabi_s + Need_based_s + Nonneed_based_s + Nongrant_Aid_s, data = final, listw = invd.weightsII, weights = undup_count_i) 
m13$lambda
hNULL<-residuals(m13)
moran.test(hNULL,invd.weightsII, zero.policy=TRUE)

m13 <- errorsarlm(formula = net_price_i ~ neigh_pu4_i + neigh_priv4_i + neigh_profit4_i + prop_part_time_i + prop_loan_i + pct_living_on_campus_i + transf_rate_FTE_i + Black_i + Hispanic_i + Asian_i + race_other_i + Northeast + Midwest + South + Rural_i + prop_EITC_z + Gini_z + med_income_z + med_rent_z + citzn_born_abroad_z + prop_crime_z + Black_z + Hispanic_z + Asian_z + race_other_z + prop_adlts_up_HS_c + unemplymnt_rate_c + prop_poverty_c + death_rate_c + prop_net_migration_c + State.Gross.Domestic.Product_s + Absolute.Domestic.Migration_s + Non.Farm.Payroll_s + state_sup_per_inhabi_s + Need_based_s + Nonneed_based_s + Nongrant_Aid_s, data = final, listw = invd.weightsII, weights = undup_count_i) 
m14$lambda
hNULL<-residuals(m14)
moran.test(hNULL,invd.weightsII, zero.policy=TRUE)

#Net tuition revenue models 
m2 <- errorsarlm(formula = net_tuition_rev_i ~ neigh_i + non_resident_alien_i + prop_part_time_i + prop_loan_i + pct_living_on_campus_i + transf_rate_FTE_i + Black_i + Hispanic_i + Asian_i + race_other_i + Northeast + Midwest + South + prop_EITC_z + Gini_z + med_income_z + med_rent_z + rent_income_ratio_z + single_mother_hshld_z + citzn_born_abroad_z + prop_crime_z + Black_z + Hispanic_z + Asian_z + prop_adlts_up_HS_c + unemplymnt_rate_c + prop_poverty_c + death_rate_c + prop_net_migration_c + State.Gross.Domestic.Product_s + Absolute.Domestic.Migration_s + Non.Farm.Payroll_s + state_sup_per_inhabi_s + Need_based_s + Nonneed_based_s + Nongrant_Aid_s, data = final, listw = invd.weightsI, weights = undup_count_i)
m2$lambda
hNULL<-residuals(m2)
moran.test(hNULL,invd.weightsI, zero.policy=TRUE)

m22 <- errorsarlm(formula = net_tuition_rev_i ~ treat_i + non_resident_alien_i + prop_part_time_i + prop_loan_i + pct_living_on_campus_i + transf_rate_FTE_i + Black_i + Hispanic_i + Asian_i + race_other_i + Northeast + Midwest + South + prop_EITC_z + Gini_z + med_income_z + med_rent_z + rent_income_ratio_z + single_mother_hshld_z + citzn_born_abroad_z + prop_crime_z + Black_z + Hispanic_z + Asian_z + prop_adlts_up_HS_c + unemplymnt_rate_c + prop_poverty_c + death_rate_c + prop_net_migration_c + State.Gross.Domestic.Product_s + Absolute.Domestic.Migration_s + Non.Farm.Payroll_s + state_sup_per_inhabi_s + Need_based_s + Nonneed_based_s + Nongrant_Aid_s, data = final, listw = invd.weightsI, weights = undup_count_i)
m22$lambda
hNULL<-residuals(m22)
moran.test(hNULL,invd.weightsI, zero.policy=TRUE)

m24 <- errorsarlm(formula = net_tuition_rev_i ~ treat_pu4_i + treat_priv4_i + treat_profit4_i + non_resident_alien_i + prop_part_time_i + prop_loan_i + pct_living_on_campus_i + transf_rate_FTE_i + Black_i + Hispanic_i + Asian_i + race_other_i + Northeast + Midwest + South + prop_EITC_z + Gini_z + med_income_z + med_rent_z + rent_income_ratio_z + single_mother_hshld_z + citzn_born_abroad_z + prop_crime_z + Black_z + Hispanic_z + Asian_z + prop_adlts_up_HS_c + unemplymnt_rate_c + prop_poverty_c + death_rate_c + prop_net_migration_c + State.Gross.Domestic.Product_s + Absolute.Domestic.Migration_s + Non.Farm.Payroll_s + state_sup_per_inhabi_s + Need_based_s + Nonneed_based_s + Nongrant_Aid_s, data = final, listw = invd.weightsI, weights = undup_count_i)
m23$lambda
hNULL<-residuals(m23)
moran.test(hNULL,invd.weightsI, zero.policy=TRUE)

m23 <- errorsarlm(formula = net_tuition_rev_i ~ neigh_pu4_i + neigh_priv4_i + neigh_profit4_i + non_resident_alien_i + prop_part_time_i + prop_loan_i + pct_living_on_campus_i + transf_rate_FTE_i + Black_i + Hispanic_i + Asian_i + race_other_i + Northeast + Midwest + South + prop_EITC_z + Gini_z + med_income_z + med_rent_z + rent_income_ratio_z + single_mother_hshld_z + citzn_born_abroad_z + prop_crime_z + Black_z + Hispanic_z + Asian_z + prop_adlts_up_HS_c + unemplymnt_rate_c + prop_poverty_c + death_rate_c + prop_net_migration_c + State.Gross.Domestic.Product_s + Absolute.Domestic.Migration_s + Non.Farm.Payroll_s + state_sup_per_inhabi_s + Need_based_s + Nonneed_based_s + Nongrant_Aid_s, data = final, listw = invd.weightsI, weights = undup_count_i)
m24$lambda
hNULL<-residuals(m24)
moran.test(hNULL,invd.weightsI, zero.policy=TRUE)

stargazer(m1,m12,m13,m14,m2,m22,m23,m24, header=FALSE, type='latex', title = "SAR Models")

m3 <- errorsarlm(formula = prop_inst_aid_i ~ neigh_i + non_resident_alien_i + prop_part_time_i + prop_loan_i + pct_living_on_campus_i + transf_rate_FTE_i + Black_i + Hispanic_i + Asian_i + race_other_i + Northeast + Midwest + South + Rural_i + prop_EITC_z + Gini_z + med_income_z + med_rent_z + rent_income_ratio_z + single_mother_hshld_z + citzn_born_abroad_z + prop_crime_z + Black_z + Hispanic_z + Asian_z + race_other_z + prop_adlts_up_HS_c + unemplymnt_rate_c + prop_poverty_c + death_rate_c + prop_net_migration_c + State.Gross.Domestic.Product_s + Absolute.Domestic.Migration_s + Non.Farm.Payroll_s + state_sup_per_inhabi_s + Need_based_s + Nonneed_based_s + Nongrant_Aid_s, data = final, listw = invd.weightsIII, weights = undup_count_i)
m3$lambda
hNULL<-residuals(m3)
moran.test(hNULL,invd.weightsIII, zero.policy=TRUE)

m32 <- errorsarlm(formula = prop_inst_aid_i ~ treat_i + non_resident_alien_i + prop_part_time_i + prop_loan_i + pct_living_on_campus_i + transf_rate_FTE_i + Black_i + Hispanic_i + Asian_i + race_other_i + Northeast + Midwest + South + Rural_i + prop_EITC_z + Gini_z + med_income_z + med_rent_z + rent_income_ratio_z + single_mother_hshld_z + citzn_born_abroad_z + prop_crime_z + Black_z + Hispanic_z + Asian_z + race_other_z + prop_adlts_up_HS_c + unemplymnt_rate_c + prop_poverty_c + death_rate_c + prop_net_migration_c + State.Gross.Domestic.Product_s + Absolute.Domestic.Migration_s + Non.Farm.Payroll_s + state_sup_per_inhabi_s + Need_based_s + Nonneed_based_s + Nongrant_Aid_s, data = final, listw = invd.weightsIII, weights = undup_count_i)
m32$lambda
hNULL<-residuals(m32)
moran.test(hNULL,invd.weightsIII, zero.policy=TRUE)

m33 <- errorsarlm(formula = prop_inst_aid_i ~ neigh_pu4_i + neigh_priv4_i + neigh_profit4_i + non_resident_alien_i + prop_part_time_i + prop_loan_i + pct_living_on_campus_i + transf_rate_FTE_i + Black_i + Hispanic_i + Asian_i + race_other_i + Northeast + Midwest + South + Rural_i + prop_EITC_z + Gini_z + med_income_z + med_rent_z + rent_income_ratio_z + single_mother_hshld_z + citzn_born_abroad_z + prop_crime_z + Black_z + Hispanic_z + Asian_z + race_other_z + prop_adlts_up_HS_c + unemplymnt_rate_c + prop_poverty_c + death_rate_c + prop_net_migration_c + State.Gross.Domestic.Product_s + Absolute.Domestic.Migration_s + Non.Farm.Payroll_s + state_sup_per_inhabi_s + Need_based_s + Nonneed_based_s + Nongrant_Aid_s, data = final, listw = invd.weightsIII, weights = undup_count_i)
m33$lambda
hNULL<-residuals(m33)
moran.test(hNULL,invd.weightsIII, zero.policy=TRUE)

m34 <- errorsarlm(formula = prop_inst_aid_i ~ treat_pu4_i + treat_priv4_i + treat_profit4_i + non_resident_alien_i + prop_part_time_i + prop_loan_i + pct_living_on_campus_i + transf_rate_FTE_i + Black_i + Hispanic_i + Asian_i + race_other_i + Northeast + Midwest + South + Rural_i + prop_EITC_z + Gini_z + med_income_z + med_rent_z + rent_income_ratio_z + single_mother_hshld_z + citzn_born_abroad_z + prop_crime_z + Black_z + Hispanic_z + Asian_z + race_other_z + prop_adlts_up_HS_c + unemplymnt_rate_c + prop_poverty_c + death_rate_c + prop_net_migration_c + State.Gross.Domestic.Product_s + Absolute.Domestic.Migration_s + Non.Farm.Payroll_s + state_sup_per_inhabi_s + Need_based_s + Nonneed_based_s + Nongrant_Aid_s, data = final, listw = invd.weightsIII, weights = undup_count_i)
m34$lambda
hNULL<-residuals(m34)
moran.test(hNULL,invd.weightsIII, zero.policy=TRUE)
# stargazer(m3,m32,m33,m34, header=FALSE, type='latex', title = "SAR Models")

m4 <- errorsarlm(formula = prop_Pell_grant_i ~ neigh_i + non_resident_alien_i + prop_part_time_i + prop_loan_i + transf_rate_FTE_i + Black_i + Hispanic_i + Asian_i + race_other_i + Northeast + Midwest + South + prop_EITC_z + Gini_z + med_income_z + med_rent_z + single_mother_hshld_z + citzn_born_abroad_z + prop_crime_z + Black_z + Hispanic_z + Asian_z + race_other_z + prop_adlts_up_HS_c + unemplymnt_rate_c + prop_poverty_c + death_rate_c + prop_net_migration_c + State.Gross.Domestic.Product_s + Absolute.Domestic.Migration_s + Non.Farm.Payroll_s + state_sup_per_inhabi_s + Need_based_s + Nonneed_based_s + Nongrant_Aid_s, data = final, listw = invd.weightsIIII, weights = undup_count_i)
summary(m4) #Community colleges surrounded by four-year neighbors tend to enroll greater proportions of full-time Pell recipients, on average. However, the composition of these neighbors does affect this outcome. 
m4$lambda
hNULL<-residuals(m4)
moran.test(hNULL,invd.weightsIIII, zero.policy=TRUE)

m42 <- errorsarlm(formula = prop_Pell_grant_i ~ treat_i + non_resident_alien_i + prop_part_time_i + prop_loan_i + transf_rate_FTE_i + Black_i + Hispanic_i + Asian_i + race_other_i + Northeast + Midwest + South + prop_EITC_z + Gini_z + med_income_z + med_rent_z + single_mother_hshld_z + citzn_born_abroad_z + prop_crime_z + Black_z + Hispanic_z + Asian_z + race_other_z + prop_adlts_up_HS_c + unemplymnt_rate_c + prop_poverty_c + death_rate_c + prop_net_migration_c + State.Gross.Domestic.Product_s + Absolute.Domestic.Migration_s + Non.Farm.Payroll_s + state_sup_per_inhabi_s + Need_based_s + Nonneed_based_s + Nongrant_Aid_s, data = final, listw = invd.weightsIIII, weights = undup_count_i)
summary(m4) #Community colleges surrounded by four-year neighbors tend to enroll greater proportions of full-time Pell recipients, on average. However, the composition of these neighbors does affect this outcome. 
m42$lambda
hNULL<-residuals(m42)
moran.test(hNULL,invd.weightsIIII, zero.policy=TRUE)

m43 <- errorsarlm(formula = prop_Pell_grant_i ~ neigh_pu4_i + neigh_priv4_i + neigh_profit4_i + non_resident_alien_i + prop_part_time_i + prop_loan_i + transf_rate_FTE_i + Black_i + Hispanic_i + Asian_i + race_other_i + Northeast + Midwest + South + prop_EITC_z + Gini_z + med_income_z + med_rent_z + single_mother_hshld_z + citzn_born_abroad_z + prop_crime_z + Black_z + Hispanic_z + Asian_z + race_other_z + prop_adlts_up_HS_c + unemplymnt_rate_c + prop_poverty_c + death_rate_c + prop_net_migration_c + State.Gross.Domestic.Product_s + Absolute.Domestic.Migration_s + Non.Farm.Payroll_s + state_sup_per_inhabi_s + Need_based_s + Nonneed_based_s + Nongrant_Aid_s, data = final, listw = invd.weightsIIII, weights = undup_count_i)
summary(m4) #Community colleges surrounded by four-year neighbors tend to enroll greater proportions of full-time Pell recipients, on average. However, the composition of these neighbors does affect this outcome. 
m43$lambda
hNULL<-residuals(m43)
moran.test(hNULL,invd.weightsIIII, zero.policy=TRUE)

m44 <- errorsarlm(formula = prop_Pell_grant_i ~ treat_pu4_i + treat_priv4_i + treat_profit4_i + non_resident_alien_i + prop_part_time_i + prop_loan_i + transf_rate_FTE_i + Black_i + Hispanic_i + Asian_i + race_other_i + Northeast + Midwest + South + prop_EITC_z + Gini_z + med_income_z + med_rent_z + single_mother_hshld_z + citzn_born_abroad_z + prop_crime_z + Black_z + Hispanic_z + Asian_z + race_other_z + prop_adlts_up_HS_c + unemplymnt_rate_c + prop_poverty_c + death_rate_c + prop_net_migration_c + State.Gross.Domestic.Product_s + Absolute.Domestic.Migration_s + Non.Farm.Payroll_s + state_sup_per_inhabi_s + Need_based_s + Nonneed_based_s + Nongrant_Aid_s, data = final, listw = invd.weightsIIII, weights = undup_count_i)
summary(m4) #Community colleges surrounded by four-year neighbors tend to enroll greater proportions of full-time Pell recipients, on average. However, the composition of these neighbors does affect this outcome. 
m44$lambda
hNULL<-residuals(m44)
moran.test(hNULL,invd.weightsIIII, zero.policy=TRUE)

stargazer(m3,m32,m33,m34,m4,m42,m43,m44, header=FALSE, type='latex', title = "SAR Models")

###############################
  ######SAR Models###########
#Table 3 Heterogeneous Requires to adjust weight matrices
###############################
  #####Neighbors N>0##########
birdI <- spline.correlog(x= final$LONGITUD[final$treat_i>0],
                         y= final$LATITUDE[final$treat_i>0],
                         z= final$net_tuition_rev_i[final$treat_i>0],
                         resamp= 100,
                         xmax= FALSE,
                         quiet= TRUE,
						 latlon = TRUE)

birdII <- spline.correlog(x= final$LONGITUD[final$treat_i>0],
                         y= final$LATITUDE[final$treat_i>0],
                         z= final$net_price_i[final$treat_i>0],
                         resamp= 100,
                         xmax= FALSE,
                         quiet= TRUE,
						 latlon = TRUE)

birdIII <- spline.correlog(x= final$LONGITUD[final$treat_i>0],
                         y= final$LATITUDE[final$treat_i>0],
                         z= final$prop_inst_aid_i[final$treat_i>0],
                         resamp= 100,
                         xmax= FALSE,
                         quiet= TRUE,
						 latlon = TRUE)

birdIIII <- spline.correlog(x= final$LONGITUD[final$treat_i>0],
                         y= final$LATITUDE[final$treat_i>0],
                         z= final$prop_Pell_grant_i[final$treat_i>0],
                         resamp= 100,
                         xmax= FALSE,
                         quiet= TRUE,
						 latlon = TRUE)
par(mfrow=c(2,2))
plot(birdI, ylab="Moran's I", xlab="Distance in Kilometers")
title(main="A. spline correlogram with 95% CIs: Tuition Revenue (distance bandwidth = 867.37 Km)")
plot(birdII, ylab="Moran's I", xlab="Distance in Kilometers")
title(main="B. spline correlogram with 95% CIs: Net Price (distance bandwidth = 693.28 Km)")
# plot(birdIII, ylab="Moran's I", xlab="Distance in Kilometers")
# title(main="C. spline correlogram with 95% CIs: Prop. Inst. Grant (distance bandwidth = 411.48 Km)")
# plot(birdIIII, ylab="Moran's I", xlab="Distance in Kilometers")
# title(main="D. spline correlogram with 95% CIs: Prop. Pell (distance bandwidth = 252.47 Km)")
#=###############################
coords<-cbind(final$LONGITUD[final$treat_i>0],final$LATITUDE[final$treat_i>0])

## Specific Moran's I values with thresholds depicted in Figure X
nb.dist.band <- dnearneigh(coords, 0, unlist(birdI$real[1]), longlat = TRUE)
distances <- nbdists(nb.dist.band,coords, longlat = TRUE)
max(unlist(nbdists(nb.dist.band,coords, longlat=T)))
invd1a <- lapply(distances, function(x) (1/(x/10)))
invd1a[1]
invd.weightsI <- nb2listw(nb.dist.band, glist = invd1a, style = "B")
moran.test(final$net_tuition_rev_i[final$treat_i>0], listw=invd.weightsI, zero.policy=T, na.action=na.omit)

nb.dist.band <- dnearneigh(coords, 0, unlist(birdII$real[1]), longlat = TRUE)
distances <- nbdists(nb.dist.band,coords, longlat = TRUE)
max(unlist(nbdists(nb.dist.band,coords, longlat=T)))
invd1a <- lapply(distances, function(x) (1/(x/10)))
invd1a[1]
invd.weightsII <- nb2listw(nb.dist.band, glist = invd1a, style = "B")
moran.test(final$net_price_i[final$treat_i>0], listw=invd.weightsII, zero.policy=T, na.action=na.omit)

nb.dist.band <- dnearneigh(coords, 0, unlist(birdIII$real[1]), longlat = TRUE)
distances <- nbdists(nb.dist.band,coords, longlat = TRUE)
max(unlist(nbdists(nb.dist.band,coords, longlat=T)))
invd1a <- lapply(distances, function(x) (1/(x/10)))
invd1a[1]
invd.weightsIII <- nb2listw(nb.dist.band, glist = invd1a, style = "B")
moran.test(final$prop_inst_aid_i[final$treat_i>0], listw=invd.weightsIII, zero.policy=T, na.action=na.omit)

nb.dist.band <- dnearneigh(coords, 0, unlist(birdIIII$real[1]), longlat = TRUE)
distances <- nbdists(nb.dist.band,coords, longlat = TRUE)
max(unlist(nbdists(nb.dist.band,coords, longlat=T)))
invd1a <- lapply(distances, function(x) (1/(x/10)))
invd1a[1]
invd.weightsIIII <- nb2listw(nb.dist.band, glist = invd1a, style = "B")
moran.test(final$prop_Pell_grant_i[final$treat_i>0], listw=invd.weightsIIII, zero.policy=T, na.action=na.omit)

final$lag.net_price_pu4_i    [final$treat_i>0&final$treat_pu4_i==0] <- 0
final$lag.net_price_priv4_i  [final$treat_i>0&final$treat_priv4_i==0] <- 0
final$lag.net_price_profit4_i[final$treat_i>0&final$treat_profit4_i==0] <- 0

m1 <- errorsarlm(formula = net_price_i ~ treat_i + neigh_pu4_i + neigh_priv4_i + neigh_profit4_i + lag.net_price_pu4_i + lag.net_price_priv4_i + lag.net_price_profit4_i + prop_Research_all_i + prop_OpenDoor_all_i + prop_part_time_i + prop_loan_i + pct_living_on_campus_i + transf_rate_FTE_i + Black_i + Hispanic_i + Asian_i + race_other_i + Northeast + Midwest + South + Rural_i + prop_EITC_z + Gini_z + med_income_z + med_rent_z + citzn_born_abroad_z + prop_crime_z + Black_z + Hispanic_z + Asian_z + race_other_z + prop_adlts_up_HS_c + unemplymnt_rate_c + prop_poverty_c + death_rate_c + prop_net_migration_c + State.Gross.Domestic.Product_s + Absolute.Domestic.Migration_s + Non.Farm.Payroll_s + state_sup_per_inhabi_s + Need_based_s + Nonneed_based_s + Nongrant_Aid_s, data = final[final$treat_i>0,], listw = invd.weightsII, weights = undup_count_i) # + prop_pri_NoProf_neigh_i + prop_Profit_neigh_i
summary(m1)
m1P<-data.frame( predict(m1))#For predictions discussed in closing section
m1P$UNITID<-final$UNITID[final$neigh_i>0]

m1$lambda
hNULL<-residuals(m1)
moran.test(hNULL,invd.weightsII, zero.policy=TRUE)

m2 <- errorsarlm(formula = net_tuition_rev_i ~ treat_i + neigh_pu4_i + neigh_priv4_i + neigh_profit4_i + lag.net_price_pu4_i + lag.net_price_priv4_i + lag.net_price_profit4_i + prop_Research_all_i + prop_OpenDoor_all_i + non_resident_alien_i + prop_part_time_i + prop_loan_i + pct_living_on_campus_i + transf_rate_FTE_i + Black_i + Hispanic_i + Asian_i + race_other_i + Northeast + Midwest + South + prop_EITC_z + Gini_z + med_income_z + med_rent_z + rent_income_ratio_z + single_mother_hshld_z + citzn_born_abroad_z + prop_crime_z + Black_z + Hispanic_z + Asian_z + prop_adlts_up_HS_c + unemplymnt_rate_c + prop_poverty_c + death_rate_c + prop_net_migration_c + State.Gross.Domestic.Product_s + Absolute.Domestic.Migration_s + Non.Farm.Payroll_s + state_sup_per_inhabi_s + Need_based_s + Nonneed_based_s + Nongrant_Aid_s, data = final[final$treat_i>0,], listw = invd.weightsI, weights = undup_count_i) #prop_pri_NoProf_neigh_i + prop_Profit_neigh_i + 
m2$lambda
hNULL<-residuals(m2)
moran.test(hNULL,invd.weightsI, zero.policy=TRUE)

m3 <- errorsarlm(formula = prop_inst_aid_i ~ treat_i + neigh_pu4_i + neigh_priv4_i + neigh_profit4_i + lag.net_price_pu4_i + lag.net_price_priv4_i + lag.net_price_profit4_i + prop_Research_all_i + prop_OpenDoor_all_i + non_resident_alien_i + prop_part_time_i + prop_loan_i + pct_living_on_campus_i + transf_rate_FTE_i + Black_i + Hispanic_i + Asian_i + race_other_i + Northeast + Midwest + South + Rural_i + prop_EITC_z + Gini_z + med_income_z + med_rent_z + rent_income_ratio_z + single_mother_hshld_z + citzn_born_abroad_z + prop_crime_z + Black_z + Hispanic_z + Asian_z + race_other_z + prop_adlts_up_HS_c + unemplymnt_rate_c + prop_poverty_c + death_rate_c + prop_net_migration_c + State.Gross.Domestic.Product_s + Absolute.Domestic.Migration_s + Non.Farm.Payroll_s + state_sup_per_inhabi_s + Need_based_s + Nonneed_based_s + Nongrant_Aid_s, data = final[final$treat_i>0,], listw = invd.weightsIII, weights = undup_count_i)
summary(m3)
m3$lambda
hNULL<-residuals(m3)
moran.test(hNULL,invd.weightsIII, zero.policy=TRUE)

final2<-final
final2$lag.net_price_priv4_i  <-final2$lag.net_price_priv4_i  /1000
final2$lag.net_price_profit4_i<-final2$lag.net_price_profit4_i/1000

m4 <- errorsarlm(formula = prop_Pell_grant_i ~ treat_i + neigh_pu4_i + neigh_priv4_i + neigh_profit4_i + lag.net_price_pu4_i + lag.net_price_priv4_i + lag.net_price_profit4_i + prop_Research_all_i + prop_OpenDoor_all_i + non_resident_alien_i + prop_part_time_i + prop_loan_i + transf_rate_FTE_i + Black_i + Hispanic_i + Asian_i + race_other_i + Northeast + Midwest + South + prop_EITC_z + Gini_z + med_income_z + med_rent_z + single_mother_hshld_z + citzn_born_abroad_z + prop_crime_z + Black_z + Hispanic_z + Asian_z + race_other_z + prop_adlts_up_HS_c + unemplymnt_rate_c + prop_poverty_c + death_rate_c + prop_net_migration_c + State.Gross.Domestic.Product_s + Absolute.Domestic.Migration_s + Non.Farm.Payroll_s + state_sup_per_inhabi_s + Need_based_s + Nonneed_based_s + Nongrant_Aid_s, data = final[final$treat_i>0,], listw = invd.weightsIIII, weights = undup_count_i)
summary(m4) #Community colleges surrounded by four-year neighbors tend to enroll greater proportions of full-time Pell recipients, on average. However, the composition of these neighbors does affect this outcome. 
m4$lambda
hNULL<-residuals(m4)
moran.test(hNULL,invd.weightsIIII, zero.policy=TRUE)

stargazer(m1,m2,m3,m4, header=FALSE, type='latex', title = "SAR Models")

#########################################
 ########Replication Ends###############