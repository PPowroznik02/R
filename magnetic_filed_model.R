library(spatstat)
library(rgdal)
library(sf)

library(maptools)
library(automap)
library(raster)

data<-read.csv("Wyniki_em_2019.csv",header=TRUE,encoding="UTF-8")

#usuniecie "\"
pom0<-substr(data$Szeroko??.geograficzna,1,nchar(data$Szeroko??.geograficzna)-1)

#podzial na stopnie minuty sekundy
pom1<-strsplit(pom0, "?" )
pom2<-strsplit(unlist(pom1), "' " )

lats<-as.numeric(unlist(pom2))

d<-lats[seq(1, length(lats), 3)]
m<-lats[seq(2, length(lats), 3)]
s<-lats[seq(3, length(lats), 3)]

#do decimal
lat<-d+m/60+s/3600

pom10<-substr(data$D?ugo??.geograficzna,1,nchar(data$D?ugo??.geograficzna)-1)

pom11<-strsplit(pom10, "?" )
pom12<-strsplit(unlist(pom11), "' " )

lons<-as.numeric(unlist(pom12))

#stopnie, min, sek dla dlugosci
d<-lons[seq(1, length(lons), 3)]
m<-lons[seq(2, length(lons), 3)]
s<-lons[seq(3, length(lons), 3)]

#do decimal
lon<-d+m/60+s/3600

#od razu wartosci pola
value<-as.numeric(data$Wynik.pomiaru..V.m.)
####value[value<0.3]<-0.3

#ramki danych dla Polski
dataP<-data.frame(longitude=lon,latitude=lat)
dataP$value<-value


dzielnice<-st_read("dzielnice_Krakowa.shp")
wojewodztwa<-st_read("Wojewodztwa.shp")



#transformacja do WGS
dzielniceWGS<-st_transform(dzielnice,crs = 4326)
wojewodztwaWGS<-st_transform(wojewodztwa,crs = 4326)


#polaczenie dzielnic
krakowWGS<-st_union(st_geometry(dzielniceWGS))
woj<-wojewodztwaWGS[1,'geometry']
krakowWGS<-st_union(st_geometry(woj))

#przejscie z sfery (lat lon) na uklad utm (małopolska w 34N)
krakowUTM<-st_transform(krakowWGS,CRS("+proj=utm +zone=34N +datum=WGS84"))


#to samo z danymi o czujnikach
data_spat<-data.frame(lon=dataP$longitude,lat=dataP$latitude,value=dataP$value)
coordinates(data_spat) <- ~lon+lat
proj4string(data_spat) <- CRS("+proj=longlat +datum=WGS84")
data_spat

#konwersja
data_UTM <- spTransform(data_spat, CRS("+proj=utm +zone=34N +datum=WGS84"))

#stworzenie obiektu ppp 2D
dataP_ppp<-ppp(x=data_UTM$lon,y=data_UTM$lat,window=as.owin(krakowUTM))
#dataP_malopolskie<-ppp(x=data_UTM$lon,y=data_UTM$lat,window=as.owin(malopolskieUTM))

#stworzenie ppp z marks czyli z danymi w punktach
dataP_ppp_v<-ppp(x=data_UTM$lon,y=data_UTM$lat,marks=data_UTM$value,window=as.owin(krakowUTM))


plot (dataP_ppp)
plot (dataP_ppp_v)


#znowu konwersja
dataP_spdf<-as.SpatialPointsDataFrame.ppp(dataP_ppp_v)

spplot(dataP_spdf)
coordinates(dataP_spdf)



#Ordinary Kriging
elev_auto <- autoKrige(marks ~ 1, input_data = dataP_spdf,model = "Mat")
plot(elev_auto)


plot(elev_auto$krige_output[1])
points(dataP_ppp_v,pch="*",col="White")
plot(Window(dataP_ppp_v),add=TRUE)
plot(krakowUTM,add=TRUE,border="Black")


#okreslenie siatki punktow do proksymacji
coord<-as.data.frame(st_coordinates(krakowUTM))
left_down<-c( min(coord$X), min(coord$Y))


#rozmiar oczka siatki w metrach
size<-c(100,100)

#wyliczenie ile puntow potrzeba w najwiekszym wymiarze
right_up<-c( max(coord$X), max(coord$Y))
points<- (right_up-left_down)/size
num_points<-ceiling(points)



bound<- GridTopology(left_down, size,num_points)

eps<-10
num_points<-num_points+eps

grid <- GridTopology(left_down, size,num_points)

#kowersja do odpowiedniego formatu w odpowiednim uk?adzie
gridpoints <- SpatialPoints(grid, proj4string = CRS("+proj=utm +zone=34N +datum=WGS84"))


#przyciecie do prostokat w ktorym miesci sie Krakow
cropped_gridpoints <- crop(gridpoints,bound )
plot(cropped_gridpoints,add=TRUE,col="Red")


#konwersja do SpatialPixels
spgrid <- SpatialPixels(cropped_gridpoints)
coordnames(spgrid) <- c("x", "y")



elev_auto <- autoKrige(marks ~ 1, input_data = dataP_spdf,new_data=spgrid,model = "Mat")
plot(elev_auto$krige_output[1])
points(dataP_ppp_v,pch="*",col="White")
plot(krakowUTM,add=TRUE,border="White")

plot(elev_auto$krige_output[3])



library(sp)
cor<-st_coordinates(krakowUTM)
cor_f<-cor[cor[,3]==1,]
p = Polygon(cbind(cor_f[,1],cor_f[,2]))
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
plot(gridpoints,col="Red")
plot(sps, add=TRUE)
plot(sps)


cropped_gridpoints <- intersect(gridpoints,sps)
plot(cropped_gridpoints,col="Red")
plot(Window(dataP_malopolskie_v),add=TRUE)


#konwersja do SpatialPixels
spgrid <- SpatialPixels(cropped_gridpoints)
coordnames(spgrid) <- c("x", "y")
plot(spgrid)
plot(Window(dataP_ppp_v),add=TRUE)


elev_auto <- autoKrige(marks ~ 1, input_data = dataP_spdf,new_data=spgrid,model = "Mat")
plot(elev_auto$krige_output[1])
points(dataP_ppp_v,pch="*",col="White")


#blad dopasowania
plot(elev_auto$krige_output[3])
points(dataP_ppp_v,pch="*",col="White")



elev_auto$krige_output[1]


a<-elev_auto$krige_output$var1.pred
b<-rep("NA",length(a))
b<-ifelse(a<=0.3,1,b)
b<-ifelse(a>0.3 & a<=0.4,2,b)
b<-ifelse(a>0.4,3,b)
elev_auto$krige_output$var1.factor<-as.factor(b)
plot(elev_auto$krige_output[4])