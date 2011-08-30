bbox2coords <-
function(center, area){
  stopifnot(center[1]<=180 & center[1]>=-180)
  stopifnot(center[2]<=90 & center[2]>=-90)
  stopifnot(length(c(center,area))==4)
  center<-as.numeric(center)
  lon<-center[1]
  lat<-center[2]
  width<- area[1]/2
  height<-area[2]/2
  a<-6378137
  esq<-(2-(1/298.257223563))*(1/298.257223563)
  W<-sqrt(1- esq*(sin(lat*pi/180))^2)
  
  M<-a*(1-esq)/W^3
  mPerLatD<-1/ ((pi/180)*M)
  top<-lat+ mPerLatD*height   
  bottom<-lat- mPerLatD*height

  N<-a/W
  mPerLonD<- 1/ ((pi/180) * N * cos(lat*pi/180))
  left <- lon- mPerLonD*width
  right<- lon+ mPerLonD*width
  if(left< -180) left<-left+360
  if(right>180) right<-right-360
  ret<- c(left, bottom, right, top)
  names(ret)<- c("minlon", "minlat", "maxlon", "maxlat") 
  ret  
}