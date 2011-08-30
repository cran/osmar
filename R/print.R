print.OSM<- function(x,...){
  str(x, max.level=1)
}

print.Node<-function(x,...){
  str(x, max.level=1)
}

print.Way<-function(x,...){
  str(x, max.level=1)
}

print.Relation<-function(x,...){
  str(x, max.level=1)
}

print.summary.Node<- function(x,...){
  x<-x[!names(x) %in% c("allValue", "topUser")]
  print(x)
}

print.summary.Way<- function(x,...){
  x<-x[!names(x) %in% c("allValue", "topUser")]
  print(x)
}

print.summary.Relation<- function(x,...){
  x<-x[!names(x) %in% c("allValue", "topUser")]
  print(x)
}

print.summary.OSM <- function(x,...){
  print(x$number)
  cat(c("\n","timestamp"), sep="\n")
  print(x$times)
  cat(c("\n","BoundingBox"), sep="\n")
  print(x$Bbox)
}