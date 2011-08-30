extractValues<-function(what, y,...){
  subDf<-subset(y, k==what)
  if(nrow(subDf)==0) return(paste(c("key <", what, "> not available"), collapse=""))
  v<-as.list(unique(subDf$v))
  valueDf<-sapply(v, function(k) sum(subDf$v %in% k))
  names(valueDf)<-unlist(v)
  valueDf<-valueDf[order(valueDf, decreasing=TRUE)]
  valueDf
}

extractValuesAll<-function(y, not, ...){
  keys<-unique(y$k)
  keys<-keys[!(keys %in% not)]
  keylist<-as.list(keys)
  ret<-lapply(keylist, extractValues, y)
  names(ret)<-unlist(keylist)
  le<-sapply(ret, length)
  ret<-ret[names(le[order(le, decreasing=TRUE)])]
  ret
}