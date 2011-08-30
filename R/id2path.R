id2path <-
function(ID, element="relation", operator){
      ## verwandelt IDs in xpathargument für relation oder ways
      ##  um sämtliche relations/ways zu finden, wo die node vorkommt.
  if(element=="relation"){
    if(operator=="and") operator<-"]andmember["
    ret<-paste("//relation[member[@ref=\"",paste(ID,collapse=paste(c("\"",operator,"@ref=\""),collapse="")),"\"]]",sep="" )
    return(ret)
  }
  if(element=="way"){
    if(operator=="and") operator<-"]andnd["
    ret<- paste("//way[nd[@ref=\"",paste(ID,collapse=paste(c("\"",operator,"@ref=\""),collapse="")),"\"]]",sep="" )
    return(ret)
  }
}