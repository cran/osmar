getOSMObject <-
function(XML, reduced=FALSE, crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")){
 
  if(reduced==TRUE){
    bbox<-formatBbox(XML, borders=TRUE)  
  } else{
    bbox<-formatBbox(XML, borders=FALSE)      
  }

  nodes_1        <-  getXMLData(bbox[["bboxnodes"]])                     
  nodes_2        <-  node2SPDF(bbox[["bboxnodes"]], crs)                 
  ways_1         <-  getXMLData(bbox[["bboxways"]])                      
  if(class(nodes_2)=="SpatialPointsDataFrame"){
    ways_2       <-  way2SLDF(bbox[["bboxways"]], nodes_2@data, crs)     
    }else{ ways_2  <- getXMLMeta(bbox[["bboxways"]])}                      
  ways_3         <-  getXMLMember(bbox[["bboxways"]])                    
  relations_1    <-  getXMLMeta(bbox[["bboxrelations"]])                 
  relations_2    <-  getXMLData(bbox[["bboxrelations"]])                
  relations_3    <-  getXMLMember(bbox[["bboxrelations"]])       
 
  ret<- OSM(Node(nodes_1, nodes_2),
            Way(ways_1, ways_2, ways_3),
            Relation(relations_1, relations_2, relations_3))
  ret
}