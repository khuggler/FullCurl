#' @title Create dynamic Shiny maps of most recent sheep locations
#' @description 
#' @param vecpath path to where GPS data is stored
#' @param sheepdb path to where capture database is stored
#' @param idcol name of column that should be subset (AID, CollarSerial, etc)
#' @param nlocs number of locations to include in map
#' @param xcol name of column where x coordinates are stored
#' @param ycol name of column where y coordinates are stored
#' @param proj character string of projection for coordinates
#' @return Returns a raster brick with all climate data for specific year
#' @keywords climate, pdsi, temperature, swe
#' @export
#' @examples
#' 

sheep.map<-function(vecpath, sheepdb, idcol, nlocs, xcol, ycol, proj){
  
  sheep<-FullCurl::sheep.gps(vecpath = vecpath, sheepdb = sheepdb)

  uni<-unique(sheep[,idcol])
  
  new.sub<-data.frame()
  for(k in 1:length(uni)){
    sub<-sheep[sheep[,idcol] == uni[k],]
    sub<-sub[order(sub$TelemDate, decreasing = TRUE),]
    
    subsub<-sub[1:nlocs,]
    
    new.sub<-rbind(subsub, new.sub)
  }
  
  sp::coordinates(new.sub)<-c(xcol, ycol)
  sp::proj4string(new.sub)<-proj
  
  pal <- colorRampPalette(brewer.pal(16, "Spectral"))
  map<-mapview::mapview(new.sub, zcol = "AID", cex = 8, map.types = 'OpenTopoMap', col.regions = pal)
  
  return(map)
}