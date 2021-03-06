# call.indicators

call.indicators <- function(language.en = TRUE) {
  
  
  require(rvest)
  require(tidyverse)
  
  
  if(language.en == TRUE) {
    
    url.indicators <- 'https://estadisticas.cepal.org/sisgen/ws/cepalstat/getThematicTree.asp'
    
    }
  
  
  else {
   
    url.indicators <- 'http://interwp.cepal.org/sisgen/ws/cepalstat/getThematicTree.asp?language=spanish'
    
    }
  
  
  xml.page <- read_xml(url.indicators)
  
  n <- xml.page %>% 
    xml_find_all('item') %>% 
    length()
  
  
  df.list <- list()
  
  for(i in 1 : n) {
    
    # area
    area <- xml.page %>% 
      xml_child(i) %>% 
      xml_attrs()
    
    # subarea
    
    m <- xml.page %>% 
      xml_child(1) %>% 
      xml_length()
    
    subdata.list <- list()
    
    for(k in 1 : m) {
      
      subarea <- xml.page %>% 
        xml_child(i) %>% 
        xml_child(k) %>% 
        xml_attrs()
      
      # indicators
      
      indicators.list <- xml.page %>% 
        xml_child(i) %>% 
        xml_child(k) %>% 
        xml_find_all('.//item') %>% 
        xml_attrs()
      
      indicators <- do.call(rbind, indicators.list) %>% data.frame()
      
      indicators$name.sa <- subarea[1]
      indicators$id_area.sa <- subarea[2]
      indicators$name.a <- area[1]
      indicators$id_area.a <- area[2]
      
      subdata.list[[k]] <- indicators
      
    }
    
    df.list[[i]] <- do.call(rbind, subdata.list) %>% data.frame()
    
    
  }
  
  indicators <- do.call(rbind, df.list) %>% data.frame()
  
}
