# Call Data ----

call.data <- function(id.indicator, language.en = TRUE) {
  
  require(rvest)
  require(tidyverse)
  
  if(language.en == TRUE) { 
    
    url.metadata <- paste0('https://estadisticas.cepal.org/sisgen/ws/cepalstat/getDataMeta.asp?idIndicator=',
                           id.indicator)
    
    url.dimensions <- paste0('https://estadisticas.cepal.org/sisgen/ws/cepalstat/getDimensions.asp?idIndicator=',
                             id.indicator)
  } 
  
  else {
    url.metadata <- paste0('https://estadisticas.cepal.org/sisgen/ws/cepalstat/getDataMeta.asp?idIndicator=',
                           id.indicator,
                           '&language=spanish')
    
    url.dimensions <- paste0('https://estadisticas.cepal.org/sisgen/ws/cepalstat/getDimensions.asp?idIndicator=',
                             id.indicator,
                             '&language=spanish')
  }
  
  # metadata 
  
  xml.page <- read_xml(url.metadata)
  
  list <- xml.page %>%
    xml_child(3) %>% # enter the third node
    xml_child(2) %>% # enter the second node within the third
    xml_find_all('//des') %>% 
    xml_attrs()
  
  metadatos <- do.call(rbind, list) %>% data.frame()
  
  
  # dimensions
  
  xml.page <- read_xml(url.dimensions)
  
  list <- xml.page %>% 
    xml_find_all('dim') %>% 
    xml_attrs()
  
  dimensions <- do.call(rbind, list) %>% data.frame()
  
  dimensions$id <- paste0('dim_',dimensions$id)
  
  
  # data 
  
  url.valores <- paste0('https://estadisticas.cepal.org/sisgen/ws/cepalstat/getDataWithoutMeta.asp?IdIndicator=',id.indicator)
  
  xml.page <- read_xml(url.valores)
  
  list <- xml.page %>% 
    xml_find_all('dato') %>% 
    xml_attrs()
  
  datos <- do.call(rbind, list) %>% data.frame()
  
  names <- names(datos)
  
  names <- names[1:(length(names)-4)]
  
  
  for( i in names) {
    
    new.col <- datos[,i] %>% data.frame()
    
    names(new.col) <- 'id'
    
    new.col <- new.col %>% 
      left_join(metadatos, by = 'id') %>% 
      select(2)
    
    names(new.col) <- dimensions$name[dimensions$id == i]
    
    datos <- cbind(datos, new.col)
    
  }
  
  return(datos)
  
}