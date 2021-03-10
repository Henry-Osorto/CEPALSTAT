#' call.data
#'
#' You can consult the data of CEPALSTAT indicators.
#'
#' @param id.indicator It requests the number of the indicator according to the names consulted in the call.indicators function.
#' @param language.en If language.en is TRUE, it will return the names of the indicators in English. If it is FALSE, it will return the names of the indicators in Spanish.

#' @export call.data

#'@importFrom xml2 read_xml
#'@importFrom xml2 xml_find_all
#'@importFrom xml2 xml_child
#'@importFrom xml2 xml_attrs
#'@importFrom xml2 xml_length
#'@importFrom dplyr %>%
#'@importFrom dplyr left_join
#'@importFrom dplyr select




call.data <- function(id.indicator, language.en = TRUE) {



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
