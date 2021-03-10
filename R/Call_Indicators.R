# 'call.indicators
#'
#'You can consult the names of the CEPALSTAT indicators
#'
#'@param language.en If language.en is TRUE, it will return the names of the indicators in English. If it is FALSE, it will return the names of the indicators in Spanish.

#' @export call.indicators

#'@importFrom xml2 read_xml
#'@importFrom xml2 xml_find_all
#'@importFrom xml2 xml_child
#'@importFrom xml2 xml_attrs
#'@importFrom xml2 xml_length
#'@importFrom dplyr %>%
#'@importFrom dplyr left_join
#'@importFrom dplyr select

call.indicators <- function(language.en = TRUE) {

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
