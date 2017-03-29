
`%>%` <- magrittr::`%>%`

x <- "http://www.politico.com/2016-election/results/map/president" %>% 
  xml2::read_html() %>% 
  rvest::html_table(fill=TRUE)

all_states <- c(state.name[1:8], "District of Columbia", state.name[9:length(state.name)])
names(x) <- all_states

final_df <- purrr::map(
  seq_along(x),
  function(i){
    temp <- x[[i]]
    names(temp) <- c("candidate", 'percent', 'total', 'ec_votes')
    
    temp <- temp %>% 
      dplyr::mutate(
        stname   = names(x)[i],
        percent  = as.numeric(gsub("%", "", percent))/100,
        total    = as.numeric(gsub(",", "", total)),
        ec_votes = ifelse(is.na(ec_votes), 0, ec_votes),
        year     = 2016
      ) %>% 
      dplyr::filter(grepl("Trump|Clinton",candidate))
  }
) %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate(
    candidate = gsub("^R Winner D. *|^R D. *|^D Winner H. *|^D H. ","",candidate)
    )

final_df
