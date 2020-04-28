## Baseball Card Webscrape 

library(Rcrawler)
library(rvest)
library(tidyverse)
library(zoo)
library(httr)

x <- LinkExtractor('https://www.tcdb.com/ViewAll.cfm/sp/basketball?MODE=Years' , 
                   urlregexfilter = 'year')

df <- data.frame(link = x$InternalLinks) %>% 
  mutate(year = str_extract(link , '\\d+$') , 
         year = as.integer(year) ,
         season = paste(year , year + 1 , sep = "-" )) %>% 
  select(year , season , link) %>% 
  arrange(desc(year))

df <- df %>% filter(year >= 1965)

replacements <- c("-" = ' ' , '/' = '' , '&amp;' = 'and')

setlist <- data.frame()

for (i in 1:nrow(df)) {
  link <- as.character(df$link[i])
  year <- df$year[i]
  x <- LinkExtractor(link , urlregexfilter = 'sid')
  tf <- data.frame(link = x$InternalLinks) %>% 
    mutate(set = str_extract_all(link , paste0('(?<=/' , year , '-).+$')) , 
           sid = str_extract(link , paste0('/\\d+/(?=' , year , ')')) ,
           year = year
    ) %>% 
    mutate_at(c('set' , 'sid') , function(x) str_replace_all(x , replacements)) %>% 
    select(sid , year , set , link) %>%  filter(str_detect(link , '/Gallery.cfm'))
  types <- read_html(link) %>% html_nodes('div.more') %>% html_text() %>% 
    str_split("\\|") %>% as.data.frame() %>% rename(x = 1) %>% 
    filter(str_detect(x , '[:alpha:]')) %>% mutate(x = str_trim(x , 'both')) %>% 
    pull(x)
  x <- read_html(link) %>% html_nodes('div.block1') %>% .[[2]] %>% html_text()
  x <- x %>% str_split('\\n') %>% as.data.frame() %>% rename(x = 1) %>% 
    filter(str_detect(x , "\\w")) %>% mutate(x = str_trim(x , 'both'))
  x <- x %>% 
    mutate(type = mapply(function(x) if (x %in% types) return(x) else NA , x) ,
           type = as.character(type))
  x$type <- na.locf(zoo(x$type)) ; x <- x %>% filter(!x %in% types)
  sid <- read_html(link) %>% html_nodes(xpath = '//*[@id="content"]/div[1]/div[2]/ul/li/a[1]') %>% 
    html_attr('href') %>% str_extract('(?<=/sid/)\\d+/') %>% str_remove_all("/$")
  x$sid <- sid ; x <- x %>% select(sid , type)
  
  tf <- tf %>% left_join(x)
  
  setlist <- bind_rows(setlist , tf)
  cat(paste0(year , ' complete\n'))
}

setlist <- setlist %>% filter(str_detect(link , '/Gallery.cfm'))

cards <- data.frame()

for (i in  1:nrow(setlist)) {
  tmp.cards <- data.frame() 
  link <- as.character(setlist$link[i]) ; year <- setlist$year[i] 
  sid <- setlist$sid[i] ; set <- setlist$set[i]
  x <- LinkExtractor(link , urlregexfilter = 'PageIndex')
  p <- data.frame(link = x$InternalLinks) %>% 
    mutate(page_index = str_extract(link , '(?<=\\=)\\d+$') , 
           page_index = as.integer(page_index)) 
  max.pg <- max(p$page_index)
  link_tmp <- p$link[p$page_index == max.pg] %>% str_remove_all(paste0(max.pg , '$')) ; rm(p)
  if(is.infinite(max.pg)) max.pg <- 1
  for (p in 1:max.pg) {
    if(max.pg == 1) link <- link else link <- paste0(link_tmp , p) 
    x <- LinkExtractor(link , urlregexfilter = 'ViewCard')
    x2 <- data.frame(link = x$InternalLinks) %>% 
      mutate(cid = str_extract_all(link , '(?<=cid/)\\d+(?=/)') , 
             player = str_extract(link , '(?<=-)\\d+.+$') , 
             player = str_remove_all(player , '\\d|\\.') ,
             player = str_replace_all(player , '-'  , ' ') , 
             sid = sid , year = year) %>% 
      select(sid , year , cid , player , link) #%>% 
    
    x <- read_html(link) %>% html_nodes('img')
    x <- data.frame(img =  x %>% html_attr('data-original') ,
                    alt = x %>% html_attr('alt')) 
    x <- x %>% filter(!is.na(img) , str_detect(img , 'Bk.jpg$|SampleCards|DefaultBack' , negate = T))
    try(x2 <- bind_cols(x2 , x))
    try(tmp.cards <- bind_rows(tmp.cards , x2))
  }
  
  try(cards <- bind_rows(cards , tmp.cards))
  cat(sprintf('%s: %s complete\n' , year , set))
}

# write_rds(cards , 'cards_tmp_save.RDS')

cards <- cards %>% mutate(link_front = paste0('https://www.tcdb.com' , img)  , 
                          link_back = gsub('Fr\\.jpg$' , 'Bk\\.jpg' , link_front) , 
                          label = gsub(' Front$' , '' , alt)
)

readr::write_rds(cards , 'temp_card_save.RDS')

## Create Type-League Heirarchy ----
types <- setlist %>% select(type) %>% distinct() %>% 
  mutate(league = if_else(!type %in% c('WNBA' , 'College' , 'Japanese League' , 'Draft' , 
                                       'Postcards' , 'Minor Leagues' , 'Test Issues' , 
                                       'Unlicensed' , 'Team Photos') , 
                          'NBA', type) , 
         league = if_else(type %in% c('Oddball' , 'Promo' , 'Unlicensed' , 
                                      'Test Issues' , 'Postcards') ,
                          'Other' , league))

# Join Types to Cards ----

cards <- cards %>% 
  left_join(setlist %>% select(-link)) %>% 
  left_join(types)

cards <- cards %>% 
  select(league , type , sid , year , set , cid , 
         player , label , link , img , link_front , link_back)

rm(list = setdiff(ls() , c('df' , 'setlist' , 'cards' , 'types')))

# write_rds(cards , 'data.RDS')
# write_rds(setlist , 'data_directory.RDS') 
# write_rds(types , 'type_hier.RDS')

