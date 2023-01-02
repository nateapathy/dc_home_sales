library(tidyverse)
library(readxl)
library(lubridate)
library(ZillowR)

salesfiles <- list.files("Documents/homesdata",
                         pattern = "*.xls*",
                         full.names = T)
salesfiles[[20]]
read_excel(salesfiles[[31]]) %>% 
  janitor::clean_names() %>%
  select(-subsidy) %>% colnames() -> recent_colnames

lapply(readin,colnames) %>% unlist() %>% unique() %>% sort()

lapply(readin,head)

readin <- list()
for (i in c(1:length(salesfiles))) {
  readin[[i]] <- read_excel(path=salesfiles[i]) %>% 
    janitor::clean_names() %>%
    rename_at(vars(matches("zip_code")), ~ "zip") %>%
    rename_at(vars(matches("settled_date")), ~ "close_date") %>%
    rename_at(vars(matches("date")), ~ "close_date") %>%
    rename_at(vars(matches("close$")), ~ "close_date") %>%
    rename_at(vars(matches("closed")), ~ "close_date") %>%
    rename_at(vars(matches("washington")), ~ "city") %>%
    rename_at(vars(matches("dc$")), ~ "state") %>%
    rename_at(vars(matches("b_rs")), ~ "br") %>%
    rename_at(vars(matches("current_price")), ~ "list_price") %>%
    rename_at(vars(matches("close_price")), ~ "sold_price") %>%
    rename_at(vars(matches("sales_price")), ~ "sold_price") %>%
    select(-subsidy)
}

# geocode the addresses
# maps API key
# AIzaSyBXtBjCbzNb804upoXDaOWw-jBiXRZV5Vw
bind_rows(readin) %>% filter(zip %in% c(20001,20009,20005)) %>%
  sample_n(1) %>% 
  mutate(addr=paste(address,city,state,zip)) %>% 
  pull(addr)

st_filter()

str(sf::st_bbox(tpg))

bind_rows(readin) %>% filter(zip %in% c(20001,20009,20005)) %>% dim() # 5971
  filter(zip %in% c(20001,20009,20005) & br>=3) %>% #View()
  mutate(pct_asking=sold_price/list_price*100,
         close_date=floor_date(close_date,"days"),
         close_mo=floor_date(close_date,"months")) %>%
  ggplot(aes(x=factor(close_mo),y=sold_price)) +
  geom_hline(yintercept = 1200000) +
  geom_boxplot(alpha=0.5) +
  scale_y_continuous(labels = scales::label_dollar()) +
  theme_minimal()

# reading in polygon
tpg <- sf::st_read("~/Downloads/the polygon.kml")
sf::st_bbox(tpg)
tpg$geometry

ggplot(data=tpg) +
  geom_sf()

ZillowR::get_zillow_web_service_id()
ZillowR::GetDeepSearchResults(address = "2014 2nd St NW",
                                citystatezip = "Washington DC 20001",
                                zws_id = )
