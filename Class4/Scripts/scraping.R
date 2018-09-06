library("tidyverse")
library("rvest")
library("stringr")

get_heads_tab <- "https://en.wikipedia.org/wiki/List_of_current_heads_of_state_and_government" %>%
    read_html() %>%
    html_nodes(xpath = '/html/body/div[3]/div[3]/div[4]/div/table[2]') %>%
    html_table(fill=TRUE) %>%
    .[[1]]

heads <- get_heads_tab %>%
  mutate(leader = gsub(".*– ", "", `Head of government`)) %>%
  mutate(leader = gsub("^Sheikh |^Cardinal |^Prince ", "", leader)) %>%
  mutate(leader = gsub(" \\(.*$|\\[.*$", "", leader)) %>%
  subset(State!="Switzerland" | (State=="Switzerland" & leader=="Alain Berset")) %>%
  mutate(leader = ifelse(leader=="Hasina", "Sheikh Hasina", leader)) %>%
  select(State, leader) %>%
  filter(!duplicated(State))

get_link_leader_example <-  "https://en.wikipedia.org/wiki/List_of_current_heads_of_state_and_government" %>%
  read_html() %>%
  html_nodes(xpath="//a[text()='Alain Berset']") %>% 
  .[[1]] %>% 
  html_attr("href")

get_link_leader <- function(leader_name){
  "https://en.wikipedia.org/wiki/List_of_current_heads_of_state_and_government" %>%
    read_html() %>%
    html_nodes(xpath=paste0("//a[text()='", leader_name, "']")) %>% 
    .[[1]] %>% 
    html_attr("href")
}

heads$link <- ""
for (i in 1:nrow(heads)) {
  tryCatch({
    heads$link[i] <- get_link_leader(heads$leader[i])
    print(paste("Got link for leader", heads$leader[i]))
  }, error = function(e) {
    print(paste("Error for", heads$leader[i]))
  })
}

heads <- heads %>%
  mutate(link=ifelse(State=="Papua New Guinea", "/wiki/Peter_O%27Neill", link))

### Get information on parties

get_party_example <- "https://en.wikipedia.org/wiki/Donald_Trump" %>%
  read_html() %>%
  html_nodes(xpath='//table[contains(@class, "infobox")]') %>%
  html_table(fill=TRUE) %>%
  .[[1]] %>%
  set_tidy_names() %>%
  filter(grepl("*olitical.*arty", .[,1])) %>%
  .[1,2]

get_party <- function(leader_link) {
  paste0("https://en.wikipedia.org", leader_link) %>%
    read_html() %>%
    html_nodes(xpath='//table[contains(@class, "infobox")]') %>%
    html_table(fill=TRUE) %>%
    .[[1]] %>%
    set_tidy_names() %>%
    filter(grepl('*olitical.*arty', .[,1])) %>%
    .[1,2]
}

heads$political_party <- ""
for (i in 1:nrow(heads)) {
  tryCatch({
    heads$political_party[i] <- get_party(heads$link[i])
    print(paste("Done", heads$leader[i]))
  }, error = function(e) {
    print(paste("Error for", heads$leader[i]))
  })
}

get_link_party_example <- "https://en.wikipedia.org/wiki/%C3%89douard_Philippe" %>%
  read_html() %>%
  html_nodes(xpath='//table[contains(@class, "infobox")]/tbody/tr/th[text()="Political party"]/../td/a') %>%
  html_attr("href") %>%
  tail(., n=1)

get_link_party <- function(leader_link){
  paste0("https://en.wikipedia.org", leader_link) %>%
    read_html() %>%
    html_nodes(xpath='//table[contains(@class, "infobox")]/tbody/tr/th[text()="Political party"]/../td/a') %>%
    html_attr("href") %>%
    tail(., n=1)
}
  
heads$link_party <- ""
for (i in 1:nrow(heads)) {
  tryCatch({
    heads$link_party[i] <- get_link_party(heads$link[i])
    print(paste("Got link for party of", heads$leader[i]))
  }, error = function(e) {
    print(paste("Error for", heads$leader[i]))
  })
}

### Exercise

get_political_position_example <- "https://en.wikipedia.org/wiki/Socialist_Party_of_Albania" %>%
  read_html() %>%
  html_nodes(xpath='//table[contains(@class, "infobox")]') %>%
  html_table(fill=TRUE) %>%
  .[[1]] %>%
  set_tidy_names() %>%
  filter(grepl('*olitical.*osition', .[,1])) %>%
  .[1,2]

get_political_position <- function(party_link){
  paste0("https://en.wikipedia.org", party_link) %>%
    read_html() %>%
    html_nodes(xpath='//table[contains(@class, "infobox")]') %>%
    html_table(fill=TRUE) %>%
    .[[1]] %>%
    set_tidy_names() %>%
    filter(grepl('*olitical.*osition', .[,1])) %>%
    .[1,2]
}

heads$political_position <- ""
for (i in 1:nrow(heads)) {
  tryCatch({
    heads$political_position[i] <- get_political_position(heads$link_party[i])
    print(paste("Got political position of", heads$leader[i]))
  }, error = function(e) {
    print(paste("Error for", heads$leader[i]))
  })
}

library("countrycode")
library("maps")

heads <- heads %>%
  mutate(political_position_clean=gsub("\\(.*\\)|\\[.*\\]", "", political_position)) %>%
  mutate(political_position_clean=gsub("Current:", "", political_position_clean)) %>%
  mutate(political_position_clean=gsub("Histor.*", "", political_position_clean)) %>%
  mutate(political_position_clean=gsub("de facto.*", "", political_position_clean)) %>%
  mutate(political_position_clean=gsub("de jure :", "", political_position_clean)) %>%
  mutate(political_position_clean=gsub("-", " ", political_position_clean)) %>%
  mutate(political_position_clean=tolower(political_position_clean)) %>%
  mutate(political_position_clean=str_trim(political_position_clean)) %>%
  mutate(political_position_clean=ifelse(political_position_clean=="", NA, political_position_clean)) %>%
  mutate(political_position_clean=ifelse(is.na(political_position_clean), "unavailable", political_position_clean)) %>%
  mutate(political_position_clean=gsub("center", "centre", political_position_clean)) %>%
  mutate(political_position_clean=gsub("big tent of the left", "big tent", political_position_clean)) %>%
  mutate(political_position_clean=gsub("big tent", "centre/big tent", political_position_clean)) %>%
  mutate(political_position_clean=gsub("^centre$", "centre/big tent", political_position_clean)) %>%
  mutate(political_position_clean=as_factor(political_position_clean)) %>%
  mutate(iso3 = countrycode(State, "country.name", "iso3c")) %>%
  mutate(political_position_clean=factor(political_position_clean,
                                         levels =c("far left","left wing","centre left to left wing", "centre left", 
                                                   "centre to centre left", "centre/big tent", "centre to centre right", "centre right",
                                                   "centre right to right wing", "right wing", "unavailable"),
                                         labels =c("far left","left wing","centre left to left wing", "centre left", 
                                                   "centre to centre left", "centre/big tent", "centre to centre right", "centre right",
                                                   "centre right to right wing", "right wing", "unavailable"))) %>%
  mutate(iso3 = ifelse(State=="Micronesia", "FSM", iso3))

map.world <- map_data("world") %>%
  mutate(iso3 = countrycode(region, "country.name", "iso3c")) %>%
  mutate(iso3 = ifelse(region=="Micronesia", "FSM", iso3)) %>%
  left_join(heads, by="iso3")

plot <- ggplot() +
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group, fill = political_position_clean)) +
  scale_fill_manual(values = c("#FF0000","#E2001C","#C60038", "#AA0055", "#8D0071", 
                               "#71008D", "#5500AA", "#3800C6","#1C00E2", "#0000FF", "#bebebe"))

plot
ggsave("Output/worldmappolitics.pdf", device = "pdf")