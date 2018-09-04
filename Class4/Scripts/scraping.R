library("tidyverse")
library("rvest")
library("stringr")

heads <- "https://en.wikipedia.org/wiki/List_of_current_heads_of_state_and_government" %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
    html_table(fill=TRUE) %>%
    .[[1]]

headstab <- heads %>%
  mutate(leader = gsub(".*– ", "", `Head of government`)) %>%
  mutate(leader = gsub("^Sheikh |^Cardinal |^Prince ", "", leader)) %>%
  mutate(leader = gsub(" \\(.*$|\\[.*$", "", leader)) %>%
  subset(State!="Switzerland" | (State=="Switzerland" & leader=="Alain Berset")) %>%
  mutate(leader = ifelse(leader=="Hasina", "Sheikh Hasina", leader)) %>%
  select(State, leader) %>%
  filter(!duplicated(State))

get_link_leader <- function(text){
  "https://en.wikipedia.org/wiki/List_of_current_heads_of_state_and_government" %>%
    read_html() %>%
    html_nodes(xpath=paste0("//a[text()='", text, "']")) %>% 
    .[[1]] %>% 
    html_attr("href")
}

headstab$link <- ""
for (i in 1:nrow(headstab)) {
  tryCatch({
    headstab$link[i] <- get_link_leader(headstab$leader[i])
    print(paste("Got link for leader", headstab$leader[i]))
  }, error = function(e) {
    print(paste("Error for", headstab$leader[i]))
  })
}

headstab <- headstab %>%
  mutate(link=ifelse(State=="Papua New Guinea", "/wiki/Peter_O%27Neill", link))

partyexample <- "https://en.wikipedia.org/wiki/Jacinda_Ardern" %>%
  read_html() %>%
  html_nodes(xpath='//table[contains(@class, "infobox")]') %>%
  html_table(fill=TRUE) %>%
  .[[1]] %>%
  set_tidy_names() %>%
  filter(.[,1] == "*olitical.*arty") %>%
  .[1,2]

get_political_party <- function(personlink) {
  paste0("https://en.wikipedia.org", personlink) %>%
    read_html() %>%
    html_nodes(xpath='//table[contains(@class, "infobox")]') %>%
    html_table(fill=TRUE) %>%
    .[[1]] %>%
    set_tidy_names() %>%
    filter(grepl('*olitical.*arty', .[,1])) %>%
    .[1,2]
}

headstab$political_party <- ""
for (i in 1:nrow(headstab)) {
  tryCatch({
    headstab$political_party[i] <- get_political_party(headstab$link[i])
    print(paste("Done", headstab$leader[i]))
  }, error = function(e) {
    print(paste("Error for", headstab$leader[i]))
  })
}

linkpartyex <-   paste0("https://en.wikipedia.org/wiki/Dmitry_Medvedev") %>%
  read_html() %>%
  html_nodes(xpath='//table[contains(@class, "infobox")]/tbody/tr/th[text()="Political party"]/../td/a') %>%
  html_attr("href") %>%
  tail(., n=1)

get_link_party <- function(personlink, text){
  paste0("https://en.wikipedia.org", personlink) %>%
    read_html() %>%
    html_nodes(xpath='//table[contains(@class, "infobox")]/tbody/tr/th[text()="Political party"]/../td/a') %>%
    html_attr("href")
}

headstab <- headstab %>%
  mutate(political_party = gsub(" \\(.*$|\\[.*$", "", political_party)) 
  
headstab$link_party <- ""
for (i in 1:nrow(headstab)) {
  tryCatch({
    headstab$link_party[i] <- get_link_party(headstab$link[i], headstab$political_party_first[i])
    print(paste("Got link for party of", headstab$leader[i]))
  }, error = function(e) {
    print(paste("Error for", headstab$leader[i]))
  })
}

partytypeexample <- "https://en.wikipedia.org/wiki/Socialist_Party_of_Albania" %>%
  read_html() %>%
  html_nodes(xpath='//table[contains(@class, "infobox")]') %>%
  html_table(fill=TRUE) %>%
  .[[1]] %>%
  set_tidy_names() %>%
  filter(grepl('*olitical.*osition', .[,1])) %>%
  .[1,2]

get_political_position <- function(partylink){
  paste0("https://en.wikipedia.org", partylink) %>%
    read_html() %>%
    html_nodes(xpath='//table[contains(@class, "infobox")]') %>%
    html_table(fill=TRUE) %>%
    .[[1]] %>%
    set_tidy_names() %>%
    filter(grepl('*olitical.*osition', .[,1])) %>%
    .[1,2]
}

headstab$political_position <- ""
for (i in 1:nrow(headstab)) {
  tryCatch({
    headstab$political_position[i] <- get_political_position(headstab$link_party[i])
    print(paste("Got political position of", headstab$leader[i]))
  }, error = function(e) {
    print(paste("Error for", headstab$leader[i]))
  })
}

library("countrycode")
library("maps")


headstab <- headstab %>%
  mutate(pos=gsub("\\(.*\\)|\\[.*\\]", "", political_position)) %>%
  mutate(pos=gsub("Current:", "", pos)) %>%
  mutate(pos=gsub("Histor.*", "", pos)) %>%
  mutate(pos=gsub("de facto.*", "", pos)) %>%
  mutate(pos=gsub("de jure :", "", pos)) %>%
  mutate(pos=gsub("-", " ", pos)) %>%
  mutate(pos=gsub("big tent of the left", "big tent", pos)) %>%
  mutate(pos=tolower(pos)) %>%
  mutate(pos=str_trim(pos)) %>%
  mutate(pos=ifelse(pos=="", NA, pos)) %>%
  mutate(pos=as_factor(pos)) %>%
  mutate(iso3 = countrycode(State, "country.name", "iso3c")) %>%
  mutate(iso3 = ifelse(State=="Micronesia", "FSM", iso3))
  
map.world <- map_data("world") %>%
  mutate(iso3 = countrycode(region, "country.name", "iso3c")) %>%
  mutate(iso3 = ifelse(region=="Micronesia", "FSM", iso3)) %>%
  left_join(headstab, by="iso3")

plot <- ggplot() +
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group, fill = pos)) +
  scale_color_gradient("#FF0000", "#0000FF")
  # geom_point(data = df.country_points, aes(x = lon, y = lat), color = "#e60000") +
  # scale_fill_manual(values = c("#CCCCCC","#e60000")) +
  # labs(title = 'Countries with highest "talent competitiveness"'
  #      ,subtitle = "source: INSEAD, https://www.insead.edu/news/2017-global-talent-competitiveness-index-davos") +
  # theme(text = element_text(family = "Gill Sans", color = "#FFFFFF")
  #       ,panel.background = element_rect(fill = "#444444")
  #       ,plot.background = element_rect(fill = "#444444")
  #       ,panel.grid = element_blank()
  #       ,plot.title = element_text(size = 30)
  #       ,plot.subtitle = element_text(size = 10)
  #       ,axis.text = element_blank()
  #       ,axis.title = element_blank()
  #       ,axis.ticks = element_blank()
  #       ,legend.position = "none"
  # )

plot
