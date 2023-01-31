library(rvest)
library(data.table)

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

url <- 'https://bulbapedia.bulbagarden.net/wiki/Category:Pok%C3%A9mon_learnsets_(Generation_I)'
page <- read_html(url)

learnsets <- page %>% html_nodes("div.mw-category-group ul li a") %>% html_attr('href')
learnsets <- paste0('https://bulbapedia.bulbagarden.net',learnsets)

# remove metapod and kakuna, butterfree and beedrill
learnsets <- learnsets[c(1:10,13,15:length(learnsets))]


pkmn_df <- list()

for(i in 1:length(learnsets)){
  msg <- paste0("Working on ",i,", ",learnsets[i])
  print(msg)
  
  pkmn_moves <- learnsets[i]
  move_page <- read_html(pkmn_moves)
  
  pkmn_name <- (move_page %>% html_nodes("small a") %>% html_text2())[1]
  pkmn_name <- substr(pkmn_name,3,nchar(pkmn_name))
  
  pkmn_img <- move_page %>% html_nodes("small img") %>% html_attr('src')
  pkmn_img <- paste0('https:',pkmn_img)
  
  lvl_moves <- (move_page %>% html_nodes("table.sortable") %>% html_table())[[1]]
  lvl_moves$Level <-as.integer(substr(lvl_moves$Level,0,nchar(lvl_moves$Level)/2))
  lvl_moves$Power <- substr(lvl_moves$Power,0,3)
  lvl_moves$Accuracy <- substr(lvl_moves$Accuracy,0,3)
  lvl_moves$Method <- 'TM/HM'
  
  tm_table <- (move_page %>% html_nodes("table") %>% html_table())[[5]]
  tm_table <- tm_table[,c(2:7)]
  tm_table <- tm_table[c(5:nrow(tm_table)),]
  
  tm_df <- as.data.frame(tm_table)
  tm_df <- header.true(tm_df)
  
  rows <- nrow(tm_df) -1
  tm_df <- tm_df[c(1:rows),]
  tm_df$Level <- 0
  
  tm_df$Move <- paste0(tm_df$TM," - ",tm_df$Move)
  tm_df$Method <- 'TM/HM'
  
  tm_df <- tm_df[,names(lvl_moves)]
  
  all_moves <- rbind(lvl_moves,tm_df)
  all_moves$pkmn_name <- pkmn_name
  all_moves$pkmn_img <- pkmn_img
  
  pkmn_df[[i]] <- all_moves
  Sys.sleep(20)
}

pkmn_movesets_df <- data.table::rbindlist(pkmn_df)
write.csv(pkmn_movesets_df,"Gen1_movesets_by_pkmn.csv",row.names = F)
