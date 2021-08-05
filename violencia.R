# DashBorad Interativo pra a nalise de Incidentes e Atos  de violencia




# Carrega pacotes na sessÃ£o (tem que ser feito a cada sessÃ£o R)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Carrega os dados e cria o dataframe
?read.csv
dados <- read.csv("dados/GTD.csv", sep = ",") 
View(dados)

# Organiza os dados pelo ano
?mutate
dados <- dados %>%
  mutate(year = iyear) %>%
  select(-c(1))

View(dados)

# Vamos realizar diversos agrupamentos de dados para alimentar nosso dashboard

# Agrupamento 1 - VariÃ¡vel nkill (nÃºmero de mortos)

# Remove registros com valores NA na variÃ¡vel nkill
dados_clean <- dados %>% drop_na(nkill)
View(dados_clean)

# Agrupa os dados por nÃºmero de ataques
?ddply
Groupcount <- ddply(dados_clean, .(gname, year), nrow)
colnames(Groupcount)[3]  <- "attack_count"

# Agrupa os dados por nÃºmero de mortes
?aggregate
GroupnkillSum <- aggregate(nkill ~ gname + year, data = dados_clean, sum)
colnames(GroupnkillSum)[3]  <- "kills_total"

# Agrupa os dados por mÃ©dia de mortes
GroupnkillMean <- aggregate(nkill ~ gname + year, data = dados_clean, mean)
colnames(GroupnkillMean)[3]  <- "kills_mean"

# Cria tabela com os dados anteriores
?left_join
GroupnkillTable <- left_join(GroupnkillSum, GroupnkillMean, by = c("gname" = "gname", "year" = "year"))
GroupTable <- left_join(GroupnkillTable, Groupcount, by = c("gname" = "gname", "year" = "year"))
View(GroupTable)

# Agrupamento 2 - VariÃ¡vel natlty (nacionalidade que sofreu o ataque)

?ddply
natltycount <- ddply(dados_clean, .(natlty1_txt, year), nrow)
colnames(natltycount)[3]  <- "attack_count"

natltynkillSum <- aggregate(nkill ~ natlty1_txt + year, data = dados_clean, sum)
colnames(natltynkillSum)[3]  <- "kills_total"

natltynkillMean <- aggregate(nkill ~ natlty1_txt + year, data = dados_clean, mean)
colnames(natltynkillMean)[3]  <- "kills_mean"

natltynkillTable <- left_join(natltynkillSum, natltynkillMean, by = c("natlty1_txt" = "natlty1_txt", "year" = "year"))
natltyTable <- left_join(natltynkillTable, natltycount, by = c("natlty1_txt" = "natlty1_txt", "year" = "year"))
View(natltyTable)

# Agrupamento 3 - VariÃ¡vel targtype (tipo de alvo do ataque)

targtypecount <- ddply(dados_clean, .(targtype1_txt, year), nrow)
colnames(targtypecount)[3]  <- "attack_count"

targtypenkillSum <- aggregate(nkill ~ targtype1_txt + year, data = dados_clean, sum)
colnames(targtypenkillSum)[3]  <- "kills_total"

targtypenkillMean <- aggregate(nkill ~ targtype1_txt + year, data = dados_clean, mean)
colnames(targtypenkillMean)[3]  <- "kills_mean"

targtypenkillTable <- left_join(targtypenkillSum, targtypenkillMean, by = c("targtype1_txt" = "targtype1_txt", "year" = "year"))
targtypeTable <- left_join(targtypenkillTable, targtypecount, by = c("targtype1_txt" = "targtype1_txt", "year" = "year"))
View(targtypeTable)

# Agrupamento 4 - VariÃ¡vel Region (regiÃ£o onde ocorreu o ataque)

Regioncount <- ddply(dados_clean, .(region_txt, year), nrow)
colnames(Regioncount)[3]  <- "attack_count"

RegionnkillSum <- aggregate(nkill ~ region_txt + year, data = dados_clean, sum)
colnames(RegionnkillSum)[3]  <- "kills_total"

RegionnkillMean <- aggregate(nkill ~ region_txt + year, data = dados_clean, mean)
colnames(RegionnkillMean)[3]  <- "kills_mean"

RegionnkillTable <- left_join(RegionnkillSum, RegionnkillMean, by = c("region_txt" = "region_txt", "year" = "year"))
RegionTable <- left_join(RegionnkillTable, Regioncount, by = c("region_txt" = "region_txt", "year" = "year"))
View(RegionTable)

# Agrupamento 5 - VariÃ¡vel Weapon (arma usada no ataque)

Weaponcount <- ddply(dados_clean, .(weaptype1_txt, year), nrow)
colnames(Weaponcount)[3]  <- "attack_count"

WeaponnkillSum <- aggregate(nkill ~ weaptype1_txt + year, data = dados_clean, sum)
colnames(WeaponnkillSum)[3]  <- "kills_total"

WeaponnkillMean <- aggregate(nkill ~ weaptype1_txt + year, data = dados_clean, mean)
colnames(WeaponnkillMean)[3]  <- "kills_mean"

WeaponnkillTable <- left_join(WeaponnkillSum, WeaponnkillMean, by = c("weaptype1_txt" = "weaptype1_txt", "year" = "year"))
WeaponTable <- left_join(WeaponnkillTable, Weaponcount, by = c("weaptype1_txt" = "weaptype1_txt", "year" = "year"))
View(WeaponTable)

# Agrupamento 6 - VariÃ¡vel attacktype (tipo de ataque)

attacktypecount <- ddply(dados_clean, .(attacktype1_txt, year), nrow)
colnames(attacktypecount)[3]  <- "attack_count"

attacktypenkillSum <- aggregate(nkill ~ attacktype1_txt + year, data = dados_clean, sum)
colnames(attacktypenkillSum)[3]  <- "kills_total"

attacktypenkillMean <- aggregate(nkill ~ attacktype1_txt + year, data = dados_clean, mean)
colnames(attacktypenkillMean)[3]  <- "kills_mean"

attacktypenkillTable <- left_join(attacktypenkillSum, attacktypenkillMean, by = c("attacktype1_txt" = "attacktype1_txt", "year" = "year"))
attacktypeTable <- left_join(attacktypenkillTable, attacktypecount, by = c("attacktype1_txt" = "attacktype1_txt", "year" = "year"))
View(attacktypeTable)

# Agrupamento 7 - VariÃ¡vel country (paÃ­s que sofreu o ataque)

countrycount <- ddply(dados_clean, .(country_txt, year), nrow)
colnames(countrycount)[3]  <- "attack_count"

countrynkillSum <- aggregate(nkill ~ country_txt + year, data = dados_clean, sum)
colnames(countrynkillSum)[3]  <- "kills_total"

countrynkillMean <- aggregate(nkill ~ country_txt + year, data = dados_clean, mean)
colnames(countrynkillMean)[3]  <- "kills_mean"

countrynkillTable <- left_join(countrynkillSum, countrynkillMean, by = c("country_txt" = "country_txt", "year" = "year"))
countryTable <- left_join(countrynkillTable, countrycount, by = c("country_txt" = "country_txt", "year" = "year"))
View(countryTable)

# TÃ­tulos das colunas
colnames(GroupTable)[1] <- "Title"
colnames(natltyTable)[1] <- "Title"
colnames(targtypeTable)[1] <- "Title"
colnames(RegionTable)[1] <- "Title"
colnames(WeaponTable)[1] <- "Title"
colnames(attacktypeTable)[1] <- "Title"
colnames(countryTable)[1] <- "Title"

# Se o tÃ­tulo estiver vazio colocamos "Unknown"
natltyTable$Title <- ifelse(natltyTable$Title == "", "Unknown", as.character(natltyTable$Title))

# Ajustamos esse tÃ­tulo pois Ã© muito longo
WeaponTable$Title <- ifelse(WeaponTable$Title == "Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)", 
                            "Vehicle (not to include vehicle-borne explosives)", 
                            as.character(WeaponTable$Title))

# TÃ­tulos das colunas
colnames(natltyTable)[1] <- "Title"
colnames(targtypeTable)[1] <- "Title"
colnames(RegionTable)[1] <- "Title"
colnames(WeaponTable)[1] <- "Title"
colnames(attacktypeTable)[1] <- "Title"
colnames(countryTable)[1] <- "Title"



#FIM












































