# Contact matrices from Mistry et al.
download.file(url = "https://raw.githubusercontent.com/mobs-lab/mixing-patterns/main/data/contact_matrices/Switzerland_country_level_F_community_setting_85.csv",
              destfile = "./data/Switzerland_country_level_F_community_setting_85.csv")
download.file(url = "https://raw.githubusercontent.com/mobs-lab/mixing-patterns/main/data/contact_matrices/Switzerland_country_level_F_household_setting_85.csv",
              destfile = "./data/Switzerland_country_level_F_household_setting_85.csv")
download.file(url = "https://raw.githubusercontent.com/mobs-lab/mixing-patterns/main/data/contact_matrices/Switzerland_country_level_F_school_setting_85.csv",
              destfile = "./data/Switzerland_country_level_F_school_setting_85.csv")
download.file(url = "https://raw.githubusercontent.com/mobs-lab/mixing-patterns/main/data/contact_matrices/Switzerland_country_level_F_work_setting_85.csv",
              destfile = "./data/Switzerland_country_level_F_work_setting_85.csv")

# Case data from Swiss authorities
library(httr)
tmp <- GET("https://www.covid19.admin.ch/api/data/context")
library(jsonlite)
tmp <- fromJSON(content(tmp, "text"))
download.file(url = paste0("https://www.covid19.admin.ch/api/data/",
                           tmp$dataVersion,
                           "/sources/COVID19Cases_geoRegion_AKL10_w.csv"),
              destfile = "./data/COVID19Cases_geoRegion_AKL10_w.csv")
download.file(url = paste0("https://www.covid19.admin.ch/api/data/",
                           tmp$dataVersion,
                           "/sources/COVID19VaccPersons_AKL10_w_v2.csv"),
              destfile = "./data/COVID19VaccPersons_AKL10_w_v2.csv")

# Mobility data
download.file(url = "https://data.humdata.org/dataset/c3429f0e-651b-4788-bb2f-4adbf222c90e/resource/55a51014-0d27-49ae-bf92-c82a570c2c6c/download/movement-range-data-2022-05-22.zip",
              destfile = "tmp.zip")
unzip(zipfile = "tmp.zip", "movement-range-2022-05-22.txt", exdir = "data")
file.remove("tmp.zip")
download.file(url = 
                "https://data.humdata.org/dataset/c3429f0e-651b-4788-bb2f-4adbf222c90e/resource/3d77ce5c-ab6d-4864-b8a2-c8bafffac4f3/download/movement-range-data-2020-03-01-2020-12-31.zip",
              destfile = "tmp.zip")
unzip(zipfile = "tmp.zip", "movement-range-data-2020-03-01--2020-12-31.txt", exdir = "data")
file.remove("tmp.zip")

tmp <- read.delim("./data/movement-range-2022-05-22.txt")
tmp <- tmp[tmp$country == "CHE", ]
write.csv(tmp, "./data/movement-range-2022-05-22.csv")
file.remove("./data/movement-range-2022-05-22.txt")
tmp <- read.delim("./data/movement-range-data-2020-03-01--2020-12-31.txt")
tmp <- tmp[tmp$country == "CHE", ]
write.csv(tmp, "./data/movement-range-data-2020-03-01--2020-12-31.csv")
file.remove("./data/movement-range-data-2020-03-01--2020-12-31.txt")

# Policy
download.file(url = "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker-legacy/main/legacy_data_202207/OxCGRT_latest.csv",
              destfile = "./data/OxCGRT_latest.csv")

# For mapping
download.file(url = "https://raw.githubusercontent.com/ebp-group/Switzerland_Tilemap/master/data/Switzerland_Tiles_CH1903LV03.CPG",
              destfile = "./data/Switzerland_Tiles_CH1903LV03.CPG")
download.file(url = "https://raw.githubusercontent.com/ebp-group/Switzerland_Tilemap/master/data/Switzerland_Tiles_CH1903LV03.dbf",
              destfile = "./data/Switzerland_Tiles_CH1903LV03.dbf")
download.file(url = "https://raw.githubusercontent.com/ebp-group/Switzerland_Tilemap/master/data/Switzerland_Tiles_CH1903LV03.prj",
              destfile = "./data/Switzerland_Tiles_CH1903LV03.prj")
download.file(url = "https://raw.githubusercontent.com/ebp-group/Switzerland_Tilemap/master/data/Switzerland_Tiles_CH1903LV03.sbn",
              destfile = "./data/Switzerland_Tiles_CH1903LV03.sbn")
download.file(url = "https://raw.githubusercontent.com/ebp-group/Switzerland_Tilemap/master/data/Switzerland_Tiles_CH1903LV03.sbx",
              destfile = "./data/Switzerland_Tiles_CH1903LV03.sbx")
download.file(url = "https://raw.githubusercontent.com/ebp-group/Switzerland_Tilemap/master/data/Switzerland_Tiles_CH1903LV03.shp",
              destfile = "./data/Switzerland_Tiles_CH1903LV03.shp")
download.file(url = "https://raw.githubusercontent.com/ebp-group/Switzerland_Tilemap/master/data/Switzerland_Tiles_CH1903LV03.shp.xml",
              destfile = "./data/Switzerland_Tiles_CH1903LV03.shp.xml")
download.file(url = "https://raw.githubusercontent.com/ebp-group/Switzerland_Tilemap/master/data/Switzerland_Tiles_CH1903LV03.shx",
              destfile = "./data/Switzerland_Tiles_CH1903LV03.shx")
download.file(url = "https://raw.githubusercontent.com/ebp-group/Switzerland_Tilemap/master/data/Switzerland_Tiles_CH1903LV03.svg",
              destfile = "./data/Switzerland_Tiles_CH1903LV03.svg")