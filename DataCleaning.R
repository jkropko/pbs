library(tidyverse)
library(readxl)

#load the raw data
data <-read_excel("Copy of PBS+and+Trust+in+America_+National+PBS+Audience+Survey_January+25,+2021_08.08.xlsx")

varnames <- read_csv("PBSvariables - PBSvariables.csv")
clean <- read_csv("PBS categories - catdata.csv")
clean$oldcategory[is.na(clean$oldcategory)] <- "NA"
clean <- clean %>%
  filter(variable != "comparepbstrust_othertext" | oldcategory != "missing")
clean_open_whyyoutrust <- read_csv("PBS categories - open_whyyoutrust.csv")
clean_platformusage_news <- read_csv("PBS categories - platformusage_news.csv")
clean_subscription_other_text <- read_csv("PBS categories - subscription_other_text.csv")
clean_whymosttrusted_select <- read_csv("PBS categories - whymosttrusted_select.csv")

#Remove first row of labels
data <- data[-1,]

#Drop columns we don't need
todrop <- filter(varnames, is.na(`New Name`))$`Current Name`
data <- select(data, -all_of(todrop))

#Rename columns
varnames2 <- filter(varnames, !(`Current Name` %in% todrop))
data <- rename_with(data, ~varnames2$`New Name`, varnames2$`Current Name`)

#Recode gender (didn't make it into the Google sheet)
x <- "gender"
cats <- unique(data$gender)
newcats <- c("Female", "Male", rep("Other", 5))
names(cats) <- newcats
data[[x]] <- fct_recode(data[[x]], !!!cats)

#Recode categories
clean_recode <- filter(clean, !is.na(newcategory))
torecode <- unique(clean_recode$variable)
torecode <- torecode[torecode != "gender"]
for(x in torecode){
  cr <- filter(clean_recode, variable==x)
  cats <- cr$oldcategory
  names(cats) <- cr$newcategory
  data[[x]] <- fct_recode(data[[x]], !!!cats)
}

#Reorder categories
clean_recode <- filter(clean, !is.na(order))
clean_recode <- clean_recode %>%
  mutate(newcategory = ifelse(is.na(newcategory), oldcategory, newcategory))
toreorder <- unique(clean_recode$variable)
for(x in toreorder){
  cr <- filter(clean_recode, variable==x) %>%
    select(newcategory, order) %>%
    filter(!duplicated(.)) %>%
    arrange(order)
  catorder <- cr$newcategory
  data[[x]] <- fct_relevel(data[[x]], catorder)
}


#clean_open_whyyoutrust
clean_open_whyyoutrust <- clean_open_whyyoutrust %>%
  filter(`Old category` != "missing")
cats <- clean_open_whyyoutrust$`Old category`
names(cats) <- clean_open_whyyoutrust$`New category`
data[["open_whyyoutrust"]] <- fct_recode(data[["open_whyyoutrust"]], !!!cats)

#clean_subscription_other_text
clean_subscription_other_text <- clean_subscription_other_text %>%
  filter(`names.mytab.` != "missing, please fix this study.")
cats <- clean_subscription_other_text$`names.mytab.`
names(cats) <- clean_subscription_other_text$`...4`
data[["subscription_other_text"]] <- fct_recode(data[["subscription_other_text"]], !!!cats)

#clean_whymosttrusted_select
cats <- clean_whymosttrusted_select$`names.mytab.`
cats <- paste(cats, collapse=',')
cats <- unique(unlist(strsplit(cats,",")))
cats <- gsub(":", "", cats)
cats <- str_trim(cats)
catnames <- tolower(cats)
catnames <- gsub(" ", "_", catnames)
catnames <- gsub("’", "", catnames)
catnames <- gsub("-", "", catnames)
catnames <- paste0("whymosttrusted_", catnames)
for(i in 1:length(cats)){
  data[[catnames[i]]] <- grepl(cats[i], data$whymosttrusted_select)
}

#clean_platformusage_news
cats <- clean_platformusage_news$`names.mytab.`
cats <- gsub("\\(e.g. Facebook, Instagram, TikTok\\)", "", cats)
cats <- paste(cats, collapse=',')
cats <- unique(unlist(strsplit(cats,",")))
cats <- cats[cats!="12"]
cats <- cats[cats!="Not applicable"]
cats <- str_trim(cats)
catnames <- tolower(cats)
catnames <- gsub(" ", "_", catnames)
catnames <- paste0("platformusage_news_", catnames)
for(i in 1:length(cats)){
  data[[catnames[i]]] <- grepl(cats[i], data$platformusage_news)
}
