# Dawum API für Sonntagsfragen
library(httr)
library(xml2)
library(tidyverse)
library(directlabels)


current <- readr::read_file("https://api.dawum.de/last_update.txt")
if (file.exists("cache.txt")){
  cache <- read_file("cache.txt")
} else {
  cache <- "Cache Datei fehlt"
}

if (cache != current) {
  dawum_api <- GET("https://api.dawum.de")
  dawum_json <- content(dawum_api)
  jsonlite::write_json(dawum_json, "dawum_json.json")
  write_file(current, "cache.txt")
  warning(paste0("Datei im Cache ist veraltet, neue Daten werden geldaden.\n Datenstand Cache: \t", cache, "\n Datenstand Website: \t", current))
} else if (file.exists("dawum_json.json")){
  dawum_json <- jsonlite::read_json("dawum_json.json")
  warning(paste0("Datei im Cache ist noch aktuell und wird genutzt.\n Datenstand Cache: \t", cache, "\n Datenstand Website: \t", current)) 
} else {
  warning("Etwas stimmt nicht beim Datenimport und cacheing")
  break
}

as_df_tibble <- function(x) tibble(as.data.frame(x))

df <- tibble(map_dfr(dawum_json$Surveys, as_df_tibble))

# Namen der Parteien inkludieren
df_parties <- map_dfr(dawum_json$Parties, as_df_tibble)
df_parties$id <- names(dawum_json$Parties)

# Namen der Parlamente inkludieren
df_parliaments <- map_dfr(dawum_json$Parliaments, as_df_tibble)
df_parliaments$id <- names(dawum_json$Parliaments)

# Informationen zu Auftraggeber hinzufügen
df_taskers <- map_dfr(dawum_json$Taskers, as_df_tibble) %>%
  rename("tasker" = "Name")
df_taskers$id <- names(dawum_json$Taskers)

# Infos zum Institut
df_institutes <- map_dfr(dawum_json$Institutes, as_df_tibble) %>%
  rename("institute" = "Name")
df_institutes$id <- names(dawum_json$Institutes)


df_long <- df %>%
  pivot_longer(cols = starts_with("Results."), names_to = "partycode", names_prefix = "Results\\.", values_to = "wert") %>%
  left_join(df_parties, by = c("partycode" = "id")) %>%
  left_join(df_parliaments, by = c("Parliament_ID" = "id"), suffix = c("_party", "_parliament")) %>%
  left_join(df_taskers, by = c("Tasker_ID" = "id")) %>%
  left_join(df_institutes, by = c("Institute_ID" = "id")) %>%
  mutate(Feldende = as.Date(Survey_Period.Date_End),
         Feldbeginn = as.Date(Survey_Period.Date_Start),
         Survey_Period.Date_Start = NULL,
         Survey_Period.Date_End = NULL,
         Date = as.Date(Date)) %>%
  relocate(starts_with("Feld"), .after = "Date") %>%
  relocate("Shortcut_parliament":"Election", .after = "Feldbeginn") %>%
  relocate("institut", .after = "Shortcut_parliament") %>%
  select(!all_of(c("Parliament_ID", "Institute_ID", "Name_parliament", "Name_party", "partycode", "Tasker_ID", "Election"))) %>%
  rename("Parlament" = "Shortcut_parliament") %>%
  rename("Befragte" = "Surveyed_Persons") %>%
  rename("Partei" = "Shortcut_party") 

colnames(df_long) <- tolower(colnames(df_long))


df_wide <- df_long %>%
  pivot_wider(names_from = "partei", values_from = "wert")



k = 7
df_long_test <- df_long %>%
  filter((partei == "SPD" | partei == "CDU/CSU" | partei == "Grüne" | partei == "Linke" |
            partei == "AfD" | partei == "FDP" | partei == "Sonstige") & parlament == "Bundestag")
df_long_test <- df_long_test %>%       
group_by(partei) %>%
  mutate(wert_rollmean = zoo::rollmean(wert, k, fill = "extend"))



ggplot(df_long_test) +
  geom_point(aes(x = date, y = wert , colour = partei), alpha = .1) +
  geom_line(aes(x = date, y =wert_rollmean , colour = partei), na.rm = T, size = 1) +
  geom_hline(aes(yintercept = min(df_long_test$wert[df_long_test$partei == "CDU/CSU"])), linetype = 1) +
  scale_color_manual(values = c("blue", "black", "gold", "seagreen", "magenta", "grey", "red"))+
  ggtitle(paste0("Umfragen für Wahl in/zum ", first(df_long_test$parlament)))+
  labs(caption = paste0("Rollt über ", k, " Beobachtungen"))+
  # coord_cartesian(xlim = c(as.Date("2020-02-15"), as.Date("2022-11-01")))+
  theme_minimal()

glimpse(df_wide)
save(df_wide, file = "svy_wide.RDATA")
