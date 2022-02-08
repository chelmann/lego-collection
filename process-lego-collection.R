library(tidyverse)
library(writexl)

# Inputs ------------------------------------------------------------------
family.sets <- readr::read_csv("data//helmann-family-set-ids.csv")

# Master List of Inventory ------------------------------------------------
inventory <- readr::read_csv("data//inventories.csv.gz")

# Mini-Figures ------------------------------------------------------------
figures.inventory <- readr::read_csv("data//inventory_minifigs.csv.gz")
mini.figs <- readr::read_csv("data//minifigs.csv.gz") %>% rename(minifig_name=name, minifig_parts=num_parts)

figures.inventory <- left_join(figures.inventory, mini.figs, by=c("fig_num"))
rm(mini.figs)

# Parts -------------------------------------------------------------------
parts.inventory <- readr::read_csv("data//inventory_parts.csv.gz")
parts <- readr::read_csv("data//parts.csv.gz") %>% rename(part_name=name)
part.categories <- readr::read_csv("data//part_categories.csv.gz") %>% rename(part_cat_name=name)
parts <- left_join(parts, part.categories, by=c("part_cat_id"="id"))
colors <- readr::read_csv("data//colors.csv.gz") %>% rename(color_name=name, color_id=id) %>% mutate(hex=paste0("#",rgb)) %>% select(-rgb)

parts.inventory <- left_join(parts.inventory, parts, by=c("part_num"))
parts.inventory <- left_join(parts.inventory, colors, by=c("color_id"))
rm(parts, part.categories, colors)

# Sets --------------------------------------------------------------------
sets <- readr::read_csv("data//sets.csv.gz") %>% rename(set_name=name, release_year=year)

# Clean up the Themes to include the name of the parent Themes (for instance Parent Theme is City for Lego City Airport where Name is Airport)
themes <- readr::read_csv("data//themes.csv.gz")
temp <- themes %>% filter(is.na(parent_id)) %>% rename(main_theme=name) %>% mutate(parent_id=id) %>% select(-id)
themes <- left_join(themes, temp, by=("parent_id")) %>%
  mutate(main_theme =case_when(
    is.na(main_theme) ~ name,
    !(is.na(main_theme)) ~ main_theme)) %>%
  mutate(parent_id =case_when(
    is.na(parent_id) ~ id,
    !(is.na(parent_id)) ~ parent_id)) %>%
  rename(theme_name=name) %>%
  select(-parent_id)

sets <- left_join(sets, themes, by=c("theme_id"="id"))
rm(temp, themes)

# Collection Tables --------------------------------------------------------
# First determine from the list of sets owned what the inventory ID's are and create a unique tibble
collection.sets <- left_join(family.sets, inventory, by=c("set_num","version"))
collection.sets <- left_join(collection.sets, sets, by=c("set_num"))
collection.ids <- collection.sets %>% select(id, set_num)
collection.set.details <- collection.sets %>% select(set_num, owner, set_name, release_year)

# List of mini-figures owned
collection.figures <- left_join(collection.ids, figures.inventory, by=c("id"="inventory_id")) %>% drop_na()
f <- collection.figures %>%
  group_by(set_num) %>%
  summarise(Figures=sum(quantity), Figures_Parts=sum(minifig_parts)) %>%
  mutate(Figures_Parts = replace_na(Figures_Parts, 0))
collection.figures <- left_join(collection.figures, collection.set.details, by=c("set_num"))

# List of parts owned
collection.parts <- left_join(collection.ids, parts.inventory, by=c("id"="inventory_id")) %>% drop_na()
p <- collection.parts %>%
  group_by(set_num) %>%
  summarise(Pieces=sum(quantity))
collection.parts <- left_join(collection.parts, collection.set.details, by=c("set_num"))

# Add in Detailed Parts Summary to Sets Collection (so it includes duplicates)
collection.sets <- left_join(collection.sets, f, by=c("set_num")) %>% mutate(Figures_Parts = replace_na(Figures_Parts, 0), Figures = replace_na(Figures, 0))
collection.sets <- left_join(collection.sets, p, by=c("set_num"))
collection.sets <- collection.sets %>% mutate(Pieces = Pieces + Figures_Parts) %>% select(-Figures_Parts)

rm(f,p, collection.ids, collection.set.details, family.sets, figures.inventory, inventory, parts.inventory, sets)

# Write data out to excel file --------------------------------------------
excel.tabs <- list("sets"=collection.sets,
                   "figures"=collection.figures,
                   "parts"=collection.parts)

write_xlsx(excel.tabs, "data//helmann-family-lego-collection.xlsx")



