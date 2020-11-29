library(tidyverse)
library(googlesheets4)
library(ggbrickr)

#DATA PROCESSING ----
gs4_deauth()

gg_trees <- "https://docs.google.com/spreadsheets/d/1FSh3w8yCoFtaPLJnSaHn3_Fobg7kykivn6RA09Icj24/edit?usp=sharing"

dat_raw <- read_sheet(gg_trees)
names(dat_raw)

dat_p1 <- dat_raw %>% 
  mutate(across(c(foliage_color, light_color, light_piece),
                ~purrr::map(., str_split, pattern = ", ")
  )) %>% 
  # Full height
  # 8 SOT units = 3 SNOT units
  # https://www.holgermatthes.de/bricks/en/snot.php
  replace_na(list(height_sot = 0, height_snot = 0)) %>% 
  mutate(height_full = height_sot + 8/3 * height_snot)

# TREE HEIGHT ----

dat_p1 %>% 
  count(foliage_orientation, height_full)

dat_p1 %>% 
  ggplot(aes(x=height_full, group = foliage_orientation)) +
  geom_density(aes(fill = foliage_orientation), alpha = 0.8)

hgt_grp = 3

dat_p1 %>% 
  mutate(height_group = height_full %/% hgt_grp) %>% 
  count(foliage_orientation, height_group) %>% 
  complete(height_group = 1:15, foliage_orientation, fill = list()) %>% 
  mutate(n = ifelse(foliage_orientation=="SOT", -1, 1)*n) %>% 
  ggplot(aes(y = factor(height_group), x = n)) +
  geom_vline(xintercept = 0, color = "#5F3109", size=2) +
  geom_col(fill = "#00852B") +
  coord_cartesian(xlim = c(-10, 10)) +
  labs(title = "Height distribution, by studs on top & studs on side",
       y = "Height (bricks)")+
  theme()


#STAR ---
unique(dat_p1$star_color)
unique(dat_p1$star_piece)

dat_p1 %>% 
  mutate(star_color2 = case_when(
    star_color %in% c("Bright yellow", "Transparent yellow",
                      "Warm gold") ~ star_color,
    star_color == "Transparent Fluorescent Green" ~ "Transparent fluorescent green", 
    TRUE ~ "Other colors"
  ),
  star_piece2 = case_when(
    star_piece %in% c("Magic Wand", "Round plate 1x1") ~ star_piece,
    star_piece == "Diamond With Stick" ~ "Diamond w stick",
    star_piece == "Star Symbol W/ Tube And Hole" ~ "Star symbol 1x1",
    TRUE ~ "Other pieces"
  )) %>% 
  group_by(star_color2) %>% 
  mutate(n_color = n()) %>% 
  ungroup() %>% 
  group_by(star_piece2) %>% 
  mutate(n_piece = n()) %>% 
  ungroup() %>%
  count(star_color2, n_color,
        star_piece2, n_piece, 
        sort=TRUE)  -> piece_summary

piece_summary %>% 
  filter(n_piece >= 3) %>% 
  ggplot(aes(x = star_color2, y = n)) +
  geom_col(aes(fill = star_color2)) +
  coord_polar() +
  facet_wrap(~star_piece2) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    legend.position = "none"
  )

#Diamond chart 
#Manual coords for a diamond shape with 72deg angle at the origin...
# 5 of them make a star
tibble::tribble(~x, ~y,
                0, 0,
                0.585,0.809,
                0, 2,
                -0.585,0.809) -> diamond_shape

scale_adjust = 0.0 # To prvent border from being covered
piece_summary %>% 
  select(color = star_color2, piece = star_piece2,
         n) %>% 
  complete(color, piece, fill = list(n=0)) %>% 
  #Color orientation
  #Order of diamond colors, starting at top and going clockwise
  mutate(color_orient = factor(color, 
                               levels = c("Bright yellow", "Warm gold", "Other colors",
                                          "Transparent fluorescent green", "Transparent yellow")),
         color_scale = n / max(n)) %>% 
  #Get diamond/polygon coords for each row.
  # Each row is a diamond, each diamond has 5 pairs of coords
  mutate(diamond = purrr::map2(
    color_orient, #.x
    color_scale,  #.y
    ~as.matrix(diamond_shape) %>% 
      `*`(.y- scale_adjust) %>% 
      spdep::Rotation((-(as.numeric(.x)-1)/5)*(2*pi)) %>% 
      as.data.frame() %>% 
      rename(x=V1, y=V2) %>% 
      mutate(x = x-sin((-(as.numeric(.x)-1)/5)*(2*pi))*(1-(.y - scale_adjust)),
             y = y+cos((-(as.numeric(.x)-1)/5)*(2*pi))*(1-(.y - scale_adjust)))
  )) -> star_plot_dat

# View(star_plot_dat)

star_plot_bg <- function(diamond_scale){
  unique(star_plot_dat$piece) %>% 
    purrr::map_dfr( function(pc){
      scale_adjust = 0
      star_plot_dat$color_orient %>% 
        unique() %>%
        purrr::map_dfr(
          ~as.matrix(diamond1) %>%
            `*`(diamond_scale - scale_adjust) %>% 
            spdep::Rotation((-(as.numeric(.x)-1)/5)*(2*pi)) %>%
            as.data.frame() %>%
            rename(x=V1, y=V2)%>% 
            mutate(x = x-sin((-(as.numeric(.x)-1)/5)*(2*pi))*(1-(diamond_scale - scale_adjust)),
                   y = y+cos((-(as.numeric(.x)-1)/5)*(2*pi))*(1-(diamond_scale - scale_adjust))) %>% 
            mutate(color = .x,
                   piece = pc)
        )
    })
}



map_color <- tibble(color = unique(star_plot_dat$color)) %>% 
  mutate(color2 = factor(color,
                        levels = c("Bright yellow", "Warm gold", "Other colors",
                        "Transparent fluorescent green", "Transparent yellow"))) %>% 
  mutate(color_hex = case_when(
    color == "Bright yellow" ~ "#F2CD37",
    color == "Transparent yellow" ~ "#FFE622",
    color == "Transparent fluorescent green" ~ "#C0F500",
    color == "Warm gold" ~ "#DAB000",
    TRUE ~ "#F06D78"
  )) %>% 
  select(-color) %>% rename(color = color2)

star_plot_dat %>% 
  left_join(map_color) %>% 
  mutate(
    color_alpha = ifelse(str_detect(color, "Transparent"), 0.9, 1),
    piece_f = factor(piece, 
                     levels = c("Diamond w stick", "Magic Wand", 
                                "Round plate 1x1", "Star symbol 1x1",
                                "Other pieces"))
  ) %>% 
  unnest(diamond) %>% 
  ggplot(aes(x, y, group = color)) +
  # geom_polygon(data = star_plot_bg(1) %>% left_join(map_color),
  #              aes(fill = color_hex), 
  #              color = NA, alpha = 0.1) +
  geom_polygon(data = star_plot_bg(1) %>% left_join(map_color),
               fill = "white",
               color = NA) +
  geom_polygon(aes(fill = color_hex, alpha = color_alpha)) +
  geom_polygon(data = star_plot_bg(1) %>% left_join(map_color),
               fill = NA, 
               color = "black", alpha = NA) +
  geom_polygon(data = star_plot_bg(0.5) %>% left_join(map_color),
               fill = NA,  linetype = "dotted",
               color = "#999999") +
  scale_fill_identity()+
  scale_alpha_identity() +
  facet_grid(cols = vars(piece_f))+
  coord_fixed() +
  theme_void() +
  theme(
    strip.background = element_rect(fill = "#00852B", color = NA),
    strip.text = element_text(color = "white", margin = margin(.2,0,.2,0, "cm"),
                              face = "bold"),
    panel.background = element_rect(fill = "#efefef", color = NA),
    legend.position = "none"
  )

#Legend --
star_plot_bg(1) %>% 
  filter(piece == "Other pieces") %>% 
  mutate(
    color_alpha = ifelse(str_detect(color, "Transparent"), 0.9, 1),
    piece_f = factor(piece, 
                     levels = c("Diamond w stick", "Magic Wand", 
                                "Round plate 1x1", "Star symbol 1x1",
                                "Other pieces"))
  ) %>% 
  ggplot(aes(x, y, group = color)) +
  geom_polygon(fill = "white",
               color = NA) +
  geom_polygon(data = star_plot_bg(1) %>% left_join(map_color) %>% 
                 # filter(color != "Bright yellow") %>% 
                 filter(piece == "Other pieces")%>% 
                 mutate(color_alpha = ifelse(str_detect(color, "Transparent"), 0.9, 1)),
               aes(fill = color_hex, alpha = color_alpha),
               color = NA) +
  geom_polygon(fill = NA,
               color = "black", alpha = NA) +
  geom_polygon(data = star_plot_bg(0.5) %>% left_join(map_color),
               fill = NA,  linetype = "dotted",
               color = "#999999") +
  scale_fill_identity()+
  scale_alpha_identity() +
  geom_label(
    data = star_plot_bg(0.5) %>% left_join(map_color) %>% 
      filter(piece == "Other pieces") %>% 
      group_by(color, color_hex) %>% 
      summarize(across(c(x, y), ~mean(.)*4/3)) %>% 
      mutate(color = str_wrap(color, width = 16)),
    aes(label = color), alpha = 0.8, color = "black", fontface = "bold",
    hjust = 0.5, vjust = 0
  )+
  coord_fixed(ylim = c(-2,2), xlim=c(-2, 2)) +
  theme_void() +
  theme(
    strip.background = element_rect(fill = "#00852B", color = NA),
    strip.text = element_text(color = "white", margin = margin(.2,0,.2,0, "cm"),
                              face = "bold"),
    panel.background = element_rect(color = NA),
    legend.position = "none"
  )

0:4 %>% 
  purrr::map_dfr(
    ~star_plot_bg(.x/4) %>% 
      filter(color == levels(map_color$color)[.x+1])
  ) %>% 
  filter(piece == "Other pieces") %>% 
  ggplot(aes(x,y, group = color)) +
  geom_polygon(data = star_plot_bg(1) %>% left_join(map_color) %>% 
                 filter(piece == "Other pieces"),
               fill = NA, color = "black") +
  geom_polygon(fill = "#00852B") +
  geom_label(
    data = star_plot_bg(0.5) %>% left_join(map_color) %>% 
      filter(piece == "Other pieces") %>% 
      group_by(color, color_hex) %>% 
      summarize(across(c(x, y), ~mean(.)*4/3), .groups="drop") %>% 
      mutate(label = paste0((row_number()-1)*2, "\nstars")),
    aes(label = label, color = label), alpha = 0.8, fontface = "bold",
    hjust = 0.5, vjust = 0.5
  )+
  scale_color_manual(values = c("#00852B", rep("black", 4))) +
  geom_polygon(data = star_plot_bg(0.5) %>% left_join(map_color),
               fill = NA,  linetype = "dotted",
               color = "#999999") +
  coord_fixed(ylim = c(-2,2), xlim=c(-2, 2)) +
  theme_void() +
  theme(
    strip.background = element_rect(fill = "#00852B", color = NA),
    strip.text = element_text(color = "white", margin = margin(.2,0,.2,0, "cm"),
                              face = "bold"),
    panel.background = element_rect(color = NA),
    legend.position = "none"
  )


#LIGHTS ----

dat_p1 %>% 
  mutate(light_color = purrr::map(light_color, unlist)) %>% 
  unnest(light_color) %>% 
  count(light_color, sort= TRUE) %>% View()


#SNOW ----
dat_p1 %>% 
  count(snow)

snow_layout <- c(1, 1, 3, 3, 5, 7, 7, 9, 9)

y = (9+1)-rep(seq_along(snow_layout), snow_layout)

x = snow_layout %>% 
  purrr::map(function(ii){
    seq(1, ii) + (9-ii)/2}
  ) %>% 
  unlist()

tibble(x=x, y=y, 
       snow = dat_p1 %>% arrange(desc(snow)) %>% pull(snow)) %>% 
  ggplot(aes(x, y)) +
  geom_tile(aes(fill = snow), color = "black") +
  scale_fill_manual(values = c("TRUE" = "#F4F4F4", "FALSE" = "#00852B")) +
  coord_fixed() +
  theme_void()

# PRESENTS ----
dat_p1 %>% 
  mutate(theme_group = case_when(
    str_detect(set_name, "Advent") ~ "Advent",
    theme %in% c("Miscellaneous", "Creator", "Friends") ~ "Other",
    TRUE ~ theme
  )) %>% 
  count(present, theme_group != "Advent")

#18 out of 25 non-advent calendar trees have presents



# TIME ----
dat_p1 %>% 
  count(year, foliage_orientation) %>% 
  pivot_wider(names_from = foliage_orientation, values_from = n)
