library(patchwork)

#STAR ---
dat_p1 %>% count(star_piece, sort = TRUE)

unique(dat_p1$star_color)
unique(dat_p1$star_piece)

piece_summary <- dat_p1 %>% 
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
        sort=TRUE)

#Rough POC radial chart ----
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

#Diamond chart  ---
#Manual coords for a diamond shape with 72deg angle at the origin...
# 5 of them make a star
tibble::tribble(~x, ~y,
                0, 0,
                0.585,0.809,
                0, 2,
                -0.585,0.809) -> diamond_shape

#Function to fill in star diamonds with generic value
star_plot_bg <- function(diamond_scale){
  unique(star_plot_dat$piece) %>% 
    purrr::map_dfr( function(pc){
      scale_adjust = 0
      star_plot_dat$color_orient %>% 
        unique() %>%
        purrr::map_dfr(
          ~as.matrix(diamond_shape) %>%
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
      `*`(.y) %>% 
      spdep::Rotation((-(as.numeric(.x)-1)/5)*(2*pi)) %>% 
      as.data.frame() %>% 
      rename(x=V1, y=V2) %>% 
      mutate(x = x-sin((-(as.numeric(.x)-1)/5)*(2*pi))*(1-.y),
             y = y+cos((-(as.numeric(.x)-1)/5)*(2*pi))*(1-.y))
  )) -> star_plot_dat


#Color name to color hex
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
  geom_polygon(data = star_plot_bg(1) %>% left_join(map_color),
               fill = "white",
               color = NA) +
  geom_polygon(aes(fill = color_hex, alpha = color_alpha)) +
  geom_polygon(data = star_plot_bg(1) %>% left_join(map_color),
               fill = NA, 
               color = "black", alpha = NA) +
  geom_polygon(data = star_plot_bg(1/3) %>% left_join(map_color),
               fill = NA,  linetype = "dotted",
               color = "#999999") +
  geom_polygon(data = star_plot_bg(2/3) %>% left_join(map_color),
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
    panel.background = element_rect(fill = "#fff8ea", color = NA),
    legend.position = "none"
  ) -> p_plot

#Legend --
leg_bounds <- c(-3.5,3.5)
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
  geom_polygon(data = star_plot_bg(1/3) %>% left_join(map_color),
               fill = NA,  linetype = "dotted",
               color = "#999999") +
  geom_polygon(data = star_plot_bg(2/3) %>% left_join(map_color),
               fill = NA,  linetype = "dotted",
               color = "#999999") +
  scale_fill_identity()+
  scale_alpha_identity() +
  geom_label(
    data = star_plot_bg(0.5) %>% left_join(map_color) %>% 
      filter(piece == "Other pieces") %>% 
      group_by(color, color_hex) %>% 
      summarize(across(c(x, y), ~mean(.)*8/3)) %>% 
      mutate(color = str_wrap(color, width = 16)),
    aes(label = color), alpha = 0.8, color = "black", fontface = "bold",
    hjust = 0.5, vjust = 0.5,
    size = 2.5
  )+
  coord_fixed(ylim = leg_bounds, xlim=leg_bounds) +
  theme_void() +
  theme(
    strip.background = element_rect(fill = "#00852B", color = NA),
    strip.text = element_text(color = "white", margin = margin(.2,0,.2,0, "cm"),
                              face = "bold"),
    panel.background = element_rect(fill = "#fff8ea", color = NA),
    legend.position = "none"
  ) -> p_legend_color

c(0, 1, 3, 6, 9) %>% 
  purrr::imap_dfr(
    ~star_plot_bg(.x/9) %>% 
      filter(color == levels(map_color$color)[.y])
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
      summarize(across(c(x, y), ~mean(.)*7/3), .groups="drop") %>% 
      mutate(label = ifelse((row_number()-1) == 0, paste0(c(0, 1, 3, 6, 9)[row_number()], "\nstars"),
                            c(0, 1, 3, 6, 9)[row_number()])),
    aes(label = label, color = label), alpha = 0.8, fontface = "bold",
    hjust = 0.5, vjust = 0.5,
    size = 3
  )+
  scale_color_manual(values = c("#00852B", rep("black", 4))) +
  geom_polygon(data = star_plot_bg(1/3) %>% left_join(map_color),
               fill = NA,  linetype = "dotted",
               color = "#999999") +
  geom_polygon(data = star_plot_bg(2/3) %>% left_join(map_color),
               fill = NA,  linetype = "dotted",
               color = "#999999") +
  coord_fixed(ylim = leg_bounds, xlim=leg_bounds) +
  theme_void() +
  theme(
    strip.background = element_rect(fill = "#00852B", color = NA),
    strip.text = element_text(color = "white", margin = margin(.2,0,.2,0, "cm"),
                              face = "bold"),
    panel.background = element_rect(fill = "#fff8ea", color = NA),
    legend.position = "none"
  ) -> p_legend_size


#Patchwork plotting

layout <- "
AAAAAA
##BC##
"

p_plot + p_legend_color + p_legend_size + 
  plot_layout(design = layout) & 
  theme(plot.background = element_rect(fill = "#fff8ea", color = NA))


ggsave("tree_stars.png", width = 10, height = 4.5)
