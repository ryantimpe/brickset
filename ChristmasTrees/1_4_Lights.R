#Lights ----

dat_lights <- dat_p1 %>% 
  mutate(light_color = purrr::map(light_color, unlist)) %>% 
  unnest(light_color)

# dat_lights %>% count(light_color, sort = TRUE) %>% View()

#For each light, what color is it usually paired with

dat_lights %>% 
  count(light_color, sort = TRUE) %>% 
  pull(light_color) %>% 
  purrr::map_dfr(
    function(xx){
      dat_lights %>% 
        group_by(set_num) %>% 
        filter(xx %in% light_color) %>% 
        ungroup() %>% 
        filter(light_color != xx) %>% 
        count(light_color, sort = TRUE) %>% 
        mutate(light_color_base = xx)
    }
  )-> dat_light_pairings


#Place all points around a circle
drawPoint <- function(currentPoint, totalPoints, r=1) {  
  
  theta = ((pi*2) / totalPoints)
  angle = (theta * (currentPoint-1))
  
  x = (r * cos(angle));
  y = (r * sin(angle));
  
  return(list(x, y));
}


dat_lights %>% 
  count(light_color, sort = TRUE) %>% 
  pull(light_color) -> light_list

plot_lights <- seq_along(light_list) %>% drawPoint(length(light_list)) %>% 
  as.data.frame() %>% 
  purrr::set_names(c("x", "y")) %>% 
  bind_cols(
    seq_along(light_list) %>% drawPoint(length(light_list), r = 1.2) %>% 
      as.data.frame() %>% 
      purrr::set_names(c("xlabel", "ylabel"))
  ) %>% 
  mutate(light_color = light_list) %>% 
  left_join(dat_lights %>% 
              count(light_color, sort = TRUE)) %>%
  left_join(brickr::lego_colors %>% 
              select(light_color = Color, hex) %>% 
              mutate(
                light_color = str_replace(light_color, "Tr. ", "Trans "),
                light_color = str_replace(light_color, "Br. ", "Bright "),
                light_color = str_replace(light_color, "Trans fl red orange", "Trans fluorescent reddish orange"),
                light_color = str_replace(light_color, "Trans medium violet", "Trans medium reddish violet")
              ) #%>% View()
  ) %>% 
  mutate(hex = case_when(
    light_color == "Warm gold" ~ "#AA7F2E",
    light_color == "Metallic silver" ~ "#898788",
    is.na(light_color) ~ "#e9e9e9",
    TRUE ~ hex
  )) %>% 
  mutate(trans = str_detect(light_color, "^Trans"),
         trans_hex = ifelse(trans, "#00852B", "#191919"),
         trans_hex_bg = ifelse(trans, "#ffffff", "#191919")) %>% 
  #labels
  replace_na(list(light_color = "{none}")) %>% 
  mutate(
    label_q1 = ifelse(x>=0 & y>=0, light_color, ""),
    label_q2 = ifelse(x< 0 & y>=0, light_color, ""),
    label_q3 = ifelse(x< 0 & y< 0, light_color, ""),
    label_q4 = ifelse(x>=0 & y< 0, light_color, "")
  )

brickr::lego_colors %>% pull(Color)

#Pairings
dat_light_pairings %>% 
  group_by(light_color) %>% 
  slice_max(n) %>% 
  ungroup() %>% 
  filter(n > 1) %>% 
  #COnvert to coords
  left_join(
    plot_lights %>% select(light_color, x, y, trans_hex)
  ) %>% 
  left_join(
    plot_lights %>% select(light_color_base = light_color,
                          xend = x, yend = y)
  ) %>% 
  #lazy way to drop duplicates
  mutate(
    light_color = factor(light_color, levels = unique(dat_lights$light_color)),
    light_color_base = factor(light_color_base, levels = unique(dat_lights$light_color)),
    index = as.numeric(light_color) + as.numeric(light_color_base) + x + y + xend + yend
  ) %>% 
  arrange(x) %>% 
  group_by(round(index, 3)) %>% 
  filter(row_number() ==1 ) %>% 
  ungroup()-> plot_lights_pairings

plot_lights %>% 
  ggplot(aes(x, y)) +
  geom_curve(data = plot_lights_pairings, 
             aes(x, y, xend = xend, yend = yend, color = trans_hex),
             curvature = 0.35) +
  geom_point(aes(size = n*2, color = trans_hex_bg, shape = trans)) +
  geom_point(aes(size = n, color = hex, shape = trans, alpha = trans)) +
  geom_text(aes(x=xlabel, y=ylabel, label = label_q1), hjust = 0, vjust = 0) +
  geom_text(aes(x=xlabel, y=ylabel, label = label_q2), hjust = 1, vjust = 0) +
  geom_text(aes(x=xlabel, y=ylabel, label = label_q3), hjust = 1, vjust = 1) +
  geom_text(aes(x=xlabel, y=ylabel, label = label_q4), hjust = 0, vjust = 1) +
  scale_color_identity() +
  scale_size_continuous(range = c(2, 15)) +
  scale_shape_manual(values = c("TRUE" = 17,  "FALSE" = 16), na.value = 15) +
  scale_alpha_manual(values = c("TRUE" = 0.5,  "FALSE" = 1), na.value = 1) +
  coord_fixed(xlim = c(-1.8, 1.8), ylim = c(-1.3, 1.3))  +
  labs(title = "Colors used in lights, by frequency and common pairings") +
  theme_void() +
  theme(
    strip.background = element_rect(fill = "#00852B", color = NA),
    strip.text = element_text(color = "white", margin = margin(.2,0,.2,0, "cm"),
                              face = "bold"),
    plot.background = element_rect(fill = "#fff8ea", color = NA),
    legend.position = "none"
  )

ggsave("tree_lights.png", width = 8, height = 6)



