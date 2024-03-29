# High level summarys

# N sets with prsents, M with snow, etc

#49 trees, so can make a stylized xmas tree chart
# bottom_branch = 7

#Square tree
# tibble(x = 1:bottom_branch %>%
#          purrr::map(function(nn){
#            1:(2*nn - 1) + (bottom_branch - nn)}) %>% unlist(),
#        y = (bottom_branch + 1) - rep(seq_along(1:bottom_branch), seq(1, bottom_branch*2, by = 2))
#        ) %>%
#   ggplot(aes(x, y)) +
#   geom_raster(fill = colorspace::darken("#00852B", 0)) +
#   coord_fixed()

#Custom tree
#tree_layout <- c(1, 1, 3, 3, 5, 5, 7, 7, 9, 9) #Old 49 version
tree_layout <- c(1, 1, 3, 3, 5, 5, 7, 7, 9, 9, 11) #61
sum(tree_layout)

tibble(x = tree_layout %>% 
         purrr::map(function(ii){
           seq(1, ii) + (length(tree_layout)-ii)/2}) %>% unlist(),
       y = (length(tree_layout)+1)-rep(seq_along(tree_layout), tree_layout)
) -> tree_shape 

custom_tree_plot <- function(bool_array = FALSE,
                             title_text = NULL,
                             tree_df = tree_shape,
                             color_base = "#00852B",
                             edge_base = "light",
                             color_detail = "#b40000",
                             edge_detail = "light",
                             x_first = FALSE){
  tree <- tree_df %>% 
    {if(x_first){
      arrange(., x, y)
    } else {
      .
    }} %>% 
    mutate(detail = bool_array %>% sort(TRUE)) %>% 
    #border color
    mutate(color_edge = case_when(
      !detail & edge_base == "light" ~ colorspace::lighten(color_base, 0.23),
      !detail & edge_base == "dark"  ~ colorspace::darken(color_base, 0.25),
      detail & edge_detail == "light" ~ colorspace::lighten(color_detail, 0.25),
      detail & edge_detail == "dark"  ~ colorspace::darken(color_detail, 0.25)
    ))
  
  tree  %>% 
    ggplot() +
    geom_tile(aes(x, y, fill = detail, color=color_edge), width=1, height=1) +
    #Studs
    ggforce::geom_circle(
      data = tree %>% rename(x0=x, y0=y),
      aes(x0=x0+0.075, y0=y0-0.075, r=(5/8)/2),
      color = NA, fill = "black", alpha = 0.2) +
    ggforce::geom_circle(
      data = tree %>% rename(x0=x, y0=y),
      aes(x0=x0, y0=y0, r=(5/8)/2*0.9, fill=detail, color = color_edge),
      size = .2) +
    #Brick color
    scale_fill_manual(values = c("TRUE" = color_detail, "FALSE" = color_base)) +
    scale_color_identity() +
    #Details
    labs(title = title_text) +
    coord_fixed() +
    theme_minimal()+
    theme(
      plot.background = element_rect(fill = "#fff8ea", color = NA),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.line.y = element_blank(),
      legend.position = "none"
    )
}

#Default tree
custom_tree_plot()
dat_p1 %>% count(str_detect(set_name, "Advent"), foliage_orientation, year) %>% 
  arrange(year) %>% 
  pivot_wider(names_from = year, values_from = n) %>% View()

#Single plots for blog ----

dat_p1 %>% 
  mutate(SNOT = foliage_orientation == "SNOT") %>%
  arrange(desc(SNOT)) %>% 
  pull() %>% sum()
  
dat_p1 %>% 
  mutate(SNOT = foliage_orientation == "SNOT") %>%
  arrange(desc(SNOT)) %>% 
  pull() %>% 
  custom_tree_plot(
    color_detail = "#FAC80A",
    x_first = TRUE,
    edge_detail = "dark"
  )
ggsave("tree_sum_snot.png", height = 2.8, width = 2.5)

#Snow trees
dat_p1 %>% 
  arrange(desc(snow)) %>% 
  pull(snow) %>% sum()
  
dat_p1 %>% 
  arrange(desc(snow)) %>% 
  pull(snow) %>% 
  custom_tree_plot(
    color_detail = "#F4F4F4",
    x_first = FALSE,
    edge_detail = "dark"
  )
ggsave("tree_sum_snow.png", height = 2.8, width = 2.5)

#Advent trees
dat_p1 %>% 
  mutate(advent = str_detect(set_name, "Advent")) %>%
  pull() %>% sum()

dat_p1 %>% 
  mutate(advent = str_detect(set_name, "Advent")) %>%
  arrange(desc(advent)) %>% 
  pull() %>% 
  custom_tree_plot(
    x_first = TRUE
  )
ggsave("tree_sum_advent.png", height = 2.8, width = 2.5)

#Colors
dat_p1 %>% 
  mutate(foliage_color = purrr::map(foliage_color, unlist)) %>% 
  unnest(foliage_color) %>% 
  group_by(set_num) %>% 
  filter(
    (n() > 1 & foliage_color != "Dark green") | n() == 1
  ) %>% 
  filter(row_number() == 1) %>% 
  mutate(foliage_color_dg = foliage_color != "Dark green") %>% 
  arrange(desc(foliage_color_dg))%>% 
  pull() %>% sum()
  
dat_p1 %>% 
  mutate(foliage_color = purrr::map(foliage_color, unlist)) %>% 
  unnest(foliage_color) %>% 
  group_by(set_num) %>% 
  filter(
    (n() > 1 & foliage_color != "Dark green") | n() == 1
  ) %>% 
  filter(row_number() == 1) %>% 
  mutate(foliage_color_dg = foliage_color != "Dark green") %>% 
  arrange(desc(foliage_color_dg))%>% 
  pull() %>% 
  custom_tree_plot(
    x_first = FALSE,
    color_detail = "#A5CA18",	# "#009894"
    edge_detail = "dark"
  ) 

ggsave("tree_sum_colors.png", height = 2.8, width = 2.5)

#Stylized version for the blog header ----
list(
  dat_p1 %>% 
    mutate(SNOT = foliage_orientation == "SNOT") %>%
    arrange(desc(SNOT)) %>% 
    pull() %>% 
    custom_tree_plot(
      color_detail = "#FAC80A",
      x_first = TRUE,
      edge_detail = "dark"
    ),
  
  #Snow trees
  dat_p1 %>% 
    arrange(desc(snow)) %>% 
    pull(snow) %>% 
    custom_tree_plot(
      color_detail = "#F4F4F4",
      x_first = FALSE,
      edge_detail = "dark"
    ),
  
  #Advent trees
  dat_p1 %>% 
    mutate(advent = str_detect(set_name, "Advent")) %>%
    arrange(desc(advent)) %>% 
    pull() %>% 
    custom_tree_plot(
      x_first = TRUE
    ),
  
  #Colors
  dat_p1 %>% 
    mutate(foliage_color = purrr::map(foliage_color, unlist)) %>% 
    unnest(foliage_color) %>% 
    group_by(set_num) %>% 
    filter(
      (n() > 1 & foliage_color != "Dark green") | n() == 1
    ) %>% 
    filter(row_number() == 1) %>% 
    mutate(foliage_color_dg = foliage_color != "Dark green") %>% 
    arrange(desc(foliage_color_dg))%>% 
    pull() %>% 
    custom_tree_plot(
      x_first = FALSE,
      color_detail = "#A5CA18",	# "#009894"
      edge_detail = "dark"
    ) 
  ) %>% 
  patchwork::wrap_plots(ncol = 4)

ggsave("legotree_header.png", width = 8, height=3)
