# TREE HEIGHT ----
library(ggforce)

#Data ----
dat_hgt <- dat_p1 %>% 
  mutate(height_full_bricks = round(height_full / 3)) #Height in bricks, to nearest brick

#EDA ----
dat_hgt %>% 
  count(foliage_orientation, height_full)
dat_hgt %>% 
  count(foliage_orientation, height_full_bricks)

dat_hgt %>% 
  ggplot(aes(x=height_full_bricks, group = foliage_orientation)) +
  geom_density(aes(fill = foliage_orientation), alpha = 0.8)


#POC chart --- not LEGO style
dat_hgt %>% 
  count(foliage_orientation, height_full_bricks) %>% 
  complete(height_full_bricks = 1:15, foliage_orientation, fill = list()) %>% 
  mutate(n = ifelse(foliage_orientation=="SOT", -1, 1)*n) %>%
  ggplot(aes(y = factor(height_full_bricks), x = n)) +
  geom_vline(xintercept = 0, color = "#5F3109", size=2) +
  geom_col(fill = "#00852B") +
  coord_cartesian(xlim = c(-10, 10)) +
  labs(title = "Height distribution, by studs on top & studs on side",
       y = "Height (bricks)")+
  theme()

# Full chart ----
# Left side looks like bricks, SOT style
# Right side looks like bricks, SNOT style (knobs facing viewer)
# Y axis in middle as trunk

#Base bricks... 
# different heights for SOT and SNOT
tree_plot_base <- dat_hgt %>% 
  count(foliage_orientation, height_full_bricks) %>% 
  complete(height_full_bricks = 1:15, foliage_orientation, fill = list()) %>% 
  mutate(n = ifelse(foliage_orientation=="SOT", -1, 1)*(n+1/2)) %>% #offset away from axis 1/2
  mutate(
    xmin = n, xmax = sign(n)*1/2,
    ymin = case_when(
      n > 0 ~ height_full_bricks - 1/2 + 1/32, 
      n < 0 ~ height_full_bricks - 1/2 + 1/3,#Bump up location of SOT
      TRUE ~ 0),
    ymax = case_when(
      n > 0 ~ height_full_bricks + 1/2 - 1/32, 
      n < 0 ~ height_full_bricks - 1/2 + (3/8) + 1/3,
      TRUE ~ 0)
  ) 

# Split bricks in length 4 with segments
1:(max(abs(tree_plot_base$n %/% 4), na.rm=TRUE) - 1) %>% 
  purrr::map_dfr(
    function(nn){
      dat_hgt %>% 
        count(foliage_orientation, height_full_bricks) %>% 
        complete(height_full_bricks = 1:15, foliage_orientation, fill = list()) %>% 
        mutate(b = n %/% 4) %>% 
        filter(b >= nn) %>% 
        mutate(x = nn * 4)
    }
  ) %>% 
  filter(n != x) %>% #Don't need the end points
  mutate(
    x =  x + 1/2, 
    xend = x,
    y = case_when(
      foliage_orientation=="SNOT" ~ height_full_bricks - 1/2,
      foliage_orientation=="SOT" ~ height_full_bricks - 1/2 + 1/3),
    yend = case_when(
      foliage_orientation=="SNOT" ~ height_full_bricks + 1/2,
      foliage_orientation=="SOT" ~ height_full_bricks - 1/2 + (3/8) + 1/3)
  ) %>% 
  mutate(across(c(x, xend),
                ~ifelse(foliage_orientation=="SOT", -1, 1)*.)) -> 
  tree_plot_base_split


#STUDS for SOT
tree_plot_sot <- dat_hgt %>% 
  count(foliage_orientation, height_full_bricks) %>% 
  complete(height_full_bricks = 1:15, foliage_orientation, fill = list()) %>% 
  mutate(n = ifelse(foliage_orientation=="SOT", -1, 1)*n) %>%
  filter(n < 0)

-1:min(tree_plot_sot$n) %>% 
  purrr::map_dfr(function(nn){
    tree_plot_sot %>% 
      filter(n <= nn) %>% 
      mutate(n = nn)
  }) %>% 
  mutate(
    offset = ifelse(foliage_orientation == "SOT", -1/2, 1/2),
    xmin = n + (1/2) - (5/8)/2 + offset, 
    xmax = n + (1/2) + (5/8)/2 + offset,
    ymin = height_full_bricks - 1/2 + (3/8) + 1/3,
    ymax = height_full_bricks - 1/2 + 1.5*(3/8) + 1/3
  ) -> tree_plot_sot2

#STUDS for SNOT
tree_plot_snot <- dat_hgt %>% 
  count(foliage_orientation, height_full_bricks) %>% 
  complete(height_full_bricks = 1:15, foliage_orientation, fill = list()) %>% 
  mutate(n = ifelse(foliage_orientation=="SOT", -1, 1)*n) %>%
  filter(n > 0)

1:max(tree_plot_snot$n) %>% 
  purrr::map_dfr(function(nn){
    tree_plot_snot %>% 
      filter(n >= nn) %>% 
      mutate(n = nn)
  }) %>% 
  mutate(
    offset = ifelse(foliage_orientation == "SOT", -1/2, 1/2),
    x0 = n - (1/2) + offset, 
    y0 = height_full_bricks,
    r = (5/8)/2
  ) -> tree_plot_snot2

#Wood / y axis
tree_plot_wood <- tibble(height_full_bricks = 1:max(tree_plot_base$height_full_bricks)) %>% 
  mutate(xmin = -1/2 + 1/16, xmax = 1/2 - 1/16,
         #Cylinder body
         ymin1 = height_full_bricks -1/2 + 1/6,
         ymax1 = height_full_bricks +1/2,
         #Cylinder bottom
         ymin2 = height_full_bricks -1/2,
         ymax2 = ymin1)


# Plot it
tree_plot_base %>% 
  ggplot() +
  #Wood
  geom_rect(data = tree_plot_wood,
            aes(xmin=xmin, xmax=xmax,
                ymin=ymin1, ymax=ymax1),
            fill = "#5F3109", color = "#cccccc", size = 0.2) +
  geom_rect(data = tree_plot_wood,
            aes(xmin=xmin*(6.5/8), xmax=xmax*(6.5/8),
                ymin=ymin2, ymax=ymax2),
            fill = colorspace::darken("#5F3109", .3), color = "#cccccc", size = 0.2) +
  geom_rect(xmin = (-1/2 + 1/16) * (5/8), xmax = (1/2 - 1/16) * (5/8),
            ymin = max(tree_plot_wood$ymax1), ymax = max(tree_plot_wood$ymax1)+(1/6),
            fill = colorspace::darken("#5F3109", .3), color = "#cccccc", size = 0.2) +
  geom_text(data = tree_plot_wood,
            aes(x=0, y=ymin1+1/2, label = height_full_bricks),
            color = "white", fontface = "bold", size = 3) +
  #Foliage bricks
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill = "#00852B", color = "#cccccc", size = 0.1) +
  geom_segment(data = tree_plot_base_split,
               aes(x=x, xend=xend, y=y, yend=yend),
               color = "#cccccc", size = 0.2) +
  #STUDS SOT
  geom_rect(data = tree_plot_sot2,
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill = "#00852B", color = "#cccccc", size = 0.1)+
  #STUDS SNOT
  ggforce::geom_circle(data = tree_plot_snot2,
                       aes(x0=x0+0.1, y0=y0-0.1, r=r), 
                       fill = "black", alpha = 0.2, color = NA) +
  ggforce::geom_circle(data = tree_plot_snot2,
                       aes(x0=x0, y0=y0, r=r*0.9), 
                       color = colorspace::darken("#00852B", .3), fill = "#00852B") +
  #LABELS
  annotate("text", label = "studs on top",
           x = -1.5, y = 1, hjust = 1,
           color = colorspace::darken("#00852B", 0.3), fontface = "bold") +
  annotate("text", label = "studs on sides",
           x =  1.5, y = 1, hjust = 0,
           color = colorspace::darken("#00852B", 0.3), fontface = "bold") +
  annotate("text", label = "tree height (bricks)",
           x = 1, y = 24, hjust = 0, vjust=0,
           color = colorspace::darken("#5F3109", 0.0)) +
  #Formatting
  #Super hacky x-axis to have my custom Y axis
  scale_x_continuous(breaks = c(seq(-12.5, -2.5, by=2), 0, seq(2.5, 12.5, by=2)), 
                     labels = c(seq(12, 2, by=-2), "", seq(2, 12, by=2))) +
  coord_fixed(xlim = c(-13, 13), ylim = c(0.5, 25), expand = FALSE) +
  labs(
    title = "Height distribution of brick-built LEGO Christmas trees",
    caption = "Estimated height in bricks, studs on top.",
    x = "# of LEGO Christmas trees",
    y = NULL
  ) +
  theme_minimal(base_size = 8) +
  theme(
    plot.background = element_rect(fill = "#fff8ea", color = NA),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(color = colorspace::darken("#00852B", 0.3)),
    axis.text.x = element_text(color = colorspace::darken("#00852B", 0)),
    axis.text.y = element_blank(),
    axis.line.y = element_blank()
  )

ggsave("tree_distribution.png", height = 5, width = 5)


ratio = 16/9
width = 6
ggsave("tree_distribution_header.png", height = width/ratio, width = width)
