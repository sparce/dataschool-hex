library(pointillist)
library(tidyverse)
library(hexSticker)
library(showtext)

font_add_google("Roboto Mono", "roboto")

ds <- img_df("dataschool.png")

points <- ds %>% 
  filter(r < 1)

set.seed(100)
points_sub <- points %>% 
  filter(row_number() %in% sample(seq_along(1:nrow(.)), nrow(.) * 0.04, replace = FALSE)) %>% 
  mutate(size = rnorm(n(), 2),
         colour = factor(sample(1:5, n(), replace = TRUE)))

p <- ggplot(points_sub, aes(col, -row)) +
  geom_point(aes(colour = colour, size = size)) +
  scale_size_continuous(range = c(0.5, 2), guide = FALSE) +
  scale_colour_brewer(palette = "Set1", guide = FALSE) +
  coord_fixed() +
  labs(x = "x", y = "y") +
  theme_void() +
  theme(axis.line = element_line(colour = "black", size = 2),
        axis.title = element_text(size = 18, colour = "black"),
        axis.title.x = element_text(size = 18, face = "bold", margin = margin(0.3, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 18, face = "bold", angle = 90, margin = margin(0, 0.3, 0, 0, unit = "cm")),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))
p
showtext_auto(enable = FALSE) #need this if you've enabled it previously
ggsave("subplot.png", p, width = 8, height = 8, type = "cairo-png", units = "in")

pwhite <- ggplot(points_sub, aes(col, -row)) +
  geom_point(aes(size = size), colour = "white", alpha = 0.8) +
  scale_size_continuous(range = c(0.5, 2), guide = FALSE) +
  coord_fixed() +
  labs(x = "x", y = "y") +
  theme_void() +
  theme(axis.line = element_line(colour = "white", size = 2),
        axis.title = element_text(colour = "white"),
        axis.title.x = element_text(size = 18, face = "bold", margin = margin(0.3, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 18, face = "bold", angle = 90, margin = margin(0, 0.3, 0, 0, unit = "cm")),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))
pwhite
showtext_auto(enable = FALSE)
ggsave("subplot-white.png", pwhite, width = 8, height = 8, type = "cairo-png", units = "in", bg = "transparent")

showtext_auto()

sticker("subplot.png", package = "FOCUS v1.0", h_color = "black", h_fill = "white",s_x = 1, s_y = 1, s_width = 0.85, s_height = 0.85, 
        p_y = 0.3, p_color = "black", p_family = "roboto", p_size = 18,
        filename = "ds-sticker.png", dpi = 600)

sticker("subplot-white.png", package = "Original v1.0", h_color = "black", h_fill = "#4daf4a",s_x = 1, s_y = 1, s_width = 0.85, s_height = 0.85, 
        p_y = 0.35, p_color = "white", p_family = "roboto", p_size = 18,
        filename = "ds-sticker-white.png", dpi = 600)
