library(ggplot2)
library(hexSticker)

set.seed(4365388)

df <- data.frame(x = round(rnorm(100, sd = 3)))

df$t <- ifelse(df$x < -2, "r",
               ifelse(df$x > 2, "g",
                      "a"))

p <- ggplot(df, aes(x, fill = t, colour = t)) + geom_dotplot() +
  theme_void() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("#f46036", "#1b998b", "#e71d36")) +
  scale_fill_manual(values = c("#f46036", "#1b998b", "#e71d36")) 
p

sticker(p, package="tout", p_size=40, p_x=1, p_y=0.6, p_family="",
        s_x=1, s_y=1.6, s_width=1.7, s_height=1.7,
        h_fill="#2e294e", h_color="#011638",
        filename = "./man/figures/logo.png")