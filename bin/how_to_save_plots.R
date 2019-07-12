####  How to save plots contains examples of plot saving options
####  because the saved graphics can differ dramatically to the preview
####  The options are for the file format png with dpi of 600

source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

# plot width is half of potrait A4 or cover two columns
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
  geom_point() +
  scale_color_manual(values = palettes_bright$colset_cheer_brights) +
  theme_generic
ggsave("results/halfA4.png", width = 7.2, height = 5.3, units = "in", dpi = 600)

# plot has the width of single column or half of the width of potrait A4
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
  geom_point() +
  scale_color_manual(values = palettes_bright$colset_cheer_brights) +
  theme_small
ggsave("results/singlecolumnA4.png", width = 3.5, height = 2, units = "in", dpi = 600)

# 2 plots have the width of single column or half of the width of potrait A4
 a <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
  geom_point() +
  scale_color_manual(values = palettes_bright$colset_cheer_brights) +
  theme_small
 b <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
   geom_point() +
   scale_color_manual(values = palettes_bright$colset_cheer_brights) +
   theme_small
 
 ggarrange(a,b, ncol = 2, nrow = 1)
ggsave("results/halfA4_2plots.png", width = 7.2, height = 2, units = "in", dpi = 600)


# 3 plots have the width of single column or half of the width of potrait A4
# shared legend
a <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
  geom_point() +
  scale_color_manual(values = palettes_bright$colset_cheer_brights) +
  theme_very_small
b <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
  geom_point(size = 0.5) +
  scale_color_manual(values = palettes_bright$colset_cheer_brights) +
  theme_very_small
c <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
  geom_point(size = 0.3) +
  scale_color_manual(values = palettes_bright$colset_cheer_brights) +
  theme_very_small

ggarrange(a,b,c, ncol = 3, nrow = 1, common.legend = TRUE)
ggsave("results/halfA4_3plots_1legend.png", width = 7.2, height = 2, units = "in", dpi = 600)

# 3 plots have the width of single column or half of the width of potrait A4
# separatelegend
a <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
  geom_point() +
  scale_color_manual(values = palettes_bright$colset_cheer_brights) +
  guides(col = guide_legend(nrow = 2, keywidth = 0.1, keyheight = 0.1))+
  theme_very_small
b <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
  geom_point(size = 0.5) +
  scale_color_manual(values = palettes_bright$colset_cheer_brights) +
  guides(col = guide_legend(nrow = 2, keywidth = 0.1, keyheight = 0.1))+
  theme_very_small
c <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
  geom_point(size = 0.3) +
  scale_color_manual(values = palettes_bright$colset_cheer_brights) +
  guides(col = guide_legend(nrow = 2, keywidth = 0.1, keyheight = 0.1))+
  theme_very_small

ggarrange(a,b,c, ncol = 3, nrow = 1, legend = "bottom")


ggsave("results/halfA4_3plots.png", width = 7.2, height = 2, units = "in", dpi = 600)

