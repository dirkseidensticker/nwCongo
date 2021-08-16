##############
# MDJ 85/001 #
##############

library(ggplot2)

df <- read.csv("C:/Users/Dirk/Dropbox/Dissertation/data/base/MDJ85-01_Niv_Profil.csv", dec=",")

ggplot(df, aes(x = X, y = Z)) +
  geom_line() + 
  coord_equal() +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave()
