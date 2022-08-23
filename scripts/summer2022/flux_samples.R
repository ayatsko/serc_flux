df <- read.csv("/Users/abbeyyatsko/Downloads/dw_sample_cubes_all - Sheet1 (4).csv")

# decay class distribution by species (bar graph)
ggplot(data = df, aes(x = decay_class)) + 
  geom_bar(position="stack", stat="count")+ 
  facet_wrap(~species)+
  theme_light()+ 
  geom_text(aes(label = ..count..), stat = "count", position = "fill")

# calculate average respiration temperature - add column
df$avg_respT <- rowMeans(df[ , c(12:16)], na.rm = TRUE)

# decay class by moisture percentage (boxplot)
ggplot(data = df, aes(x = decay_class, y = moisture_percentage_g)) + 
  geom_boxplot()+ 
  theme_light()

# decay class by fruiting body (bar graph)
# code variable as: 
# 0 = no FB
# 1 = yes FB
df$fruiting_bods[df$fruiting_bods == ""] <- "0"
df$fruiting_bods[df$fruiting_bods == "y"] <- "1"

ggplot(df, aes(x = decay_class, fill = fruiting_bods)) +
  geom_bar(stat = "count", position = "dodge")+
  theme_classic()

# decay class by termite
# code variable as: 
# 0 = no termites
# 1 = yes termites
df$termite[df$termite == ""] <- "0"
df$termite[df$termite == "y"] <- "1"

ggplot(df, aes(x = decay_class, fill = termite)) +
  geom_bar(stat = "count", position = "dodge")+
  theme_classic()
