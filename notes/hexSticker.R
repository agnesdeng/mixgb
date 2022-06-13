imputed.data <- mixgb(data = nhanes3_newborn, m = 5, nrounds = 20)

system.time(imputed.data <- mixgb(data = nhanes3_newborn, m = 5, maxit = 1, ordinalAsInteger = FALSE))

system.time(imputed.data <- mixgb(data = nhanes3_newborn, m = 5, maxit = 1, ordinalAsInteger = TRUE))

withNA.df <- createNA(data = nhanes3_newborn, var.names = c("HSHSIZER", "HSAGEIR", "HSSEX", "DMARETHN", "HYD1"), p = 0.1)
colSums(is.na(withNA.df))

imputed.data <- mixgb(data = withNA.df, m = 5)

library(hexSticker)

imputation.list = imputed.data
var.num = "BMPHEAD"
var.fac = "HSSEX"
original.data = withNA.df
color.pal = NULL
true.data=NULL
shape=FALSE
library(data.table)
imp.sum <- summary2var(imputation.list = imputation.list, var.x = var.num, var.y = var.fac, original.data = original.data, true.data = true.data, color.pal = color.pal, shape = shape)
all.dt <- imp.sum$all.dt
color.pal <- imp.sum$color.pal
gp <- ggplot(all.dt, aes(x = .data[[var.fac]], y = .data[[var.num]])) +
  geom_jitter(alpha = 0.3, size=0.1,position = position_jitter(), aes(color = m.set, fill = m.set))+
  geom_boxplot(alpha = 0.3, size=0.2,aes(fill = m.set), outlier.shape = NA) +
  facet_grid(cols = vars(m.set))+
  scale_color_manual(values = color.pal) +
  scale_fill_manual(values = color.pal) +
  guides(fill = "none", color = "none", shape = guide_legend(override.aes = list(size = 3))) +
  theme(
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    strip.text.x = element_text(face = "bold")
  )

blush<-"#FEC5E5"
rose<-"#FC94AF"
taffy<-"#FA86CA"
lace<-"#FFD8F0"
brink<-"#FF6090"
babypink<-"#F4C2C2"
ballet<-"#F79AC0"
cream<-"#f6efe6"
#coffee<-"#70695D"
coffee<-"#6a533b"
berry<-"#EC8094"
berry<-"#dd87a8"

fall1<-"#f9f8eb"
fall2<-"#645E54"
s<-sticker(gp,package="mixgb",s_x=1,s_y=1,s_width = 1.8,s_height=1.1,
           p_color=fall2,p_y=1.7,p_size=20,p_fontface="bold",
           h_fill=fall1,
           h_color = fall2)
plot(s)

#outfile<-tempfile(fileext=".png")
outfile<-"/Users/agnes/Desktop/mixgb.png"
sticker(gp,package="mixgb",s_x=1,s_y=1,s_width = 1.8,s_height=1.1,
        p_color=fall2,p_y=1.7,p_size=20,p_fontface="bold",
        h_fill=fall1,
        h_color = fall2,
        filename=outfile)


p <- ggplot(all.dt, aes(x = .data[[var.x]], y = .data[[var.y]])) +
  geom_point(alpha = 0.3, size=0.5,aes(shape = NULL, color = m.set, fill = m.set)) +
  facet_grid(cols = vars(m.set))+
  scale_color_manual(values = color.pal) +
  scale_fill_manual(values = color.pal) +
  guides(fill = "none", color = "none", shape = guide_legend(override.aes = list(size = 3))) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    strip.text.x = element_text(face = "bold")
  )

s<-sticker(p,package="mixgb",s_x=1,s_y=1,s_width = 1.8,s_height=1.1,p_color="black",p_y=1.7,p_size=15,h_fill="white",h_color = "black")
plot(s)

#outfile<-tempfile(fileext=".png")
outfile<-"/Users/agnes/Desktop/mixgb.png"
sticker(p,package="mixgb",filename=outfile,s_width = 1.5,s_height=0.3)


p <- ggplot(aes(x = mpg, y = wt), data = mtcars) + geom_point()
p <- p + theme_void() + theme_transparent()
outfile <- "/Users/agnes/Desktop/test.png"
sticker(p, package="hexSticker", filename=outfile)
p
