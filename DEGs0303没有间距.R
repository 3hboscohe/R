library(ggpubr)
library(readxl)



d <- read_excel("figuretrend2ndshoot.xlsx", sheet = "shoot0")

plot <-ggbarplot(d, "Time", "Genes", color="treatment",fill="treatment", 
                 #alpha=0.4,
                 position = position_dodge(0.5),
                 ylim=c(0, 1600),                  
                 xlim=c(0, 10),
                 width = 0.45,# ２つのtreatmentがあるので、72x2=144までにしないと表示されない
                 combine = T)+
  scale_y_continuous(expand=c(0,0))+
  theme(axis.title = element_text(size=22,face = "bold"),
        axis.text = element_text(size=20))+
  scale_x_discrete(breaks = c(0, 0.5, 1, 2, 4, 8, 12, 24, 36, 48, 72)) 


print(plot)
# 保存图形
ggsave("2ndshoot1.png", plot, width = 10, height = 6, dpi = 600)

