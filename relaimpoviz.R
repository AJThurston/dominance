library(xlsx)
library(ggplot2)
library(relaimpo)
library(formattable)
windowsFonts(Times=windowsFont("TT Times New Roman"))
USFcols = c("#009374","#7E96A0","#9CCB3B","#80B0A6","#466069")

data = read.xlsx("data.relimpo.xlsx", sheetName = "Sheet1")

names = c("Y1","X1","X2","X3","X4","X5")
nvars = length(names)
preds = c("X1","X2","X3","X4","X5")
model1 = c("Y1 ~ X1 + X2 + X3")
model2 = c("Y1 ~ X1 + X2 + X3 + X4")
model3 = c("Y1 ~ X1 + X2 + X3 + X4 + X5")
models = c(model1, model2, model3)
colnames(data)

lm1 = lm(model1, data = data) 
lm_dominance1 <- calc.relimp(lm1, type = "lmg")
lm1dom = lm_dominance1$lmg
lm1domcs = cumsum(lm1dom)
lm1domm = c(1,1,1,1,1)

lm2 = lm(model2, data = data) 
lm_dominance2 <- calc.relimp(lm2, type = "lmg")
lm2dom = lm_dominance2$lmg
lm2domcs = cumsum(lm2dom)
lm2domm = c(2,2,2,2,2)

lm3 = lm(model3, data = data) 
lm_dominance3 <- calc.relimp(lm3, type = "lmg")
lm3dom = lm_dominance3$lmg
lm3domcs = cumsum(lm3dom)
lm3domm = c(3,3,3,3,3)

n <- max(length(lm1dom), length(lm2dom), length(lm3dom))
length(lm1dom) <- n                      
length(lm2dom) <- n
length(lm3dom) <- n
length(lm1domcs) <- n                      
length(lm2domcs) <- n
length(lm3domcs) <- n

domres1 = cbind(lm1domm,preds,lm1dom,lm1domcs)
domres2 = cbind(lm2domm,preds,lm2dom,lm2domcs)
domres3 = cbind(lm3domm,preds,lm3dom,lm3domcs)
domres = rbind(domres1,domres2,domres3)
rownames(domres) = NULL
colnames(domres) = c("Model","Predictor","Importance","Cumul")
domres = as.data.frame(domres)
domres$Importance = as.numeric(domres$Importance) 
domres$Cumul = as.numeric(domres$Cumul)

rm(counts,p,preds,n,lm1,lm2,lm3,lm1dom,lm2dom,lm3dom,lm_dominance1,lm_dominance2,lm_dominance3)

imp.plot = ggplot(data = domres, 
                  aes(x = Model, y = Importance, fill = Predictor)
) +
  geom_bar(stat="identity", 
           position = position_stack(reverse = TRUE),
           width = .25,
           color = "black"
  ) +
  geom_text(data=domres,aes(x=Model,y=Cumul,label=paste0(Predictor, "\n", round(Importance, digits = 2))),vjust=0) +
  scale_x_discrete(limits = rev(levels(domres$Model))) +
  scale_y_continuous(limits = c(0,50), labels = scales::percent) +
  scale_fill_manual(values = USFcols) +
  coord_flip() +
  
  theme(text = element_text(size = 20),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),
        axis.text.y = element_text(color = 'black'),
        axis.text.x = element_text(color = 'black'),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_line(color="black"),
        legend.position = "top"
  )
imp.plot

# Write plot and data to working directory -----------------------
ggsave("relimpo.png", 
       plot = plot1, 
       scale = 1, 
       width = 6.5, 
       height = 4, 
       units = "in",
       dpi = 300)

write.xlsx(data, "data.relimpo.xlsx")
setwd("C:/Owner/AJ Thurston/Desktop")


domresm = melt(data=domres, id.vars = Model)
help(melt)
colnames(domresm) = c("Predictor","Model", "Importance")




for (i in models)
{
  assign(paste(names(i),"lm",sep = "."), i) 
}

names(models)
models
