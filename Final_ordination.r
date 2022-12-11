### BIOL694 Final Project
#Author: Kyoko Okano
#Original: 11/27/2022
#Revised: 12/10/2022

## Data: Obtained from Eddy covariance flux tower at Shrub snow fence 
## Purpose: Separate data into two: south (control) and north (snow drift) and compare ordination plots

setwd("C:/Users/kokano/Documents/UAF_PhD/BIOL694/Final") # This is my working directory
rm(list = ls()) # Clean environment

library("tidyverse")
library("BiodiversityR")
library(vegan)

Shrub2022 <- read.csv("./SHRB_2022.csv")
#Shrub <- read.csv("./TK_SHRB_all_clean.csv")
#Shrub2022<- as.data.frame(Shrub2022)

##### Subset south vs. north

south <- Shrub2022 %>% 
  filter(!is.na(WD)) %>% 
  filter(!is.na(gC_NEE)) %>% 
  filter(!is.na(gC_ER)) %>% 
  filter(!is.na(gC_GPP)) %>% 
  filter(WD >= 160) %>% 
  filter(WD <= 200) %>% 
  select(LE_cw_gf, H_c_gf,VPD_gf,Ta_gf,Tsoil_gf,SWC_1_f,SWC_2_f,Tsoil_2,ALBEDO_f, WS_1,WD, gC_NEE, gC_ER, gC_GPP)

north <- Shrub2022 %>% 
  filter(!is.na(WD)) %>% 
  filter(!is.na(gC_NEE)) %>% 
  filter(!is.na(gC_ER)) %>% 
  filter(!is.na(gC_GPP)) %>% 
  filter(WD >= 340|WD <= 20 ) %>% 
  filter(WD!="")%>% 
  select(LE_cw_gf, H_c_gf,VPD_gf,Ta_gf,Tsoil_gf,SWC_1_f,SWC_2_f,Tsoil_2,ALBEDO_f, WS_1,WD, gC_NEE, gC_ER, gC_GPP)

### south 

# removing variables with lots of NA
colSums(is.na(south)) #number of NA will be shown
which(colSums(is.na(south))> 100) #variable name and column number of na >100 are shown 
south2<- na.omit(south[, -c(5,6,8)])
south_env<- south2[, c(1:8)]

# correlation
south_cor =cor(south2, use = "complete") #calculate correlation
corrplot(south_cor)
corrplot(south_cor,method="ellipse")

# pca
south2_pca<-prcomp(south2, center = T, scale = T)
# biplot(south2_pca, ylim=c(-0.2,0.2), cex = 1)
# biplot(south2_pca, ylim=c(-0.2,0.2), label = F, pch = 2)

# pca plot
plot_s = ordiplot(south2_pca, display = "sites",xlim=c(-8, 8),ylim=c(-8, 8))
s_fit<-envfit(south2_pca,south2[,c(1:8)], scaling = "sites")
# plot(s_fit,add=TRUE, col = "red")
plot(s_fit,add=TRUE, col = "red",cex = 0.5) # add arrows
abline (h = 0,lty="dashed")
abline (v = 0,lty="dashed")

# save plot as pdf
dev.copy2pdf(file = "./plot_s.pdf",
             width = 8, height = 7, bg = "white", compress = F, out.type = "pdf")

#make a label for ggplot
plot_s= ordiplot(south2_pca)
sites.long_s <- sites.long(plot_s, env.data=south2)
species.long_s <- species.long(plot_s)

# using ggPlot 
south2<- na.omit(south[, -c(5,6,8)])
south2_std <- scale(south2, center = T, scale = T)
south2_pca <- prcomp(south2_std)

s_df = as.data.frame(south2_pca$x)
ggplot(data = s_df, aes(x=PC1, y=PC2)) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point() + 
  geom_segment(aes(x = 0, y = 0, xend = PC1 * 4, yend = PC2 * 4, color = "red"), 
               data = as.data.frame(south2_pca$rotation), 
               arrow = arrow(length = unit(0.3, "cm")))+
  geom_label(aes(x = axis1*6, y = axis2*6, colour="red",label=labels),
             data = species.long_s)+ 
  theme(legend.position = "none")

# save ggplot
ggsave("plot_s_gg.pdf", plot=last_plot(), device="pdf", path=NULL,
       scale=1, width=8, height=7, dpi=300, limitsize=TRUE)


### north

# removing variables with lots of NA
colSums(is.na(north)) #number of NA will be shown
which(colSums(is.na(north))> 100) #variable name and column number of na >100 are shown 
north2<- na.omit(north[, -c(5,6,8)])
north_env<- north2[, c(1:8)]

# correlation
north_cor =cor(north2, use = "complete") #calculate correlation
corrplot(north_cor)
corrplot(north_cor,method="ellipse")

#pca
north2_pca<-prcomp(north2, center = T, scale = T)
# biplot(north2_pca, ylim=c(-3.0,5))
# biplot(north2_pca, ylim=c(-0.15,0.15), label = F, pch = 20)

# plot
plot_n = ordiplot(north2_pca, display = "sites",xlim=c(-8, 8),ylim=c(-8, 8))
n_fit<-envfit(north2_pca,north2[,c(1:8)], scaling = "sites")
# plot(n_fit,add=TRUE, col = "red")
plot(n_fit,add=TRUE, col = "red",cex = 0.5)
abline (h = 0,lty="dashed")
abline (v = 0,lty="dashed")

# save plot as pdf
dev.copy2pdf(file = "./plot_n.pdf",
             width = 8, height = 7, bg = "white", compress = F, out.type = "pdf")

# creating labels for ggplot
plot1 = ordiplot(north2_pca)
sites.long1 <- sites.long(plot1, env.data=north2)
species.long2 <- species.long(plot1)

# ggPlot 
north2<- na.omit(north[, -c(5,6,8)])
north2_std <- scale(north2, center = T, scale = T)
north2_pca <- prcomp(north2_std)

n_df = as.data.frame(north2_pca$x)
ggplot(data = n_df, aes(x=PC1, y=PC2)) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point() + 
  geom_segment(aes(x = 0, y = 0, xend = PC1 * 4, yend = PC2 * 4, color = "red"), 
               data = as.data.frame(north2_pca$rotation), 
               arrow = arrow(length = unit(0.3, "cm")))+
  geom_label(aes(x = axis1*6, y = axis2*6, colour="red",label=labels),
             data = species.long2)+ 
  theme(legend.position = "none")

# save ggplot
ggsave("plot_n_gg.pdf", plot=last_plot(), device="pdf", path=NULL,
       scale=1, width=8, height=7, dpi=300, limitsize=TRUE)

