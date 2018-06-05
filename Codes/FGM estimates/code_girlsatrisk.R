library(plyr)
library(dplyr)
library(magrittr)
library(tidyverse)
library(ggrepel)
library(gdata)
library(knitr)
library(devtools)
library(xtable)
library(readxl)
library(ggplot2)
library(xlsx)
library(ggthemes)
library(scales)
#wpp_2017 <- read_excel("C:/Users/weny/Google Drive/2018/FGM/Survival Analysis/Data/popdata_female_singleyear_2015-2030.xlsx")
wpp_2017 <- read_excel("C:/Users/Kathrin Weny/Google Drive (weny@unfpa.org)/2018/FGM/Survival Analysis/Data/popdata_female_singleyear_2015-2030.xlsx")
#Loop through countries
countries <- c("benin","burkina faso","central_african_republic","chad","cote_divoire",
               "egypt","ethiopia","gambia","ghana","guinea bissau","guinea","indonesia","iraq","kenya",
               "mali","mauritania","niger","nigeria","senegal","sierra_leone","somalia",
               "sudan","tanzania","togo","yemen")

girls_atrisk<- data.frame(matrix(NA, nrow = 17, ncol = 0))

for(n in countries){

#filename <- paste("C:/Users/weny/Google Drive/2018/FGM/Survival Analysis/KM_estimates/",n,".xlsx",sep="") 
filename <- paste("C:/Users/Kathrin Weny/Google Drive (weny@unfpa.org)/2018/FGM/Survival Analysis/KM_estimates/",n,".xlsx",sep="") 

#Read in data
fgm <- read_excel(filename)

#Select pop data
pop_data <- wpp_2017%>%
  filter(country==n)
pop_data <- pop_data[,-c(1)]
pop_data <- t(pop_data)
pop_data <- as.data.frame(pop_data)
colnames(pop_data) <- pop_data[1,]
pop_data <- pop_data[-c(1),]

#Create fgm data
prob_cut <- fgm[,5]
sum_cut <- t(fgm[,4])

#Create dataframe in order to take out girls that have already been cut before (shift cells up by 1)
sum_cut_t0 <- cbind(0,sum_cut)  
sum_cut_t0 <- sum_cut_t0[1,1:15] 


#Loop over years
#2015 = t0 or j=1
outputname                <- paste("full_",n,sep="")
outputname<- (pop_data[,1]*prob_cut)*(1-sum_cut_t0)


for(j in 2:16){  # from j=2=2016 to j=16=2030 

#Select pop data from year j
pop_t1 <- as.data.frame(pop_data[,j])

#Loop over ages
#age 0 or i = 1
year <- pop_t1[1,]*prob_cut[1,] 

#all other ages
for(i in 2:15){ 
    year <- rbind(year,(pop_t1[i,]*(1-sum_cut[,i-1]))*prob_cut[i,])
#Taking out survival rate 1-(pop_t0[i-1,]-pop_t1[i,])/pop_t0[i-1,] as kaplan meier takes account of censoring
    
}

outputname[,j]             <- year

}

colnames(outputname)       <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025,2026,2027,2028,2029,2030)

#Sum over ages to get number of girls to be cut each year
outputname_2              <- paste("sum_years_",n,sep="")
outputname_2              <- as.data.frame(colSums(outputname))
colnames(outputname_2)      <- n

outputname_3              <- paste("sum_total_",n,sep="")
outputname_3              <- as.data.frame(colSums(outputname_2))
row.names(outputname_3)    <- "Total"
colnames(outputname_3)     <- n

outputname_4              <- paste("Total",n,sep="")
outputname_4              <- rbind(outputname_2, outputname_3)


#write.xlsx(outputname_4, paste("C:/Users/weny/Google Drive/2018/FGM/Survival Analysis/Output/",n,".xlsx",sep=""))

write.xlsx(outputname_4, paste("C:/Users/Kathrin Weny/Google Drive (weny@unfpa.org)/2018/FGM/Survival Analysis/Output/",n,".xlsx",sep=""))


girls_atrisk[,n] <- outputname_4

}

###########################

#Plot

rownames(girls_atrisk) <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025,2026,2027,2028,2029,2030,"total")
#girls_atrisk<- cbind(girls_atrisk, World = rowSums(girls_atrisk))

plot_df <- data.frame(t(girls_atrisk))
plot_df <- rownames_to_column(plot_df, "country")
colnames(plot_df)<- c("country",2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025,2026,2027,2028,2029,2030,"total")
plot_df <- gather(plot_df, key="year", value="girls_cut",-country)

plot_df <-  plot_df%>%
  filter(year != "total")%>%
  mutate(data = girls_cut/1000)

plot_df_national <- plot_df%>%
  filter(country !="World")

plot_df_national$country <- as.factor(plot_df_national$country)
plot_df_national$year <- as.factor(plot_df_national$year)

plot_df_national$country <- factor(plot_df_national$country, levels = rev(unique(plot_df_national$country[order(plot_df_national$data)])))



##############################################################
#Area grap
##############################################################

#With Indonesia

plot_df_national <- plot_df_national%>%
  mutate(id  = ifelse(country=="indonesia", 1, 0))

totals_in <- plot_df_national %>%
  group_by(year) %>%
  summarize(total = sum(data))

totals <- subset(plot_df_national, country!="indonesia") %>%
  group_by(year) %>%
  summarize(total = sum(data))

plot_df_national$year <- as.numeric(as.character(plot_df_national$year))
totals$year <- as.numeric(as.character(totals$year))
totals_in$year <- as.numeric(as.character(totals_in$year))
str(plot_df_national$year)

df_1  <- data.frame(x1 = 2030, x2 = 2027, y1 = totals_in[16,2]-0.1, y2 = 3.8)
x1 = 2030
x2 = 2027
y1 = totals_in[16,2]-0.1
y2 = 3.8

text = c("Estimate including girls \n projected to be cut in the\n first year of their life\n in Indonesia")

  ggplot() +
  geom_bar( stat="identity",position="stack", subset(plot_df_national,country!="indonesia"), 
           mapping=aes(x=year,y=data,fill=country))+
    geom_point(data=totals_in, aes(x=year, y=total),
             shape = 95, color="black", size = 3, stroke = 2)+
 
     # scale_fill_manual( values=c("brown1","coral","darkgoldenrod1","brown3","chocolate",
          #                    "darkorange","firebrick1","darkgreen", "dodgerblue","darkslategray2",
           #                   "green4","lightblue2","lightgoldenrod4","lavenderblush3","indianred4",
            #                  "lightsalmon","lightskyblue","navy","lightseagreen","olivedrab",
             #                 "orangered3","seagreen","peru","palegreen2"))+
 scale_color_brewer(type="diverging",palette = "Greens")+
  #scale_fill_hue(h = -c(100, 460))+
  #scale_fill_viridis()+ 
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = df_1)+
  theme_bw()+
   guides(colour = guide_legend(ncol = 1))+
    scale_y_continuous(limits = c(0, 5))+
    #geom_text(aes(year, total, label = paste(round(total,1)),vjust=-1), data = totals_in, size=4)+
  geom_text(aes(x=2027,y=3,label=text), size=3)+ 
  # theme_classic()+ 
    labs(title = " ",
        caption = "Data: Latest nationally representative househod survey and \n World Population Prospects: the 2017 Revision, UN DESA, Population Division",
        x = " ", y = "Nr of girls, millions", fill = "Country")
  
  
  ggplot() +
    geom_bar( stat="identity",position="stack", subset(plot_df_national,country!="indonesia"), 
              mapping=aes(x=year,y=data,fill=country))+

    scale_color_brewer(type="diverging",palette = "Greens")+

    theme_bw()+
    guides(colour = guide_legend(ncol = 1))+
    scale_y_continuous(limits = c(0, 5))+
  
    labs(title = " ",
         caption = "Data: Latest nationally representative househod survey and \n World Population Prospects: the 2017 Revision, UN DESA, Population Division",
         x = " ", y = "Nr of girls, millions", fill = "Country")



df_1  <- data.frame(x1 = totals[1,1], x2 = totals[1,1], y1 = totals[1,2], y2 = totals_in[1,2]-0.1)
x1 = totals[1,1]
x2 = totals[1,1]
y1 = totals[1,2]
y2 = totals_in[1,2]-0.1
df_2  <- data.frame(x1a = totals[2,1], x2a = totals[2,1], y1a = totals[2,2], y2a = totals_in[2,2]-0.1)
x1a = totals[2,1]
x2a = totals[2,1]
y1a = totals[2,2]
y2a = totals_in[2,2]-0.1

df_3  <- data.frame(x1b = totals[3,1], x2b = totals[3,1], y1b = totals[3,2], y2b = totals_in[3,2]-0.1)
x1b = totals[3,1]
x2b = totals[3,1]
y1b = totals[3,2]
y2b = totals_in[3,2]-0.1

df_4  <- data.frame(x1c = totals[4,1], x2c = totals[4,1], y1c = totals[4,2], y2c = totals_in[4,2]-0.1)
x1c = totals[4,1]
x2c = totals[4,1]
y1c = totals[4,2]
y2c = totals_in[4,2]-0.1

df_5  <- data.frame(x1d = totals[5,1], x2d = totals[5,1], y1d = totals[5,2], y2d = totals_in[5,2]-0.1)
x1d = totals[5,1]
x2d = totals[5,1]
y1d = totals[5,2]
y2d = totals_in[5,2]-0.1

df_6  <- data.frame(x1e = totals[6,1], x2e = totals[6,1], y1e = totals[6,2], y2e = totals_in[6,2]-0.1)
x1e = totals[6,1]
x2e = totals[6,1]
y1e = totals[6,2]
y2e = totals_in[6,2]-0.1

df_7  <- data.frame(x1f = totals[7,1], x2f = totals[7,1], y1f = totals[7,2], y2f = totals_in[7,2]-0.1)
x1f = totals[7,1]
x2f = totals[7,1]
y1f = totals[7,2]
y2f = totals_in[7,2]-0.1


df_8  <- data.frame(x1g = totals[8,1], x2g = totals[8,1], y1g = totals[8,2], y2g = totals_in[8,2]-0.1)
x1g = totals[8,1]
x2g = totals[8,1]
y1g = totals[8,2]
y2g = totals_in[8,2]-0.1

df_9  <- data.frame(x1h = totals[9,1], x2h = totals[9,1], y1h = totals[9,2], y2h = totals_in[9,2]-0.1)
x1h = totals[9,1]
x2h = totals[9,1]
y1h = totals[9,2]
y2h = totals_in[9,2]-0.1

df_10 <- data.frame(x1i = totals[10,1], x2i = totals[10,1], y1i = totals[10,2], y2i = totals_in[10,2]-0.1)
x1i = totals[10,1]
x2i = totals[10,1]
y1i = totals[10,2]
y2i = totals_in[10,2]-0.1

df_11 <- data.frame(x1j = totals[11,1], x2j = totals[11,1], y1j = totals[11,2], y2j = totals_in[11,2]-0.1)

x1j = totals[11,1]
x2j = totals[11,1]
y1j = totals[11,2]
y2j = totals_in[11,2]-0.1


df_12 <- data.frame(x1k = totals[12,1], x2k = totals[12,1], y1k = totals[12,2], y2k = totals_in[12,2]-0.1)

x1k = totals[12,1]
x2k = totals[12,1]
y1k = totals[12,2]
y2k = totals_in[12,2]-0.1

df_13 <- data.frame(x1l = totals[13,1], x2l = totals[13,1], y1l = totals[13,2], y2l= totals_in[13,2]-0.1)
x1l = totals[13,1]
x2l = totals[13,1]
y1l = totals[13,2]
y2l= totals_in[13,2]-0.1

df_14 <- data.frame(x1m = totals[14,1], x2m = totals[14,1], y1m = totals[14,2], y2m = totals_in[14,2]-0.1)

x1m = totals[14,1]
x2m = totals[14,1]
y1m = totals[14,2]
y2m = totals_in[14,2]-0.1

df_15 <- data.frame(x1n = totals[15,1], x2n = totals[15,1], y1n = totals[15,2], y2n = totals_in[15,2]-0.1)
x1n = totals[15,1]
x2n = totals[15,1]
y1n = totals[15,2]
y2n = totals_in[15,2]-0.1

df_16 <- data.frame(x1o = totals[16,1], x2o = totals[16,1], y1o = totals[16,2], y2o = totals_in[16,2]-0.1)

x1o = totals[16,1]
x2o = totals[16,1]
y1o = totals[16,2]
y2o = totals_in[16,2]-0.1



plot1 +

 geom_segment(aes(x = x1, y = y1, xend = x1, yend = y2), data = df_1, arrow = arrow(length = unit(0.3, "cm")))+
 geom_segment(aes(x = x1a, y = y1a, xend = x1a, yend = y2a), data = df_2, arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x = x1b, y = y1b, xend = x1b, yend = y2b), data = df_3, arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x = x1c, y = y1c, xend = x1c, yend = y2c), data = df_4, arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x = x1d, y = y1d, xend = x1d, yend = y2d), data = df_5, arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x = x1e, y = y1e, xend = x1e, yend = y2e), data = df_6, arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x = x1f, y = y1f, xend = x1f, yend = y2f), data = df_7, arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x = x1g, y = y1g, xend = x1g, yend = y2g), data = df_8, arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x = x1h, y = y1h, xend = x1h, yend = y2h), data = df_9, arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x = x1i, y = y1i, xend = x1i, yend = y2i), data = df_10, arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x = x1j, y = y1j, xend = x1j, yend = y2j), data = df_11, arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x = x1k, y = y1k, xend = x1k, yend = y2k), data = df_12, arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x = x1l, y = y1l, xend = x1l, yend = y2l), data = df_13, arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x = x1m, y = y1m, xend = x1m, yend = y2m), data = df_14, arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x = x1n, y = y1n, xend = x1n, yend = y2n), data = df_15, arrow = arrow(length = unit(0.3, "cm")))+  
  geom_segment(aes(x = x1o, y = y1o, xend = x1o, yend = y2o), data = df_16, arrow = arrow(length = unit(0.3, "cm")))

#Without Indonesia
  
  totals <- subset(plot_df_national, country!="indonesia") %>%
    group_by(year) %>%
    summarize(total = sum(data))
  

ggplot() +
  
  geom_bar( stat="identity",position="stack", subset(plot_df_national,country!="indonesia"), 
            mapping=aes(x=year,y=data,fill=country))+
  
  geom_text(aes(year, total, label = paste(round(total,1)),vjust=-1), data = totals, size=4)+
  scale_fill_manual( values=c("brown1","coral","darkgoldenrod1","brown3","chocolate",
                              "darkorange","firebrick1","darkgreen", "dodgerblue","darkslategray2",
                              "green4","lightblue2","lightgoldenrod4","lavenderblush3","indianred4",
                              "lightsalmon","lightskyblue","navy","lightseagreen","olivedrab",
                              "orangered3","seagreen","peru","palegreen2","salmon"))+
  
  scale_y_continuous(limits = c(0, 5))+
  theme_classic()+ 
  labs(title = " ",
       caption = "Data: Latest nationally representative househod survey and \n World Population Prospects: the 2017 Revision, UN DESA, Population Division",
       x = " ", y = "Nr of girls, millions", fill = "Country")

###################
#Round graph
###################
plot_df_world2<- read_excel("C:/Users/weny/Google Drive/2018/FGM/Survival Analysis/plot/Plot_world2.xlsx")

plot_df_world2
plot_df_world2$Type <- as.numeric(plot_df_world2$Type)

ggplot(subset(plot_df_world2,Type !=2031), aes(x=Year, y=Data, 
                           fill=Type)) +
  geom_col()+
 coord_polar() +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_blank())+
  theme_minimal()+
  
 # scale_fill_discrete() +
  theme(legend.position="none", axis.text=element_text(size=6))+
  scale_x_continuous(name ="Year", 
                  breaks = seq(2015, 2030, by = 1))+
  scale_y_continuous(limits = c(0, 72))+
  geom_text_repel(data = subset(plot_df_world2, Type==2031),
   aes(label = paste(round(Data,0)), size=Data), nudge_y=0.6,fontface = 'bold')+
   scale_size(range=c(1, 4.5), guide=FALSE)+
  
  labs(title = "",
       caption = "Data: Latest nationally representative househod survey and \n World Population Prospects: the 2017 Revision, UN DESA, Population Division",
       x = " ", y = " ")




