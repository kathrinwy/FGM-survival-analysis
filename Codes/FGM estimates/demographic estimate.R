# Copyright statement comment ---------------------------------------------

# @UNFPA

# Author comment ----------------------------------------------------------

# work is progress-

# File description, purpose of code, inputs and output --------------------

# this code has merges KM estimates with single year estimates and 
# projections from the WPP 2017

# Source codes, libraries, global options and working directory -----------

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

# WD - either UNFPA PC or private computers

# UNFPA computer
# wpp_2017 <- read_excel("C:/Users/weny/Google Drive/2018/FGM/Survival Analysis/Data/popdata_female_singleyear_2015-2030.xlsx")

# Private computer
wpp_2017 <- read_excel("C:/Users/Kathrin Weny/Google Drive (weny@unfpa.org)/2018/FGM/01 -Survival Analysis/03 -Data/popdata_female_singleyear_2015-2030.xlsx")

# Executed statements -----------------------------------------------------

# Loop through countries
countries <- c("benin","burkina faso", 
               "central_african_republic","chad","cote_divoire",
               "egypt","ethiopia",
               "gambia","ghana","guinea bissau","guinea",
               "indonesia","iraq",
               "kenya",
               "mali","mauritania",
               "niger","nigeria",
               "senegal","sierra_leone","somalia","sudan",
               "tanzania","togo",
               "yemen")

girls_atrisk<- data.frame(matrix(NA,
                                 nrow = 17, 
                                 ncol = 0)
                          )

for(n in countries){
  
  #filename <- paste("C:/Users/weny/Google Drive/2018/FGM/Survival Analysis/KM_estimates/",n,".xlsx",sep="") 
  filename <- paste("C:/Users/Kathrin Weny/Google Drive (weny@unfpa.org)/2018/FGM/01 -Survival Analysis/04 -KM estimates/",
                    n,
                    ".xlsx",
                    sep=""
                    ) 
  
  #Read in data
  fgm <- read_excel(filename)
  
  #Select pop data
  pop_data <- wpp_2017%>%
    filter(country==n)
  pop_data <- pop_data[,
                       -c(1)]
  pop_data <- t(pop_data)
  pop_data <- as.data.frame(pop_data)
  colnames(pop_data) <- pop_data[1,
                                 ]
  pop_data <- pop_data[-c(1),
                       ]
  
  #Create fgm data
  prob_cut <- fgm[,
                  5]
  sum_cut <- t(fgm[,
                   4])
  #Create dataframe in order to take out girls that 
  #have already been cut before (shift cells up by 1)
  sum_cut_t0 <- cbind(0,
                      sum_cut
                      )  
  sum_cut_t0 <- sum_cut_t0[1,
                           1:15
                           ] 
  #Loop over years
  #2015 = t0 or j=1
  outputname <- paste("full_",
                      n,
                      sep="")
  outputname<- (pop_data[,1]*prob_cut)*(1-sum_cut_t0)
  
  for(j in 2:16){  # from j=2=2016 to j=16=2030 
    
    #Select pop data from year j
    pop_t1 <- as.data.frame(pop_data[,j])
    
    #Loop over ages
    #age 0 or i = 1
    year <- pop_t1[1,]*prob_cut[1,] 
    
    #all other ages
    for(i in 2:15){ 
      year <- rbind(year,
                    (pop_t1[i,]*(1-sum_cut[,i-1]))*prob_cut[i,])
      #Taking out survival rate 1-(pop_t0[i-1,]-pop_t1[i,])/pop_t0[i-1,] as kaplan meier takes account of censoring
      
    }
    
    outputname[,j]             <- year
    
  }
  
  colnames(outputname)       <- c(2015,2016,2017,2018,2019,
                                  2020,2021,2022,2023,2024,
                                  2025,2026,2027,2028,2029,2030)
  
  #Sum over ages to get number of girls to be cut each year
  outputname_2              <- paste("sum_years_",
                                     n,
                                     sep="")
  outputname_2              <- as.data.frame(colSums(outputname))
  colnames(outputname_2)      <- n
  
  outputname_3              <- paste("sum_total_",
                                     n,
                                     sep="")
  outputname_3              <- as.data.frame(colSums(outputname_2))
  row.names(outputname_3)    <- "Total"
  colnames(outputname_3)     <- n
  
  outputname_4              <- paste("Total",
                                     n,
                                     sep="")
  outputname_4              <- rbind(outputname_2, 
                                     outputname_3)
  #write.xlsx(outputname_4, paste("C:/Users/weny/Google Drive/2018/FGM/Survival Analysis/Output/",n,".xlsx",sep=""))
  write.xlsx(outputname_4, paste("C:/Users/Kathrin Weny/Google Drive (weny@unfpa.org)/2018/FGM/01 -Survival Analysis/04 -KM estimates/Output/",n,".xlsx",sep=""))

  girls_atrisk[,n] <- outputname_4
  
}


# Visualization -----------------------------------------------------

rownames(girls_atrisk) <- c(2015,2016,2017,2018,2019,
                            2020,2021,2022,2023,2024,
                            2025,2026,2027,2028,2029,2030,
                            "total")


plot_df <- data.frame(t(girls_atrisk))
plot_df <- rownames_to_column(plot_df, 
                              "country")
colnames(plot_df)<- c("country",
                      2015,2016,2017,2018,2019,
                      2020,2021,2022,2023,2024,
                      2025,2026,2027,2028,2029,2030,
                      "total")
plot_df <- gather(plot_df, 
                  key="year", 
                  value="girls_cut",
                  -country)

plot_df <-  plot_df%>%
  filter(year != "total")%>%
  mutate(data = girls_cut/1000)

plot_df_national <- plot_df%>%
  filter(country !="World")

plot_df_national$country <- as.factor(plot_df_national$country)
plot_df_national$year <- as.factor(plot_df_national$year)

plot_df_national$country <- factor(plot_df_national$country, 
                                   levels = rev(unique(plot_df_national$country[order(plot_df_national$data)])))



# With Indonesia

plot_df_national <- plot_df_national%>%
  mutate(id  = ifelse(country=="indonesia", 
                      1, 
                      0))

totals_in <- plot_df_national %>%
  group_by(year) %>%
  summarize(total = sum(data))

totals <- subset(plot_df_national, 
                 country!="indonesia") %>%
  group_by(year) %>%
  summarize(total = sum(data))

plot_df_national$year <- as.numeric(as.character(plot_df_national$year))
totals$year <- as.numeric(as.character(totals$year))
totals_in$year <- as.numeric(as.character(totals_in$year))
str(plot_df_national$year)

df_1  <- data.frame(x1 = 2030, 
                    x2 = 2027, 
                    y1 = totals_in[16,2]-0.1, 
                    y2 = 3.8)
x1 = 2030
x2 = 2027
y1 = totals_in[16,2]-0.1
y2 = 3.8

text = c("Estimate including girls \n projected to be cut in the\n first year of their life\n in Indonesia")


ggplot() +
  geom_bar( stat="identity",position="stack", 
            subset(plot_df_national,country!="indonesia"), 
            mapping=aes(x=year,y=data,fill=country))+
  geom_point(data=totals_in, 
             aes(x=year, 
                 y=total),
             shape = 95, 
             color="black", 
             size = 3, 
             stroke = 2)+
  
  scale_color_brewer(type="diverging",
                     palette = "Greens")+
  #scale_fill_hue(h = -c(100, 460))+
  #scale_fill_viridis()+ 
  geom_segment(aes(x = x1, 
                   y = y1, 
                   xend = x2, 
                   yend = y2), 
               data = df_1)+
  theme_bw()+
  guides(colour = guide_legend(ncol = 1))+
  scale_y_continuous(limits = c(0, 5))+

  geom_text(aes(x=2027,
                y=3,
                label=text), 
            size=3)+ 

  labs(title = " ",
       caption = "Data: Latest nationally representative househod survey and \n World Population Prospects: the 2017 Revision, UN DESA, Population Division",
       x = " ", 
       y = "Nr of girls, millions", 
       fill = "Country")

# without Indonesia

ggplot() +
  geom_bar( stat="identity",
            position="stack", 
            subset(plot_df_national,country!="indonesia"), 
            mapping=aes(x=year,
                        y=data,
                        fill=country))+
  
  scale_color_brewer(type="diverging",
                     palette = "Greens")+
  
  theme_bw()+
  guides(colour = guide_legend(ncol = 1))+
  scale_y_continuous(limits = c(0, 
                                5))+
  
  labs(title = " ",
       caption = "Data: Latest nationally representative househod survey and \n World Population Prospects: the 2017 Revision, UN DESA, Population Division",
       x = " ",
       y = "Nr of girls, millions",
       fill = "Country")

# Without Indonesia - different color scheme

totals <- subset(plot_df_national,
                 country!="indonesia") %>%
  group_by(year) %>%
  summarize(total = sum(data))


ggplot() +
  
  geom_bar( stat="identity",
            position="stack", 
            subset(plot_df_national,country!="indonesia"), 
            mapping=aes(x=year,
                        y=data,
                        fill=country))+
  
  geom_text(aes(year, 
                total, 
                label = paste(round(total,
                                    1)),
                vjust=-1), 
            data = totals, 
            size=4)+
  scale_fill_manual( values=c("brown1","coral","darkgoldenrod1",
                              "brown3","chocolate",
                              "darkorange","firebrick1","darkgreen", 
                              "dodgerblue","darkslategray2",
                              "green4","lightblue2","lightgoldenrod4",
                              "lavenderblush3","indianred4",
                              "lightsalmon","lightskyblue","navy",
                              "lightseagreen","olivedrab",
                              "orangered3","seagreen","peru","palegreen2",
                              "salmon"))+
  
  scale_y_continuous(limits = c(0, 5))+
  theme_classic()+ 
  labs(title = " ",
       caption = "Data: Latest nationally representative househod survey and \n World Population Prospects: the 2017 Revision, UN DESA, Population Division",
       x = " ", 
       y = "Nr of girls, millions", 
       fill = "Country")

