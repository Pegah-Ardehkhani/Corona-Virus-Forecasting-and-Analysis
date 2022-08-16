packages <- c('dplyr','ggplot2','tidyverse','countrycode','IRdisplay','htmlwidgets','plotly','tidyr','choroplethr','choroplethrMaps','lubridate','choroplethr','readr','skimr','naniar','ggmap','RColorBrewer','rworldmap','reshape2')

install_package <- packages %in% row.names(installed.packages())
if (any (install_package ==  FALSE))
  install.packages(packages[!install_package])

sapply(packages, require, character.only = TRUE)

#Loading the data
df <- read.csv('C:/Users/acer/Desktop/country_vaccinations.csv')

#View the data
head(df)

#Structure of data
str(df)

#Dimensions of the data
dim(df)

#Drop unnecessary features source_name and source_website
df_copy <- df[,1:13]
colnames(df_copy)

#Total NA in the entire data
paste("Total NA count: ",sum(is.na(df_copy)))

#% of NA in entire data
((sum(is.na(df_copy)) / prod(dim(df_copy))) * 100) %>%
  round (2)

#NA per feature can also be visualized as below
vis_miss(df_copy, warn_large_data=FALSE)

#Total number of NA per feature
data.frame("Total_NA" = colSums(is.na(df_copy))) %>%
  mutate ("Percentage_of_NA" = (colSums(is.na(df_copy))/dim(df_copy)[1]) %>% 
            round (3) * 100)

#Countries for which the data is collected
unique(df_copy$country)

# There is United kingdom and also its related countries included in the data, 
# let us verify if they have same number of records included
df_copy %>%
  filter (country %in% c('United Kingdom','Northern Ireland','England','Wales','Scotland'))%>%
  group_by(country)%>%
  count()

#Data Cleaning
#There are few steps in this section:

#As shown by .head() method, our data obviously contains missing values, so we'll need to deal with it
#Some of the country names appeared in the data, does not represent a single country (constituent countries) and also the union territories
#Multiple vaccines are updated in a single row and needs to be separated

#Handling the missing values
df_copy[is.na(df_copy)] = 0
vis_miss(df_copy, warn_large_data=FALSE)
#There are no more NA in the data set

# Removing the UK soviet countries
# I also consider to eliminate Gibraltar, Isle of Man, Cayman Islands, Falkland Islands, Guernsey, 
# Saint Helena, Turks and Caicos Islands (UK pertinence), Faeroe Islands (Denmark pertinence).
remove_countries = c('England','Northern Ireland','Scotland','Wales','Falkland Islands','Faeroe Islands','Isle of Man','Cayman Islands','Saint Helena','Saint Lucia','Saint Vincent and the Grenadines','Saint Kitts and Nevis')
df_copy <- df_copy %>%
  filter (!country %in% remove_countries) 
unique(df_copy$country)

#The Vaccines used worldwide
df_copy$vaccines <- str_replace_all(df_copy$vaccines, " ","")
#Remove all spaces in between
vaccine_val<- unique(df_copy$vaccines)
vaccine<- vector()
for (i in vaccine_val){
  for (j in strsplit(i, ",")){
    vaccine<- c(vaccine, j)
  }
}
vaccine_used<- unique(vaccine)
vaccine_used

#Inspect the usage of vaccine used worldwide
df_val <- data.frame(matrix(ncol = length(vaccine_used), nrow = 0))
for (i in df_copy$vaccines){
  df_val<- rbind(df_val, Vectorize(grepl, USE.NAMES = TRUE)(vaccine_used, str_replace_all(i," ","")))
}
df_val[df_val == TRUE] = 1
df_val[df_val == FALSE] =0
colnames(df_val) <- paste0(unique(vaccine))

df_val %>%
  summarise_all(sum)%>%
  gather(key="Vaccine_name", value = "Vaccine_count")%>%
  mutate (Vaccine_count1 = round(Vaccine_count/sum(Vaccine_count),3))%>%
  ggplot(mapping=aes(x=reorder(Vaccine_name,-Vaccine_count1), y=Vaccine_count1, fill =Vaccine_count1, alpha=0.7))+
  geom_col()+
  labs(x = "Vaccines", y = "Proportions", title  = "Percentage of vaccines used worldwide")+
  geom_text(aes(label = paste(Vaccine_count1*100,"%")), vjust=-0.5)+
  scale_y_continuous(labels = scales::percent) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "None")


#No of countries using these vaccines
vaccine_in_countries<- df_val %>%
  mutate(country = df_copy$country)%>%
  group_by(country)%>%
  summarise_all(sum)

data <- data.frame("No_of_countries"= apply(vaccine_in_countries[-1],2, function(c)sum(c!=0)))
cbind("Vaccine"=row.names(data),data) %>%
  ggplot(mapping=aes(x=reorder(Vaccine, -No_of_countries), y=No_of_countries, fill = Vaccine, alpha=0.5))+
  geom_col() +
  labs(x = "Vaccines", y = "No. of Countries", title  = "Number of Countries using vaccine")+
  geom_text(aes(label = No_of_countries), vjust=-0.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "None")


#Spread of Vaccine across the world
world_data<- df_copy[,c(1,2,length(df_copy))] %>%
  distinct()%>%
  separate_rows(vaccines, sep=",")

options(repr.plot.width=20, repr.plot.height=8)

plot_world_map <- function(iso,vacc) {
  d <- joinCountryData2Map(iso, joinCode = "ISO3",
                           nameJoinColumn = "iso_code", verbose= FALSE)
  mapCountryData(d, nameColumnToPlot="vaccines", catMethod = "categorical",
                 missingCountryCol = "white", addLegend = FALSE, mapTitle = paste("Countries using", vacc), oceanCol="lightblue")
  identifyCountries(d, nameCountryColumn = "iso_code",plotSelected = TRUE)
}

for (vac in vaccine_used){
  plot_world_map(world_data[world_data$vaccines == vac,],vac)
}


#Total vaccination in the countries
df_copy <- df_copy %>%
  mutate("continent" = countrycode(sourcevar = df_copy[, "country"],
                                   origin = "country.name",destination = "continent"))


continent_list <- unique(df_copy$continent)
par(mfrow=c(2,2))

for (cont in continent_list){
  cont_data<- df_copy %>%
    select ("country","total_vaccinations","continent")%>%
    filter(continent == cont)%>%
    group_by(country)%>%
    filter (total_vaccinations == max(total_vaccinations))
  
  print(ggplot(cont_data, mapping=aes(x=reorder(country,+total_vaccinations), y=total_vaccinations, fill=total_vaccinations))+
          geom_col() +
          coord_flip()+
          geom_text(aes(label = total_vaccinations), vjust=0.5,hjust=0.01,color="Red")+
          scale_y_continuous(labels = scales::comma)+
          labs(x="",y = "Total Vaccinations", title  = paste ("Total Vaccinations in countries of", cont))+
          theme_bw()+
          theme(legend.position = "none",axis.text=element_text(color="black",size=13),plot.title = element_text(size=22)))
}


s<- df_copy %>%
  select ("country","total_vaccinations","continent")%>%
  group_by(country)%>%
  filter (total_vaccinations == max(total_vaccinations)) %>%
  arrange(desc(total_vaccinations)) %>%
  ungroup() %>%
  slice_max(total_vaccinations,n=10)


options(repr.plot.width=6, repr.plot.height=6)

top_cot_plot<- ggplot (s, mapping=aes(x=reorder(country,-total_vaccinations),y=total_vaccinations))+
  geom_col(fill="#990000", alpha=0.8)+
  scale_y_continuous(labels = scales::comma)+
  theme_classic()+
  theme(axis.text=element_text(color="black",size=13),plot.title = element_text(size=22))+
  labs(x="",y = "Total Vaccinations", title  = "Top 10 total Vaccinated Countries")

#p1<-ggplotly(top_cot_plot, tooltip = c("y"))
#htmlwidgets::saveWidget(p1, "p1.html")
#isplay_html('')
show(top_cot_plot)


dose2<- df_copy %>%
  select ("country","people_fully_vaccinated")%>%
  group_by(country)%>%
  filter (people_fully_vaccinated == max(people_fully_vaccinated))%>%
  filter (people_fully_vaccinated != 0)%>%
  ungroup() %>%
  distinct() %>%
  slice_max(people_fully_vaccinated,n=10)

dose1<- df_copy %>%
  select ("country","people_vaccinated")%>%
  group_by(country)%>%
  filter (people_vaccinated == max(people_vaccinated))%>%
  filter (people_vaccinated != 0)%>%
  ungroup() %>%
  distinct() %>%
  slice_max(people_vaccinated,n=10)%>%
  arrange(desc(country))


final_country<- unique(rbind(dose2[,"country"], dose1[,"country"]))


dossage_data<- df_copy %>%
  filter (country %in% pull(final_country, country)) %>%
  select ("country","people_vaccinated","people_fully_vaccinated","total_vaccinations") %>%
  group_by(country)%>%
  filter (total_vaccinations == max(total_vaccinations)) %>%
  arrange(desc(total_vaccinations))

dossage_data2<- melt(dossage_data[,c("country","people_vaccinated","people_fully_vaccinated")], id.vars='country')

options(repr.plot.width=15, repr.plot.height=10)
ggplot(dossage_data2, aes(x=reorder(country,-value), y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge', width=0.6)+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",y = "Total Vaccinations", title  = "Vaccination counts in both dosages",fill = element_blank())+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values=c("#3366CC", "#FF9933"), labels=c("1st Dosage", "2nd Dosage"))


df_copy$date = as.Date(df_copy$date)

df_copy %>%
  select ("date","people_vaccinated","people_fully_vaccinated")%>%
  group_by(date) %>%
  summarise (people_vaccinated=sum(people_vaccinated), people_fully_vaccinated=sum(people_fully_vaccinated)) %>%
  ggplot()+
  geom_line(aes(x=date, y=people_vaccinated, col="1st Dosage"), size = 1)+
  geom_line(aes(x=date, y=people_fully_vaccinated, col="2nd Dossage"), size=1)+
  scale_y_continuous(labels = scales::comma)+
  labs(x=element_blank(),y ="Vaccination count", title  = "Vaccination trends seen in 3 months", col=element_blank())+
  theme_bw()+
  scale_color_manual(values = c('1st Dosage' = '#3366CC','2nd Dossage' = '#FF9933'))

options(warn=-1)
top_vacc_countries<- df_copy%>%
  select (country, daily_vaccinations)%>%
  group_by(country)%>%
  filter (daily_vaccinations == max(daily_vaccinations)) %>%
  distinct () %>%
  ungroup()%>%
  slice_max(daily_vaccinations,n=10)%>%
  select(country)


df_copy %>%
  filter (country %in% pull(top_vacc_countries, country))%>%
  select (country, date, daily_vaccinations) %>%
  ggplot(mapping=aes(x=date,y=daily_vaccinations))+
  geom_line(size=0.6, col="#CC3333")+
  scale_y_continuous(labels = scales::comma)+
  facet_wrap(~country)+
  theme_gray()+
  labs(x=element_blank(),y ="Vaccination count", title  = "Daily Vaccination trend in top countries", col=element_blank())

pop_vacc_dose1<-df_copy%>%
  select ("country","people_vaccinated_per_hundred")%>%
  group_by (country)%>%
  filter (people_vaccinated_per_hundred == max(people_vaccinated_per_hundred))%>%
  arrange(desc(people_vaccinated_per_hundred))%>%
  distinct()%>%
  ungroup %>%
  rename(region = country, value=people_vaccinated_per_hundred) %>%
  mutate(region = tolower(region))%>%
  mutate(region = recode(region, "united states"    = "united states of america",
                         "north macedonia"  = "macedonia",
                         "serbia" = "republic of serbia"))



pop_vacc_dose2<-df_copy%>%
  select ("country","people_fully_vaccinated_per_hundred")%>%
  group_by (country)%>%
  filter (people_fully_vaccinated_per_hundred == max(people_fully_vaccinated_per_hundred))%>%
  arrange(desc(people_fully_vaccinated_per_hundred))%>%
  distinct()%>%
  ungroup %>%
  rename(region = country, value=people_fully_vaccinated_per_hundred) %>%
  mutate(region = tolower(region))%>%
  mutate(region = recode(region, "united states"    = "united states of america",
                         "north macedonia"  = "macedonia",
                         "serbia" = "republic of serbia"))

options(warn=-1)
data(country.map, package = "choroplethrMaps")
con_df<-data.frame("region"=unique(country.map$region))


country_choropleth(pop_vacc_dose1,num_colors=9)+
  labs(title = "Dose 1: Global Populate coverage", subtitle = "Vaccination doses administered per 100 people")+
  theme(legend.position = "bottom", legend.title = element_text(size = 15), legend.text=element_text(size=15),plot.title = element_text(size=25), plot.subtitle = element_text(size=15))+
  scale_fill_brewer(palette="YlOrRd")

country_choropleth(pop_vacc_dose2,num_colors=7)+
  labs(title = "Dose 1: Global Population coverage", subtitle = "Vaccination doses administered per 100 people")+
  theme(legend.position = "bottom", legend.title = element_text(size = 15), legend.text=element_text(size=15), plot.title = element_text(size=25), plot.subtitle = element_text(size=15))+
  scale_fill_brewer(palette="YlOrRd")


vacc_total<- df_copy %>%
  select ("country","total_vaccinations")%>%
  group_by (country) %>%
  filter (total_vaccinations == max(total_vaccinations))%>%
  distinct()%>%
  ungroup()%>%
  slice_max(total_vaccinations,n=6)


df_copy %>%
  select ("date","country","daily_vaccinations_per_million")%>%
  filter (country %in% pull (vacc_total, country))%>%
  group_by(country)%>%
  ggplot (mapping =aes(x=date, y=daily_vaccinations_per_million))+
  geom_line(aes(color=country),size =0.8)+
  ylab("Count per million")+
  ggtitle("Vaccination trends per million in Top 6 countries")+
  theme_bw()+
  theme(legend.position="top", legend.title = element_blank(), axis.title.x = element_blank(), plot.title = element_text(size=23), legend.text = element_text(size=15))
