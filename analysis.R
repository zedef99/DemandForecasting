#load the libraries
library(fpp3)
library(scales)
library(stringr)
library(plotly)
library(urca)
library(xtable)
library(fable.prophet)

#import the .txt
df<-read.table("PixartPrintingCube [STG] Daily Ordered (V0167).txt", header = TRUE, sep = "\t", fileEncoding = "UTF-8", dec = ",")
df1<-read.table("PixartPrintingCube [STG] Daily Ordered (V0167)_FY2023.txt", header = TRUE, sep = "\t", fileEncoding = "UTF-8", dec=",")
all<-df
fy23<-df1

#create a column Date in the correct format
all$Date <- ymd(all$Day)
fy23$Date <- ymd(fy23$Day)

#delete the col Day
all<-all%>%select(-Day)
fy23<-fy23%>%select(-Day)

#create a column only for now (need to adjust the extraction)
fy23$Product.Group.Refact<-NA

#keep only the final month for the fy23
fy23<-fy23%>%
  filter(Date>"2023-05-29" & Date<="2023-06-30")

#reorder the fy23
fy23<-fy23%>%select("Reference.Scenario", "Country.LV1", "Product.Family","Product.Group.Refact","IC.Partner","Value.Stream", "X.STG..Daily.Ordered..V0167.","Date")                        

#merge the df
ordered<-rbind(all, fy23)

#rename the columns
ordered<- ordered %>% 
  rename("Scenario" = "Reference.Scenario", "Country" = "Country.LV1", "Prod_Family" =  "Product.Family","Single_Prod" ="Product.Group.Refact", "Partner"="IC.Partner", "Value_Stream"= "Value.Stream", "QtyEur"= "X.STG..Daily.Ordered..V0167.")

mapping=c("Brochure & Magazine","Flyer","Labels & Stickers & Tags","Rigid Material","Banner & Fabrics","Display","Business Cards",
          "Decals","Standard packaging","Corrugated packaging","Flexible packaging","Office","Posters","Photo Products","Shopper","Agenda & Calendar",
          "Other","Design Services","PPAG","Masks")

ordered$Product_Family <- factor(ordered$Prod_Family, levels = c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200), labels = mapping)

#transform into categorical 
cat_var<-c("Country", "Partner")

for (col in cat_var){
  ordered[col] <- lapply(ordered[col], factor)
}

#keep only act rows
ordered_act<-ordered%>%filter(Scenario=="ACT")

#delete useless cols
ordered_act<-ordered_act%>%select(-Scenario, -Value_Stream, -Prod_Family)

#reorder the df
ordered_act<-ordered_act%>%select(Date, Country, Product_Family, Single_Prod, Partner, QtyEur)

#Explorative Analysis

#keep data within the time period 2018-07-01 to 2023-05-29
ordered_act<-ordered_act%>%filter(Date>="2018-07-01")

#part cross-business 74 millions
ord_act_web<-ordered_act%>%filter(Partner=="Third Parties")

#delete the col Partner
ord_act_web<-ord_act_web%>%select(-Partner)

#brief summary of the data
str(ordered_act)
dim(ordered_act) 
summary(ordered_act)
sum(is.na(ordered_act)) 

#check the number of levels for country and prod family 
unique(ordered_act$Product_Family)
unique(ordered_act$Country)

#ts to plot the entire period (2018-07-01 to 2023-06-30)
ts<-ord_act_web%>%
  group_by(Date)%>%
  summarise(Qty_Eur=sum(QtyEur))%>%
  as_tsibble(index = Date)

ts%>%
  autoplot(Qty_Eur/1000)+
  scale_x_date(labels = scales::date_format("%d/%m/%Y"))+
  labs(title="Ordered by year", y= "K€")

ggsave("ordered_per_year.jpeg", width = 10, height = 6, dpi = 300)

#ordered by product family in the time frame
ord_act_web%>%
  group_by(Product_Family)%>%
  summarise(tot_eur=round(sum(QtyEur)/1000))%>%
  arrange(desc(tot_eur))

#tot eur ordered from web -> 764938
ord_act_web%>%summarise(tot=round(sum(QtyEur)/1000))

#Brochure & Magazine, Flyer, Labels & Stickers & Tags and Rigid Material represent the 61.08% of the total euro ordered in the web business
(170505+150501+86333+71343)/783703

#plot web ordered by product family
web_ordered_by_prodfam<-ord_act_web%>%
  group_by(Product_Family)%>%
  summarise(tot_eur=round(sum(QtyEur)/1000))%>%
  arrange(desc(tot_eur))

ggplot(web_ordered_by_prodfam, aes(x=reorder(Product_Family, tot_eur), y=tot_eur, fill= Product_Family))+
  geom_col(show.legend = FALSE)+
  geom_text(aes(label=scales::comma(tot_eur)), stat = "identity", size=4, hjust = -0.1)+
  labs(title = "Ordered by Product Family from 2018 to 2023", x="Prod Family", y="K€")+
  coord_flip()+
  scale_y_continuous(labels = scales::comma)+
  theme_minimal()

ggsave("ord_byprodfam.jpeg", width=40, height = 10, units = "cm")

#plot web ordered by country during the entire time frame
web_ordered_by_country<-ord_act_web%>%
  group_by(Country)%>%
  summarise(tot_eur=round(sum(QtyEur)/1000))%>%
  arrange(desc(tot_eur))

ggplot(web_ordered_by_country, aes(x=reorder(Country, -tot_eur), y=tot_eur, fill= Country))+
  geom_col(show.legend = FALSE)+
  geom_text(aes(label=scales::comma(tot_eur)), stat = "identity", size=4, vjust = -0.5)+
  labs(title = "Ordered by Country from 2018 to 2023", x="Country", y="K€")+
  scale_fill_brewer(palette = "Set3")+
  scale_y_continuous(labels = scales::comma)+
  theme_minimal()

ggsave("ord_bycountry.jpeg", width=20, height = 15, units = "cm")

(414576+179421+81857)/783703 #86.24% of the ordered divided between IT, FR e ES

#create a key for web customer
ord_act_web$cf<-paste(ord_act_web$Country,ord_act_web$Product_Family, sep="_")

#Time-series for Web customer
web_ord_ts<-ord_act_web%>%
  group_by(Date, cf)%>%
  summarise(tot_eur=sum(QtyEur))

web_ord_ts[c('Country', 'Product_Family')] <- str_split_fixed(web_ord_ts$cf,"_", 2)

web_ord_ts$Month <- month.name[month(web_ord_ts$Date)]
web_ord_ts$Year <- year(web_ord_ts$Date)
web_ord_ts$Day <- wday(web_ord_ts$Date, label = TRUE)

web_ord_ts<-web_ord_ts%>%
  select(Date, Year, Month, Day, Country, Product_Family, cf, tot_eur)%>%
  as_tsibble(index = Date, key=cf)

#generic plots
web_ord_ts%>%
  filter(Product_Family=="Flyer", Country=="IT", Date>="2022-07-01")%>%
  autoplot(tot_eur/1000)+
  labs(title= "Flyer in IT in FY23", y= "K€")+
  scale_x_date(labels = scales::date_format("%d/%m/%Y"))

web_ord_ts%>%
  filter(Product_Family=="Brochure & Magazine", Country=="IT", Date>="2022-07-01")%>%
  autoplot(tot_eur/1000)+
  labs(title= "Brochure & Magazine in IT in FY23", y= "K€")+
  scale_x_date(labels = scales::date_format("%d/%m/%Y"))

#plot the fy 21-22-23
ts%>%
  filter(Date>="2020-07-01")%>%
  autoplot(Qty_Eur)+
  scale_x_date(labels = scales::date_format("%d/%m/%Y"))+
  labs(title="Ordered by year", y= "K€")

ggsave("ordered_212223.jpeg")

#only for the country and product of interest
countries <- c("IT", "ES", "FR")
product_families <- c("Brochure & Magazine", "Flyer", "Labels & Stickers & Tags", "Rigid Material")

wip_tib<-ord_act_web%>%
  filter(Product_Family%in%product_families, Country%in%countries, Date>="2020-07-01")%>%
  group_by(Date)%>%
  summarise(qty=sum(QtyEur)/1000)

#create the column fiscal year 
wip_tib$FiscalYear <- ifelse(month(wip_tib$Date) >= 7, year(wip_tib$Date) + 1 , year(wip_tib$Date))

wip_tib$Month <- month.name[month(wip_tib$Date)]

wip_tib<-wip_tib%>%mutate(mylabel=case_when(day(Date) == 1L ~ format(Date, "%b %d"),TRUE ~ NA_character_))

#converting into factor
cat_var<-c("FiscalYear", "Month")

for (col in cat_var){
  wip_tib[col] <- lapply(wip_tib[col], factor)
}

#filter to keep fy 21-22-23
wip_fy<-wip_tib%>%filter(Date>="2020-07-01")

#create and index to keep the right order
wip_fy <- wip_fy %>% 
  group_by(FiscalYear) %>% 
  mutate(order = row_number()) %>% 
  ungroup()

#replace `order` label with `myLabel` created above
x_break <- wip_fy$order[!is.na(wip_fy$mylabel)][1:12]
x_label <- wip_fy$mylabel[x_break]
x_label

plot <- ggplot(wip_fy, aes(x = order, y = qty, color = FiscalYear, group = FiscalYear)) +
  geom_line() +
  theme_minimal(base_size = 16) +  
  scale_x_continuous(
    breaks = x_break,
    labels = x_label) +
  scale_color_brewer("Fiscal Year", palette = "Dark2") +
  labs(title = "Ordered by FY", x = NULL, y = "K€")+
  theme(
    legend.position = "bottom",    
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12),   
    plot.margin = unit(c(1, 1, 3, 1), "lines")  
  )

print(plot)

# save the plot
ggsave("ordered_by_FY.jpeg", plot, width = 10, height = 8, dpi = 300)

#2° part of the Explorative Analysis
# Define a function to automate the process to plot a gg_season()
generate_plot <- function(country, product_family) {
  filtered_data <- web_ord_ts %>%
    filter(Date >= "2020-07-01", Country == country, Product_Family == product_family)
  
  filtered_data$FiscalYear <- ifelse(month(filtered_data$Date) >= 7, year(filtered_data$Date) + 1, year(filtered_data$Date))
  
  filtered_data <- filtered_data %>%
    mutate(mylabel = case_when(day(Date) == 1L ~ format(Date, "%b %d"), TRUE ~ NA_character_)) %>%
    group_by(FiscalYear) %>%
    mutate(order = row_number()) %>%
    ungroup()
  
  cat_var <- c("FiscalYear", "Month")
  for (col in cat_var) {
    filtered_data[col] <- lapply(filtered_data[col], factor)
  }
  
  x_break <- filtered_data$order[!is.na(filtered_data$mylabel)][1:12]
  x_label <- filtered_data$mylabel[x_break]
  
  plot <- ggplot(filtered_data, aes(x = order, y = tot_eur/1000, color = FiscalYear, group = FiscalYear)) +
    geom_line() +
    theme_minimal(base_size = 16) +  
    scale_x_continuous(
      breaks = x_break,
      labels = x_label) +
    scale_color_brewer("Fiscal Year", palette = "Dark2") +
    labs(title = paste("Ordered by FY for", product_family, "in", country), x = NULL, y = "K€")+
    theme(
      legend.position = "bottom",    
      legend.title = element_text(size = 14),  
      legend.text = element_text(size = 12),   
      plot.margin = unit(c(1, 1, 3, 1), "lines")
    )
}

# Call the function for different combinations of country and product family
countries <- c("IT", "ES", "FR")
product_families <- c("Brochure & Magazine", "Flyer", "Labels & Stickers & Tags", "Rigid Material")

for (country in countries) {
  for (family in product_families) {
    p <- generate_plot(country, family)
    print(p)
    filename <- paste(country, family, ".jpeg", sep = "_")
    ggsave(filename, p, width = 10, height = 8, dpi = 300)
  }
}


#STL decomposition per IT FLYER
try<-ord_act_web%>%
  group_by(Date, cf)%>%
  summarise(QtyEur=sum(QtyEur))

try[c('Country', 'Product_Family')] <- str_split_fixed(try$cf,"_", 2)

try$Month <- month.name[month(try$Date)]
try$Year <- year(try$Date)
try$Day <- wday(try$Date, label = TRUE)

try<-try%>%
  select(Date, Year, Month, Day, Country, Product_Family, cf, QtyEur)%>%
  as_tsibble(index = Date, key=cf)

#select only act 21-22-23
act212223<-try%>%filter(Date>="2020-07-01", Country %in% c("IT", "FR", "ES"))

#univariate analysis before proceeding
summary(act212223$QtyEur)#min, max, median and mean
IQR(act212223$QtyEur) #47742.81
sd(act212223$QtyEur) #13502.23

ggplot()+
  geom_boxplot(data=act212223, aes(y=QtyEur/1000, fill=Country))+
  facet_wrap("Country")+
  labs(title = "Boxplot for the QtyEur variable", y="QtyEur in K€")+
  theme(legend.position = "none")

ggplot()+
  geom_histogram(data=act212223, aes(x=QtyEur/1000, fill=Country))+
  facet_wrap("Country")+
  labs(title = "Histogram for the QtyEur variable", x="QtyEur in K€")+
  theme(legend.position = "none")

ggplot()+
  geom_density(data=act212223, aes(x=QtyEur/1000, fill=Country))+
  facet_wrap("Country")+
  labs(title = "Density plot for the QtyEur variable", x="QtyEur in K€")+
  theme(legend.position = "none")

#with log trasnformation
ggplot() +
  geom_boxplot(data = act212223, aes(y = log(QtyEur), fill = Country)) +
  facet_wrap("Country") +
  labs(title = "Boxplot for the QtyEur variable", y = "QtyEur") +
  theme(legend.position = "none")

ggplot()+
  geom_histogram(data=act212223, aes(x=log(QtyEur), fill=Country))+
  facet_wrap("Country")+
  labs(title = "Histogram for the QtyEur variable", x="QtyEur")+
  theme(legend.position = "none")

ggplot()+
  geom_density(data=act212223, aes(x=log(QtyEur), fill=Country))+
  facet_wrap("Country")+
  labs(title = "Density plot for the QtyEur variable", x="QtyEur")+
  theme(legend.position = "none")

#mathematical adjustments-> with log 
#log dcmp
for (country in countries) {
  for (product_family in product_families) {
    df_filtered <- try %>%
      filter(Date >= "2020-07-01" & Date <= "2023-06-30", Product_Family == product_family, Country == country) %>%
      select(-cf)
    
    plot_title <- latex2exp::TeX(paste0("Transformed QtyEur Ordered for ", country," ", product_family))
    
    print(df_filtered %>%
            model(STL(log(QtyEur)~ season(window = "periodic"), robust = TRUE)) %>%
            components() %>%
            autoplot())+
      labs(title = plot_title, y = "K€")
    
    filename <- paste("dcmp",country, product_family, ".jpeg", sep = "_")
    ggsave(filename, width = 10, height = 8, dpi = 300)
  }
}

#MODELS IMPLEMENTATION
# Define a function to perform the analysis for a given combination
analyze_combination <- function(country, product_category) {
  cat("Analyzing combination:", paste(country, product_category, sep = " - "), "\n")
  
  # Filter the data for the current combination
  filtered_data <- try %>% 
    filter(Country == country, Product_Family == product_category) %>%
    select(-cf)
  
  # Split the data into train and test sets
  train <- filtered_data %>%
    filter(Date >= "2020-07-01" & Date <= "2023-05-31")
  
  test <- filtered_data %>%
    filter(Date > "2023-05-31")
  
  #MODELS
  
  set.seed(42)
  
  #fit models 
  fit <- train%>%
    model(
      autoarima = ARIMA(log(QtyEur)),
      prop=prophet(log(QtyEur)~season(period = "day") + season(period = "week") + season(period = "year")),
      nn=NNETAR(log(QtyEur), scale_inputs = TRUE)
    ) 
  
  
  #forecast
  fc<-fit%>%forecast(h="1 month")
  
  #model specification
  spec<-fit%>%pivot_longer(everything(), names_to = "Model name",
                           values_to = "Orders")
  
  #test evitabili
  res<-fit%>% select(autoarima)%>% gg_tsresiduals()
  
  print(augment(fit) |>
          filter(.model == "autoarima") |>
          features(.innov, ljung_box, dof=5, lag=14))
  
  plot_title <- latex2exp::TeX(paste0(country," ", product_category))
  
  #plot the forecast
  plot1<-fc%>%autoplot(train%>%filter(Date>"2023-01-01"), level= NULL)+
    geom_line(data=test,mapping=aes(y=QtyEur, x=Date), col="purple")+
    labs(title = plot_title)
  
  plot2<-fc%>%autoplot(train%>%filter(Date>"2023-05-01"), level=95)+
    geom_line(data=test,mapping=aes(y=QtyEur, x=Date), col="purple")+
    facet_wrap(vars(.model))+
    labs(title = plot_title)+
    theme(legend.position = "none")
  
  plot_list<-list(plot1, plot2)
  
  #accuracy
  accuracy_matrix<-fc%>%fabletools::accuracy(test)%>%select(.model,RMSE, MAPE)
  
  #compute the coverage probability for the seasonal ARIMA
  int <- fc%>%filter(.model=="autoarima") %>% pull(QtyEur) %>% hilo() 
  within_interval<-test$QtyEur > int$lower & test$QtyEur < int$upper
  
  # determine the probability coverage
  probability_coverage_arima <- round(mean(within_interval), digits = 2)*100
  
  #RAE arima
  rae_arima<-round(rae(test$QtyEur, fc%>%filter(.model=="autoarima")%>%pull(.mean)), digits = 2) 
  
  #compute the coverage probability for prophet
  int <- fc%>% filter(.model=="prop")%>%pull(QtyEur) %>% hilo() 
  within_interval<-test$QtyEur > int$lower & test$QtyEur < int$upper
  
  probability_coverage_prophet <- round(mean(within_interval), digits = 2)*100 
  
  #RAE Prophet
  rae_prophet<-round(rae(test$QtyEur, fc%>%filter(.model=="prop")%>%pull(.mean)), digits = 2)  
  
  #probability coverage nnetar
  int <- fc %>%filter(.model=="nn")%>% pull(QtyEur) %>% hilo() 
  within_interval<-(test$QtyEur > int$lower) & (test$QtyEur < int$upper)
  
  # determine the probability coverage
  probability_coverage_nnetar <- round(mean(within_interval), digits = 2)*100 
  
  rae_nnetar<-round(rae(test$QtyEur, fc%>%filter(.model=="nn")%>%pull(.mean)), digits = 2)  
  
  #accuracy mtrix updated
  accuracy_matrix<-accuracy_matrix%>%mutate(RAE=c(rae_arima, rae_nnetar, rae_prophet), CP=c(probability_coverage_arima, probability_coverage_nnetar, probability_coverage_prophet))
  
  #save the results
  results <- list(
    train=train,
    test=test,
    specification=spec,
    residd=res,
    fc = fc,
    plot1=plot1,
    plot2=plot2,
    accuracy_matrix = accuracy_matrix
  )
  
  return(results)
}

# Define the combinations of countries and product categories
countries <- c("FR", "IT", "ES")
product_categories <- c("Brochure & Magazine", "Flyer")

# Perform the analysis for each combination
results_list <- list()
for (country in countries) {
  for (product_category in product_categories) {
    results <- analyze_combination(country, product_category)
    results_list[[paste(country, product_category, sep = " - ")]] <- results
  }
}

#accuracy matrix
knitr::kable(results_list[["IT - Flyer"]]$accuracy_matrix, format = "latex")
knitr::kable(results_list[["FR - Flyer"]]$accuracy_matrix, format = "latex")
knitr::kable(results_list[["ES - Flyer"]]$accuracy_matrix, format = "latex")
knitr::kable(results_list[["IT - Brochure & Magazine"]]$accuracy_matrix, format = "latex")
knitr::kable(results_list[["FR - Brochure & Magazine"]]$accuracy_matrix, format = "latex")
knitr::kable(results_list[["ES - Brochure & Magazine"]]$accuracy_matrix, format = "latex")

#forecast plot
results_list[["IT - Flyer"]]$plot1+theme_minimal()+theme(legend.position = "bottom")
filename="IT Flyer forecast.jpeg"
ggsave(filename, height = 6, width=10, dpi = 300)
results_list[["FR - Flyer"]]$plot1+theme_minimal()+theme(legend.position = "bottom")
filename="FR Flyer forecast.jpeg"
ggsave(filename, height = 6, width=10, dpi = 300)
results_list[["ES - Flyer"]]$plot1+theme_minimal()+theme(legend.position = "bottom")
filename="ES Flyer forecast.jpeg"
ggsave(filename, height = 6, width=10, dpi = 300)
results_list[["IT - Brochure & Magazine"]]$plot1+theme_minimal()+theme(legend.position = "bottom")+ylim(0,150000)
filename="IT Brochure & Magazine forecast.jpeg"
ggsave(filename, height = 6, width=10, dpi = 300)
results_list[["FR - Brochure & Magazine"]]$plot1+theme_minimal()+theme(legend.position = "bottom")
filename="FR Brochure & Magazine forecast.jpeg"
ggsave(filename, height = 6, width=10, dpi = 300)
results_list[["ES - Brochure & Magazine"]]$plot1+theme_minimal()+theme(legend.position = "bottom")
filename="ES Brochure & Magazine forecast.jpeg"
ggsave(filename, height = 6, width=10, dpi = 300)

#plot the data splitted for one combination as example
ggplot() +
  geom_line(data = results_list[["IT - Flyer"]]$train, aes(x = Date, y = QtyEur, color = "Train")) +
  geom_line(data = results_list[["IT - Flyer"]]$test, aes(x = Date, y = QtyEur, color = "Test")) +
  labs(x = "Date", y = "€", color = "Serie") +
  scale_color_manual(values = c("Train" = "blue", "Test" = "red")) +
  theme_minimal()+
  labs(title = "Split for the IT - Flyer combination")+
  theme(legend.position = "bottom")

ggsave("split.jpeg", dpi=300, width = 15, height = 5)

#model specification
results_list[["IT - Flyer"]]$specification
results_list[["FR - Flyer"]]$specification
results_list[["ES - Flyer"]]$specification
results_list[["IT - Brochure & Magazine"]]$specification
results_list[["FR - Brochure & Magazine"]]$specification
results_list[["ES - Brochure & Magazine"]]$specification

#plot per ciascuna combinazione di forecast + modello ma con prediction interval
results_list[["IT - Flyer"]]$plot2
ggsave("IT Flyer confint.jpeg", dpi=300, width = 20, height =10)
results_list[["FR - Flyer"]]$plot2
ggsave("FR Flyer confint.jpeg", dpi=300, width = 20, height =10)
results_list[["ES - Flyer"]]$plot2
ggsave("ES Flyer confint.jpeg", dpi=300, width = 20, height =10)
results_list[["IT - Brochure & Magazine"]]$plot2
ggsave("IT BM confint.jpeg", dpi=300, width = 20, height =10)
results_list[["FR - Brochure & Magazine"]]$plot2
ggsave("FR BM confint.jpeg", dpi=300, width = 20, height =10)
results_list[["ES - Brochure & Magazine"]]$plot2
ggsave("ES BM confint.jpeg", dpi=300, width = 20, height =10)



