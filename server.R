#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(rsconnect)
#Sys.setlocale(category = "LC_TIME", locale="en_GB.UTF-8")


dat <- read_csv2("data_ulule_2019_managed.csv")
months = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
years = c("2011","2012","2013","2014","2015","2016","2017","2018","2019")




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    #NUMBER OF CAMPAIGNS
    output$distPlot_gen_year <- renderPlot({
        if (input$choix_categorie != "All"){
            dat <- filter(dat, category == input$choix_categorie)
        }
        ggplot(data = dat, aes(x = as.character(year(date_start))) )+ geom_bar(fill="darkslategray") + ggtitle("Number of campaigns by year",subtitle=paste0("Category : ",input$choix_categorie)) + xlab("Year") + ylab("Number of campaigns") + theme_minimal()
        
    })
    
    output$distPlot_gen_month <- renderPlot({
        if (input$choix_categorie != "All"){
            dat <- filter(dat, category == input$choix_categorie)
        }
        ggplot(data = dat, aes(x = month(date_start, label=TRUE)) )+ geom_bar(fill="lightblue4") + ggtitle("Number of campaigns by month",subtitle=paste0("Category : ",input$choix_categorie)) + xlab("Month") + ylab("Number of campaigns") + theme_minimal()
        
    })
    
    output$distPlot <- renderPlot({
        year_nb = input$year
        if (input$choix_categorie != "All"){
            dat <- filter(dat, category == input$choix_categorie)
        }
        ggplot(data = dat %>% filter(year(date_start)==year_nb), aes(x = month(date_start, label= TRUE))  )+ geom_bar(aes(fill = factor(..x..))) + ggtitle("Number of campaigns by month for a certain year",subtitle=paste0("Year: ",year_nb, "     Category : ",input$choix_categorie)) + xlab("Month of debut") + ylab("Number of campaigns") + scale_fill_discrete(name = "Month of debut", labels= months) + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) + theme_minimal()
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
    })
    

    #PERCENTAGE OF COMPLETED CAMPAIGNS
    output$distPlotPercent <- renderPlot({
        year_percent = input$year_percent
        if (input$choix_categorie_perc != "All"){
            dat <- filter(dat, category == input$choix_categorie_perc)
        }
        
        data5 <- dat %>% 
            filter(year(date_start) == year_percent) %>% 
            group_by(month(date_start,label = TRUE)) %>%
            filter(goal_raised==TRUE) %>%
            summarise(financees1=n())
        
        data6 <- dat %>% 
            filter(year(date_start) == year_percent) %>% 
            group_by(month(date_start,label=TRUE)) %>%
            summarise(nb_camp=n()) 
        
        names(data5)[1] <- "date.start"
        names(data6)[1] <- "date.start"
        
        data7 <- left_join(data6,data5,by="date.start")
        data7[is.na(data7)] <- 0
        
        data7$financees <-(data7$financees1/data7$nb_camp)*100
        
        ggplot(data = data7, aes(date.start,financees) )+ geom_bar(fill="lemonchiffon3",stat="identity") + ggtitle("Percentage of completed campaigns by month of debut for a certain year",subtitle=paste0("Year: ",year_percent, "     Category : ",input$choix_categorie_perc)) + xlab("Month") + ylab("Percentage of completed campaigns") + theme_minimal()#+xlim(2011,2019)  #+ scale_x_continuous(breaks=seq(1,12),labels = months)
        #ggplot(data = B, aes(x = colmois, y = prop_g)) + geom_bar(aes(fill = factor(..x..)),stat="identity") + ggtitle("Percentage of completed campaigns by month of debut for a certain year",subtitle=paste0("Year: ",year_percent, "     Category : ",input$choix_categorie_perc)) + xlab("Month of debut")+ylab("Percentage of completed campaigns") + scale_fill_discrete(name = "Month of debut", labels= months) + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme_minimal()+scale_x_continuous(breaks=seq(1,12),labels = months)+ geom_text(aes(label=prop_g), vjust=1.6, color="white", position = position_dodge(0.9),size=4)
    })
    
    output$distPlotPercent_Year <- renderPlot({
        if (input$choix_categorie_perc != "All"){
            dat <- filter(dat, category == input$choix_categorie_perc)
        }
        
        data5 <- dat %>% 
            group_by(year(date_start)) %>%
            filter(goal_raised==TRUE) %>%
            summarise(financees1=n())
        
        data6 <- dat %>% 
            group_by(year(date_start)) %>%
            summarise(nb_camp=n()) 
        
        names(data5)[1] <- "date.start"
        names(data6)[1] <- "date.start"
        
        data7 <- left_join(data6,data5,by="date.start")
        data7[is.na(data7)] <- 0
        
        data7$financees <-(data7$financees1/data7$nb_camp)*100
        
        ggplot(data = data7, aes(as.character(date.start),financees) )+ geom_bar(fill="darkslategray",stat="identity") + ggtitle("Percentage of completed campaigns by year",subtitle=paste0("Category : ",input$choix_categorie_perc)) + xlab("Year") + ylab("Percentage of completed campaigns") + theme_minimal()# + scale_x_continuous(breaks=seq(1,9),labels = years) + theme_minimal()
    })
    
    output$distPlotPercent_Month <- renderPlot({
        if (input$choix_categorie_perc != "All"){
            dat <- filter(dat, category == input$choix_categorie_perc)
        }
        
        data5 <- dat %>% 
            group_by(month(date_start,label = TRUE)) %>%
            filter(goal_raised==TRUE) %>%
            summarise(financees1=n())
        
        data6 <- dat %>% 
            group_by(month(date_start,label = TRUE)) %>%
            summarise(nb_camp=n()) 
        
        names(data5)[1] <- "date.start"
        names(data6)[1] <- "date.start"
        
        data7 <- left_join(data6,data5,by="date.start")
        data7[is.na(data7)] <- 0
        
        data7$financees <-(data7$financees1/data7$nb_camp)*100
        
        ggplot(data = data7, aes(date.start,financees) )+ geom_bar(fill="lightblue4",stat="identity") + ggtitle("Percentage of completed campaigns by month",subtitle=paste0("Category : ",input$choix_categorie_perc)) + xlab("Month") + ylab("Percentage of completed campaigns") + theme_minimal() #+ scale_x_continuous(breaks=seq(1,12),labels = months)
    })
    
    
    ### AVERAGE AMOUNT
    output$distPlotAvg <- renderPlot({
        year_avg = input$year_avg
        dat <- filter(dat, year(date_start) == input$year_avg)
        if (input$choix_categorie_avg != "All"){
            dat <- filter(dat, category == input$choix_categorie_avg)
        }
        data5 <- dat %>% 
            group_by(month(date_start,label = TRUE)) %>%
            filter(goal_raised==TRUE) %>%
            summarise(moy=mean(amount_raised_eur))
        

        names(data5)[1] <- "date.start"
        
        ggplot(data = data5, aes(date.start,moy) )+ ggtitle("Average amount raised by completed campaigns by month for a year of interest",subtitle=paste0("Year: ",input$year_avg, "     Category : ",input$choix_categorie_avg)) + theme_minimal()+geom_bar(fill="lemonchiffon3",stat = "identity") + xlab("Month") + ylab("Average amount raised (in EUR)")# + scale_x_continuous(breaks = seq(1,12)) #scale_x_continuous(breaks=seq(1,12),labels = months)
    })
    
    output$distPlotAvg_Year <- renderPlot({
        if (input$choix_categorie_avg != "All"){
            dat <- filter(dat, category == input$choix_categorie_avg)
        }
        data5 <- dat %>% 
            group_by(year(date_start)) %>%
            filter(goal_raised==TRUE) %>%
            summarise(moy=mean(amount_raised_eur))

        names(data5)[1] <- "date.start"
        
        ggplot(data = data5, aes(as.character(date.start),moy) )+ ggtitle("Average amount raised by completed campaigns by year",subtitle=paste0("Category : ",input$choix_categorie_avg))+geom_bar(fill="darkslategray",stat = "identity") + xlab("Year") + ylab("Average amount raised (in EUR)")  + theme_minimal()
    })

    output$distPlotAvg_Month <- renderPlot({
        if (input$choix_categorie_avg != "All"){
            dat <- filter(dat, category == input$choix_categorie_avg)
        }
        data5 <- dat %>% 
            group_by(month(date_start,label = TRUE)) %>%
            filter(goal_raised==TRUE) %>%
            summarise(moy=mean(amount_raised_eur))
        
        names(data5)[1] <- "date.start"
        
        ggplot(data = data5, aes(date.start,moy) )+ ggtitle("Average amount raised by completed campaigns by month",subtitle=paste0("Category : ",input$choix_categorie_avg))+geom_bar(fill="lightblue4",stat = "identity") + xlab("Month") + ylab("Average amount raised (in EUR)")  + theme_minimal()
    })
    
})
