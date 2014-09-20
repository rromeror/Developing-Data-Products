library(UsingR)

BW_eff <- function(s,t,h,m){
        Bw <- log(1 + m*(exp(s*t*h) - 1)/h)/(s*t)
        return(Bw)
}

BW_prima <- function(s,t,h,m){
        Bw_prima <- (exp(s*t*h) - 1)/(s*t*(h + m*(exp(s*t*h) - 1)))
        return(Bw_prima)
}

charge <- function(s = 0.1,t = 1,h = 0,m = 0,d = 1){
        mu <- 0.0000045
        #Calculate tariff due to duration of the connection in cost units per month [cu/month]
        tariff_duration_month <- mu*(BW_eff(s,t,h,m) - BW_prima(s,t,h,m)*m)*2628000
        #Calculate tariff due to volume of information transferred in cost units per MegaByte [cu/MByte]
        tariff_usage_MByte <- mu*BW_prima(s,t,h,m)*8
        # Calculate the volume of information transferred in one month [MBytes]
        Volume_info_MBytes <- m*2628000/8
        #------------------------- Charge Calculation----------------------------------------------------
        #Calculate charge component due to a duration of one month [cu]
        charge_duration_month <- tariff_duration_month
        #Calculate charge due to volume of information transferred in in one month [cu]
        charge_usage_MByte <- tariff_usage_MByte*Volume_info_MBytes 
        # Calculate the total charge
        charge <- charge_duration_month + charge_usage_MByte   
        bill <- data.frame("Charge" = round(charge,1),
                           "Duration" = round(charge_duration_month,1),
                           "Usage" = round(charge_usage_MByte,1),
                           "prop_fixed" <- round(charge_duration_month/charge,2),
                           "prop_usage" <- round(charge_usage_MByte/charge,2),
                           "Traffic" = round(Volume_info_MBytes,1)
        )  
        return(bill)
}

my_charge <- function(m = 0, h = 0){
        s <- 0.11; t <- 1; d <- 1
        x <- seq(0,h,h/100)     
        my_charge <- charge(s,t,h,x,d)   
        plot(x,my_charge$Charge,type = "ln",main = "Charge [cu/month]",ylab = "Charge [cu/month]",xlab = "Mean Internet connection speed [Mbps]")
        points(x,my_charge$Duration,type = "ln",col = "blue")
        points(x,my_charge$Usage,type = "ln", col = "red")
        lines(c(m,m),c(12,charge(s,t,h,h,d)$Charge),col = "brown", lwd = 5)
        legend("topright", legend = c("Total charge [cu/month]","Fixed charge [cu/month]","Usage charge [cu/month]"), col=c("black","blue","red"), lty=1)
        text(m + 2,charge(s,t,h,m,d)$Charge - 2,paste(charge(s,t,h,m,d)$Charge," [cu/month]"))
        text(m + 2,charge(s,t,h,m,d)$Duration - 2,paste(charge(s,t,h,m,d)$Duration," [cu/month]"),col = "blue")
        text(m + 2,charge(s,t,h,m,d)$Usage - 6,paste(charge(s,t,h,m,d)$Usage," [cu/month]"),col = "red")
        text(m + 1,0,paste(m," [Mbps]"))
        return(charge(s,t,h,m,d))
}

shinyServer(
        function(input, output) {
                
                output$my_charge <- renderPlot({
                        m <- input$Mean
                        h <- input$Internet_speed  
                        if(m < h ){
                                m <- input$Mean
                                h <- input$Internet_speed      
                                results <- my_charge(m,h)
                                results$Traffic <- results$Traffic/1000
                                results$Internet_speed <- h
                                results$mean <- m
                                results$use_factor <- 2628000*m/(3600*h)
                                names(results) <- c("Total [cu/month]","Fixed [cu/month]","Usage [cu/month]","Fixed [%]","Usage [%]","Volume [GBytes/month]","Max speed [Mbps]","Mean speed [Mbps/month]","Utilization [hours/month]")
                                output$results1 <- renderPrint({results[,1:5]})
                                output$results2 <- renderPrint({results[,6:9]})
                                
                        } else  {
                                
                                output$results1 <- renderPrint({"Mean bit rate cannot exceed maximum internet conection speed!"})
                                output$results2 <- renderPrint({"Mean bit rate cannot exceed maximum internet conection speed!"}) 
                        }
                        
                })                               
        }
)