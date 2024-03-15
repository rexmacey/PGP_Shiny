#' Create a portfolio names
#' 
#' @param stock_wt Weights of stocks in portfolio decimal.
#'
#' @return character string "PortxSyB" where x is stock weight and y is bond weight
#'
#' @examples get_port_name(0.1)
get_port_name <- function(stock_wt){
    paste0("Port", round(stock_wt*100,0), "S", round(100-stock_wt*100,0), "B")
}

get_pgp_name <- function(pgp_wt){
    paste0("PGP", round(pgp_wt*100,0), "_Bond", round(100-pgp_wt*100,0))
}
add_port_returns_and_prices <- function(pgp){
    stock_wts <- pgp$params$stockwts
    data <- pgp$data
    for(i in stock_wts){
        data[,get_port_name(i)] <- i * data$RiskyRet + (1-i) * data$Bonds
    }
    # Create Price series (growth of 1) for all columns except date
    for(i in colnames(data)[!(colnames(data) %in% "Date")]){
        data[,paste0(i,"Px")] <- cumprod(1+data[,i])
    }
    data<-rbind(data[1,],data) # add a row at top; contents will be replaced
    data[1,"Date"] <- format(as.Date(ISOdate(year(data$Date[1]),month(data$Date[1]),1))-1, "%Y-%m-%d")
    data[1,2:(ncol(data)/2+1)] <- NA
    data[1,(ncol(data)/2+2):ncol(data)] <- 1
    pgp$data <- data
    return(pgp)
}

add_pgp_returns <- function(pgp){
    pgp_wts <- pgp$params$pgpwts
    data <- pgp$data
    for(i in pgp_wts){
        data[,get_pgp_name(i)] <- i * data$PGP.net + (1-i) * data$Bonds
    }
    pgp$data<-data
    return(pgp)
}

#' Set Parameters of PGP object
#'
#'
#' @return pgp object with list named params. Should contain at least width and commission
#' @export
#'
#' @examples set_params(pgp,11,0.1)
set_params<-function(pgp, ...){
    params <- list(...)
    if(!("width" %in% names(params))){
        params$width <- 12
        warning("width parameter missing. Setting to 12.")
    } 
    if(!("commission" %in% names(params))) {
        params$commission <- 0
        warning("commission parameter missing. Setting to 0.")
    }
    if(!("stockwts" %in% names(params))) {
        params$portwts <- seq(0,1,.01)
        warning("stockwts parameter missing. Setting to 0% to 100% by 10%.")
    }
    if(!("pgpwts" %in% names(params))) {
        params$pgpwts <- c(1, 0.8, 0.6, 0.5, 0.4)
        warning("pgpwts parameter missing. Setting to c(1, 0.8, 0.6, 0.5, 0.4).")
    }
    if(!("frequency" %in% names(params))) {
        params$frequency <- "monthly"
        warning("frequency parameter missing. Setting to monthly.")
    }
    if(!("rolling" %in% names(params))) {
        params$rolling <- c(12, 36, 60, 120)
        warning("rolling parameter missing. Setting to c(12, 36, 60, 120)")
    }
    pgp$params <- params
    # pgp$width<-width
    # pgp$commission<-commission
    return(pgp)
}

#' pgp_backtest
#' 
#' Runs a backtest on the data.
#'
#' @param pgp A pgp object which contains:
#'  data: a tibble with the monthly returns for raw data and portfolios, and PX values 
#'  frequency: monthly or daily
#'  width: 12 window of pgp
#'  commission for pgp trades e.g. 0.1 for 0.1%
#'  returns: monthly returns for pgp has lag
#'  legs: used for tax analysis
#'  g1d grows of $1, same periods as returns
#'  risky_dates for shading charts
#'
#' @return pgp updated pgp with results
#' @export
#'
#' @examples pgp_backtest(pgp)
pgp_backtest<-function(pgp){
    library(lubridate)
    library(xts)
    pgp<-add_port_returns_and_prices(pgp)
    pgp<-avg_px(pgp)
    pgp<-signal(pgp)
    pgp<-period_returns(pgp)
    pgp<-add_pgp_returns(pgp)
    # returns<-pgp$data[,c("Date","RiskyRet","Cash","PGP.gross","PGP.net")]
    substrRight <- function(x, n){
        substr(x, nchar(x)-n+1, nchar(x))
    }
    returns<-pgp$data[, c("Date", "RiskyRet", "Cash", "PGP.gross", "PGP.net", 
                          names(pgp$data)[substr(names(pgp$data),1,4) %in% "Port" & substrRight(names(pgp$data),2) != "Px"],
                          names(pgp$data)[substr(names(pgp$data),1,3) %in% "PGP"])]
    returns<-returns[complete.cases(returns),]
    #returns<-xts(data.frame(returns),order.by = returns$Date)
    pgp$returns<-returns
    pgp$legs<-analyze_legs(pgp)
    pgp$g1d <- pgp_g1d(pgp) 
    pgp$risky_dates <- get_risky_dates(pgp)
    pgp$rolling_tables <- create_rolling_returns_tables(pgp)
    return(pgp)
}

#' Average Price - adds a moving average of the prices
#'
#' @param pgp A PGP class object with at least a data frame with the RiskyPx column
#'
#' @return pgp object
#'
#' @examples avg_px(pgp)
avg_px<-function(pgp){
    pgp$data$AvgPx<-rollmean(pgp$data$RiskyRetPx,k=pgp$params$width,fill=NA,align="right")
    return(pgp)
}

#' Signal - Adds signal to pgp object.
#' 
#' The signal is true (hold risky) when RiskyPx >= AvgPx, otherwise false (hold
#' safe).  The signal is calculated at the end of the period and should be
#' applied to the following month.
#' 
#' @param pgp
#'   
#' @return pgp clas with signal column
#' 
#' @examples signal(pgp)
signal<-function(pgp){
    pgp$data$Signal<-pgp$data$RiskyRetPx>=pgp$data$AvgPx
    return(pgp)
}

#' Period Returns - generate periodic returns (monthly, daily depending on frequency of data)
#'
#' Calculates both gross and net returns using the commission rate.
#'
#' @param pgp pgp object
#'
#' @return pgp object
#'
#' @examples period_returns(pgp)
period_returns<-function(pgp){
    pgp$data$PGP.gross<-NA
    pgp$data$PGP.net<-NA
    signal.lag<-c(NA,pgp$data$Signal[1:(nrow(pgp$data)-1)])
    temp<-signal.lag*pgp$data$RiskyRet + (1-signal.lag)*pgp$data$Cash
    pgp$data$PGP.gross<-temp
    signal.lag2<-c(NA,NA,pgp$data$Signal[1:(nrow(pgp$data)-2)])
    pgp$data$Trade<-signal.lag!=signal.lag2
    pgp$data$Trade[pgp$params$width+1]<-FALSE
    pgp$data$PGP.net<-pgp$data$PGP.gross - pgp$data$Trade*pgp$params$commission/100
    return(pgp)
}


#' Analyze Legs
#' 
#' Useful for analyzing taxes and the degree of turnover.  A leg is a period
#' over which a position in either the risky or safe asset is held, that is from
#' the time it is bought until it is sold.
#' 
#' @param pgp  A pgp object
#'   
#' @return list A list with a detail of each leg including the start and end
#'   dates, the number of periods, the type (Risky or Safe) and the gross and
#'   net returns.  The Summary is a data frame with rows for the long term gains
#'   and losses, short-term gains and losses (for the Risky asset) and a row for
#'   the safe investments.  The summary for each row includes the number of
#'   legs, how many periods in total were invested in those legs and the
#'   cumulative gross and net returns.
#'   
#' @examples analyze_legs(pgp)
analyze_legs<-function(pgp){
    pi1y<-switch(pgp$params$frequency,monthly="12",daily=252)
    data<-pgp$data[complete.cases(pgp$data$Trade),]
    legs<-data.frame(Start=as.Date(character()),End=as.Date(character()),Periods=numeric(),Type=character(),PGP.gross=numeric(),
                     PGP.net=numeric())
    out<-list()
    legs.cnt<-1
    legs[legs.cnt,"Start"]<-data[1, "Date"]
    legs[legs.cnt,"PGP.gross"]<-1
    legs[legs.cnt,"PGP.net"]<-1
    legs[legs.cnt,"Type"]<-ifelse(data[1,"Signal"],"Risky","Safe")
    legs[legs.cnt,"Periods"]<-0
    for (i in 1:nrow(data)){
        if(as.logical(data[i,"Trade"])){ 
            #legs[legs.cnt,"End"]<-rownames(data)[i-1]
            legs[legs.cnt,"End"]<-data$Date[i-1]
            legs.cnt<-legs.cnt+1
            #legs[legs.cnt,"Start"]<-rownames(data)[i]
            legs[legs.cnt,"Start"]<-data$Date[i]
            legs[legs.cnt,"PGP.gross"]<-1+data[i,"PGP.gross"]
            legs[legs.cnt,"PGP.net"]<-1+data[i,"PGP.net"]
            legs[legs.cnt,"Type"]<-ifelse(data[i-1,"Signal"],"Risky","Safe")
            legs[legs.cnt,"Periods"]<-1
        } else {
            legs[legs.cnt,"PGP.gross"]<-legs[legs.cnt,"PGP.gross"]*(1+data[i,"PGP.gross"])
            legs[legs.cnt,"PGP.net"]<-legs[legs.cnt,"PGP.net"]*(1+data[i,"PGP.net"])
            legs[legs.cnt,"Periods"]<-legs[legs.cnt,"Periods"]+1
        }
    }
    legs[legs.cnt,"End"]<-data$Date[nrow(data)]
    legs.summary<-data.frame(NumLegs=numeric(5),NumPeriods=numeric(5),Gross.cumret=numeric(5),Net.cumret=numeric(5))
    rownames(legs.summary)<-c("LT Gains","LT Losses","ST Gains","ST Losses","Safe")
    idx <- legs$Type=="Risky" & legs$Periods>=pi1y & legs$PGP.gross>=1
    legs.summary["LT Gains","NumLegs"]<-sum(idx)
    legs.summary["LT Gains","NumPeriods"]<-sum(legs[idx,"Periods"])
    legs.summary["LT Gains","Gross.cumret"]<-prod(legs[idx,"PGP.gross"])-1
    legs.summary["LT Gains","Net.cumret"]<-prod(legs[idx,"PGP.net"])-1
    idx <- legs$Type=="Risky" & legs$Periods<pi1y & legs$PGP.gross>=1
    legs.summary["ST Gains","NumLegs"]<-sum(idx)
    legs.summary["ST Gains","NumPeriods"]<-sum(legs[idx,"Periods"])
    legs.summary["ST Gains","Gross.cumret"]<-prod(legs[idx,"PGP.gross"])-1
    legs.summary["ST Gains","Net.cumret"]<-prod(legs[idx,"PGP.net"])-1
    idx <- legs$Type=="Risky" & legs$Periods>=pi1y & legs$PGP.gross<1
    legs.summary["LT Losses","NumLegs"]<-sum(idx)
    legs.summary["LT Losses","NumPeriods"]<-sum(legs[idx,"Periods"])
    legs.summary["LT Losses","Gross.cumret"]<-prod(legs[idx,"PGP.gross"])-1
    legs.summary["LT Losses","Net.cumret"]<-prod(legs[idx,"PGP.net"])-1
    idx <- legs$Type=="Risky" & legs$Periods<pi1y & legs$PGP.gross<1
    legs.summary["ST Losses","NumLegs"]<-sum(idx)
    legs.summary["ST Losses","NumPeriods"]<-sum(legs[idx,"Periods"])
    legs.summary["ST Losses","Gross.cumret"]<-prod(legs[idx,"PGP.gross"])-1
    legs.summary["ST Losses","Net.cumret"]<-prod(legs[idx,"PGP.net"])-1
    idx <- legs$Type=="Safe"
    legs.summary["Safe","NumLegs"]<-sum(idx)
    legs.summary["Safe","NumPeriods"]<-sum(legs[idx,"Periods"])
    legs.summary["Safe","Gross.cumret"]<-prod(legs[idx,"PGP.gross"])-1
    legs.summary["Safe","Net.cumret"]<-prod(legs[idx,"PGP.net"])-1
    out$detail<-legs
    out$summary<-legs.summary
    return(out)
}

pgp_g1d <- function(pgp){
    d1 <- pgp$returns[1, "Date", drop=TRUE] %m-% months(1)
    x <- apply(1+pgp$returns[,!(colnames(pgp$returns) %in% "Date")], 2, cumprod)
    x <- rbind(1,x)
    x <- tibble::as.tibble(x)
    x <- cbind(Date=c(d1, pgp$returns$Date), x)
    return(x)
}

plot.pgp <- function(pgp){
    idx <- pgp$legs$detail$Type=="Risky"
    sdt <- as.Date(as.yearmon(pgp$legs$detail[idx,"Start"]),frac=1)
    edt <- as.Date(as.yearmon(pgp$legs$detail[idx,"End"]),frac=1)
    risky.dates <- data.frame(BegRisky=sdt, EndRisky=edt)
    
    ggplot(data = pgp$g1d) + geom_line(aes(x=Date, y=RiskyRet, col="SP5")) +
        theme_bw() +
        geom_rect(data=risky.dates, aes(xmin=BegRisky, xmax=EndRisky, ymin=0.1, ymax=+Inf), fill="pink", alpha=1,
                  inherit.aes = TRUE) +
        geom_line(aes(x=Date, y=RiskyRet, col="SP5")) +
        geom_line(data = pgp$g1d, aes(x=Date, y=PGP.net, col="PGP")) +
        geom_line(data = pgp$g1d, aes(x=Date, y=Cash, col="CASH")) +
        scale_y_log10() + theme_bw() +
        scale_color_manual(labels=c("Cash", "PGP", "S&P Index"), 
                           values = c("SP5"="red", "PGP"="blue","CASH"="green"),
                           name = "") +
        ggtitle("Long Term Growth of Stocks, Cash, and PGP") + xlab("") + ylab("Growth of $1") +
        theme(legend.position = "top", legend.direction = "horizontal")
}

pgp_backtest2<-function(pgp){
    library(lubridate)
    library(xts)
    pgp<-avg_px(pgp)
    pgp<-signal(pgp)
    pgp<-period_returns(pgp)
    cnames<-c("Date", "RiskyRet","Cash","PGP.gross","PGP.net", "BalRet")
    returns<-pgp$data[complete.cases(pgp$data[,cnames]),cnames]
    d <- returns$Date
    returns$Date <- NULL
    returns<-xts(returns,order.by = d)
    pgp$returns<-returns
    pgp$legs<-analyze_legs2(pgp)
    pgp$g1d <- pgp_g1d(pgp) 
    return(pgp)
}

analyze_legs2<-function(pgp){
    pi1y<-switch(pgp$params$frequency,monthly="12",daily=252)
    cnames<-c("Date", "RiskyRet","Cash","PGP.gross","PGP.net", "BalRet", "AvgPx", "Signal", "Trade",
              "RiskyPx", "SafePx", "BalPx")
    data<-pgp$data[complete.cases(pgp$data[,cnames]),cnames]
    legs<-data.frame(Start=character(),End=character(),Periods=numeric(),Type=character(),PGP.gross=numeric(),
                     PGP.net=numeric())
    out<-list()
    legs.cnt<-1
    legs[legs.cnt,"Start"]<-rownames(data)[1]
    legs[legs.cnt,"PGP.gross"]<-1
    legs[legs.cnt,"PGP.net"]<-1
    legs[legs.cnt,"Type"]<-ifelse(data[1,"Signal"],"Risky","Safe")
    legs[legs.cnt,"Periods"]<-0
    browser()
    for (i in 1:nrow(data)){
        if(data[i,"Trade"]){ 
            legs[legs.cnt,"End"]<-rownames(data)[i-1]
            legs.cnt<-legs.cnt+1
            legs[legs.cnt,"Start"]<-rownames(data)[i]
            legs[legs.cnt,"PGP.gross"]<-1+data[i,"PGP.gross"]
            legs[legs.cnt,"PGP.net"]<-1+data[i,"PGP.net"]
            legs[legs.cnt,"Type"]<-ifelse(data[i-1,"Signal"],"Risky","Safe")
            legs[legs.cnt,"Periods"]<-1
        } else {
            legs[legs.cnt,"PGP.gross"]<-legs[legs.cnt,"PGP.gross"]*(1+data[i,"PGP.gross"])
            legs[legs.cnt,"PGP.net"]<-legs[legs.cnt,"PGP.net"]*(1+data[i,"PGP.net"])
            legs[legs.cnt,"Periods"]<-legs[legs.cnt,"Periods"]+1
        }
    }
    legs[legs.cnt,"End"]<-rownames(data)[nrow(data)]
    legs.summary<-data.frame(NumLegs=numeric(5),NumPeriods=numeric(5),Gross.cumret=numeric(5),Net.cumret=numeric(5))
    rownames(legs.summary)<-c("LT Gains","LT Losses","ST Gains","ST Losses","Safe")
    idx <- legs$Type=="Risky" & legs$Periods>=pi1y & legs$PGP.gross>=1
    legs.summary["LT Gains","NumLegs"]<-sum(idx)
    legs.summary["LT Gains","NumPeriods"]<-sum(legs[idx,"Periods"])
    legs.summary["LT Gains","Gross.cumret"]<-prod(legs[idx,"PGP.gross"])-1
    legs.summary["LT Gains","Net.cumret"]<-prod(legs[idx,"PGP.net"])-1
    idx <- legs$Type=="Risky" & legs$Periods<pi1y & legs$PGP.gross>=1
    legs.summary["ST Gains","NumLegs"]<-sum(idx)
    legs.summary["ST Gains","NumPeriods"]<-sum(legs[idx,"Periods"])
    legs.summary["ST Gains","Gross.cumret"]<-prod(legs[idx,"PGP.gross"])-1
    legs.summary["ST Gains","Net.cumret"]<-prod(legs[idx,"PGP.net"])-1
    idx <- legs$Type=="Risky" & legs$Periods>=pi1y & legs$PGP.gross<1
    legs.summary["LT Losses","NumLegs"]<-sum(idx)
    legs.summary["LT Losses","NumPeriods"]<-sum(legs[idx,"Periods"])
    legs.summary["LT Losses","Gross.cumret"]<-prod(legs[idx,"PGP.gross"])-1
    legs.summary["LT Losses","Net.cumret"]<-prod(legs[idx,"PGP.net"])-1
    idx <- legs$Type=="Risky" & legs$Periods<pi1y & legs$PGP.gross<1
    legs.summary["ST Losses","NumLegs"]<-sum(idx)
    legs.summary["ST Losses","NumPeriods"]<-sum(legs[idx,"Periods"])
    legs.summary["ST Losses","Gross.cumret"]<-prod(legs[idx,"PGP.gross"])-1
    legs.summary["ST Losses","Net.cumret"]<-prod(legs[idx,"PGP.net"])-1
    idx <- legs$Type=="Safe"
    legs.summary["Safe","NumLegs"]<-sum(idx)
    legs.summary["Safe","NumPeriods"]<-sum(legs[idx,"Periods"])
    legs.summary["Safe","Gross.cumret"]<-prod(legs[idx,"PGP.gross"])-1
    legs.summary["Safe","Net.cumret"]<-prod(legs[idx,"PGP.net"])-1
    out$detail<-legs
    out$summary<-legs.summary
    return(out)
}

plot.pgp2 <- function(pgp){
    risky.dates <- pgp$risky_dates
    ggplot(data = pgp$g1d) + geom_line(aes(x=Date, y=RiskyRet, col="SP5")) +
        theme_bw() +
        geom_rect(data=risky.dates, aes(xmin=BegRisky, xmax=EndRisky, ymin=0.1, ymax=+Inf), fill="pink", alpha=1,
                  inherit.aes = TRUE) +
        geom_line(aes(x=Date, y=RiskyRet, col="SP5")) +
        geom_line(data = pgp$g1d, aes(x=Date, y=PGP.net, col="PGP")) +
        geom_line(data = pgp$g1d, aes(x=Date, y=Cash, col="CASH")) +
        geom_line(data = pgp$g1d, aes(x=Date, y=BalRet, col="BAL")) +
        scale_y_log10() + theme_bw() +
        scale_color_manual(labels=c("Balanced", "Cash", "PGP", "S&P Index"), 
                           values = c("SP5"="red", "PGP"="blue","CASH"="green", "BAL"="chartreuse"),
                           name = "") +
        ggtitle("Long Term Growth") + xlab("") + ylab("Growth of $1") +
        theme(legend.position = "top", legend.direction = "horizontal")
}



#' Get Risky Dates
#'
#' @param pgp 
#'
#' @return data frame with two columns, start date of risky, end date of risky
#' @export
#'
#' @examples get_risky_dates(pgp)
get_risky_dates <- function(pgp){
    idx <- pgp$legs$detail$Type=="Risky"
    sdt <- as.Date(as.yearmon(pgp$legs$detail[idx,"Start"]),frac=0)
    edt <- as.Date(as.yearmon(pgp$legs$detail[idx,"End"]),frac=1)
    return(data.frame(BegRisky=sdt, EndRisky=edt))
}


# start plotly related functions

#' Create Rectangle list object for use in shading G1D chart
#'
#' @param i The row number of the "risky_dates" data frame
#' @param shade_dates 
#'
#' @return list with information for shading
#'
#' @examples create_rectangle(1, risky_dates)
create_rectangle <- function(i, shade_dates){
    # shades dates is a data.frame. First column is start date, second is end. 
    list(type = "rect",
         fillcolor = "pink", line=list(color="pink"), opacity = 0.4,
         x0 = shade_dates[i,1], x1 = shade_dates[i,2], xref = "x",
         y0 = 0, y1=1, yref="paper")
}

#' Create G1D chart in plotly
#'
#' @param pgp 
#'
#' @return plotly chart
#' @export
#'
#' @examples create_g1d_plotly(pgp)
create_g1d_plotly <- function(pgp){
    p <- plot_ly(data = pgp$g1d, x = ~Date, y = ~RiskyRet, mode = "lines", type = "scatter",
                 name = "S&P Index",
                 line = list(color="red")) %>%
        add_trace(y = ~PGP.net, name = "PGP", line = list(color = "blue")) %>%
        add_trace(y = ~BalRet, name = "Balanced", line = list(color = "chartreuse")) %>%
        add_trace(y = ~Cash, name = "Cash", line = list(color = "green")) %>%
        layout(title = "Long Term Growth",
               yaxis = list(title = "Growth of $1", type="log"),
               xaxis = list(title = ""))
    
    temp <- lapply(1:nrow(risky_dates), create_rectangle, risky_dates=pgp$risky_dates)
    g1d <- layout(p, shapes = temp)
    return(g1d)
}


subsetByDateRange <- function(df, daterange){
    subset(df, Date>=substr(daterange,1,10) & Date<=substr(daterange,12,21))
}

subsetg1d <- function(df, daterange, pgp_names, port_names){
    data <- subsetByDateRange(df, daterange)
    temp <- data[,c(pgp_names,port_names)]
    return(cbind(Date=data$Date, data.frame(lapply(temp, function(X) X/X[1]))))
}

make_table_growth <- function(pgp, daterange, pgp_names, port_names){
    data <- subsetg1d(pgp$g1d, daterange, pgp_names, port_names)
    r <- 100*(data[nrow(data),2:ncol(data)]^ min(1, 12/(nrow(data)-1)) - 1)
    rownames(r) <- ifelse(nrow(data)<=13, "Return", "AnnualizedReturn")
    r <- data.frame(t(r))
    return(r)
}

subset_risky_dates <- function(pgp, daterange){
    out <- pgp$risky_dates
    out <- subset(out, EndRisky>=substr(daterange,1,10) & BegRisky<=substr(daterange,12,21))
    out$BegRisky <- pmax(out$BegRisky, substr(daterange,1,10))
    out$EndRisky <- pmin(out$EndRisky, substr(daterange,12,21))
    return(out)
}

percent_risky <- function(pgp, daterange){
    data <- subset(pgp$data, Date>=substr(daterange,1,10) & Date<substr(daterange,12,21))  # < is intentional. 
    100 * sum(data$Signal) / nrow(data)
}

plotly_g1d <- function(pgp, daterange, pgp_names, port_names){
    data <- subsetg1d(pgp$g1d, daterange, pgp_names, port_names)
    if(ncol(data)<=1) error("No data to plot")
    p <- plot_ly(data = data, x = ~Date)
    for(i in c(pgp_names, port_names)){
        p <- p %>% 
            add_trace(y = data[,i], name = i, type="scatter", mode="lines")
    }
    p <- p %>% 
        layout(title = "Growth of $1",
        yaxis = list(title = "Portfolio Value ($)", type="log"),
        xaxis = list(title = ""))
    
    risky_dates <- pgp$risky_dates[pgp$risky_dates[,2] >= data[1,"Date"] & 
                                   pgp$risky_dates[,1] <= last(data[,"Date"]),]
    risky_dates[1,1] <- max(data[1,"Date"], risky_dates[1,1])
    risky_dates[nrow(risky_dates),2] <- min(last(data[,"Date"]), last(risky_dates[,2]))
    
    temp <- lapply(1:nrow(risky_dates), create_rectangle, shade_dates=risky_dates)
    g1d <- layout(p, shapes = temp)
    return(g1d)
}

create_g1d_plotly2 <- function(pgp){
    
    p <- plot_ly(data = pgp$g1d, x = ~Date, y = ~RiskyRet, mode = "lines", type = "scatter",
                 name = "S&P Index",
                 line = list(color="red")) %>%
        add_trace(y = ~PGP.net, name = "PGP", line = list(color = "blue")) %>%
        add_trace(y = ~Cash, name = "Cash", line = list(color = "green")) %>%
        layout(title = "Long Term Growth",
               yaxis = list(title = "Growth of $1", type="log"),
               xaxis = list(title = ""))
    
    temp <- lapply(1:nrow(pgp$risky_dates), create_rectangle, shade_dates=pgp$risky_dates)
    g1d <- layout(p, shapes = temp)
    return(g1d)
}

create_1boxplot_plotly <- function(nmonths, pgp, daterange, pgp_names, port_names){
    if(sum(pgp$params$rolling == nmonths)<=0) {error("nmonths not a parameter in rolling")}
    data <- pgp$rolling_tables[[paste0("Rolling_", nmonths)]]
    data <- subsetByDateRange(data, daterange)[, c(pgp_names, port_names)]
    mypalette <- brewer.pal(length(pgp_names)+length(port_names)+1, "Dark2")
    p <- plot_ly(data = data, type="box")
    for(i in c(pgp_names, port_names)){
        p <- p %>% 
            add_trace(y = data[,i], name = i)
    }
    ctitle <- ifelse(nmonths %% 12 == 0, paste0(round(nmonths/12,0), " Year"), paste0(nmonths, "Month"))
    a<-list(text=paste("Rolling", ctitle, "Returns"),
            xref = "paper",
            yref = "paper",
            yanchor = "bottom",
            xanchor = "center",
            color = c(pgp_names, port_names),
            colors = mypalette,
            align = "left",
            valign = "top",
            x = 0.5,
            y = 1,
            showarrow = FALSE)
    p <- p %>% layout(colorway=mypalette, annotations=a)
    return(p)
}    

create_boxplot_grid_plotly <- function(pgp, daterange, pgp_names, port_names){
    boxplot.lst <- lapply(pgp$params$rolling, create_1boxplot_plotly,  
                          pgp=pgp, daterange=daterange, pgp_names=pgp_names, port_names=port_names)
    names(boxplot.lst) <- paste0("Rolling_", pgp$params$rolling)
    p <- subplot(boxplot.lst, titleX=FALSE, titleY=FALSE, nrows=2, margin=0.05) %>%
        layout(showlegend = FALSE,
               yaxis = list(title = ""), 
               xaxis = list(title = ""))
    return(p)
}

create_capture_grid_plotly <- function(pgp, daterange, pgp_names, port_names){
    capture.lst <- lapply(pgp$params$rolling, create_1capture_plotly,  
                          pgp=pgp, daterange=daterange, pgp_names=pgp_names, port_names=port_names)
    names(capture.lst) <- paste0("Rolling_", pgp$params$rolling)
    p <- subplot(capture.lst, titleX=FALSE, titleY=TRUE, nrows=length(capture.lst), margin=0.05) %>%
        layout(showlegend = FALSE,
               yaxis = list(title = "Return (%)"), 
               xaxis = list(title = ""))
    return(p)
}

create_1capture_plotly <- function(pgp, nmonths, daterange, pgp_names, port_names){
    capture_table <- calc_capture(pgp, nmonths, daterange, pgp_names, port_names)
    period <- c(paste0("S&P Up (", capture_table["Number of Periods", "Up"], " Periods)"), 
                paste0("S&P Down (", capture_table["Number of Periods", "Down"], " Periods)"), 
                paste0("All (", capture_table["Number of Periods", "All"], " Periods)"))
    mkt <- capture_table["S&P Index Return",]
    pgp <- capture_table["PGP Return",]
    port <- capture_table["Portfolio Return",]
    data <- data.frame(Period=period, "SP Index"= mkt, pgp=pgp, portfolio=port)
    mypalette <- brewer.pal(3, "Dark2")
    ctitle <- ifelse(nmonths %% 12 == 0, paste0(round(nmonths/12,0), " Year"), paste0(nmonths, "Month"))
    p <- plot_ly(data, x = ~Period, y = ~SP.Index, type = 'bar', name = "S&P",
                 text="100%", textposition='auto') %>%
        add_trace(y = ~pgp, name=pgp_names, 
                  text=paste0(capture_table["PGP Percent Capture", ],"%"), textposition='auto') %>%
        add_trace(y = ~portfolio, name=port_names,
                  text=paste0(capture_table["Port Percent Capture", ],"%"), textposition='auto') %>%
        layout(yaxis = list(title = "Return (%)"), 
               xaxis = list(title = ""), 
               barmode = 'group')
    a<-list(text=paste("Rolling", ctitle, "Periods"),
            xref = "paper",
            yref = "paper",
            yanchor = "bottom",
            xanchor = "center",
            color = c(pgp_names, port_names),
            colors = mypalette,
            align = "left",
            valign = "top",
            x = 0.5,
            y = 1,
            showarrow = FALSE)
    p <- p %>% layout(colorway=mypalette, annotations=a)
    return(p)
}

create_rolling_grid_plotly <- function(pgp, daterange, pgp_names, port_names){
    plot.lst <- lapply(pgp$params$rolling, create_1rolling_plotly,  
                          pgp=pgp, daterange=daterange, pgp_names=pgp_names, port_names=port_names)
    names(plot.lst) <- paste0("Rolling_", pgp$params$rolling)
    p <- subplot(plot.lst, titleX=FALSE, titleY=FALSE, nrows=2, margin=0.05) %>%
        layout(showlegend = TRUE,
               yaxis = list(title = ""), 
               xaxis = list(title = ""))
    return(p)
}

create_1rolling_plotly <- function(nmonths, pgp, daterange, pgp_names, port_names){
    if(sum(pgp$params$rolling == nmonths)<=0) {error("nmonths not a parameter in rolling")}
    data <- pgp$rolling_tables[[paste0("Rolling_", nmonths)]]
    data <- subsetByDateRange(data, daterange)[, c("Date", pgp_names, port_names)]
    mypalette <- brewer.pal(length(pgp_names)+length(port_names)+1, "Dark2")
    p <- plot_ly(data = data, type="scatter", mode="lines")
    for(i in pgp_names){
        p <- p %>% 
            add_trace(x=~Date, y = data[,i], name = i, mode="lines", legendgroup="groupPGP")
    }
    for(i in port_names){
        p <- p %>% 
            add_trace(x=~Date, y = data[,i], name = i, mode="lines", legendgroup="groupPort")
    }
    ctitle <- ifelse(nmonths %% 12 == 0, paste0(round(nmonths/12,0), " Year"), paste0(nmonths, "Month"))
    a<-list(text=paste("Rolling", ctitle, "Returns"),
            xref = "paper",
            yref = "paper",
            yanchor = "bottom",
            xanchor = "center",
            color = c(pgp_names, port_names),
            colors = mypalette,
            align = "left",
            valign = "top",
            x = 0.5,
            y = 1,
            showarrow = FALSE)
    p <- p %>% layout(colorway=mypalette, annotations=a)
    return(p)
}   

square_plot <- function(data){
    data <- data[complete.cases(data),]
    lowlim <- min(data, na.rm=TRUE)
    highlim <- max(data, na.rm=TRUE)
    # highlim <- max(abs(lowlim), highlim)
    # lowlim <- -highlim 
    #ggplot(data=data) + geom_point(aes(x=SPIndex, y=PGP, fill=year(Index)), color="blue") +
    p <- ggplot(data=data) + geom_point(aes(x=SPIndex, y=PGP, color=year(Index))) +
        labs(color="Year") + scale_color_gradient(low="lightblue", high="darkblue") +
        theme(legend.title=element_text(size=10), 
              legend.background = element_rect(size=.75), 
              legend.text=element_text(size=8)) +
        geom_hline(yintercept = 0, color="darkgray") +
        geom_vline(xintercept = 0, color="darkgray") +
        geom_abline(intercept=0,slope=1, color = "red") +
        scale_x_continuous(limits=c(lowlim,highlim), 
                           breaks = round(seq(signif(lowlim,-1),signif(highlim,-1), by=10))) +
        scale_y_continuous(limits=c(lowlim,highlim), 
                           breaks = round(seq(signif(lowlim,-1),signif(highlim,-1), by=10))) +
        ggtitle("")
    q <- ggplot(data=data) + geom_point(aes(x=SPIndex, y=Balanced, color=year(Index))) +
        labs(color="Year") + scale_color_gradient(low="lightblue", high="darkblue") +
        theme(legend.title=element_text(size=10), 
              legend.background = element_rect(size=.75), 
              legend.text=element_text(size=8)) +
        geom_hline(yintercept = 0, color="darkgray") +
        geom_vline(xintercept = 0, color="darkgray") +
        geom_abline(intercept=0,slope=1, color = "red") +
        scale_x_continuous(limits=c(lowlim,highlim), 
                           breaks = round(seq(signif(lowlim,-1),signif(highlim,-1), by=10))) +
        scale_y_continuous(limits=c(lowlim,highlim), 
                           breaks = round(seq(signif(lowlim,-1),signif(highlim,-1), by=10))) +
        ggtitle("")
    #  xlim(lowlim, highlim) + ylim(lowlim, highlim) +
    return(list(PGP=p, Balanced=q))
}

create_scatter_pair_plotly <- function(pgp, nmonths, daterange, pgp_names, port_names){
    e<-list(xref = "paper",
            yref = "paper",
            yanchor = "bottom",
            xanchor = "center",
            align = "left",
            valign = "top",
            x = 0.5,
            y = 1,
            showarrow = FALSE)
    sdt <- as.character(as.Date(substr(daterange,1,10)) %m+% months(pgp$params$width + nmonths -1))
    edt <- as.character(as.Date(substr(daterange,12,21)))
    data <- data.frame(pgp$rolling_tables[[paste0("Rolling_", nmonths)]][,c("Date", "RiskyRet", pgp_names, port_names)], row.names = "Date")
    idx <- rownames(data) >= sdt & rownames(data)<=edt
    data <- data[idx,]
    colnames(data) <- c("SPIndex", "PGP", "Balanced")
    data <- xts(data, order.by= as.Date(row.names(data)))
    ctitle <- ifelse(nmonths %% 12 == 0, paste0(round(nmonths/12,0), " Year"), paste0(nmonths, "Month"))
    ggscat <- square_plot(data) #+ coord_equal(ratio=1)
    scat_lh <- ggplotly(ggscat$PGP) %>% 
        layout(annotations=rlist::list.append(text=paste0(ctitle, ": ", pgp_names, " v. S&P Index"),e))
    scat_rh <- ggplotly(ggscat$Balanced) %>% 
        layout(annotations=rlist::list.append(text=paste0(ctitle, ": ", port_names, " v. S&P Index"),e))
    scat_comb <- subplot(list(scat_lh,scat_rh), titleX=FALSE, titleY=FALSE, nrows=1, margin=0.05) %>%
        layout(showlegend = FALSE,
               yaxis = list(title = ""), 
               xaxis = list(title = ""))
    
    return(scat_comb)
}

# end plotly related functions

calc_rolltable <- function(nmonths, g1d){
    out <- round(100*rollapply(g1d[, !(names(g1d) %in% "Date")], 
                               FUN = function(x) (x[nmonths+1]/x[1])^ifelse(nmonths<=12,1,12/nmonths)-1, width = nmonths+1),1)
    out <- cbind(Date = g1d$Date[(nmonths+1):nrow(g1d)], data.frame(out))
    return(out)
}

create_rolling_returns_tables <- function(pgp){
    pgp$rolling_tables <- lapply(pgp$params$rolling, calc_rolltable, g1d=pgp$g1d)
    names(pgp$rolling_tables) <- paste0("Rolling_", pgp$params$rolling)
    return(pgp$rolling_tables)
}

create_mkt_drawdown_table<-function(pgp, daterange, pgp_names, port_names, threshold = -0.1){
    data <- data.frame(subsetByDateRange(pgp$returns, daterange)[, c("Date", "RiskyRet", pgp_names, port_names)])
    row.names(data)<-data$Date
    #data <- data[,!(names(data) %in% "Date")]
    ddtable1<-subset(table.Drawdowns(data[,"RiskyRet", drop=FALSE], top  =100),
                     Depth <= threshold)
    return_in_dd <- function(ddrow){
        data <- 1+subset(pgp$returns, Date>=ddtable1[ddrow,"From"] & Date<=ddtable1[ddrow,"Trough"], 
                         select= (names(pgp$returns) %in% c("RiskyRet", pgp_names, port_names)))
        return(apply(data,2,prod)-1)}
    out <-cbind(ddtable1[, c("From", "Trough", "To Trough")], round(100*t(sapply(1:nrow(ddtable1), return_in_dd)),1))
    out <- out[,c("From", "Trough", "To Trough", "RiskyRet", pgp_names, port_names)]
    out$From<-as.character(as.Date(out$From))
    out$Trough <- as.character(as.Date(out$Trough))
    out$`To Trough`<-as.integer(out$`To Trough`)
    names(out)[4] <- "S&P Index"
    names(out)[3] <- "Num. Months"
    return(out)
    # col.names=c("From", "Trough", "Months", "S&P Index", "PGP","Balanced"),
}
create_mkt_drawdown_table2<-function(pgp){
    data <- data.frame(pgp$returns)
    row.names(data)<-data$Date
    #data <- data[,!(names(data) %in% "Date")]
    ddtable1<-subset(table.Drawdowns(data[,"RiskyRet", drop=FALSE], top  =100),
                     Depth <= -0.1)
    return_in_dd <- function(ddrow){
        data <- 1+subset(pgp$returns, Date>=ddtable1[ddrow,"From"] & Date<=ddtable1[ddrow,"Trough"], 
                       select= !(names(pgp$returns) %in% "Date"))
        apply(data,2,prod)-1}
    return(cbind(ddtable1, t(sapply(1:nrow(ddtable1), return_in_dd))))
    # col.names=c("From", "Trough", "Months", "S&P Index", "PGP","Balanced"),
}

create_drawdown_table<-function(pgp, daterange, varname, threshold = -0.1){
    data <- data.frame(subsetByDateRange(pgp$returns, daterange)[, c("Date", varname)], row.names = "Date")
    ddtable<-subset(table.Drawdowns(data, top  =100),
                    Depth <= -0.1)
    ddtable[,1] <- as.character(as.Date(ddtable[,1]))
    ddtable[,2] <- as.character(as.Date(ddtable[,2]))
    ddtable[,3] <- as.character(as.Date(ddtable[,3]))
    ddtable[,4] <- 100*ddtable[,4]
    ddtable[,5] <- as.integer(ddtable[,5])
    ddtable[,6] <- as.integer(ddtable[,6])
    ddtable[,7] <- as.integer(ddtable[,7])
    return(ddtable)
}

create_calyr_table <- function(pgp, daterange, pgp_names, port_names){
    syr <- substr(daterange,1,4)
    eyr <- substr(daterange,12,15)
    data<- data.frame(pgp$returns[, c("Date", pgp_names, port_names)], row.names = "Date")
    idx <- year(row.names(data)) >= syr & year(row.names(data)) <= eyr
    return(table.CalendarReturns(data[idx,])[, c(port_names, pgp_names)])
}



calc_capture <- function(pgp, nmonths, daterange, pgp_names, port_names){
    sdt <- as.character(as.Date(substr(daterange,1,10)) %m+% months(pgp$params$width + nmonths -1))
    edt <- as.character(as.Date(substr(daterange,12,21)))
    data <- data.frame(pgp$rolling_tables[[paste0("Rolling_", nmonths)]][,c("Date", "RiskyRet", pgp_names, port_names)], row.names = "Date")
    idx <- rownames(data) >= sdt & rownames(data)<=edt
    data <- data[idx,]
    up <- subset(data, RiskyRet>=0)
    dn <- subset(data, RiskyRet<0)
    out<-matrix(0, nrow=6, ncol=3)
    colnames(out) <- c("Up","Down", "All")
    rownames(out) <- c("Number of Periods",
                       "S&P Index Return", 
                       "PGP Return",
                       "Portfolio Return","PGP Percent Capture","Port Percent Capture")
    
    out[1,] <- c(nrow(up),nrow(dn), nrow(data))
    out[2:4,1] <- t(apply(up,2,mean))
    out[2:4,2] <- t(apply(dn,2,mean))
    out[2:4,3] <- t(apply(data,2,mean))
    out[5,] <- round(100*out[3,]/out[2,],1)
    out[6,] <- round(100*out[4,]/out[2,],1)
    out <- round(out,1)
    return(out)
}