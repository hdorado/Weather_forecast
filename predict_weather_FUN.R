
# Predictive weather FUN


# Parameters

# historical_oni <- tail(train_oni,oni_length)


# oni_length <- 4 # How periods of ONI will be considerated to indetify analogs years
# 
# k_nearest <- 7
# 
# predLeng <- 6
# 
# Periods_ref <- historical_oni
# 
# Date_ini_stations <- "1979-01-01"
# 
# enso <- enso


# Instroduce the Oni more recents

weather_predict <- function(oni_length,k_nearest,predLeng,enso_pred,enso_m ,
                            Date_ini_stations,completeWeather,
                            train_oni=train_oni)
{
  
  # Start from the maximun month that will be forecasted
  
  train_oni$Month <- month(train_oni$Date)
  
  train_oni <- train_oni %>% filter(Date >= as.Date(Date_ini_stations))
  
  train_oni <- train_oni[which(enso_m[1]==train_oni$Month)[1]:nrow(train_oni),]
  
  # Extracting all oni values in the same months

  process_par <- train_oni %>% filter(Month %in%  enso_months_fut)

  seq_sort <- unlist(lapply( 1:(length(unique(process_par$Year))+200),rep,
                           oni_length))

  rl <- rep(1:oni_length,length=length(seq_sort))

  historical_records <- data.frame(process_par,
                                 Group = seq_sort[1:nrow(process_par)],
                                 rl=rl[1:nrow(process_par)])


  process_par <- (historical_records %>% mutate(Month=paste0("M",Month)) %>% 
                    pivot_wider(id_cols = Month,names_from = Group,
                                                     values_from = ONI ))[,-1]

  n <- nrow(process_par)

  
  euc_dist <- sapply(sqrt((as.matrix(enso_pred) - process_par)^2/n),sum)

  euc_dist <- sort(euc_dist)

  plot(2:length(euc_dist),euc_dist[-1],type = "l",ylab="Euclidian distance",xlab="group")
  points(2:length(euc_dist),euc_dist[-1],pch=16)


  nearGroups <- as.numeric(names(euc_dist[2:(k_nearest+1)]))

  selected <- historical_records %>% 
    filter(Group %in% nearGroups & rl == 1)

  positon_hom <- which(train_oni$Date %in% selected$Date)+1

  selected2 <- train_oni[positon_hom,]

  selected2$Date <- as.Date(selected2$Date)

  # Extraction of analogs weather days
  
  final_pos <- which(completeWeather$Date %in% selected2$Date)

  lpos <- lapply(final_pos,function(x){x:(x+predLeng*30)})

  lst_estation <- lapply(lpos , function(w){completeWeather[w,]})

  estimation <- do.call(rbind,lst_estation) %>% 
    pivot_longer(!Date,names_to = "Variable",values_to = "Value")

  estimation <- mutate(estimation,Month = month(Date), Day =day(Date) )
  
  estimation <- estimation %>% group_by(Variable,Month,Day) %>% 
    dplyr::summarise(Estimation = mean(Value, na.rm=T))

  average_estimation <-
  estimation %>% pivot_wider(names_from = Variable ,values_from =  Estimation)

  list(analogs_years = lst_estation,average_estimation = average_estimation)
}
