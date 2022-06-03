rm(list = ls())

.rs.restartR()
par(mfrow =c(1,1))
# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Path to save results
data_path <- "Data"
pairs <- list.files(paste0(getwd(),"/", data_path))

# pairs <- c("ATOMUSD", "BTCUSD", "ADAUSD", "DOTUSD", "EOSUSD", "ETHUSD",
#            "MANAUSD",  "NANOUSD", "UNIUSD")
# 
# pairs <- c("ADAUSD","LINKUSD", "MATICUSD",
#            "ATOMUSD", "ALGOUSD", "KEEPUSD", "ETHUSD")
# Or choose a single one
ticks <- c(1)

units <- c(rep("minutes", 1))
intervals <- paste(ticks, units, sep = " ")
library(tidyquant)

all <- lapply(pairs, function(x)trades_to_OHLC(pair = x,
                                               interval = intervals,
                                               from_date = "2022-04-01",
                                               to_date = "2022-04-08",
                                               date_subset = T))
all_red <- list()
i <- 1
key <- c("interval", "close")


for(i in 1:length(all)){
  df <- as.data.table(all[[i]])
  all_red[[i]] <- df[, ..key]
  setnames(all_red[[i]], "close", pairs[i])
  
}
all <- Reduce(function(x, y) merge(x, y, all=TRUE), all_red)
dim(all)

# all <- all[112:nrow(all), ]
all$BTCUSD <- NULL
colnames(all)
all[, BTCUSD := na.locf(BTCUSD)]
all[, ATOMUSD := na.locf(ATOMUSD)]
all[, ADAUSD := na.locf(ADAUSD)]
all[, DOTUSD := na.locf(DOTUSD)]
all[, EOSUSD := na.locf(EOSUSD)]
all[, ETHUSD := na.locf(ETHUSD)]
all[, LINKUSD := na.locf(LINKUSD)]
all[, MANAUSD := na.locf(MANAUSD)]
all[, MATICUSD := na.locf(MATICUSD)]
all[, NANOUSD := na.locf(NANOUSD)]
all[, UNIUSD := na.locf(UNIUSD)]
all[, ETHUSD := na.locf(ETHUSD)]
all[, KEEPUSD := na.locf(KEEPUSD)]
all[, ALGOUSD := na.locf(ALGOUSD)]
all[, SOLUSD := na.locf(SOLUSD)]
all[, LTCUSD := na.locf(LTCUSD)]
all[, ETCUSD := na.locf(ETCUSD)]

M <- cor(all[, -1])


corrplot::corrplot(M, method = 'number')


selected_pair <- c("SOLUSD", "ETHUSD")
key <- c("interval",selected_pair)

df <- all[, ..key]
nm1 <- grep(paste0(selected_pair, collapse ="|"), names(df))
df[ , (nm1) := lapply(.SD, scale), .SDcols = nm1]

# plot(df$keep_sc, type= "l")
# lines(df$atom_sc, type= "l", col = "blue")
df[, diff := get(selected_pair[[1]])-get(selected_pair[[2]])]
plot(df$diff, type= "l")
abline(h = 0)
library(tseries)
adf.test(df$diff)

par(mfrow=c(1,1))
plot(df$diff, type= "l")
adf.test(df$diff)
abline(h =0, col = "red")
abline(h =sd(df$diff), col = "green")
abline(h =-sd(df$diff), col = "red")

abline(h =2*sd(df$diff), col = "green")
abline(h =-2*sd(df$diff), col = "red")

df[diff > 2*sd(diff), (paste0("pos_", selected_pair[[1]])) := "short"]
df[diff > 2*sd(diff), (paste0("pos_", selected_pair[[2]])) := "long"]

df[diff < -2*sd(diff), (paste0("pos_", selected_pair[[1]])) := "long"]
df[diff < -2*sd(diff), (paste0("pos_", selected_pair[[2]])) := "short"]

# Define entries
df[!is.na(get(paste0("pos_", selected_pair[[1]]))), signal := "enter"]


df[, groups:= as.character(rep(1:length(rle(df$signal)$lengths),
                               rle(df$signal)$lengths))]

df[is.na(signal), groups := NA]

df[, index :=  1:nrow(df)]

entries <- df[, head(index, 1), by=groups][!is.na(groups)]
df[entries$V1, entry := "enter"]
df[, position := character()]
idx_signals <- which(df$entry == "enter")
price_signals <- as.vector(na.omit(df$diff[(df$entry == "enter")]))

i <-1

while (i <= length(idx_signals)) {
  
  # print(idx_signals)
  # print(idx_signals[i])
  if(is.na(df$position[idx_signals[i]])  & df$entry[idx_signals[i]] == "enter"){
    returns_1 <- df$diff[idx_signals[i]:nrow(df)]
      

    returns_1 <- returns_1[-1]
    if(returns_1[1] < 0){
      exit_1 <- head(which(returns_1 > 0), 1)
      if(length(exit_1) == 0){
        exit_1 <- nrow(df)-idx_signals[i]
      }  
      
    }
  
    if(returns_1[1] > 0){
      exit_1 <- head(which(returns_1 < 0), 1)
      if(length(exit_1) == 0){
        exit_1 <- nrow(df)-idx_signals[i]
      }  
      
    }
    
      
      df[idx_signals[i]: (exit_1+idx_signals[i]), position := "open_position"]
      df[idx_signals[i], status := "entered_trade"]
      df[(exit_1+idx_signals[i]), status := "exited_trade"]
      df[idx_signals[i]: (exit_1+idx_signals[i]), trade_id := sample(5000:500000, 1)]
    
  } 
  i <- i+1 
  print(i)
  }


trades <- unique(df$trade_id)
trades <- trades[!is.na(trades)]

key <- c("interval", selected_pair)
df_original <- all[, ..key]
setnames(df_original, c(selected_pair[1], selected_pair[2]),
         c( paste0(selected_pair[1], "_OG")  , paste0(selected_pair[2], "_OG")))

df <- merge(df, df_original, by = "interval", all.x = T)

key <- c(paste0(selected_pair[1], "_OG"), paste0(selected_pair[2], "_OG"),
         paste0("pos_", selected_pair[1]),
         paste0("pos_", selected_pair[2]),
         "status",
         "trade_id")

entering <- df[status == "entered_trade", ..key]

setnames(entering, c(paste0(selected_pair[1], "_OG"), paste0(selected_pair[2], "_OG"), "status"), c(paste0(paste0(selected_pair[1], "_OG"), "_enter"), paste0(paste0(selected_pair[2], "_OG"), "_enter"), "status_enter"))

key <- c(paste0(selected_pair[1], "_OG"), paste0(selected_pair[2], "_OG"),
         "status",
         "trade_id")

exiting <- df[status == "exited_trade", ..key]
setnames(exiting, c(paste0(selected_pair[1], "_OG"), paste0(selected_pair[2], "_OG"), "status"), c(paste0(paste0(selected_pair[1], "_OG"), "_exit"), paste0(paste0(selected_pair[2], "_OG"), "_exit"), "status_exit"))

results <- merge(entering, exiting, by = "trade_id")

results[, diff1 := ((get(paste0(paste0(selected_pair[1], "_OG"), "_exit")) - get(paste0(paste0(selected_pair[1], "_OG"), "_enter")))/get(paste0(paste0(selected_pair[1], "_OG"), "_enter")))*100]
results[get(paste0("pos_", selected_pair[1])) == "short", diff1 := diff1*(-1)]
  
results[, diff2 := ((get(paste0(paste0(selected_pair[2], "_OG"), "_exit")) - get(paste0(paste0(selected_pair[2], "_OG"), "_enter")))/get(paste0(paste0(selected_pair[2], "_OG"), "_enter")))*100]
results[get(paste0("pos_", selected_pair[2])) == "short", diff2 := diff2*(-1)]

results[, result_pair := diff1 + diff2]
sum(results$result_pair)

# Trades area
df[, x := 1:nrow(df)]


df1 <- copy(df)
entries <- df1[status == "entered_trade", .(x, get(selected_pair[1]))][, status := "entered_trade"]
exits <- df1[status == "exited_trade", .(x, get(selected_pair[1]))][, status := "exited_trade"]
all <- rbind(entries, exits)
setnames(all, "V2", selected_pair[1])

df1 <- copy(df)
entries2 <- df1[status == "entered_trade", .(x, get(selected_pair[2]))][, status := "entered_trade"]
exits2 <- df1[status == "exited_trade", .(x, get(selected_pair[2]))][, status := "exited_trade"]
all2 <- rbind(entries2, exits2)
setnames(all2, "V2", selected_pair[2])


p <- ggplot(df1, aes(x = x, y = get(selected_pair[1]))) +
  geom_line(alpha= 1, size = 0.1)+ theme(legend.title = element_blank())
xsmin <- df1[status == "entered_trade", x]
xsmax <- df1[status == "exited_trade", x]

p2 <- p +  theme(legend.position="none")+

  geom_point(data = all, 
             mapping = aes(x = x, y = get(selected_pair[1]), color = factor(status)))+
  scale_color_manual(values=c("green", "red")) 
p3 <- p2 + geom_line(data = df1, aes(x = x, y = get(selected_pair[2])),alpha = 1,size = 0.1, color = "blue") +
  geom_point(data = all2,
             mapping = aes(x = x, y = get(selected_pair[2]), color = factor(status)))

ggplotly(p3)





# Trades area
df[, x := 1:nrow(df)]


df1 <- copy(df)
entries <- df1[status == "entered_trade", .(x, SOLUSD_OG)][, status := "entered_trade"]
exits <- df1[status == "exited_trade", .(x, SOLUSD_OG)][, status := "exited_trade"]
all <- rbind(entries, exits)
setnames(all, "SOLUSD_OG", "SOLUSD")

df1 <- copy(df)
entries2 <- df1[status == "entered_trade", .(x, ETHUSD_OG)][, status := "entered_trade"]
exits2 <- df1[status == "exited_trade", .(x, ETHUSD_OG)][, status := "exited_trade"]
all2 <- rbind(entries2, exits2)
setnames(all2, "ETHUSD_OG", "ETHUSD")


View(results)



p <- ggplot(df1, aes(x = x, y = SOLUSD_OG)) +
  geom_line(alpha= 1, size = 0.1)+ theme(legend.title = element_blank())
xsmin <- df1[status == "entered_trade", x]
xsmax <- df1[status == "exited_trade", x]

p2 <- p +  theme(legend.position="none")+
  
  geom_point(data = all, 
             mapping = aes(x = x, y = SOLUSD, color = factor(status)))+
  scale_color_manual(values=c("green", "red")) 
p3 <- p2 + geom_line(data = df1, aes(x = x, y = ETHUSD_OG,alpha = 1,size = 0.1, color = "blue")) +
  geom_point(data = all2,
             mapping = aes(x = x, y = ETHUSD, color = factor(status)))

ggplotly(p3)
p2
p3 <- ggplot(df1, aes(x = x, y = ETHUSD_OG)) +
  geom_line(alpha= 1, size = 0.1)+ theme(legend.title = element_blank())+
  geom_point(data = all2, 
             mapping = aes(x = x, y = ETHUSD, color = factor(status)))+
  scale_color_manual(values=c("green", "red"))
library(ggpubr)
ggarrange(p2, p3, ncol = 2)
