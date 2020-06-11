#导入数据
data_2 <- read.table("./household_power_consumption.txt", 
                   header = T, sep = ";", na.strings = "?",
                   colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

#调整格式
data_2$Date <- as.Date(data_2$Date, "%d/%m/%Y")

#过滤至特定日期区间
data_2 <- subset(data_2, Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

#筛选有效数据
data_2 <- data_2[complete.cases(data_2),]

#合并日期时间列
dateTime <- paste(data_2$Date, data_2$Time)
dateTime <- setNames(dateTime, "DateTime")
data_2 <- data_2[ , !(names(data_2) %in% c("Date", "Time"))]
data_2 <- cbind(dateTime, data_2)
data_2$dateTime <- as.POSIXct(dateTime)

#Histogram
hist(data_2$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red")
dev.copy(png, "plot1.png", width = 480, height = 480)
dev.off()

#plot2
plot(data_2$Global_active_power~data_2$dateTime, type = "S", ylab = "Global Active Power (kilowatts)", xlab = "")
dev.copy(png, "plot2.png", width = 480, height = 480)
dev.off()

#plot3
with(data_2, {
    plot(data_2$Sub_metering_1~data_2$dateTime, type = "S", ylab = "Energy sub metering", xlab = "")
    lines(data_2$Sub_metering_2~data_2$dateTime, col = 'Red')
    lines(data_2$Sub_metering_3~data_2$dateTime, col = 'Blue')
})
legend("topright", col = c("black", "red", "blue"), lwd = c(0.5, 0.5, 0.5),
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")) 
par(mar=c(1, 1, 1, 1))
dev.copy(png, "plot3.png", width = 480, height = 480)
dev.off()

#plot 4
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(data_2, {
    plot(data_2$Global_active_power~data_2$dateTime, type = "S", ylab  = "Global Active Power (kilowatts)", xlab = "")
    plot(data_2$Voltage~data_2$dateTime, type = "S", ylab = "Voltage (volt)", xlab = "")
    plot(data_2$Sub_metering_1~data_2$dateTime, type = "S", ylab = "Global Active Power (kilowatts)", xlab = "")
    lines(data_2$Sub_metering_2~data_2$dateTime, col = 'Red')
    lines(data_2$Sub_metering_3~data_2$dateTime, col = 'Blue')
    legend("topright", col = c("black", "red", "blue"),
           c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), bty = "n") 
    plot(data_2$Global_reactive_power~data_2$dateTime, type = "S", ylab = "Global Reactive Power (kilowatts)", xlab = "")
})
dev.copy(png, "plot4.png", width = 480, height = 480)
dev.off()














