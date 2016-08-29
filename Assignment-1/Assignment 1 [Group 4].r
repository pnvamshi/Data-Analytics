# P.N. Vamshi (13EC10044), Manognya Deepthi G (13IM10008), Nikhil Kashyap (13EE10033), Mukesh Sahani (13EC10039), Manoj Meena (13EE10028)
# Data Analysis (ASSIGNMENT 1)
# DA-04

table1 = read.csv("C:\\Users\\P.N.Vamshi\\Desktop\\Data Analysis\\Data1.csv", header = T, sep = ",")
table2 = read.csv("C:\\Users\\P.N.Vamshi\\Desktop\\Data Analysis\\Data2.csv", header = T, sep = ",")

rowVars <- function (x,na.rm = TRUE) 
{
    sqr = function(x) x * x
    n = rowSums(!is.na(x))
    n[n <= 1] = NA
    return(rowSums(sqr(x - rowMeans(x,na.rm = na.rm)), na.rm = na.rm)/(n - 1))
  }

number.columns1 <- ncol(table1)
number.columns2 <- ncol(table2)
number.rows1 <- nrow(table1)
number.rows2 <- nrow(table2)

# Subject Wise Mean of Data1.csv
cat("\n")
i <- 2
j <- 1
mean.subject.data1 = vector(length = number.columns1-1)
while(i <= number.columns1)
{
	mean.subject.data1[j] <- mean(table1[[i]],na.rm = TRUE)
	print(paste("Mean of Subject [",i-1,"] in Data1.csv"))
	print(mean.subject.data1[j])
	i <- i+1
	j <- j+1
}

# Subject Wise Mean of Data2.csv
cat("\n")
i <- 2
j <- 1
mean.subject.data2 = vector(length = number.columns2-1)
while(i <= number.columns2)
{
	mean.subject.data2[j] <- mean(table2[[i]],na.rm = TRUE)
	print(paste("Mean of Subject [",i-1,"] in Data2.csv"))
	print(mean.subject.data2[j])
	i <- i+1
	j <- j+1
}

# Student Wise Mean Data1.csv
cat("\n")
i <- 1
j <- 1
mean.student.data1 = vector(length = number.rows1-1)
while(i <= number.rows1)
{
	mean.student.data1[j] <- rowMeans(table1[i,2:number.columns1],na.rm = TRUE)
	print(paste("Mean of Student [",i,"] in Data1.csv"))
	print(mean.student.data1[j])
	i <- i+1
	j <- j+1
}

# Student Wise Mean of Data2.csv
cat("\n")
i <- 1
j <- 1
mean.student.data2 = vector(length = number.rows2-1)
while(i <= number.rows2)
{
	mean.student.data2[j] <- rowMeans(table2[i,2:number.columns2],na.rm = TRUE)
	print(paste("Mean of Student [",i,"] in Data2.csv"))
	print(mean.student.data2[j])
	i <- i+1
	j <- j+1
}

# Subject Wise Variance of Data1.csv
cat("\n")
i <- 2
j <- 1
var.subject.data1 = vector(length = number.columns1-1)
while(i <= number.columns1)
{
	var.subject.data1[j] <- var(table1[[i]],na.rm = TRUE)
	print(paste("Variance of Subject [",i-1,"] in Data1.csv"))
	print(var.subject.data1[j])
	i <- i+1
	j <- j+1
}

# Subject Wise Variance of Data2.csv
cat("\n")
i <- 2
j <- 1
var.subject.data2 = vector(length = number.columns2-1)
while(i <= number.columns2)
{
	var.subject.data2[j] <- var(table2[[i]],na.rm = TRUE)
	print(paste("Variance of Subject [",i-1,"] in Data2.csv"))
	print(var.subject.data2[j])
	i <- i+1
	j <- j+1
}

# Student Wise Variance of Data1.csv
cat("\n")
i <- 1
j <- 1
var.student.data1 = vector(length = number.rows1-1)
while(i <= number.rows1)
{
	var.student.data1[j] <- rowVars(table1[i,2:number.columns1],na.rm = TRUE)
	print(paste("Variance of Student [",i,"] in Data1.csv"))
	print(var.student.data1[j])
	i <- i+1
	j <- j+1
}

# Student Wise Variance of Data2.csv
cat("\n")
i <- 1
j <- 1
var.student.data2 = vector(length = number.rows2-1)
while(i <= number.rows2)
{
	var.student.data2[j] <- rowVars(table2[i,2:number.columns2],na.rm = TRUE)
	print(paste("Variance of Student [",i,"] in Data2.csv"))
	print(var.student.data2[j])
	i <- i+1
	j <- j+1
}

# Subject Wise Standard Deviation of Data1.csv 
cat("\n")
j <- 1
sd.subject.data1 = vector(length = number.columns1-1)
while(j < number.columns1)
{
	sd.subject.data1[j] <- sqrt(var.subject.data1[j])
	print(paste("Standard Deviation of Subject [",j,"] in Data1.csv"))
	print(sd.subject.data1[j])
	j <- j+1
}

# Subject Wise Standard Deviation of Data2.csv
cat("\n")
j <- 1
sd.subject.data2 = vector(length = number.columns2-1)
while(j < number.columns2)
{
	sd.subject.data2[j] <- sqrt(var.subject.data2[j])
	print(paste("Standard Deviation of Subject [",j,"] in Data2.csv"))
	print(sd.subject.data2[j])
	j <- j+1
}

# Student Wise Standard Deviation of Data1.csv
cat("\n")
j <- 1
sd.student.data1 = vector(length = number.rows1-1)
while(j <= number.rows1)
{
	sd.student.data1[j] <- sqrt(var.student.data1[j])
	print(paste("Standard Deviation of Student [",j,"] in Data1.csv"))
	print(sd.student.data1[j])
	j <- j+1
}

# Student Wise Standard Deviation of Data2.csv
cat("\n")
j <- 1
sd.student.data2 = vector(length = number.rows2-1)
while(j <= number.rows2)
{
	sd.student.data2[j] <- sqrt(var.student.data2[j])
	print(paste("Standard Deviation of Student [",j,"] in Data2.csv"))
	print(sd.student.data2[j])
	j <- j+1
}

# Subject Wise Median of Mean Values in Data1.csv
cat("\n")
print("Median of mean values Subject wise in Data1.csv")
print(median(mean.subject.data1))

# Subject Wise Median of Mean Values in Data2.csv
cat("\n")
print("Median of mean values Subject wise in Data2.csv")
print(median(mean.subject.data2))

# Variance of Subject Wise Variance Values in Data1.csv
cat("\n")
print("Variance of Subject Wise Variance Values in Data1.csv")
print(var(var.subject.data1))

# Variance of Student Wise Variance Values in Data1.csv
cat("\n")
print("Variance of Student Wise Variance Values in Data1.csv")
print(var(var.student.data1))

# Variance of Subject Wise Variance Values in Data2.csv
cat("\n")
print("Variance of Subject Wise Variance Values in Data2.csv")
print(var(var.subject.data2))

# Variance of Student Wise Variance Values in Data2.csv
cat("\n")
print("Variance of Student Wise Variance Values in Data2.csv")
print(var(var.student.data2))

cat("\n")
print("Since Variance of Variance Subject wise and Student wise of Data1.csv is least. Hence it is more consistent")
