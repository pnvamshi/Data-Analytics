# P.N. Vamshi (13EC10044), Manognya Deepthi G (13IM10008), Nikhil Kashyap (13EE10033), Mukesh Sahani (13EC10039), Manoj Meena (13EE10028)
# Data Analysis (ASSIGNMENT 2)
# Group: DA-04

# Function to check a given number is integer or not
check.integer <- function(N)
{
    !grepl("[^[:digit:]]", format(N,  digits = 20, scientific = FALSE))
}

t <- 1

rowVars <- function (x,na.rm = TRUE) 
{
    sqr = function(x) x * x
    n = rowSums(!is.na(x))
    n[n <= 1] = NA
    return(rowSums(sqr(x - rowMeans(x,na.rm = na.rm)), na.rm = na.rm)/(n - 1))
}

while(t)
{
	n <- readline(prompt="Enter sample size: ")
	n <- as.numeric(n)
	if (check.integer(n) && n>=0)
		t <- 0
}

# Reading number of trials for Binomial Distribution
t <- 1
while(t)
{
	s <- readline(prompt="Enter number of trials: ")
	s <- as.numeric(s)
	if (check.integer(s) && s>=0)
		t <- 0
}

p <- readline(prompt="Enter probability of success: ")
p <- as.numeric(p)

i <- 1
while(i <= 10000)
{
	seq1 <- dbinom(0:n, s, p)
	mat1 <- matrix(seq1, 10000, n, byrow = T)
	i <- i+1
}

cat("\n")
j <- 1
mean.row.mat1 = vector(length = n-1)
while(j <= n)
{
	mean.row.mat1[j] <- rowMeans(mat1[j,1:n, drop=FALSE],na.rm = TRUE)
	print(paste("Mean of Row [",j,"] is "))
	print(mean.row.mat1[j])
	j <- j+1
}

cat("\n")
j <- 1
var.row.mat1 = vector(length = n-1)
while(i <= n)
{
	var.row.mat1[j] <- rowVars(mat1,na.rm = TRUE)
	print(paste("Variance of Student [",j,"] is "))
	print(var.row.mat1[j])
	j <- j+1
}

#print(rowVars(mat1,na.rm = TRUE))