# P.N. Vamshi (13EC10044), Manognya Deepthi G (13IM10008), Nikhil Kashyap (13EE10033), Mukesh Sahani (13EC10039), Manoj Meena (13EE10028)
# Data Analysis (ASSIGNMENT 2)
# Group: DA-04
# Poisson Distribution

# Function to check a given number is integer or not
check.integer <- function(N)
{
    !grepl("[^[:digit:]]", format(N,  digits = 20, scientific = FALSE))
}

# Function to calculate variance row wise
rowVars <- function (x,na.rm = TRUE) 
{
    sqr = function(x) x * x
    n = rowSums(!is.na(x))
    n[n <= 1] = NA
    return(rowSums(sqr(x - rowMeans(x,na.rm = na.rm)), na.rm = na.rm)/(n - 1))
}

# Reading sample size for Poisson Distribution
t <- 1
while(t)
{
	n <- readline(prompt="Enter sample size: ")
	n <- as.numeric(n)
	if (check.integer(n) && n>=0)
		t <- 0
}

# Reading mean/variance of for Poisson Distribution
t <- 1
while(t)
{
	lam <- readline(prompt="Enter Mean of the Normal Distribution: ")
	lam <- as.numeric(lam)
	if (lam>=0)
		t <- 0
}


i <- 1
while(i <= 10000)
{
	seq1 <- rpois(0:n, s)
	mat1 <- matrix(seq1, 10000, n, byrow = T)
	i <- i+1
}

poisson.mean <- matrix(rowMeans(mat1,na.rm = TRUE), 10000, n, byrow = T)

poisson.var <- matrix(rowVars(mat1, na.rm = TRUE), 10000, n, byrow = T)

hist(poisson.mean)
hist(poisson.var)