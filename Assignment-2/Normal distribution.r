# P.N. Vamshi (13EC10044), Manognya Deepthi G (13IM10008), Nikhil Kashyap (13EE10033), Mukesh Sahani (13EC10039), Manoj Meena (13EE10028)
# Data Analysis (ASSIGNMENT 2)
# Group: DA-04
# Normal Distribution

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

# Reading sample size for Normal Distribution
t <- 1
while(t)
{
	n <- readline(prompt="Enter sample size: ")
	n <- as.numeric(n)
	if (check.integer(n) && n>=0)
		t <- 0
}

# Reading mean of Normal Distribution
t <- 1
while(t)
{
	s <- readline(prompt="Enter Mean of the Normal Distribution: ")
	s <- as.numeric(s)
	if (s>=0)
		t <- 0
}

# Reading probability of success for Normal Distribution
t <- 1
while(t)
{
	p <- readline(prompt="Enter Standard Deviation of Distribution: ")
	p <- as.numeric(p)
	if (p>=0)
		t <- 0
}


seq1 <- rnorm(n*10000, s, p)
mat1 <- matrix(seq1, 10000, n, byrow = T)


normal.mean <- matrix(rowMeans(mat1,na.rm = TRUE), 10000, n, byrow = T)

normal.var <- matrix(rowVars(mat1, na.rm = TRUE), 10000, n, byrow = T)

hist(normal.mean)
hist(normal.var)
