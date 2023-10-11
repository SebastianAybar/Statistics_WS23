library("ggplot2")


#the sum of 52.3, 74.8, 3.17
52.3 + 74.8 + 3.17
#the square root of 144
sqrt(144)
#the 10-based logarithm of 200 multiplied with sin of π/4
log10(200) * sin(pi/4)
#the cumulative sum of the numbers 1,3,18,20,2 
vec <- c(1, 2, 3, 4, 5)
cumsum(vec)
#find 10 numbers between 0 and 20 rounded to the nearest integer
#value (hint use the command sample() or a combination of the
#commands round() and runif()).
random_numbers <- runif(10, min = 0, max = 20)
rounded_numbers <- round(random_numbers)
random_numbers_with_sample <- sample(0:20, 10, replace = TRUE)
#Assign the number 5 to x and the number 10 to y
x <- 5
y <- 10

# Assign the number 5 to x and the number 10 to y.
# Calculate the product of x and y.
# Store the result in a new variable z.
# Inspect your workspace by clicking the “environment” tab in RStudio, and find the three objects.
# Make a vector myvec of the objects x,y,z.
# Find the minimum, the maximum and the mean of the vector.
# Remove myvec from the workspace.
z <- x * y
myvec <- c(x, y, z)
min_myvec <- min(myvec)
max_myvec <- max(myvec)
mean_myvec <- mean(myvec)

#The numbers below are the first ten days of rainfall in a year
#0.1 0.5 2.3 1.1 11.3 14.7 23.4 15.7 0 0.9
#Read them into a vector using the c() command.
rainfall_vec <- c(0.1, 0.5, 2.3, 1.1, 11.3, 14.7, 23.4, 15.7, 0, 0.9)
#Calulate the mean and the standard deviation.
rainfall_mean <- mean(rainfall_vec)
rainfall_sd <- sd(rainfall_vec)
#Calculate the cumulative rainfall over these ten days. What is total sum of the rainfall?
rainfall_sum <- sum(rainfall_vec)
#Which day saw the highest rainfall? Find an appropriate R command.
rainfall_max <- max(rainfall_vec)
#Take a subset of the rainfall data where rain is larger than 10.
rainfall_subset <- rainfall_vec[rainfall_vec>10]
#What is mean rainfall for days where the rainfall was at least 5?
rainfall_atleast_five <- rainfall_vec[rainfall_vec<=5]
mean_of_rainfall_atleast_five <- mean(rainfall_atleast_five)
#Subset the vector where it is either exactly 0 or 1.1 and find the corresponding days.
rainfall_subset_2 <- which(rainfall_vec==0 | rainfall_vec==1.1)

#Aufgabe 4
#Read these vectors into two vectors with appropriate names.
cylinder_length_vec <- c(2.5, 3.4, 4.8, 3.1, 1.7)
cylinder_diameters_vec <- c(0.7, 0.4, 0.5, 0.5, 0.9)
#Volumen = pi * r2 * h
cylinder_volumes_vec<-c()
for (i in 1:length(cylinder_length_vec)) {
  cylinder_volumes_vec[i] <- cylinder_length_vec[i]*pi*((cylinder_diameters_vec[i]/2)^2)
}
#Recalculate to cubic milimeters
cylinder_volumes_cubic_milimeters_vec <- cylinder_volumes_vec * 1000

#Aufgabe 5
x <- c(1,2,3,4,5)
y <- c(3,5,7,9)
union <- union(x, y)
intersection <- intersect(x, y)
x_without_y <- setdiff(x,y)
print(x_without_y)
y_without_x <- setdiff(y, x)
print(x_without_y)
vec <- c(x, y)

#Aufgabe 6
first_row_vec <- seq(from=0, to=18, by=2)
random_number_vec <- as.integer(runif(70, min=0, max=10))
matrix <- matrix(data=c(first_row_vec, random_number_vec), byrow=TRUE, nrow=8, ncol=10)
row_means <- as.integer(rowMeans(matrix))
sd_rows <- sd(matrix)
new_matrix = matrix[2:8, 1:10]
col_means <- colMeans(new_matrix)
hist(col_means)

#Aufgabe 7
dataset <- str(mpg)

#Aufgabe 8
liste_1 <- list(vater="John", mutter="Mary", kinder_alter=c(4, 6, 10))
liste_2 <- list(kind_1_name="Bob", kind_2_name="Cate", kind_3_name="Susan")
list_concatinate_1 <- c(liste_1, liste_2)
list_concatinate_2 <- list(liste_1, liste_2)


# Neue Liste erstellen
liste_kinder <- list()

# Schleife, um Namen und Alter hinzuzufügen
for (i in 1:length(liste_2)) {
  kind_name <- liste_2[[paste0("kind_", i, "_name")]]
  kind_alter <- liste_1[["kinder_alter"]][i]
  liste_kinder[[i]] <- list(name = kind_name, alter = kind_alter)
}













