# ============================================
# R PROGRAMMING PRACTICAL
# Name: JEEVAN C
# College: GOLDEN GATE UNIVERSITY
# ============================================


# PROGRAM 1: Find the Larger of Two Numbers
# Explanation: Compare two numbers using if-else.

a <- 40
b <- 23

if (a < b) {
  print("a is smaller than b")
} else if (a > b) {
  print("a is greater than b")
} else {
  print("Both numbers are equal")
}


# PROGRAM 2: Check Positive, Negative, or Zero
# Explanation: Check the sign of a number.

num <- 10

if (num > 0) {
  print("The number is positive")
} else if (num < 0) {
  print("The number is negative")
} else {
  print("The number is zero")
}


# PROGRAM 3: Identify Even and Odd Numbers
# Explanation: Use ifelse() and the modulus operator.

v <- 1:10

ifelse(v %% 2 == 0, "Even", "Odd")


# PROGRAM 4: Print Names Longer Than Five Characters
# Explanation: Use a for loop and nchar().

var <- c("Samridhi", "Jeevan", "Gurneet",
         "Krishna", "Ram", "siya")

for (i in var) {
  if (nchar(i) > 5) {
    print(i)
  }
}


# PROGRAM 5: Print Numbers from 1 to 10
# Explanation: Use a while loop.

i <- 1

while (i <= 10) {
  print(i)
  i <- i + 1
}


# PROGRAM 6: Multiplication Table of 5
# Explanation: Print the table using a while loop.

n <- 5
i <- 1

while (i <= 10) {
  print(paste(n, "x", i, "=", n * i))
  i <- i + 1
}


# PROGRAM 7: Multiplication Table of a Given Number
# Explanation: Use a for loop to print the table.

No <- 5

for (i in 1:10) {
  print(paste(No, "x", i, "=", No * i))
}


# PROGRAM 8: Sum of the Series
# 1 + 1/2 + 1/3 + ... + 1/10
# Explanation: Calculate the sum using a for loop.

n <- 10
s <- 0

for (i in 1:n) {
  s <- s + 1 / i
}

print(s)


# PROGRAM 9: Sum of the Series Using a while Loop
# Explanation: Add each reciprocal until i reaches 10.

n <- 10
i <- 1
total <- 0

while (i <= n) {
  total <- total + 1 / i
  i <- i + 1
}

print(total)


# PROGRAM 10: Reverse the Digits of a Number
# Explanation: Use %% to extract digits and %/% to
# remove the last digit.

reverse_number <- function(num) {
  num <- abs(as.integer(num))
  reversed <- 0
  
  while (num > 0) {
    digit <- num %% 10
    reversed <- reversed * 10 + digit
    num <- num %/% 10
  }
  
  return(reversed)
}

num <- 12345

print(reverse_number(num))


# PROGRAM 11: Modulus Operator
# Explanation: Find the remainder after division.

print(15 %% 4)
print(15 %% 2)

