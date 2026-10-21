### LAB-3 
### PART-1
### CONTROL_STATEMENTS
### JEEVAN C
### DATA_104_INTRODUCTION TO R PROGRAM



rm(list = ls())

############################################################
# SETTINGS
# FALSE = paste whole script into console, uses default test values
# TRUE  = real prompts (run the saved file with source("file.R"))
############################################################
INTERACTIVE <- FALSE

ask <- function(prompt, default) {
  if (INTERACTIVE) {
    return(as.integer(readline(prompt)))
  }
  cat(prompt, default, "\n")
  return(as.integer(default))
}

is_prime <- function(n) {
  if (is.na(n) || n < 2) return(FALSE)
  if (n < 4) return(TRUE)          # 2 and 3
  if (n %% 2 == 0) return(FALSE)
  if (n < 9) return(TRUE)          # 5 and 7
  for (i in 3:floor(sqrt(n))) {
    if (n %% i == 0) return(FALSE)
  }
  return(TRUE)
}

# Basic variables
x <- 10
y <- 20
result <- sum(x, y)
print(result)


############################################################
# 1. Check Whether a Number is Odd or Even
############################################################
num <- ask("Enter a number: ", 7)

if (num %% 2 == 0) {
  cat(num, "is an Even number\n")
} else {
  cat(num, "is an Odd number\n")
}


############################################################
# 2. Check Whether a Number is Positive, Negative, or Zero
############################################################
num <- ask("Enter a number: ", -4)

if (num > 0) {
  cat("Positive number\n")
} else if (num < 0) {
  cat("Negative number\n")
} else {
  cat("Zero\n")
}


############################################################
# 3. Find the Largest of Two Numbers
############################################################
a <- ask("Enter first number: ", 15)
b <- ask("Enter second number: ", 42)

if (a > b) {
  cat(a, "is larger\n")
} else if (b > a) {
  cat(b, "is larger\n")
} else {
  cat("Both numbers are equal\n")
}


############################################################
# 4. Find the Largest of Three Numbers
############################################################
a <- ask("Enter first number: ", 12)
b <- ask("Enter second number: ", 99)
third <- ask("Enter third number: ", 57)

cat("Largest number is", max(a, b, third), "\n")


############################################################
# 5. Factorial of a Number (for loop)
############################################################
n <- ask("Enter a non-negative number: ", 5)
fact <- 1

if (n < 0) {
  cat("Factorial is not defined for negative numbers\n")
} else {
  if (n >= 2) {
    for (i in 2:n) {
      fact <- fact * i
    }
  }
  cat("Factorial of", n, "is", fact, "\n")
}


############################################################
# 6. Fibonacci Series (while loop)
############################################################
n <- ask("Enter number of terms: ", 10)

a <- 0
b <- 1
i <- 1

cat("Fibonacci series: ")

while (i <= n) {
  cat(a, " ")
  temp <- a + b
  a <- b
  b <- temp
  i <- i + 1
}
cat("\n")


############################################################
# 7. Sum of First n Natural Numbers (while loop)
############################################################
n <- ask("Enter a non-negative number: ", 10)
total <- 0
i <- 1

while (i <= n) {
  total <- total + i
  i <- i + 1
}

cat("Sum of first", n, "natural numbers is", total, "\n")


############################################################
# 8. Demonstration of break Statement
############################################################
cat("Numbers from 1 to 9 using break:\n")

for (i in 1:20) {
  if (i == 10) {
    break
  }
  cat(i, " ")
}
cat("\n")


############################################################
# 9. Demonstration of next Statement
############################################################
cat("Numbers from 1 to 15 skipping multiples of 3:\n")

for (i in 1:15) {
  if (i %% 3 == 0) {
    next
  }
  cat(i, " ")
}
cat("\n")


############################################################
# 10. Check Whether a Number is Prime
############################################################
num <- ask("Enter a number: ", 29)

if (is_prime(num)) {
  cat(num, "is a Prime number\n")
} else {
  cat(num, "is NOT a Prime number\n")
}


############################################################
# 11. Check Whether a Number is Composite
############################################################
num <- ask("Enter a number: ", 28)

if (num >= 4 && !is_prime(num)) {
  cat(num, "is a Composite number\n")
} else {
  cat(num, "is NOT a Composite number\n")
}


############################################################
# 12. Check Whether a Number is Palindrome
############################################################
num <- ask("Enter a non-negative number: ", 12321)
temp <- num
rev_num <- 0

while (temp > 0) {
  digit <- temp %% 10
  rev_num <- rev_num * 10 + digit
  temp <- temp %/% 10
}

if (num == rev_num) {
  cat(num, "is a Palindrome number\n")
} else {
  cat(num, "is NOT a Palindrome number\n")
}


############################################################
# 13. Check Whether a Number is Armstrong
############################################################
num <- ask("Enter a non-negative number: ", 153)
temp <- num
total <- 0
digits <- nchar(as.character(num))

while (temp > 0) {
  digit <- temp %% 10
  total <- total + digit^digits
  temp <- temp %/% 10
}

if (total == num) {
  cat(num, "is an Armstrong number\n")
} else {
  cat(num, "is NOT an Armstrong number\n")
}


############################################################
# 14. Check Whether a Number is Perfect
############################################################
num <- ask("Enter a positive number: ", 28)
total <- 0

if (num > 1) {
  for (i in 1:(num - 1)) {
    if (num %% i == 0) {
      total <- total + i
    }
  }
}

if (num > 0 && total == num) {
  cat(num, "is a Perfect number\n")
} else {
  cat(num, "is NOT a Perfect number\n")
}


############################################################
# 15. Print Prime Numbers in a Given Range
############################################################
start <- ask("Enter start value: ", 1)
end <- ask("Enter end value: ", 30)

cat("Prime numbers in the range:\n")

if (start <= end) {
  for (num in start:end) {
    if (is_prime(num)) {
      cat(num, " ")
    }
  }
}
cat("\n")


############################################################
# 16. Menu Driven Program (repeat + break)
############################################################
# In non-interactive mode the menu runs these scripted choices in order
menu_script <- c(1, 2, 3, 4)
menu_inputs <- c(17, 121, 8)   # number for option 1, option 2, terms for option 3
menu_step <- 0
input_step <- 0

repeat {
  cat("\nMenu\n")
  cat("1. Check Prime\n")
  cat("2. Check Palindrome\n")
  cat("3. Generate Fibonacci\n")
  cat("4. Exit\n")
  
  menu_step <- menu_step + 1
  choice <- ask("Enter your choice: ",
                if (menu_step <= length(menu_script)) menu_script[menu_step] else 4)
  
  if (is.na(choice)) {
    cat("Invalid choice\n")
    next
  }
  
  if (choice == 1) {
    input_step <- input_step + 1
    num <- ask("Enter number: ", menu_inputs[input_step])
    
    if (is_prime(num)) {
      cat("Prime number\n")
    } else {
      cat("Not Prime\n")
    }
    
  } else if (choice == 2) {
    input_step <- input_step + 1
    num <- ask("Enter a non-negative number: ", menu_inputs[input_step])
    temp <- num
    rev_num <- 0
    
    while (temp > 0) {
      digit <- temp %% 10
      rev_num <- rev_num * 10 + digit
      temp <- temp %/% 10
    }
    
    if (num == rev_num) {
      cat("Palindrome\n")
    } else {
      cat("Not Palindrome\n")
    }
    
  } else if (choice == 3) {
    input_step <- input_step + 1
    n <- ask("Enter number of terms: ", menu_inputs[input_step])
    a <- 0
    b <- 1
    
    for (i in seq_len(max(0, n))) {
      cat(a, " ")
      temp <- a + b
      a <- b
      b <- temp
    }
    cat("\n")
    
  } else if (choice == 4) {
    cat("Exiting program...\n")
    break
    
  } else {
    cat("Invalid choice\n")
  }
}


############################################################
# 17. Multiplication Tables from 1 to 10
############################################################
for (i in 1:10) {
  cat("\nMultiplication table of", i, "\n")
  
  for (j in 1:10) {
    cat(i, "x", j, "=", i * j, "\n")
  }
}


############################################################
# 18. User-Defined Function: Check Even or Odd
############################################################
check_even_odd <- function(num) {
  if (num %% 2 == 0) {
    return("Even number")
  } else {
    return("Odd number")
  }
}

n <- ask("Enter a number: ", 14)
result <- check_even_odd(n)
cat(n, "is an", result, "\n")


############################################################
# 19. Filter and Normalize mtcars Data
############################################################

# Filter cars with mpg greater than 25
filter_high_mpg <- function(data, threshold) {
  data[data$mpg > threshold, ]
}

print(filter_high_mpg(mtcars, 25))

# Normalize mpg values between 0 and 1
normalize <- function(x) {
  if (max(x) == min(x)) {
    return(rep(0, length(x)))
  }
  
  (x - min(x)) / (max(x) - min(x))
}

mtcars$mpg_norm <- normalize(mtcars$mpg)
print(head(mtcars))




rm(list = ls())

############################################################
# SETTINGS
# FALSE = paste whole script into console (uses default values)
# TRUE  = real prompts (run saved file with source("file.R"))
############################################################
INTERACTIVE <- FALSE

ask <- function(prompt, default) {
  if (INTERACTIVE) {
    return(as.integer(readline(prompt)))
  }
  cat(prompt, default, "\n")
  return(as.integer(default))
}


############################################################
# 1. Multiplication Table of a Number Entered by the User
############################################################
n <- ask("Enter a number: ", 7)

cat("Multiplication table of", n, "\n")
for (j in 1:10) {
  cat(n, "x", j, "=", n * j, "\n")
}


############################################################
# 2. Multiplication Tables from 1 to 10 (Nested Loop)
############################################################
for (i in 1:10) {                      # Outer loop -> tables (rows)
  cat("\nMultiplication of", i, "\n")
  for (j in 1:10) {                    # Inner loop -> columns
    cat(i, "x", j, "=", i * j, "\n")
  }
}


############################################################
# 3. User-Defined Function: Check Even or Odd
############################################################

# function_name <- function(parameters) {
#   function body
#   statements
#   return(value)   # optional
# }

# Define a function
check_even_odd <- function(num) {
  
  # Check condition
  if (num %% 2 == 0) {
    return("Even number")
  } else {
    return("Odd number")
  }
}

# Take input from the user
n <- ask("Enter a number: ", 14)

# Call the function and store the result
result <- check_even_odd(n)

# Display the result
cat(n, "is an", result, "\n")


############################################################
# 4. Filter Cars with Mileage Greater Than 25 (mtcars)
############################################################
filter_high_mpg <- function(data, threshold) {
  data[data$mpg > threshold, ]
}

# Cars with mileage greater than 25
print(filter_high_mpg(mtcars, 25))


############################################################
# 5. Normalize mpg Values Between 0 and 1 (mtcars)
############################################################
normalize <- function(x) {
  if (max(x) == min(x)) {
    return(rep(0, length(x)))
  }
  (x - min(x)) / (max(x) - min(x))
}

mtcars$mpg_norm <- normalize(mtcars$mpg)
print(head(mtcars))

