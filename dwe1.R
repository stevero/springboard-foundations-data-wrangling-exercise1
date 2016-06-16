library('dplyr')

setwd("~/Documents/Springboard/Foundations of Data Science")

# standardize company names
cleanCompany <- function(company) {
  if (startsWith(company, 'p') | startsWith(company, 'P') | startsWith(company, 'f')) {
    return('phillips')
  } else if (startsWith(company, 'a') | startsWith(company, 'A')) {
    return('azko')
  } else if (startsWith(company, 'v') | startsWith(company, 'V')) {
    return('van houten')
  } else if (startsWith(company, 'u') | startsWith(company, 'U')) {
    return('unilever')
  } else {
    return(company)
  }
}

splitStr <- function(x, index) {
  unlist(strsplit(x, '-'))[index]
}

# return category from category code
addCategory <- function(code) {
  switch (code,
      p = 'Smartphone',
      v = 'TV',
      x = 'Laptop',
      q = 'Tablet',
      code
  )
}

# convert boolean to integer
boolToInteger <- function(bool) {
  if(bool) {
    1
  } else {
    0
  }
}

# 0. Load the data. Funny that an exercise in R would start with step 0 instead of 1.
refine_cleaned <- read.table("refine_original.csv", header=TRUE, sep=",")

# Create new variables
refine_cleaned <- mutate(refine_cleaned, product_code=Product.code...number, product_number=Product.code...number, category=product_code, full_address='')

# 1. Clean company names. Funny that an exercise in R would start with step 0 instead of 1.
refine_cleaned$company <- sapply(as.vector(refine_cleaned$company), cleanCompany)

# 2. Split the brilliantly named Product.code...number variable at the dash and create 2 new variables from the result
refine_cleaned$product_code <- sapply(as.vector(refine_cleaned$product_code), splitStr, 1)
refine_cleaned$product_number <- sapply(as.vector(refine_cleaned$product_number), splitStr, 2)

# 3. Interpret product code to get product category
refine_cleaned$category <- sapply(as.vector(refine_cleaned$product_code), addCategory)

# 4. Concatenate address fields
refine_cleaned$full_address <- paste(refine_cleaned$address, refine_cleaned$city, refine_cleaned$country, sep = ',')

# "5: Create dummy variables for company and product category", whatever that means. Using booleans instead of integers. Duh.
refine_cleaned <- mutate(refine_cleaned, company_philips=(company=='phillips'), company_akzo=(company=='azko'), company_van_houten=(company=='van houten'), company_unilever=(company=='unilever'))
refine_cleaned <- mutate(refine_cleaned, product_smartphone=(category=='Smartphone'), product_tv=(category=='TV'), product_laptop=(category=='Laptop'), product_tablet=(category=='Tablet'))
refine_cleaned$company_philips <- sapply(refine_cleaned$company_philips, boolToInteger)
refine_cleaned$company_akzo <- sapply(refine_cleaned$company_akzo, boolToInteger)
refine_cleaned$company_van_houten <- sapply(refine_cleaned$company_van_houten, boolToInteger)
refine_cleaned$company_unilever <- sapply(refine_cleaned$company_unilever, boolToInteger)
refine_cleaned$product_smartphone <- sapply(refine_cleaned$product_smartphone, boolToInteger)
refine_cleaned$product_tv <- sapply(refine_cleaned$product_tv, boolToInteger)
refine_cleaned$product_laptop <- sapply(refine_cleaned$product_laptop, boolToInteger)
refine_cleaned$product_tablet <- sapply(refine_cleaned$product_tablet, boolToInteger)

# Write it
write.table(refine_cleaned, file = 'refine_cleaned.csv', quote=FALSE, sep=',', row.names=FALSE)
