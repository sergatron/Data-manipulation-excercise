library(dplyr)
library(tidyr)
library(readr)
refine_original <- read_csv("refine_original.csv")

refine = as.data.frame(refine_original)
glimpse(refine)
head(refine)

# ------ CLEAN UP BRAND NAMES ------
# convert to lower case, and eliminate spelling errors
# philips, akzo, van houten, unilever
# use 'gsub' to find pattern at every instance and replace with correct spelling
company_lower = tolower(refine$company)
philips_clean = gsub(pattern = '^p.*ps$|^fi.*ps$', replacement = 'philips', x = company_lower) 
akzo_clean =  gsub(pattern = '^ak.*o$|^ak.*0$', replacement = 'akzo', x = philips_clean)
unilever_clean =  gsub(pattern = '^uni.*ver$', replacement = 'unilever', x = akzo_clean)
company_clean =  gsub(pattern = '^van.*ten$', replacement = 'van houten', x = unilever_clean)
company_clean


# ------ PRODUCT CODE AND NUMBERS ------
# seperate code and number into seperate columns
# add new columns: 'product_code' and 'product_number'  
code_number = refine %>% select(`Product code / number`)
code_number


# create new columns named 'product_code' and 'product_number'
# DPLYR method creates a list. Base R creates a character
name_code_col = separate(code_number, `Product code / number`, c('product_name', 'product_code'), sep = '-')
product_name = name_code_col$product_name
product_code = name_code_col$product_code

typeof(prod_name)
typeof(prod_code)

# add column for product categories
# p = Smartphone, v = TV, x = Laptop, q = Tablet
list_p = categories$product_name
p = gsub(pattern = 'p', replacement = 'Smartphone', x = list_p)
v = gsub(pattern = 'v', replacement = 'TV', x = p)
x = gsub(pattern = 'x', replacement = 'Laptop', x = v)
q = gsub(pattern = 'q', replacement = 'Tablet', x = x)

categories_done = name_code_col %>% mutate(product_category = q)
categories_done
product_category = categories_done$product_category

class(prod_cat)
typeof(prod_cat)

# ------ ADD FULL ADDRESS ------
# unite address, city, country 
head(refine)
address = unite(refine, 'full_address', address, city, country, sep = ', ')
full_address = address$full_address

country = refine$country
full_address_clean = gsub(pattern = 'th.*', replacement = 'netherlands', x = full_address)


typeof(address)
class(address)
class(full_address)

# ------ DUMMY VARIABLES ------
# add four binary columns for each company: company_philips, company_akzo, company_unilever, company_van_houten
# add four binary columns for each product category: product_smartphone, product_TV, product_laptop, product_tablet

company_philips = refine$company_philips = 0
company_akzo = refine$company_akzo = 0
company_unilever = refine$company_unilever = 0
company_van_houten = refine$company_van_houten = 0

product_smartphone = refine$product_smartphone = 0
product_TV = refine$product_TV = 0
product_laptop = refine$product_laptop = 0
product_tablet = refine$product_tablet = 0

glimpse(refine)

# ------ PUT IT ALL TOGETHER ------
# each componenet must be one-dimensional vector

refine_clean = data_frame(company_clean, product_name, product_code, product_category, full_address_clean, 
                          company_philips, company_akzo, company_unilever, company_van_houten,
                          product_smartphone, product_TV, product_laptop, product_tablet)

class(refine_clean)
glimpse(refine_clean)
head(refine_clean)
write_csv(refine_clean, "refine_clean.csv")
