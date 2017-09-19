library(dplyr)
library(tidyr)
library(readr)
refine_original <- read_csv("refine_original.csv")


refine = tbl_df(refine_original)
refine = as.data.frame(refine_original)
glimpse(refine)
head(refine)

# CLEAN DATA  
# create new data frame to store clean data
# data_frame(company_clean, )


# Show company names
# BASE R METHOD
company_lower = tolower(refine$company)
class(company_lower)
# ------ CLEAN UP BRAND NAMES ------
# convert to lower case, and eliminate spelling errors
# philips, akzo, van houten, unilever
# DPLYR METHOD
company_low = refine %>%
  select(company) %>%
  tolower

class(company_lower)

# ------ PHILIPS ------
# find all 'philips' spelling types
# use 'grep' and 'gsub' to correct spelling and replace with corrected name
philips_vect = c(grep(pattern = '^ph|^f', x = company_lower))
philips_dirty = company_lower[philips_vect]
philips_dirty

mode(philips_dirty)
class(philips_dirty)

# correct 'philips' company name
# search for pattern, replace with correct name spelling within the company_lower vector

philips_clean = gsub(pattern = '^ph.*ps$|^fi.*ps$', replacement = 'philips', x = philips_dirty)
philips_clean

mode(philips_clean)

# ------ AKZO ------
# find all 'akzo' spelling types
akzo_vect = c(grep(pattern = '^ak', x = company_lower))
akzo_dirty = company_lower[akzo_vect]
akzo_dirty
# correct 'akzo' company name
# 'x' can also be replaced with 'philips_clean' and then passed down
akzo_clean =  gsub(pattern = '^ak.*o$|^ak.*0$', replacement = 'akzo', x = philips_clean)


# ------ UNILEVER ------
# find all 'unilever' spelling types
uni_vect = c(grep(pattern = '^uni', x = company_lower))
uni_dirty = company_lower[uni_vect]
uni_dirty
# correct the spelling
unilever_clean =  gsub(pattern = '^uni.*ver$', replacement = 'unilever', x = uni_dirty)
typeof(unilever_clean)

# ------ VAN HOUTEN ------
# find all 'van houten' spelling types
vanhouten_vect = c(grep(pattern = '^van', x = company_lower))
vanhouten_dirty = company_lower[vanhouten_vect]
vanhouten_dirty
# correct the spelling
vanhouten_clean =  gsub(pattern = '^van.*ten$', replacement = 'van houten', x = vanhouten_dirty)
typeof(vanhouten_clean)

# combine all clean company names into single column


# ------ PRODUCT CODE AND NUMBERS ------
# seperate code and number into seperate columns
# add new columns: 'product_code' and 'product_number'  
code_number = refine %>% select(`Product code / number`)
code_number
# unlist before splitting
un = unlist(code_number)
# split apart the product from number
splt_code_num = strsplit(as.character(un), '-')
mode(splt_code_num)
# check to see if it worked
splt_code_num[2]
splt_code_num[[2]][2]
splt_code_num[[2]][1]

# splt_code_num %>% select()

# create new columns named 'product_code' and 'product_number'
# DPLYR method creates a list. Base R creates a character
name_code_col = separate(code_number, `Product code / number`, c('product_name', 'product_code'), sep = '-')
prod_name = name_code_col %>% select(product_name)
# selecting a specific row after using DPLYR will result in error
prod_name[2]
prod_code = name_code_col$product_code
prod_code[2]

typeof(prod_name)
typeof(prod_code)
# add column for product categories
# p = Smartphone, v = TV, x = Laptop, q = Tablet
# categories = name_code_col %>% group_by(product_name) %>% mutate(prod_cat = prod_name[])

# typeof(categories)
# mutate(name_code_col, prod_cat = prod_name[])

list_p = categories$product_name
p = gsub(pattern = 'p', replacement = 'Smartphone', x = list_p)
v = gsub(pattern = 'v', replacement = 'TV', x = p)
x = gsub(pattern = 'x', replacement = 'Laptop', x = v)
q = gsub(pattern = 'q', replacement = 'Tablet', x = x)

categories_done = name_code_col %>% mutate(product_category = q)
categories_done
prod_cat = categories_done$product_category

class(prod_cat)
typeof(prod_cat)


# ------ ADD FULL ADDRESS ------
# unite address, city, country 
# 
head(refine)
address = unite(refine, 'full_address', address, city, country, sep = ', ')
full_address = address$full_address

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

refine_clean = data_frame(company_clean, prod_name, prod_code, prod_cat, full_address, 
                   company_philips, company_akzo, company_unilever, company_van_houten,
                   product_smartphone, product_TV, product_laptop, product_tablet)
class(clean)
glimpse(clean)
write_csv(refine_clean, "refine_clean.csv")

