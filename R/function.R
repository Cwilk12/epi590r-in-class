#would never actually do this lol; we want to write funcitons for things that dont exist yet

new_mean <-function(x) {
	n<-length(x)
	mean_val<-sum(x)/n
	return(mean_val)
}

x<-c(10,15,25,30)

new_mean(x)
mean(x)



#square exercise
# start out with a number to test
x <- 3
# you'll want your function to return this number
x^2


#function must include whatever x represents; then squared_val; then return ()
square <- function(x) {
	squared_val<-x^2
	return(squared_val)

}
# test it out
square(x)
square(53)
53^2 # does this match? YESSSS


#proportion (x and multiplier are both arguments)
prop<-function(x, multiplier){
	n<-length(x)
	proportion_val<-sum(x)/n
	multiplied_val<-multiplier*proportion_val
	return(multiplied_val)

}

x<-c(0,1,1)
multiplier<-100
multiplier*sum(x)/length(x)


prop(c(1,1,1,0,0), multiplier=100)




#create a function that raises a number to any power


raise <- function(x, power=2) {
	raised_val<-x^power
	return(raised_val)
}


# test with
raise(x = 2, power = 4)
# should give you
2^4




# test
raise(x = 5, power = 2)
# should give you
5^2


raise(x=7, power=10)
square(5)


raise(x=5)



#ask about above and raise vs. raise 2



#need to load data

library(tidyverse) #for so many things
library(gtsummary) #for descriptive plots

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")

# read in raw data, replacing missing values with NA
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols)

# create factors for categorical variables
nlsy_cats <- nlsy |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))


logistic_model <- glm(glasses ~ eyesight_cat + sex_cat,
											data = nlsy_cats, family = binomial()
)
poisson_model <- glm(nsibs ~ eyesight_cat + sex_cat,
										 data = nlsy_cats, family = poisson()
)
logbinomial_model <- glm(glasses ~ eyesight_cat + sex_cat,
												 data = nlsy_cats, family = binomial(link = "log")
)


tbl_regression(
	poisson_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight"
	)
)

#robust standard error (regarding homoscedacticity--robust standard errors addresses this issue)

new_table_function <- function(model) {
	tbl_regression(
		model,
		exponentiate = TRUE,
		label = list(
			sex_cat ~ "Sex",
			eyesight_cat ~ "Eyesight"
		), tidy_fun = partial(tidy_robust, vcov = "HC1")
	)
}

#create the above as a function to make life easier and not have to right the tbl_regression
#one multiple times; any time you find yourself doing the same thing (ex. create smaller data sets with function)

new_table_function(logistic_model)
new_table_function(poisson_model)
new_table_function(logbinomial_model)


#copy from her example-analysis in quarto!!!!


