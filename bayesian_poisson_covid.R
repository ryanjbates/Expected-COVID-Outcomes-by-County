library(tidyverse)
library(rjags)
library(mapproj)
library(maps)
library(ggmap)
raw_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
counties = raw_data |>
  group_by(county_name, state_name) |>
  summarise(covid_19_confirmed_cases = sum(covid_19_confirmed_cases), 
            covid_19_deaths = sum(covid_19_deaths), total_population = total_population,
            area = area, population_density = population_density,
            houses_density = houses_density, less_than_high_school_diploma = less_than_high_school_diploma, 
            high_school_diploma_only = high_school_diploma_only, some_college_or_higher = some_college_or_higher, total_college_population = total_college_population,
            percent_smokers = percent_smokers, percent_diabetes = percent_diabetes, median_household_income = median_household_income, percent_insured = percent_insured,
            gdp_per_capita = gdp_per_capita, latitude = latitude, longitude = longitude, county_fips = county_fips, infants = age_0_4, children = age_5_9 + age_10_14, teens = age_15_19,
            yadults = age_20_24, adults = age_25_29+age_30_34+age_35_39+age_40_44, middle_aged = age_45_49+age_50_54+age_55_59, elderly = age_60_64+age_65_69+age_70_74, seniors = age_75_79
            +age_80_84+age_85_or_higher
        ) |>
  ungroup()
d = duplicated(counties)

counties = counties |>
  mutate(t = d)
counties = counties |>
  filter(t == FALSE) |>
  mutate(infection_rate = covid_19_confirmed_cases/total_population) |>
  drop_na() 

counties2 = counties |>
  mutate(across(c(7:17,21:28), ~ .x-mean(.x)))
  

cat('
model {
  # Likelihood
  for (i in 1:N) {
    y[i] ~ dpois(mu[i])
    
    log(mu[i]) <- b0 +  X[i,]%*%b[] + log(Pop[i])
  }
  
  # Prior on constant term
  b0 ~ dnorm(0,0.01)
  
  # noninformative priors on betas 
  for (j in 1:p) {
    b[j] ~ dnorm(0,.001)  
  }
  
  
  
}', file = {covid = tempfile()})


X = as.matrix(counties2[ , c(7,12:15,21:28)])



cdata = list(y = counties$covid_19_confirmed_cases,
             X = X,
             Pop = counties$total_population,
             N = nrow(counties),
             p = ncol(X))

m <- jags.model(covid, cdata, n.chains=2)

x <- coda.samples(m, c("b0","b"), n.iter=2000)

gelman.diag(x)
summary(x)




counties2 = counties2 |>
  mutate(predicted_cases = exp(-4.035 + 0.000001882*population_density-.06332*total_college_population
                               +.006348*percent_smokers+.00167*percent_diabetes
                               -.000003660*median_household_income+.01660*infants
                               -.01692*children+.06958*teens-.04402*yadults+.03380*adults
                               -.08695*middle_aged-.03049*elderly+.03772*seniors+ log(total_population)))
counties2 = counties2 |>
  mutate(cases_above_expected = ((covid_19_confirmed_cases-predicted_cases)/total_population)*10000)
X2 = as.matrix(counties2[, c(13:15, 21:28)])

l2data = list(y = counties$covid_19_deaths,
              X = X2,
              Pop = counties$covid_19_confirmed_cases,
              N = nrow(counties),
              p = ncol(X2))

m2 <- jags.model(covid, l2data, n.chains=2)

x2 <- coda.samples(m2, c("b0","b"), n.iter=2000)
gelman.diag(x2)
summary(x2)



counties2 = counties2 |>
  mutate(predicted_deaths =  exp(-3.557 -.03491*percent_smokers 
                                 +.04714*percent_diabetes +.000001885*median_household_income
                                 -.01093*infants -.006550*children +.0759*teens - .07569*yadults
                                 +.04114*adults+.02398*middle_aged-.05843*elderly+.08813*seniors +log(covid_19_confirmed_cases)))
counties2 = counties2 |>
  mutate(deaths_above_expected = ((covid_19_deaths - predicted_deaths)/covid_19_confirmed_cases)*10000)




county <- map_data("county")
c = sapply(strsplit(county.fips$polyname, ","), "[", 1)
cc = sapply(strsplit(county.fips$polyname, ","), "[", 2)
county2 = county.fips |>
  mutate(region = c) |>
  mutate(subregion = cc)
   

locations = county |>
  left_join(county2, by = c("region", "subregion"), relationship = 'many-to-many') |>
  rename(county_fips = fips)

locations2 = counties2 |>
  right_join(locations, by = "county_fips")

ggplot(locations2, aes(x = long, y = lat, group = group, fill = cases_above_expected))  +
  geom_polygon() + 
  scale_fill_gradient2(low = "blue", high = "red",na.value = "white")+
  coord_map() +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()
        )

ggplot(locations2, aes(x = long, y = lat, group = group, fill = deaths_above_expected)) +
  geom_polygon() + 
  scale_fill_gradient2(low = "blue", high = "red", na.value = "white")+
  coord_map() +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()
  )



gelman.plot(x)


