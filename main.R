#############################
## PART 1 - DATA WRANGLING ##
#############################

# Set up the mongolite package.
install.packages("mongolite")
library(mongolite)

# Install Tidyverse
install.packages("tidyverse")
library(tidyverse)

# Use a connection_string to connect to MongoDB.
connection_string = "mongodb_connection_string"

# Load the data from MongoDB.
data = mongo(collection = "listingsAndReviews", db = "sample_airbnb", url = connection_string)

# Convert the data to a Tibble:
data_tb = as_tibble(data$aggregate())

# Remove unused fields
data_tb = select(data_tb, -last_scraped, -calendar_last_scraped, -name, -summary,
                 -space, -description, -neighborhood_overview, -notes, -transit,
                 -access, -interaction, -house_rules)


# Unnest the review_scores, and remove the reviews.
# (Reviews contain the comments left by a user.)
data_tb = select(add_column(data_tb,
                            transmute(data_tb,
                                      review_scores_accuracy = data_tb$review_scores$review_scores_accuracy, 
                                      review_scores_cleanliness = data_tb$review_scores$review_scores_cleanliness, 
                                      review_scores_checkin = data_tb$review_scores$review_scores_checkin,
                                      review_scores_communication = data_tb$review_scores$review_scores_communication,
                                      review_scores_location = data_tb$review_scores$review_scores_location,
                                      review_scores_value = data_tb$review_scores$review_scores_value,
                                      review_scores_rating = data_tb$review_scores$review_scores_rating)),
                 -review_scores, -reviews)

# Remove images from the dataset.
data_tb = select(data_tb, -images)

# Unnest the availability.
data_tb = select(
  add_column(data_tb,
             transmute(data_tb, 
                       availability_30 = data_tb$availability$availability_30, 
                       availability_60 = data_tb$availability$availability_60, 
                       availability_90 = data_tb$availability$availability_90, 
                       availability_365 = data_tb$availability$availability_365)),
  -availability)

# Unnest the host data. (Note, this discards some data.)
data_tb = select(
  add_column(data_tb,
             transmute(data_tb,
                       host_response_time=data_tb$host$host_response_time,
                       host_response_rate=data_tb$host$host_response_rate,
                       host_is_superhost=data_tb$host$host_is_superhost,
                       host_has_profile_pic=data_tb$host$host_has_profile_pic,
                       host_identity_verified=data_tb$host$host_identity_verified,
                       host_listings_count=data_tb$host$host_listings_count,
                       host_total_listings_count=data_tb$host$host_total_listings_count,
             )),
  -host
)

# Unnest address information. (Note, this discards some data.)
data_tb = select(
  add_column(data_tb,
             transmute(data_tb,
                       address_country_code=data_tb$address$country_code,
                       address_location_coordinates=data_tb$address$location$coordinates,
                       address_location_is_location_exact=data_tb$address$location$is_location_exact
             )),
  -address
)

# Save the data to a file to be loaded later.
write_tsv(data_tb, file="data.tsv")

##################################
## PART 2 - DATA TRANSFORMATION ##
##################################

# Because we have removed the unnecessary columns, and unnested the data in the step above,
# and due to the fact that this sample dataset is already ready for analysis, no cleaning is required.


#################################
## PART 3 - DATA VISUALISATION ##
#################################

# The required libraries for this section:

install.packages('tidyverse')
install.packages("caret")
install.packages("mlr")

library(tidyverse)
library(caret)
library(mlr)

# Load the data
data = read_tsv('data.tsv')
View(data)


# Figure 2 - Histogram showing distribution of review_score_raiting
ggplot( data = data, 
        aes( x    = review_scores_rating, 
             fill = "Histogram") ) +
        geom_histogram( color = "black", 
                        fill  = "lightblue", 
                        bins  = 40, 
                        na.rm = TRUE ) +
        labs(fill = "") +
        theme_minimal() +
        labs( title="review_scores_rating histogram" )



#-------------------------------------
# review score types correlation
#-------------------------------------

#review score subtypes: cleanliness, checkin, communication, location, value


# cleanliness

# statistic from table1

cor( data$review_scores_rating, 
     data$review_scores_cleanliness,
     method = "pearson",
     use = "complete.obs" )


# checkin

# statistic from table1

cor( data$review_scores_rating, 
     data$review_scores_checkin,
     method = "pearson",
     use = "complete.obs" )


# communication

# statistic from table1

cor( data$review_scores_rating, 
     data$review_scores_communication,
     method = "pearson",
     use = "complete.obs" )



# location

# statistic from table1

cor( data$review_scores_rating, 
     data$review_scores_location,
     method = "pearson",
     use = "complete.obs" )

# cleanliness has the highest correlation coefficient.



#-------------------------------------
# Exploring other variables
#-------------------------------------


# review score vs host identity verified

# Figure 3
qplot( review_scores_rating, 
       number_of_reviews, 
       data  = data, 
       color = host_identity_verified ) +
       labs( title="number_of_reviews VS review_score_rating scatterplot with host_identity_verified colorcoding" )


# Country codes vs review score rating

# Figure 4
install.packages('ggridges')
library( ggridges )

ggplot( data, 
        aes( x    = review_scores_rating, 
             y    = address_country_code, 
             fill = 0.5 - abs( 0.5 - stat( ecdf ) ) 
            ) ) +
        stat_density_ridges( geom       = "density_ridges_gradient", 
                             calc_ecdf  = TRUE ) +
        scale_fill_viridis_c( name      = "Tail probability", 
                              direction = -1 ) + 
        labs( title="address_country_codes vs review_scores_rating" )



# review score vs property type

# Figure 5
ggplot( data, 
        aes( x    = review_scores_rating, 
             y    = reorder( property_type,
                             review_scores_rating,
                             na.rm = TRUE ), 
             fill = property_type ) ) + 
  geom_boxplot() + 
  labs( title = "property type vs review scores rating",
        y = "property_type" )+
  theme(legend.position="none" )


# Figure 6
ggplot( data, aes( y    = reorder( property_type,
                                   review_scores_rating,
                                   FUN   = sum,
                                   na.rm = TRUE ),
                   fill = property_type ) ) +
        geom_bar() +
        guides(fill = "none") +
        labs( title = "property_type bar graph",
                  x = "count",
                  y = "property_type")



# review score vs response times

# Figure 7
ggplot( data, aes( x    = reorder( host_response_time,
                                   review_scores_rating,
                                   FUN   = sum,
                                   na.rm = TRUE ),
                   fill = host_response_time ) ) +
  geom_bar() +
  guides(fill = "none") +
  labs( title = "host_response_time bar graph",
        x = "" )

  
# Figure 8
qplot( review_scores_rating, host_response_time, 
       data  = data ) +
  labs( title = "review_scores_rating vs response time",
        x = "review_scores_rating",
        y = "response_time")


# Figure 9
ggplot( data , aes( x = review_scores_rating, 
                    y = host_response_time, 
                    fill = number_of_reviews)) +
  geom_tile() +
  scale_fill_distiller( palette   = "Reds", 
                        direction = 1 ) + 
  labs( title = "host_response_time heatmap" )


install.packages( "hrbrthemes" )
library(hrbrthemes)


# Figure 10
data %>%
  mutate( reviewRating = cut( review_scores_rating, 
                              c( 50, 60, 70, 80, 90 ) 
                            ), 
          numberOfReviews = cut( number_of_reviews, 
                                 breaks = c( 0, 5, 50, 100, 200, 400, 600 ) 
                                )
        ) %>%
  ggplot( aes( reviewRating,
               host_response_time, 
               fill = numberOfReviews ) ) + 
  geom_tile() +
  scale_fill_manual( drop     = FALSE, 
                     values   = colorRampPalette( c( "white","red" ) )( 6 ), 
                     na.value ="#EEEEEE", 
                     name   = "numberOfReviews" ) +
  labs( title = "host_response_time heatmap",
        y     = "response time",
        x     = "review_scores_rating" )



#-------------------------------------
# prediction model for price vs rating
#-------------------------------------

install.packages('factoextra')
install.packages('cluster')
install.packages('tidyverse')
install.packages("caret")
install.packages("mlr")
install.packages('Amelia')
install.packages('ISLR')
install.packages('Hmisc')

library(Hmisc)
library(tidyverse)
library(ISLR)
library(caret)
library(mlr)
library(Amelia)

data = read_tsv('data.tsv')

data2 <- within(data, rm("first_review", "last_review", "address_country_code", "address_location_coordinates", "address_location_is_location_exact", "_id", "listing_url", "host_is_superhost", "host_has_profile_pic", "host_listings_count", "host_total_listings_count", "host_identity_verified"))
data2 <- within(data2, rm("property_type", "room_type", "bed_type", "minimum_nights", "maximum_nights", "accommodates", "bedrooms", "beds", "bathrooms", "amenities", "extra_people", "guests_included", "weekly_price", "monthly_price"))
data2 <- within(data2, rm("cancellation_policy", "security_deposit", "cleaning_fee"))
data2 <- within(data2, rm("reviews_per_month"))
data2 <- within(data2, rm("number_of_reviews", "availability_30", "availability_60", "availability_90", "host_response_time", "host_response_rate", "availability_365"))


#summary(data)
#describe(data)
#nrow(data)

options(scipen=999)

# Load the data

# Define the target variable column name
target_col <- "price"
set.seed(1)

sample <- sample(c(TRUE, FALSE), nrow(data2), replace = TRUE, prob=c(0.7,0.3))
train <- data2[sample, ]
test <- data2[!sample, ]

train <- na.omit(train)
test <- na.omit(test)

model <- lm(review_scores_value~price, data=train)
summary(model)

predicted = predict(model, test, type="response")

plot(x = train$review_scores_value,
     y = model$fitted.values,
     xlab = "true values",
     ylab = "fitted values",
     main = "chart")
abline(b = 1, a = 0)

print(cbind(train, model$fitted.values))

install.packages("splines")
library("splines")


qplot( review_scores_value, 
       price, 
       data    = data,
       geom    = c( "point", "smooth" ),
       method  = "lm",
       #formula = y ~ poly( x, 2 ),
       #se = FALSE,
       ylim    = c( 0, 4000 ) )




#-------------------------------------
# model for price vs lease contents 
#-------------------------------------


# Define the target variable column name
target_col <- "price"

# Define the predictor column names
predictor_cols <- c(data$beds, data$bedrooms, data$bathrooms)

# Split the data into training and testing sets
set.seed(123)

inTraining <- createDataPartition(predictor_cols, p = .80, list = FALSE)
training <- Sonar[ inTraining,]
testing  <- Sonar[-inTraining,]


train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Create the formula for modeling
formula <- as.formula(paste(target_col, paste(predictor_cols, collapse = " + "), sep = " ~ "))

# Train the model
model <- train(formula, data = train_data, method = "rf")

# Make predictions on the test data
predictions <- predict(model, newdata = test_data)

# Evaluate the model
performance <- confusionMatrix(predictions, test_data[[target_col]])
print(performance)

#####################################
## PART 4A - UNSUPERVISED LEARNING ##
#####################################

# The required libraries for this section:
library(Hmisc)
library(tidyverse)
library(ISLR)
library(caret)
library(mlr)
library(Amelia)
library(ClusterR)
library(factoextra)
library(ggplot2)

# Read the data from part 1.
data = read_tsv('data.tsv')

#Finding the correlation between the three variables. Was discovered that 
#minimum nights has little to no bearing on other variables

#select(data, c(price, minimum_nights, number_of_reviews)) |>
#  cor(use = "pairwise.complete.obs") |>
#  round(2)


#Plotting data to see correlation - Was seen to be few high cost and many low 
#cost properties

#ggplot(data, aes(number_of_reviews, price, color = room_type, shape = room_type)) +
#  geom_point(alpha = 0.25) +
#  xlab("Number of reviews") +
#  ylab("Price")

#Normalising data

data[, c("price", "number_of_reviews")] = scale(data[, c("price", "number_of_reviews")])


#Making the model using price to number of reviews

airbnb_2cols <- data[, c("price", "number_of_reviews")]
set.seed(1337)
km.out <- kmeans(airbnb_2cols, centers = 3, nstart = 20)
km.out

#Determining the number of loops

n <- 10
wss <- numeric(n)

#Looping possible clusters n times
for (i in 1:n) {
  #Fitting the model.
  km.out <- kmeans(airbnb_2cols, centers = i, nstart = 20)
  #Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

#Producing a scree plot - doing an "elbow test" on the data
wss_df <- tibble(clusters = 1:10, wss = wss)

#Displaying scree plot
scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')
scree_plot

#Showing old graph with how the clusters have organised the info
data$cluster_id <- factor(km.out$cluster)
ggplot(data, aes(number_of_reviews, price, color = cluster_id)) +
  geom_point(alpha = 0.25) +
  xlab("Number of reviews") +
  ylab("Price")



###################################
## PART 4B - SUPERVISED LEARNING ##
###################################

# The required libraries for this section:
library(Hmisc)
library(tidyverse)
library(ISLR)
library(caret)
library(mlr)
library(Amelia)
library(ClusterR)
library(factoextra)

#Loading the data
data = read_tsv('data.tsv')


#Gathering information
#summary(data)
#describe(data)
#nrow(data)

#Removing scientific notation
options(scipen=999)

#Normalising data
data[, c("price", "number_of_reviews", "review_scores_rating")]= scale(data[, c("price", "number_of_reviews", "review_scores_rating")])



#data[, c("price", "number_of_reviews")] = scale(data[, c("price", "number_of_reviews")])
#airbnb_2cols <- data[, c("price", "number_of_reviews")]

#Setting seed
set.seed(1337)

#Splitting dataset into training and testing sets(70/30%)
sample <- sample(c(TRUE, FALSE), nrow(data), replace = TRUE, prob=c(0.7,0.3))
train <- data[sample, ]
test <- data[!sample, ]

#Training the model
model <- lm(number_of_reviews~price+review_scores_rating, data=train)
#summary(model)

#Showing the plots of the model (looking for bias)
par(mfrow=c(2,2))
plot(model)

#Creating final graph
price.graph <- ggplot(data, aes(x=number_of_reviews, y=price)) + geom_point()
price.graph <- price.graph + geom_smooth(method="lm", col="white")
price.graph + 
  theme_bw()  +
  labs(title = "Number of reviews in relation to price",
       x = "Number of reviews (0 to 10)",
       y = "price x 10,000")


#Calculating Mean Squared Error score
mean(model$residuals^2)
