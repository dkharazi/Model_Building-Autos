Overview

Over 370,000 used cars were scraped from ads with Scrapy by Ebay-Kleinanzeigen. The content of the data is in German, meaning some of the data may need to be translated. The original dataset has been uploaded to [Kaggle](https://www.kaggle.com/), but I've included both the training and testing datasets that were split from the original data on Kaggle. The training and testing datasets are used in this analysis, rather than the autos data found on Kaggle.

*Notice*: The fields 'lastSeen' and 'dateCreated' could be used to estimate how long a car will be at least online before it is sold.

Dataset: [Autos Data](https://www.kaggle.com/orgesleka/used-cars-database) [18 MB]

## Variable Descriptions

- `dateCrawled:` The date that the ad was created.
- `name:` The name of the car.
- `seller:` The type of seller: either a private seller or a dealer.
- `offerType:` The type of offer.
- `price:` The selling price of the car on the ad.
- `abtest:` The test taken.
- `vehicleType:` The type of vehicle.
- `yearOfRegistration:` The year that the car was first registered.
- `powerPS:` The power of the car in PS.
- `model:` The model of the car.
- `kilometer:` The distance that the car has traveled in kilometers.
- `monthOfRegistration:` The month that the car was first registered.
- `fuelType:` The type of fuel that the car requires.
- `brand:` The brand of car.
- `notRepairedDamage:` An indicator of whether the car is damaged or not.
- `dateCreated:` The date on which the ad was created.
- `nrOfPictures:` The number of pictures in the ad.
- `postalCode:` The postal code at which the car is located.
- `lastSeenOnline:` The date that the ad was last seen.
