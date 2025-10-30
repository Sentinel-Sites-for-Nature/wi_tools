## Distribute WI computer-vision classified images to individuals (students) for verification

## Load necessary packages & image distributor function
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
source("WI_Image_Distributor/WI_Image_Distributor_Function.R")

## Read in the table of images from wildlife insights
## When exporting data from wildlife insights, be sure to include ALL data (especially images in the "identify" tab)
images <- read_csv("WI_Image_Distributor/images_2008093.csv")
images2 <- read_csv("WI_Image_Distributor/images_2010895.csv")

## Read in a list of students from a Canvas gradebook export
students <- read_csv("WI_Image_Distributor/canvas_gradebook_example.csv") 

## Examples of image distribution:;
## 1) 50 images per student, assigned by date/time across the entire project
##    and blanks valued at 30% of images that likely have an animal.
DTTMdist50 <- WIdist(images,students,imagesPerStudent=50,blankVal=0.3)

## 2) No number of images specified, assigned by deployment and full date
##    and blanks valued at 10% of images that likely have an animal.
DeployDist <- WIdist(images,students,byDeployment=TRUE,blankVal=0.1)

## 3) An example with multiple deployments, where images are divided as evenly
##    as possible, while still assigning images to each student, etc.
DeployDist2 <- WIdist(images2,students,byDeployment=TRUE,blankVal=0.1)

## 4) An example with multiple deployments with 200 images assigned per student,
##    by deployment, where there aren't enough images to fully provide all students
##    with at least 200 images.
DeployDist3 <- WIdist(images2,students,imagesPerStudent=200,byDeployment=TRUE,blankVal=0.1)

## 5) An example with multiple deployments, where there are extra unassigned
##    images after assigning a minimum of 20 to each student.
DeployDist4 <- WIdist(images2,students,imagesPerStudent=20,byDeployment=TRUE,blankVal=0.1)

