# rqmlativ
This repo provides an SDK for Skyward's QMLATIV generic API.

## Install

```r
remotes::install_github("samterfa/rqmlativ")
```

## Getting Started

Follow Skyward's instructions for creating a Qmlativ API user [here](https://help.skyward.com/Documentation/000006Administrative_Access_-_System_Administration/000002API/000002Creating_API_User_Access_Records). Select "Generic" for the API Type, and OAuth 2.0 as the authentication type.

## Usage

Begin by setting relevant environmental variables based on the credentials created above.

```r
Sys.setenv(SkywardConsumerKey = {YourConsumerKey})
Sys.setenv(SkywardConsumerSecret = {YourConsumerSecret})
Sys.setenv(SkywardBaseUrl = {YourBaseUrl})
```

Or, even better, place the above in a .Renviron in your working directory so that they are automatically loaded when using rqmlativ.

Your base url is likely your Skyward URL but ending in API instead of STS.

Then make a call!

```r
library(rqmlativ)
#> 
#> Attaching package: 'rqmlativ'
#> The following objects are masked from 'package:methods':
#> 
#>     getClass, getGroup
#> The following object is masked from 'package:base':
#> 
#>     getElement

listEntities(Name = T)
#> Adding .tokenCache/rqmlativToken to .gitignore
#>                              Name
#> 1               Minnehaha Academy
#> 2  Minnehaha Academy Upper School
#> 3  Minnehaha Academy Lower School
#> 4 Minnehaha Academy Middle School
```

## Resources

Function documentation for this package is available at https://samterfa.github.io/rqmlativ/reference/index.html.
