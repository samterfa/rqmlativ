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
umbrella::listOrganizations()

#>[[1]]
#>[[1]]$organizationId
#> [1] "1234567"
#>
#>[[1]]$name
#> [1] "ACME Inc."

umbrella::listIdentities() %>% purrr::pluck('data') %>% purrr::map(~.x$type$type) %>% unlist() %>% table()

#> ad_connector directory_computer   directory_domain    directory_group     directory_user  domain_controller 
#>       1              1210                1                  258                1105              6 
#>  internal_network      mobile_device            network            roaming               site 
#>        15                  1427                    2                  72                   1 
```

## Resources

Function documentation for this package is available at https://samterfa.github.io/umbrella/reference/index.html.

See [Cisco Umbrella API Documentation](https://docs.umbrella.com/umbrella-api/reference) for details on the various APIs available.