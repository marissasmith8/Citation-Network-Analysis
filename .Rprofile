message("rscopus Elsevier API set")
Sys.setenv('Elsevier_API' = '52d7e732620d031cdead3385013c9ea6')
# googlesheets::gs_auth(key = "757764669349-di8f63svsbdm339d1nbnq0kkpvp1i941.apps.googleusercontent.com", secret = "0j8D-PWeQyfL2VOzMT6M6fl8")
tryCatch(http_error = function(cnd) message("Google sheets authenticated"),
			error = function(cnd) message("Google sheets authentication failed, is VPN active?"), 
			googlesheets::gs_auth(key = "757764669349-di8f63svsbdm339d1nbnq0kkpvp1i941.apps.googleusercontent.com", secret = "0j8D-PWeQyfL2VOzMT6M6fl8"))