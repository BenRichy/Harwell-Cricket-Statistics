#read api key in
FP_polished_api_key <- "api/polished_app_api_key.txt"
polished_api_key <- readLines(FP_polished_api_key)

#deploy app to polished
polished::deploy_app(
  app_name = "harwell-cricket-statistics",
  api_key = polished_api_key
)
