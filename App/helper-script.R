#helper
#working on API calls

# require("httr")
# require("jsonlite")
# 
# url <- "http://hackathon.siim.org/fhir/Patient/siimjoe"
# #dependency: local system variable called SiimApiKey
# myKey <-Sys.getenv('SiimApiKey')
# patientReturn <- GET(url, accept_json(), add_headers('apikey' = myKey))
# get_patient_text <- content(patientReturn, "text")
# get_patient_json <- fromJSON(get_patient_text, flatten = TRUE) 
# #view json object return
# get_patient_json
# 
# #view table of returned data
# data <- as.data.frame(get_patient_json)

jsonedit(data)
