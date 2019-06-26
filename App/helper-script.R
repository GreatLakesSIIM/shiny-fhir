#helper
#working on API calls

base <- "http://hackathon.siim.org/fhir/Patient/siimjoe"

call <- paste(base,endpoint,"?","ticker","=", detail, sep="")


get_patient <- GET(call1, authenticate(username,password, type = "basic")) 



get_patient_json <- fromJSON(get_patient_text, flatten = TRUE) 

data <- as.data.frame(get_patient_json)