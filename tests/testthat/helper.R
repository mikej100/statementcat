library(logger)

setwd(test_path("../../"))
logger::log_appender(logger::appender_file(file.path(test_path(),"log/log.txt")))
logger::log_info( "helper, after setwd:: getwd: {getwd()}" )
