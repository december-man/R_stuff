test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)
p <- lapply(lapply(test_data, table), chisq.test)
 