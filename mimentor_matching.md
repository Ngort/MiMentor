MiMentor Matching Algorithm
---------------------------

This algorithm assigns a compatibility score between each mentee and
mentor, sets a ranking of compatibility for each potential mentor and
implements the Top Trading Cycle algorithm through matchingR to find the
most optimal pairing (Shapley & Scarf, 1974).

<http://www.sciencedirect.com/science/article/pii/0304406874900330?via%3Dihub>

### Libraries

    library(tidyverse)

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

    library(matchingR)

    ## Loading required package: Rcpp

### Function bank

    matchcat <- function(a,b){ #score the compatibility between two individuals based on multiple choice questions such as majors, industries, regions, etc.
        catsa <- (a %>% strsplit(split=', '))[[1]] #split categories described in form by commas
        catsb <- (b %>% strsplit(split=', '))[[1]]
        missing <- vector()
        for(i in catsa){
            if (i %in% catsa && !(i %in% catsb)){
                missing <- append(missing, i) #append every category present present in the mentee's form that's absent in the mentors' forms to missing 
            }
        }
        return(1-length(missing)/length(catsa)) #return ratio as score
    }

    match_score <- function(a,b){ #mentee a with respect to mentor b
        sheeta <- sheet21[rownames(sheet21)==a,]
        sheetb <- sheet49[rownames(sheet49)==b,]
        
        eval_class <- 0 #default scores = 0
        eval_gender <- 0
        eval_majors <- 0
        eval_interests <- 0
        eval_country <- 0
        eval_region <- 0
        
        
        if(sheeta$pref_class == 'no preference'){ #set score to 0.5 if no class preference
            eval_class <- 0.5 #this is actually irrelevnat since class is not considered for those who didn't set a preference
        }else if(sheetb$class == sheeta$pref_class){
            eval_class <- 1
        }
        
        if(sheeta$gender == sheetb$gender){
            eval_gender = 1    # Same gender -> score = 1. Otherwise 0.
        }else if(!(sheeta$gender%in%c('Male','Female'))){
            if(!(sheetb$gender%in%c('Male','Female'))){
                eval_gender = 1 # Those who filled Other get good Gender Score with each other
            }
        }
        
        eval_majors <- matchcat(sheeta$majors,sheetb$majors)
        eval_interests <- matchcat(sheeta$industries,sheetb$industries)
        
        if(sheeta$country == sheetb$country){
            eval_country = 1 # matching first countres equals 1 point
        }else if(sheeta$country == sheetb$country2 | sheeta$country2 == sheetb$country){
            eval_country = 0.5 #matching first-second countries equals 0.5 points
        }else if(sheeta$country2 == sheetb$country2){
            eval_country = 0.25 #matching second countries equals 0.25 points
        }
        
        eval_region <- matchcat(sheeta$region,sheetb$region)
        
        
        prefvar1 <- eval(parse(text=paste0('eval_',sheeta$pref1))) #score for first priority
        prefvar2 <- eval(parse(text=paste0('eval_',sheeta$pref2))) #score for second priority
        prefvar3 <- eval(parse(text=paste0('eval_',sheeta$pref3))) #and so on
        prefvar4 <- eval(parse(text=paste0('eval_',sheeta$pref4)))
        prefvar5 <- eval(parse(text=paste0('eval_',sheeta$pref5)))
        
        
        if(sheeta$pref_class == 'no preference'){
            return(1.75**4 * prefvar1 + 1.75**3 * prefvar2 + 1.75**2 * prefvar3 + 1.75 * prefvar4 + prefvar5)
        }else{
            return(1.75**4 * prefvar1 + 1.75**3 * prefvar2 + 1.75**2 * prefvar3 + 1.75 * prefvar4 + prefvar5 + 1.75**5 * eval_class)
        }
    } #compute final score as sum(s_i*1.75^(5-p_i)) where s_i is the score for each element i and p_i is its priority.

    vec_score <- Vectorize(match_score)

### Read and process mock data

    sheet1 <- read.csv('test_data.csv',stringsAsFactors = FALSE)
    sheet21 <- sheet1[sheet1$class == 2021,]
    sheet49 <- sheet1[sheet1$class != 2021,]
    rownames(sheet21) <- 1:72
    rownames(sheet49) <- 1:72

### Create compatibility ranking matrix based on compatbility scores between each mentee and each potential mentor

    rank_table <- data.frame(matrix(NA, nrow = nrow(sheet21), ncol = nrow(sheet49)),row.names = rownames(sheet21))

    for(i in rownames(sheet21)){
        rank_table[i,] <- rownames(sheet49)[vec_score(i,rownames(sheet49)) %>% order %>% rev]
    }

    rank_matrix <- rank_table %>% unname %>% as.matrix() %>% apply(1,as.numeric)

### Apply Top Trading Cycle Algorithm, check for stable solution

    results <- toptrading(pref = rank_matrix)
    matched <- as.data.frame(toptrading(pref = rank_matrix))
    matched$mentee <- sheet21$name
    matched$mentee_email <- sheet21$email
    matched$mentor <- sheet49$name[matched$V1]
    matched$mentor_email <- sheet49$email[matched$V1]
    matched <- matched[colnames(matched)!='V1']

    toptrading.checkStability(pref = rank_matrix,matchings =results)

    ## [1] TRUE

### Plot histogram of the ranking of each matched mentor with respect to its mentee.

    matched_ranks <- numeric()

    for(i in 1:nrow(sheet21)){
        matched_ranks[i] <- which(results[i,] == rank_table[i,])
    }

    matched$rank <- matched_ranks

    hist(matched_ranks,breaks=62,col='orange') #average rank of the mentor assigned for each mentee as a measure of success of matching

![](mimentor_matching_files/figure-markdown_strict/unnamed-chunk-6-1.png)

### Output summary of matches with information about mentees provided to each assigned mentor

    matched_info <- matched[colnames(matched)!='rank']
    matched_info <- cbind(matched_info[c(3,4,1,2)],sheet21[c(5,7:11)])

    names(matched_info) <- c('mentor','mentor_email','mentee','mentee_email','mentee_gender','mentee_majors','mentee_industries','mentee_region','mentee_country','mentee_country2')
    write.csv(matched_info,'mimentor_matches_test.csv')
    head(matched_info)

    ##    mentor            mentor_email     mentee               mentee_email
    ## 1 Shannon shannon@minerva.kgi.edu     Leslie     leslie@minerva.kgi.edu
    ## 2   Diane   diane@minerva.kgi.edu      Peggy      peggy@minerva.kgi.edu
    ## 3  Rachel  rachel@minerva.kgi.edu    Rebecca    rebecca@minerva.kgi.edu
    ## 4  Stacey  stacey@minerva.kgi.edu Antoinette antoinette@minerva.kgi.edu
    ## 5   Pedro   pedro@minerva.kgi.edu    Melanie    melanie@minerva.kgi.edu
    ## 6  Carlos  carlos@minerva.kgi.edu    Adriana    adriana@minerva.kgi.edu
    ##                   mentee_gender
    ## 1                        Female
    ## 2                        Female
    ## 3                        Female
    ## 4 Why do you need to know that?
    ## 5                        Female
    ## 6                        Female
    ##                                    mentee_majors
    ## 1               Business, Computational Sciences
    ## 2                               Natural Sciences
    ## 3                                Social Sciences
    ## 4           Arts and Humanities, Social Sciences
    ## 5 Arts and Humanities, Business, Social Sciences
    ## 6              Social Sciences, Natural Sciences
    ##                                                                                                                                               mentee_industries
    ## 1                                                                     Business & Consulting & Management, Marketing & Adverting & PR, STEM Research, Technology
    ## 2                                                                                                                                        Medical, STEM Research
    ## 3 Business & Consulting & Management, Government & Public Policy, Marketing & Adverting & PR, People Development, Social Enterprise & International Development
    ## 4                                                                                           Design and Visual Arts, Journalism & Media, Social Science Research
    ## 5                                                                                    Performing Arts & Music, Startups & Enterpreneurship, Teaching & Education
    ## 6                                 Environment & Agriculture, Government & Public Policy, Social Enterprise & International Development, Social Science Research
    ##     mentee_region mentee_country mentee_country2
    ## 1     Middle East          Egypt                
    ## 2  Eastern Europe        Romania                
    ## 3 South East Asia        Vietnam         Vietnam
    ## 4  Eastern Europe      Lithuania                
    ## 5       East Asia          China                
    ## 6       East Asia          China           Italy
