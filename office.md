Office
================
Gaurav Sharma
15/06/2020

# Load the Data

``` r
ratings_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   season = col_double(),
    ##   episode = col_double(),
    ##   title = col_character(),
    ##   imdb_rating = col_double(),
    ##   total_votes = col_double(),
    ##   air_date = col_date(format = "")
    ## )

``` r
schrute::theoffice
```

    ## # A tibble: 55,130 x 12
    ##    index season episode episode_name director writer character text 
    ##    <int>  <int>   <int> <chr>        <chr>    <chr>  <chr>     <chr>
    ##  1     1      1       1 Pilot        Ken Kwa~ Ricky~ Michael   All ~
    ##  2     2      1       1 Pilot        Ken Kwa~ Ricky~ Jim       Oh, ~
    ##  3     3      1       1 Pilot        Ken Kwa~ Ricky~ Michael   So y~
    ##  4     4      1       1 Pilot        Ken Kwa~ Ricky~ Jim       Actu~
    ##  5     5      1       1 Pilot        Ken Kwa~ Ricky~ Michael   All ~
    ##  6     6      1       1 Pilot        Ken Kwa~ Ricky~ Michael   Yes,~
    ##  7     7      1       1 Pilot        Ken Kwa~ Ricky~ Michael   I've~
    ##  8     8      1       1 Pilot        Ken Kwa~ Ricky~ Pam       Well~
    ##  9     9      1       1 Pilot        Ken Kwa~ Ricky~ Michael   If y~
    ## 10    10      1       1 Pilot        Ken Kwa~ Ricky~ Pam       What?
    ## # ... with 55,120 more rows, and 4 more variables: text_w_direction <chr>,
    ## #   imdb_rating <dbl>, total_votes <int>, air_date <fct>

# Clean the data

``` r
remove_regex <- "[:punct:]|[:digit:]|part |parts |the |and"

office_ratings <- ratings_raw %>% 
    transmute(episode_name = str_to_lower(title),
              episode_name = str_remove_all(episode_name, remove_regex),
              episode_name = str_trim(episode_name),
              imdb_rating) 
    
office_info <- schrute::theoffice %>% 
    mutate(
        season = as.numeric(season),
        episode = as.numeric(episode),
        episode_name = str_to_lower(episode_name),
        episode_name = str_remove_all(episode_name, remove_regex),
        episode_name = str_trim(episode_name)) %>% 
    select(season, episode, episode_name, director, writer, character)


office_ratings %>% 
    distinct(episode_name) %>% 
    anti_join( office_info %>% 
    distinct(episode_name))
```

    ## Joining, by = "episode_name"

    ## # A tibble: 3 x 1
    ##   episode_name      
    ##   <chr>             
    ## 1 email surveillance
    ## 2 coverup           
    ## 3 sex ed

``` r
funModeling::compare_df(office_ratings %>% distinct(episode_name),
                        office_info %>% distinct(episode_name),
                        c('episode_name'))
```

    ## Coincident keys: 182

    ## 

    ## 

    ## $coincident
    ##   [1] "a benihana christmas"            "aarm"                           
    ##   [3] "after hours"                     "alliance"                       
    ##   [5] "angry y"                         "baby shower"                    
    ##   [7] "back from vacation"              "banker"                         
    ##   [9] "basketball"                      "beach games"                    
    ##  [11] "ben franklin"                    "blood drive"                    
    ##  [13] "boat"                            "body language"                  
    ##  [15] "booze cruise"                    "boys  girls"                    
    ##  [17] "branch closing"                  "branch wars"                    
    ##  [19] "broke"                           "business ethics"                
    ##  [21] "business school"                 "business trip"                  
    ##  [23] "cafe disco"                      "carpet"                         
    ##  [25] "casino night"                    "casual friday"                  
    ##  [27] "chair model"                     "china"                          
    ##  [29] "christening"                     "christmas party"                
    ##  [31] "christmas wishes"                "chump"                          
    ##  [33] "classy christmas"                "client"                         
    ##  [35] "cocktails"                       "company picnic"                 
    ##  [37] "conflict resolution"             "convention"                     
    ##  [39] "convict"                         "costume contest"                
    ##  [41] "counseling"                      "coup"                           
    ##  [43] "couples discount"                "crime aid"                      
    ##  [45] "customer loyalty"                "customer survey"                
    ##  [47] "delivery"                        "deposition"                     
    ##  [49] "did i stutter"                   "dinner party"                   
    ##  [51] "diversity day"                   "diwali"                         
    ##  [53] "doomsday"                        "double date"                    
    ##  [55] "dream team"                      "drug testing"                   
    ##  [57] "duel"                            "dunder mifflin infinity"        
    ##  [59] "dundies"                         "dwight christmas"               
    ##  [61] "dwight k schrute acting manager" "dwights speech"                 
    ##  [63] "employee transfer"               "farm"                           
    ##  [65] "fight"                           "finale"                         
    ##  [67] "fire"                            "frame toby"                     
    ##  [69] "free family portrait studio"     "fun run"                        
    ##  [71] "fundraiser"                      "garage sale"                    
    ##  [73] "garden party"                    "gay witch hunt"                 
    ##  [75] "get girl"                        "gettysburg"                     
    ##  [77] "golden ticket"                   "goodbye michael"                
    ##  [79] "goodbye toby"                    "gossip"                         
    ##  [81] "grief counseling"                "halloween"                      
    ##  [83] "happy hour"                      "health care"                    
    ##  [85] "heavy competition"               "here comes treble"              
    ##  [87] "hot girl"                        "incentive"                      
    ##  [89] "initiation"                      "injury"                         
    ##  [91] "inner circle"                    "job"                            
    ##  [93] "job fair"                        "junior salesman"                
    ##  [95] "jury duty"                       "koi pond"                       
    ##  [97] "last day in florida"             "launch party"                   
    ##  [99] "lecture circuit"                 "lice"                           
    ## [101] "list"                            "livin dream"                    
    ## [103] "local ad"                        "lotto"                          
    ## [105] "lover"                           "mafia"                          
    ## [107] "manager  salesman"               "meeting"                        
    ## [109] "merger"                          "michael scott paper company"    
    ## [111] "michaels birthday"               "michaels last dundies"          
    ## [113] "money"                           "moroccan christmas"             
    ## [115] "moving on"                       "mrs california"                 
    ## [117] "murder"                          "negotiation"                    
    ## [119] "nepotism"                        "new boss"                       
    ## [121] "new guys"                        "new leads"                      
    ## [123] "niagara"                         "night out"                      
    ## [125] "office olympics"                 "pams replacement"               
    ## [127] "paper airplane"                  "pda"                            
    ## [129] "performance review"              "phyllis wedding"                
    ## [131] "pilot"                           "pool party"                     
    ## [133] "prince family paper"             "product recall"                 
    ## [135] "promos"                          "promotion"                      
    ## [137] "return"                          "roys wedding"                   
    ## [139] "sabre"                           "safety training"                
    ## [141] "scotts tots"                     "search"                         
    ## [143] "search committee"                "secret"                         
    ## [145] "secret santa"                    "secretarys day"                 
    ## [147] "seminar"                         "sexual harassment"              
    ## [149] "shareholder meeting"             "special project"                
    ## [151] "spooked"                         "st patricks day"                
    ## [153] "stairmageddon"                   "sting"                          
    ## [155] "stress relief"                   "suit warehouse"                 
    ## [157] "surplus"                         "survivor man"                   
    ## [159] "take your daughter to work day"  "tallahassee"                    
    ## [161] "target"                          "test store"                     
    ## [163] "threat level midnight"           "todd packer"                    
    ## [165] "training day"                    "traveling salesmen"             
    ## [167] "trivia"                          "turf war"                       
    ## [169] "two weeks"                       "ultimatum"                      
    ## [171] "valentines day"                  "valism"                         
    ## [173] "viewing party"                   "weight loss"                    
    ## [175] "welcome party"                   "whale"                          
    ## [177] "whistleblower"                   "womens appreciation"            
    ## [179] "work bus"                        "wuphfcom"                       
    ## [181] "ys ancestry"                     "ys play"                        
    ## 
    ## $rows_not_in_X
    ## [1] "cover"             "email surveilance" "sx ed"            
    ## 
    ## $rows_not_in_Y
    ## [1] "coverup"            "email surveillance" "sex ed"

``` r
characters <- office_info %>% 
    count(episode_name,character) %>% 
    add_count(character, wt = n, name = "character_count") %>% 
    filter(character_count > 800) %>% 
    select(-character_count) %>% 
    pivot_wider(names_from = character,
                values_from = n,
                values_fill = list(n=0))


creators <- office_info %>% 
    distinct(episode_name, director,writer) %>% 
    pivot_longer(director:writer, names_to = "role", values_to = "person") %>% 
    separate_rows(person, sep = ";") %>% 
    add_count(person) %>% 
    filter(n>10) %>% 
    distinct(episode_name, person) %>%
    mutate(person_value = 1) %>% 
    pivot_wider(names_from = person,
                values_from = person_value, 
                values_fill = list(person_value = 0))

office <- office_info %>% 
    distinct(season, episode, episode_name) %>% 
    inner_join(characters) %>% 
    inner_join(creators) %>% 
    inner_join(office_ratings) %>% 
    clean_names()    
```

    ## Joining, by = "episode_name"

    ## Joining, by = "episode_name"
    ## Joining, by = "episode_name"

# Lets do some EDA

``` r
office %>% 
    mutate(season = factor(season)) %>% 
    ggplot(aes(season, imdb_rating, fill = season)) +
    geom_boxplot(show.legend = F)
```

![](office_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
office %>% 
    mutate(episode = factor(episode)) %>% 
    ggplot(aes(episode, imdb_rating, fill = episode)) +
    geom_boxplot(show.legend = F)
```

![](office_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

# Lets split the data

``` r
office_split <- office %>%
    initial_split(strata = season)

office_train <- training(office_split)
office_test <- testing(office_split)
```

# Lets create a recipe

``` r
office_rec <- recipe(imdb_rating ~ ., data = office_train) %>%
  update_role(episode_name, new_role = 'ID') %>%
  step_zv(all_numeric(),-all_outcomes()) %>%
  step_normalize(all_numeric(),-all_outcomes())
  
office_prep <- office_rec %>% 
    prep(strings_as_factors = F)
```

# Lets build a model spec

``` r
lasso_spec <- linear_reg(penalty = 0.1,
           mixture = 1) %>% 
    set_engine(engine = 'glmnet') %>% 
    set_mode(mode = 'regression')
```

# Lets create and fit a workflow

``` r
wf <- workflow() %>% 
    add_recipe(office_rec)

lasso_fit <- wf %>%
    add_model(lasso_spec) %>% 
    fit(data = office_train)

lasso_fit %>% 
    pull_workflow_fit() %>% 
    tidy()
```

    ## # A tibble: 1,614 x 5
    ##    term         step estimate lambda dev.ratio
    ##    <chr>       <dbl>    <dbl>  <dbl>     <dbl>
    ##  1 (Intercept)     1  8.34     0.180    0     
    ##  2 (Intercept)     2  8.34     0.164    0.0233
    ##  3 michael         2  0.0161   0.164    0.0233
    ##  4 (Intercept)     3  8.34     0.149    0.0478
    ##  5 angela          3  0.00427  0.149    0.0478
    ##  6 michael         3  0.0305   0.149    0.0478
    ##  7 (Intercept)     4  8.34     0.136    0.0783
    ##  8 angela          4  0.0169   0.136    0.0783
    ##  9 michael         4  0.0431   0.136    0.0783
    ## 10 (Intercept)     5  8.34     0.124    0.104 
    ## # ... with 1,604 more rows

# Lets tune lasso model

``` r
set.seed(1234)
office_boot <- bootstraps(office_train, strata = season)

tune_spec <- linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine(engine = 'glmnet') %>% 
  set_mode(mode = 'regression')

lambda_grid <- grid_regular(penalty(), 
                            levels = 50)
doParallel::registerDoParallel()

set.seed(2020)
lasso_grid <- tune_grid(wf %>%
            add_model(tune_spec),
          resamples = office_boot,
          grid = lambda_grid
)
```

``` r
lasso_grid %>% 
  collect_metrics() %>% 
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin = mean-std_err,
                    ymax = mean+std_err),
                alpha = 0.9, color = 'gray') +
  facet_wrap(~.metric, scales = 'free', nrow = 2) +
  scale_x_log10() +
  theme(legend.position = 'none')
```

    ## Warning: Removed 3 row(s) containing missing values (geom_path).

![](office_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

# Lets us finalize the workflow

``` r
lowest_penalty <- lasso_grid %>%
  select_best('rmse')

final_lasso <- finalize_workflow(wf %>% add_model(tune_spec),
                                 lowest_penalty)

#install.packages('vip')
library(vip)
```

    ## 
    ## Attaching package: 'vip'

    ## The following object is masked from 'package:utils':
    ## 
    ##     vi

``` r
final_lasso %>%
  fit(office_train) %>%
  pull_workflow_fit() %>%
  vi(lambda = lowest_penalty$penalty) %>%
  mutate(Importance = abs(Importance),
         Variable = fct_reorder(Variable, Importance)) %>%
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  labs(y = NULL) +
  scale_x_continuous(expand = c(0, 0))
```

![](office_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> \# Lets see
how the model performs on Test Data

``` r
last_fit(lasso_fit, office_split) %>% 
  collect_metrics()
```

    ## # A tibble: 2 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 rmse    standard       0.535
    ## 2 rsq     standard       0.103

\`\`\`

### In this exercise we saw how Tidymodel framework can be used to tune a Linear Reg Model with Lasso regularization. Thanks
