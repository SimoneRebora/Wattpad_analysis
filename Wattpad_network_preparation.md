Wattpad Network Preparation
================

This is the R Markdown document that prepares the Wattpad dataset for network analysis<br/> First, let's upload the corpus.

Upload dataset
--------------

Each entry in the corpus is composed by 10 parts:<br/> - unique identifier for book, chapter, and paragraph<br/> - title of book, chapter, and paragraph<br/> - name of the user and date of his/her comment<br/> - the comment<br/> - a logical TRUE/FALSE, that indicates if the comment is a reply to a previuos comment<br/> <br/> Note that the corpus is a "sample" generated on the basis of the "Pride and Prejudice" dataset.<br/> The text is the same as it appears in Wattpad and Project Gutenberg; usernames have been anonymized; dates and replies indicators have been scrambled; comments have been generated artifically by re-mixing the words of the actual comments.

``` r
wattpad_df <- read.csv("Sample_wattpad_corpus.csv", stringsAsFactors = F)
summary(wattpad_df)
```

    ##      bookID    chapterID      paragraphID        book          
    ##  Min.   :1   Min.   : 1.00   Min.   : 1.00   Length:36491      
    ##  1st Qu.:1   1st Qu.: 2.00   1st Qu.: 6.00   Class :character  
    ##  Median :1   Median :10.00   Median :15.00   Mode  :character  
    ##  Mean   :1   Mean   :18.92   Mean   :18.56                     
    ##  3rd Qu.:1   3rd Qu.:34.00   3rd Qu.:27.00                     
    ##  Max.   :1   Max.   :61.00   Max.   :77.00                     
    ##    chapter           paragraph           username        
    ##  Length:36491       Length:36491       Length:36491      
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##      date             comment            reply        
    ##  Length:36491       Length:36491       Mode :logical  
    ##  Class :character   Class :character   FALSE:25733    
    ##  Mode  :character   Mode  :character   TRUE :10758    
    ##                                                       
    ##                                                       
    ## 

Corpus pre-processing
---------------------

### Step 1

Exclude not commented paragraphs (as a convention, hey are identified by the "NO\_COMMENTS" string in the "username" position)

``` r
exclude <- which(wattpad_df$username=="NO_COMMENTS")
wattpad_df <- wattpad_df[-exclude,]
```

### Step 2

General statistics on comments/replies

``` r
# percentage of replies
percentage_replies <- length(which(wattpad_df$reply == T))/length(wattpad_df$reply)*100
# percentage of comments
percentage_comments <- length(which(wattpad_df$reply == F))/length(wattpad_df$reply)*100
cat("Percentage of comments:",
    percentage_comments,
    "Percentage of replies:",
    percentage_replies,
    sep = "\n")
```

    ## Percentage of comments:
    ## 69.98326
    ## Percentage of replies:
    ## 30.01674

### Step 3

Exclude comments with no user (probably deleted from Wattpad). If kept, they will be confused with each other

``` r
exclude <- which(wattpad_df$username=="-")
wattpad_df <- wattpad_df[-exclude,]
```

### Step 4

Run the analysis!<br/> It is an iterative process, that works on the "reply" part of the dataset:<br/> - if reply is "FALSE", it generates a connection between the user and the commented paragraph;<br/> - if reply is "TRUE", it generates a connection between the current user and the last user for whom reply was set as "FALSE" (i.e. between the current user and the last user who wrote the comment to which he/she is replying).<br/> The results are saved in a vector of strings, composed with the convention: "USER current\_user \#PAIR\# PARAGRAPH current\_paragraph / USER previous\_user"

``` r
user_book_pair <- character()
for(i in 1:length(wattpad_df$reply)){
  if(wattpad_df$reply[i] == "TRUE"){
    if(wattpad_df$reply[i-1] == "FALSE"){
      target <- paste("USER", wattpad_df$username[i-1], sep = " ")
    }
    user_book_pair[i] <- paste(paste("USER", wattpad_df$username[i], sep = " "), target, sep = " #PAIR# ")
    #print(i/length(wattpad_df$reply)) ##in case you want to see the progress...
    next
  }
  user_book_pair[i] <- paste(paste("USER", wattpad_df$username[i], sep = " "), paste("PARAGRAPH ", wattpad_df$bookID[i], "_", wattpad_df$chapterID[i], "_", wattpad_df$paragraphID[i], sep = ""), sep = " #PAIR# ")
  #print(i/length(wattpad_df$reply)) ##in case you want to see the progress...
}
cat("Result:", head(user_book_pair), "...", sep = "\n")
```

    ## Result:
    ## USER User_3cMAV #PAIR# PARAGRAPH 1_1_1
    ## USER User_JPrug #PAIR# PARAGRAPH 1_1_1
    ## USER User_BvCbH #PAIR# PARAGRAPH 1_1_1
    ## USER User_IE8YE #PAIR# PARAGRAPH 1_1_1
    ## USER User_Kwswz #PAIR# PARAGRAPH 1_1_1
    ## USER User_hkkod #PAIR# USER User_Kwswz
    ## ...

### Step 5

Now everything is ready to calculate the first general statistics

``` r
user_book_pair_stats <- sort(table(user_book_pair), decreasing = T)
print(head(user_book_pair_stats))
```

    ## user_book_pair
    ## USER User_mAxY6 #PAIR# PARAGRAPH 1_1_1 
    ##                                      4 
    ## USER User_sjIPv #PAIR# PARAGRAPH 1_1_1 
    ##                                      4 
    ## USER User_xwZ00 #PAIR# PARAGRAPH 1_1_1 
    ##                                      4 
    ## USER User_10QNF #PAIR# PARAGRAPH 1_1_1 
    ##                                      3 
    ## USER User_2ECfb #PAIR# PARAGRAPH 1_1_1 
    ##                                      3 
    ## USER User_3oCQG #PAIR# PARAGRAPH 1_1_1 
    ##                                      3

### Step 6

Now simplify the connections, by collapsing paragraphs into chapters. If you are working on more than one book and you want to see connections with books instead of chapters, please set the "choice" variable accordingly choice:"book" (focuses on one book and collapses paragraphs into chapters) choice:"category" (focuses on one category and collapses paragraphs into books)

``` r
choice <- "book"
#choice <- "category"

###exclude relations with self
commentor_commenter <- strsplit(names(user_book_pair_stats), " #PAIR# ")
exclude_tmp <- lapply(commentor_commenter, function(x){which(x[1]==x[2])})
exclude <- which(exclude_tmp==1)
user_book_pair_stats <- user_book_pair_stats[-exclude]

###prepare final dataframe
source_target <- strsplit(names(user_book_pair_stats), " #PAIR# ")
Source <- unlist(lapply(source_target, function(x) x[1]))
Target <- unlist(lapply(source_target, function(x) x[2]))

commented_paragraphs <- which(substr(Target, 1, 9) == "PARAGRAPH")

##Simplify analysis to commented books
if(choice == "category"){
  for(i in commented_paragraphs){
    Target[i] <- substr(Target[i], 11, nchar(Target[i]))
    Target[i] <- unlist(strsplit(Target[i], "_"))[1]
    Target[i] <- paste("BOOK", books[as.numeric(Target[i])])
    #print(i)
  }
}

##Simplify analysis to commented chapters for single books
if(choice == "book"){
  for(i in commented_paragraphs){
    Target[i] <- substr(Target[i], 11, nchar(Target[i]))      
    selected_chapter <- unlist(strsplit(Target[i], "_"))[2]
    Target[i] <- paste("CHAP_", selected_chapter, sep = "")
    #print(i)
  }
}
```

### Step 7

Collapsing paragraphs into books/chapters might have generated some duplicated entries, thus sum the values of repeated entries and save all into a dataframe (tabular format)

``` r
##Aggregate in dataframe
user_book_pair_stats_tmp <- data.frame(Weight=as.vector(user_book_pair_stats), source_target = paste(Source, Target, sep = " #PAIR# "), stringsAsFactors = F)

##Sum repeated entries
user_book_pair_stats_tmp <- with(user_book_pair_stats_tmp, aggregate(list(Weight = Weight), list(source_target = source_target), sum))
```

### Step 8

Filter weak connections (i.e. exclude pairs that are repeated less than a certain number of times)

``` r
activity_limit <- 3
exclude <- which(user_book_pair_stats_tmp$Weight < activity_limit)
user_book_pair_stats_tmp <- user_book_pair_stats_tmp[-exclude,]
```

### Step 9

Prepare dataframe for final results.<br/> Values are normalized between 0 and 10 to favour visualization in Gephi

``` r
##Prepare final dataframe
source_target <- strsplit(user_book_pair_stats_tmp$source_target, " #PAIR# ")
Source <- unlist(lapply(source_target, function(x) x[1]))
Target <- unlist(lapply(source_target, function(x) x[2]))
user_book_pair_stats_df <- data.frame(Source, Target, Weight=user_book_pair_stats_tmp$Weight, Type="directed", stringsAsFactors = F)

##Normalize weights between 0 and 10
normalize_weight <- 10
user_book_pair_stats_df$Weight <- user_book_pair_stats_df$Weight/max(user_book_pair_stats_df$Weight)*normalize_weight
```

### Step 10

Anonymize all user names

``` r
##Anonymize
full_node_names <- unique(c(user_book_pair_stats_df$Source, user_book_pair_stats_df$Target))
anonymizing_table <- data.frame(full_node_names, anonymized_names = character(length(full_node_names)), stringsAsFactors = F)
book_count <- 0
user_count <- 0
for(i in 1:length(anonymizing_table$full_node_names)){
  if(substr(anonymizing_table$full_node_names[i], 1, 4) == "BOOK"){
    book_count <- book_count + 1
    anonymizing_table$anonymized_names[i] <- paste("BOOK_", book_count, sep = "")
  }
  if(substr(anonymizing_table$full_node_names[i], 1, 4) == "USER"){
    user_count <- user_count + 1
    anonymizing_table$anonymized_names[i] <- paste("USER_", user_count, sep = "")
  }
  if(substr(anonymizing_table$full_node_names[i], 1, 4) == "CHAP"){
    anonymizing_table$anonymized_names[i] <- anonymizing_table$full_node_names[i]
  }
}

for(i in 1:length(user_book_pair_stats_df$Source)){
  user_book_pair_stats_df$Source[i] <- anonymizing_table$anonymized_names[which(anonymizing_table$full_node_names == user_book_pair_stats_df$Source[i])]
  user_book_pair_stats_df$Target[i] <- anonymizing_table$anonymized_names[which(anonymizing_table$full_node_names == user_book_pair_stats_df$Target[i])]
}
```

### Step 11

Save all results in separated tables: nodes, edges, and table with real/anonymized names

``` r
###Prepare nodes table
nodes_table <- data.frame(Id = anonymizing_table$anonymized_names, Label = anonymizing_table$anonymized_names, group = substr(anonymizing_table$anonymized_names, 1, 1), stringsAsFactors = F)
for(i in 1:length(nodes_table$Id)){
  if(substr(nodes_table$Label[i], 1, 4) == "BOOK"){
    nodes_table$Label[i] <- substr(anonymizing_table$full_node_names[i], 6, nchar(anonymizing_table$full_node_names[i]))
  }
}

##Save all
write.csv(user_book_pair_stats_df, file = "network_files/edges.csv", row.names = F)
write.csv(nodes_table, file = "network_files/nodes.csv", row.names = F)
write.csv(anonymizing_table, file = "network_files/anonymizing_table.csv", row.names = F)
```

### Step 12 (in Gephi)

Now everything is ready to visualize and analyze your graph in Gephi!
