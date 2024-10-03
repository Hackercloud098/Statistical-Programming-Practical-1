#Ignacio Porras Vargas (S2742622)
#Per discussed with instructor, given that all groups were full, I am the sole contributor to this project (Ignacio Porras Vargas -> 100%)


a <- scan("4300-0.txt",what="character",skip=73,nlines=32858-73,
fileEncoding="UTF-8") ## inputting the text "Ulysses" to vector 'a'

a <- gsub("_(","",a,fixed=TRUE) ## remove "_(" from vector 'a'


split_punct <- function(vec_words,punctuation_mark) { ## function to split punctuation marks from every word in the vector
	
	punct <- paste0("[",punctuation_mark,"]") ## concatenate the punctuation mark with square brackets to be used as a regex pattern targeting the specific punctuation mark

	words_with_punctuation <- grep(punct,vec_words) ## filter words that contain the given punctuation mark

	words_without_punctuation <- grep(punct,vec_words,invert=TRUE) ## filter words that do NOT contain the given punctuation mark

	empty_vector <- rep("",length(words_with_punctuation)+length(vec_words)) ## initialize an empty vector with the size of the original vector plus the words that contain a punctuation mark (this will enable the slots to input the punctuation mark)

	for (i in words_with_punctuation){ ## for loop to iterate through words with a punctuation mark
			
			empty_vector[i] <- gsub(punct,"",vec_words[i]) ##  add the word without the punctuation mark
			empty_vector[i+1] <- punctuation_mark ## add the punctuation mark in the following slot of the vector
		
		}
	empty_vector[empty_vector == ""] <- vec_words[words_without_punctuation] ## input the remaining words (which do not include the GIVEN punctuation mark) to the vector

	return(empty_vector)

}



for (j in c("!","?",":",";",".",",")){ 
	a <- split_punct(a,j) ## iterate through each punctuation mark to apply the splitting function
}


a <- tolower(a) ## convert vector to lower case

unique_words <- unique(a) ## vector with unique words

matching_words <- match(a,unique_words) ## vector with indices matching unique words with respect to the original vector

counter_unique_words <- tabulate(matching_words) ## vector containing the frequency of each word

sorted_counter_unique_words <- sort(counter_unique_words, decreasing = TRUE) ## sorting through the vector from highest frequency to lowest frequency of word occurrence in the text


thresh_freq <- sorted_counter_unique_words[1000] ## obtaining the threshold frequency on the 1000th element of the sorted vector


b <- unique_words[counter_unique_words >= thresh_freq] ## obtaining vector with most commonly occurring words 


k_idx <- match(a, b) ## obtaining vector with the indices between original vector and most commonly occurring words vector

matrix_M <- function(k_m = k_idx,n = length(a),mlag = 4){ ## create matrix M 


m_temp <- matrix(NA, nrow = n - mlag, ncol = mlag + 1) ## initialize (n - mlag) × (mlag + 1) matrix


m_temp[, 1] <- k_m[1:length(m_temp[,1])] ## filling first column of matrix 'M' with the corresponding indices of common words


    for (i in 2:(mlag + 1)) { ## loop to input values for matrix M

        sh_idx <- k_m + i - 1 ## shifting index '(i-1)' will indicate the word on the lag of interest
        
	sh_idx <- sh_idx[sh_idx >= 1 & sh_idx <= length(b)] ## remove entries from beginning and/or end of the corresponding shift within an appropriate range
	
	sh_idx <- sh_idx[1:length(m_temp[,1])] ## matching number of rows to further avoid any inconsistency in the length of the vector

	m_temp[, i] <- sh_idx ## input the shifting index vector the to the corresponding column of the matrix

    }

    return(m_temp)
    
}


M <- matrix_M() ## created matrix

nw <- 50 ## initializing word sections

sim <- c(nw)  ## empty vector for the simulated text

sim[1] <- sample(b, 1) ## first random token

mlag <- 4

for (i in 2:nw) {
    for (j in mlag:1) {
        if (i > j) {  ## skip lags too long

            sim[i] <- paste(sim[i], b[M[i, 1:(j + 1)]], sep = " ") ## concatenating the generated text

            break  ## Break out of the j loop after the word has been generated

        }
    }
}


cat(sim,sep = " ")




