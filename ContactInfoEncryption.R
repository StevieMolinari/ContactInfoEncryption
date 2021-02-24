#### Contact Info Encryption ####
## encodes phone number as its prime factorization and email with affine cipher

#### PHONE NUMBER ####
## provides prime factorization via naive iterative check up to sqrt.

## returns T if all are False, returns F is any are TRUE
none <- function(x){all(!x)}

# ## testing
# none(TRUE)
# none(FALSE)
# none(c(F,F))
# none(c(T, F))

sampleSize = 6
x = sample(0:1, size = sampleSize, replace = T, prob = c(9, 1)/10)
x
none(x)

## Q: What does *prob* need to be in sample so that $\P(none(x) = TRUE) == 1/2$?


## inpputs a positive integer; checks division by all leq sqrt(n)
### returns T if prime and F if composite; 1 returns F
is.prime <- function(n){
  ## checks for positive integer
  if(!is.numeric(n)) return("please provide a numeric")
  if(as.integer(n) != n) return("please provide an integer")
  if(n <= 0) return("please provide a positive integer")
  
  if(n %in% 2:3) return(TRUE)
  
  rootN = floor(sqrt(n))
  remainders = n %% (2:rootN)
  return(sum(remainders == 0) == 0)
}

# ## testing
# is.prime(7)
# is.prime(9)
# is.prime(2)
# is.prime(1)
# is.prime("blah-di-dah")
# is.prime(2.34)
# is.prime(-4)
# rbind(1:10, sapply(1:10, is.prime))


## brute force (or really brute force) factorizer
prime.factors <- function(n, naively = TRUE, prime = TRUE){
  ## checks for positive integer
  if(!is.numeric(n)) return("please provide a numeric")
  if(as.integer(n) != n) return("please provide an integer")
  if(n <= 0) return("please provide a positive integer")
  
  ## naive appraoch checks all numbers leq to n
  if(naively){
    factorsOfN = (1:n)[n%%(1:n)==0]
    
    if(length(factorsOfN) == 2) return(paste(n, "is prime"))

    if(!prime){
      return(paste0(n, "'s factors are ", 
                    paste(factorsOfN, collapse = ", ")))
    } else{
      if(n == 1) return("1 is weird/a unit")
      primeFactors = factorsOfN[sapply(factorsOfN, is.prime)]
      return(paste0(n, "'s prime factors are ",
                    paste(primeFactors, collapse = ", ")))
    }
  }
  
  ## "not naive" approach checks only numbers up to sqrt(n)
  sqrtN = floor(sqrt(n))
  remAreZero = (n %% (2:sqrtN) == 0)
  
  if(none(remAreZero)) return(paste(n, "is prime"))
  
  factorsLeSqrt = (2:sqrtN)[remAreZero]
  factorsGeSqrt = rev(n/factorsLeSqrt)
  factorsOfN = c(1, union(factorsLeSqrt, factorsGeSqrt), n)
  
  if(!prime){
    return(paste0(n, "'s factors are ", 
                  paste(factorsOfN, collapse = ", ")))
  } else{
    if(n == 1) return("1 is weird/a unit")
    primeFactors = factorsOfN[sapply(factorsOfN, is.prime)]
    return(paste0(n, "'s prime factors are ", 
        paste(primeFactors, collapse = ", ")))
  }
}

# prime.factors(42)
# prime.factors(2*3*5*7*11*16)
# prime.factors(7)
# prime.factors(1)
# prime.factors(1, prime = FALSE)
# prime.factors(1.5)
# prime.factors(289478934)
# prime.factors(289478934, naively = FALSE)

prime.factors(304)
304/2
304/4
304/8
304/16

prime.factors(615)
615/15

prime.factors(2566)
prime.factors(1283)



#### EMAIL ####

## adjusts mod arithmetic for R's 1 indexing
zero.to.n <- function(x, n = 26){ifelse(x==0, n, x)}

# # testing
# zero.to.n(0)
# zero.to.n(57)
# zero.to.n(0:5)
# zero.to.n(-2:2)

## encryption is an affine cipher
### i.e. cipher text = a * plaintext + b

## Q: what conditions on a & b do we need here?
### certainly a = 26 is bad, also a = 13.

scaleFactor = 1
shiftFactor = 1
ptInds <- 1:26
ctInds <-  zero.to.n((ptInds*scaleFactor + shiftFactor) %% 26)
ptAlphabet <- letters[1:26]
ctAlphabet <- letters[ctInds]


#### decoding by hand ####
## to encoded: plaintext to ciphertext is top to bottom 
rbind(ptAlphabet, ctAlphabet)

## to decode: ciphertext to plaintext is top to bottom 
rbind(ctAlphabet, ptAlphabet)


## encode message in lower case letters
encode.letter <- function(letter){
  ptInd = which(letters == letter)
  letters[ctInds[ptInd]]
}

is.letter <- function(x){
  x %in% letters
}


encode.message <- function(secretMessage, lowerCase = TRUE){
  if(!lowerCase){
    return("uh oh... can't encode non lower case messages")
  } else{
    messageChars = unlist(strsplit(secretMessage, split = ""))
    letterInds = as.vector(sapply(messageChars, is.letter))
    messageLets = messageChars[letterInds]
    cypherLets = sapply(messageLets, encode.letter)
    cyperChars = messageChars
    cyperChars[letterInds] = cypherLets
    return(paste0(cyperChars, collapse = ""))
  }
}

encode.message("hello world")
encode.message("stephen.molinari@gmail.com")

#### TO DO #### 
## decryption scheme given 
