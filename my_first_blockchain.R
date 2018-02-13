### Building my First Blockchain in R
### date: 02/11/2018

### Structure of my Block
block_example <- list(index = 1,
                      timestamp = "2018-02-11 14:10:41 GMT",
                      data = "some data",
                      last_hash = 0,
                      proof = 1,
                      new_hash = NULL)

### Cryptography: hashing blocks with digest()
library(digest)
hash_block <- function(block){
  block$new_hash <- digest(c(block$index, 
                             block$timestamp,
                             block$data,
                             block$last_hash), algo = "sha256")
  return(block)
}

### Proof of Work: hard to find but easy to verify
mine_block <- function(last_proof){
  proof <- last_proof + 1
  
  # task for PoW: find the int divisible by 99 and by last proof
  while(!(proof %% 99 == 0 & proof %% last_proof ==0)){
    proof <- proof + 1
  }
  
  return(proof)
}

### generate new block
gen_new_block <- function(block){
  new_proof <- mine_block(block$proof)
  new_block <- list(index = block$index + 1,
                    timestamp = Sys.time(),
                    data = paste("This is block #", block$index + 1, "."),
                    last_hash = block$new_hash,
                    proof = new_proof,
                    new_hash = NULL)
  return(hash_block(new_block))
}

### define genesis block
block_gene <- list(index = 1,
                  timestamp = Sys.time(),
                  data = "This is block #1.",
                  last_hash = "0",
                  proof = 123,
                  new_hash = NULL)

### build the first 12 blocks in the blockchain
block_gene <- hash_block(block_gene)
blockchain <- list(block_gene)
last_block <- blockchain[[1]]
N <- 11
for (i in 1:N){
  new_block <- gen_new_block(last_block)
  blockchain[[i+1]] <- list(new_block)
  last_block <- new_block
  
  print(cat(paste0("Block #", new_block$index, " has been added", "\n",
                   "\t", "Proof: ", new_block$proof, "\n",
                   "\t", "Hash: ", new_block$new_hash)))
}