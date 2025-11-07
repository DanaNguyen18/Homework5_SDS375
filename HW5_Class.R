## HW5 Class/Methods

# Class definition

setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

# Validity method

setValidity("sparse_numeric", function(object) {
    if (length(object@value) != length(object@pos)) {
      return("Length of value needs to equal pos")
    }
    if (any(object@pos > object@length)) {
      return("Pos index needs to be less than or equal to length")
    }
    TRUE
  })

# Coercion methods

setAs("numeric", "sparse_numeric",
      function(from) {
        pos <- which(from != 0)
        new("sparse_numeric",
            value = from[pos],
            pos = as.integer(pos),
            length = as.integer(length(from)))
      })

setAs("sparse_numeric", "numeric",
      function(from) {
        x <- numeric(from@length)
        x[from@pos] <- from@value
        x
      })

# Generic methods

setGeneric("sparse_add", function(x, y, ...) {
  standardGeneric("sparse_add")
})

setGeneric("sparse_mult", function(x, y, ...) {
  standardGeneric("sparse_mult")
})

setGeneric("sparse_sub", function(x, y, ...) {
  standardGeneric("sparse_sub")
})

setGeneric("sparse_crossprod", function(x, y, ...) {
  standardGeneric("sparse_crossprod")
})

setGeneric("sparse_sqrt", function(x, ...) {
  standardGeneric("sparse_sqrt")
})


# Add method

setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"),
          function(x, y){
            if (x@length != y@length)
              stop("Lengths of sparse vectors must equal")
            
            all_positions <- sort(unique(c(x@pos, y@pos))) # non zeros
            x_index <- match(all_positions, x@pos)
            y_index <- match(all_positions, y@pos)
            
            x_vals <- ifelse(is.na(x_index), 0, x@value[x_index]) # assign 0 for NA values, or X value otherwise
            y_vals <- ifelse(is.na(y_index), 0, y@value[y_index])
            
            results <- x_vals + y_vals
            nonzero_results <- results != 0
            
            new("sparse_numeric",
                value = results[nonzero_results],
                pos = as.integer(all_positions[nonzero_results]),
                length = x@length)
          })

# Multiply method

setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"),
          function(x, y){
            if (x@length != y@length)
              stop("Lengths of sparse vectors must equal")
            
            common_positions <- intersect(x@pos, y@pos)
            results <- x@value[match(common_positions, x@pos)] * y@value[match(common_positions, y@pos)]
            nonzero_results <- results != 0
            
            new("sparse_numeric",
                value = results[nonzero_results],
                pos = as.integer(common_positions[nonzero_results]),
                length = x@length)
          })

# Subtraction method

setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"),
          function(x, y){
            if (x@length != y@length)
              stop("Lengths of sparse vectors must equal")
            
            all_positions <- sort(unique(c(x@pos, y@pos))) # non zeros
            x_index <- match(all_positions, x@pos)
            y_index <- match(all_positions, y@pos)
            
            x_vals <- ifelse(is.na(x_index), 0, x@value[x_index]) # assign 0 for NA values, or X value otherwise
            y_vals <- ifelse(is.na(y_index), 0, y@value[y_index])
            
            results <- x_vals - y_vals
            nonzero_results <- results != 0
            
            new("sparse_numeric",
                value = results[nonzero_results],
                pos = as.integer(all_positions[nonzero_results]),
                length = x@length)
          })

# Cross product method

setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            if (x@length != y@length)
              stop("Lengths of sparse vectors must equal")
            common_positions <- intersect(x@pos, y@pos)
            sum(x@value[match(common, x@pos)] * y@value[match(common, y@pos)])
          })

# Square root method

setMethod("sparse_sqrt", "sparse_numeric", function(x) {
  sqrt(sum(x@value^2))
})

# Methods for +, *, and -

setMethod("+", c("sparse_numeric", "sparse_numeric"), function(e1, e2) {
  sparse_add(e1,e2)
})

setMethod("*", c("sparse_numeric", "sparse_numeric"), function(e1, e2) {
  sparse_mult(e1,e2)
})

setMethod("-", c("sparse_numeric", "sparse_numeric"), function(e1, e2) {
  sparse_sub(e1,e2)
})

# Show method
setMethod("show", "sparse_numeric", function(object) {
  cat("Here is an object of class 'sparse_numeric'\n")
  cat("The length of the sparse vector is ", object@length)
  cat(paste("Pos:Value", object@pos, ":", object@value, collapse = ", "))
})

# Plot method
setMethod("plot", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) {
            plot(x@pos, x@value, col = "blue", pch = 20,
                 xlab = "Position", ylab = "Value",
                 main = "Sparse Vectors Comparison", ...)
            points(y@pos, y@value, col = "orange", pch = 18)
            legend("bottomright", legend = c("x", "y"),
                   col = c("blue", "orange"), pch = c(20, 18))
          })