// totientRange.go - Parallel Euler Totient Function (Go Version)
// compile: go build
// run:     totientRange lower_num upper_num

// Chase Condon	   10/11/2019
// Modified from sequential version by Phil Trinder

// This program calculates the sum of the totients between a lower and an
// upper limit
//
// Each function has an executable Haskell specification
//
// It is based on earlier work by: Greg Michaelson, Patrick Maier, Phil Trinder,
// Nathan Charles, Hans-Wolfgang Loidl and Colin Runciman

package main

import (
	"fmt"
	"os"
	"strconv"
	"time"
	"sync"
)

// Compute the Highest Common Factor, hcf of two numbers x and y
//
// hcf x 0 = x
// hcf x y = hcf y (rem x y)

func hcf(x, y int64) int64 {
	var t int64
	for y != 0 {
		t = x % y
		x = y
		y = t
	}
	return x
}

// relprime determines whether two numbers x and y are relatively prime
//
// relprime x y = hcf x y == 1

func relprime(x, y int64) bool {
	return hcf(x, y) == 1
}

// euler(n) computes the Euler totient function, i.e. counts the number of
// positive int64egers up to n that are relatively prime to n
//
// euler n = length (filter (relprime n) [1 .. n-1])

func euler(n int64) int64 {
	var length, i int64

	length = 0
	for i = 1; i < n; i++ {
		if relprime(n, i) {
			length++
		}
	}
	return length
}

// helper function for workers 
// pulls jobs from the job pool and runs the Euler function 
func worker(id int, jobs <-chan int64, results chan<- int64, wg *sync.WaitGroup) {
	for j := range jobs {
		results <- euler(j)
		wg.Done()
	}
}

// sumTotient lower upper sums the Euler totient values for all numbers
// between "lower" and "upper".
// 
// creates a pool of worker threads and gives them a job channel
// which is populated with numbers in the range [lower, upper]
//
// sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])

func sumTotient(lower, upper, num_threads int64) int64 {
	var wg sync.WaitGroup
	jobs := make(chan int64, upper)
	results := make(chan int64, upper)
	var sum, i int64

	for w := 1; w <= int(num_threads); w++ {
		go worker(w, jobs, results, &wg)
	}

	for i = lower; i <= upper; i++ {
		wg.Add(1)
		jobs <- i
	}
	close(jobs)
	wg.Wait()
	close(results)

	sum = 0
	for result := range results {
		sum += result
	}
	
	return sum
}

func main() {
	var lower, upper, num_threads int64
	var err error

	// Read and validate lower and upper arguments
	if len(os.Args) < 4 {
		panic(fmt.Sprintf("Usage: must provide lower, upper range limits, and number of threads as arguments"))
	}

	if lower, err = strconv.ParseInt(os.Args[1], 10, 64); err != nil {
		panic(fmt.Sprintf("Can't parse first argument"))
	}
	if upper, err = strconv.ParseInt(os.Args[2], 10, 64); err != nil {
		panic(fmt.Sprintf("Can't parse second argument"))
	}
	if num_threads, err = strconv.ParseInt(os.Args[3], 10, 64); err != nil {
		panic(fmt.Sprintf("Can't parse third argument"))
	}

	start := time.Now()

	totientSum := sumTotient(lower, upper, num_threads)

	fmt.Println("Sum of Totients between", lower, "and", upper, "is", totientSum)
	elapsed := time.Since(start)
	fmt.Println("Elapsed time", elapsed.Seconds())
}
