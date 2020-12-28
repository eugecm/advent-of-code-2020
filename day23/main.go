package main

import "fmt"

type Cup struct {
	value int
	next  int
}

func initNextTo(l []int, size int) map[int]int {
	m := make(map[int]int, size)
	for i, n := range l {
		m[n] = l[(i+1)%len(l)]
	}

	if size > len(l) {
		for i := len(l); i < size; i++ {
			m[i+1] = i + 2
		}

		// Tie the original block to the new one
		m[l[len(l)-1]] = len(l)+1

		// Loop the whole thing
		m[size] = l[0]
	}

	return m
}

func move(cur int, nextTo map[int]int) int {
	// Removing x1 x2 x3 is the same as linking cur with nextTo[x3]
	x1 := nextTo[cur]
	x2 := nextTo[x1]
	x3 := nextTo[x2]
	nextTo[cur] = nextTo[x3]

	// Destination cup is (n-1) (assuming it wasn't removed)
	destination := cur - 1
	if destination == 0 {
		destination = len(nextTo)
	}
	for destination == x1 || destination == x2 || destination == x3 {
		destination--
		if destination == 0 {
			destination = len(nextTo)
		}
	}

	// link destination with the elements we removed
	next := nextTo[destination]
	nextTo[destination] = x1
	nextTo[x3] = next

	return nextTo[cur]
}

func labels(m map[int]int, len int, start int) []int {
	lb := make([]int, 0, len)
	for i := 0; i < len; i++ {
		n := m[start]
		lb = append(lb, n)
		start = n
	}

	return lb
}

func main() {
	l := []int{4, 6, 7, 5, 2, 8, 1, 9, 3}
	// l := []int{3, 8, 9, 1, 2, 5, 4, 6, 7}
	nextTo := initNextTo(l, 1_000_000)
	s := l[0]
	for i := 0; i < 10_000_000; i++ {
		s = move(s, nextTo)
	}
	fmt.Println(nextTo[1])
	fmt.Println(nextTo[nextTo[1]])
	fmt.Printf("answer: %d\n", nextTo[1] * nextTo[nextTo[1]])
}
