package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
	"strings"
)

const (
	TOP = iota
	RIGHT
	BOTTOM
	LEFT
)

type Tile struct {
	ID   string
	data [][]string
}

func (t *Tile) ToString() string {
	b := bytes.NewBufferString("")

	for _, row := range t.data {
		b.WriteString(strings.Join(row, ""))
		b.WriteString("\n")
	}

	return b.String()
}

func (t *Tile) Copy() *Tile {
	rows := make([][]string, 0, len(t.data))
	for _, r := range t.data {
		_row := make([]string, 0, len(r))
		for _, c := range r {
			_row = append(_row, c)
		}
		rows = append(rows, _row)
	}

	return &Tile{
		ID:   t.ID,
		data: rows,
	}
}

func (t *Tile) Rotate() {
	l := len(t.data)
	for y := 0; y < l/2; y++ {
		for x := y; x < l-1-y; x++ {
			x_1, y_1 := l-y-1, x
			x_2, y_2 := l-y_1-1, x_1
			x_3, y_3 := l-y_2-1, x_2
			x_4, y_4 := x, y

			t.data[x_1][y_1], t.data[x_2][y_2], t.data[x_3][y_3], t.data[x_4][y_4] = t.data[x_4][y_4], t.data[x_1][y_1], t.data[x_2][y_2], t.data[x_3][y_3]
		}
	}
}

func (t *Tile) Shrink() {
	l := len(t.data)

	rows := make([][]string, 0, l-2)
	for y := 1; y < l-1; y++ {
		cols := make([]string, 0, l-2)
		for x := 1; x < l-1; x++ {
			cols = append(cols, t.data[y][x])
		}

		rows = append(rows, cols)
	}

	t.data = rows
}

func (t *Tile) Flip() {
	l := len(t.data)
	for x := 0; x < l; x++ {
		for y := 0; y <= (l/2)-1; y++ {
			t.data[x][y], t.data[x][l-1-y] = t.data[x][l-1-y], t.data[x][y]
		}
	}
}

func (t *Tile) GenerateAlternatives() []*Tile {
	r1 := t.Copy()
	r1.Rotate()
	r1.ID = r1.ID + "_R1"
	r2 := t.Copy()
	r2.Rotate()
	r2.Rotate()
	r2.ID = r2.ID + "_R2"
	r3 := t.Copy()
	r3.Rotate()
	r3.Rotate()
	r3.Rotate()
	r3.ID = r3.ID + "_R3"
	f := t.Copy()
	f.Flip()
	f.ID = f.ID + "_F"
	f1 := f.Copy()
	f1.Rotate()
	f1.ID = f1.ID + "_R1"
	f2 := f.Copy()
	f2.Rotate()
	f2.Rotate()
	f2.ID = f2.ID + "_R2"
	f3 := f.Copy()
	f3.Rotate()
	f3.Rotate()
	f3.Rotate()
	f3.ID = f3.ID + "_R3"

	return []*Tile{
		t.Copy(),
		r1, r2, r3,
		f, f1, f2, f3,
	}
}

func (t *Tile) Sides() []string {
	sides := make([]string, 4)

	// TOP
	sides[TOP] = strings.Join(t.data[0], "")

	// RIGHT
	var right string
	for i := 0; i < len(t.data[0]); i++ {
		right = right + t.data[i][len(t.data[0])-1]
	}
	sides[RIGHT] = right

	// BOTTOM
	sides[BOTTOM] = strings.Join(t.data[len(t.data[0])-1], "")

	// LEFT
	var left string
	for i := 0; i < len(t.data[0]); i++ {
		left = left + t.data[i][0]
	}
	sides[LEFT] = left

	return sides
}

func getCanonicalID(id string) string {
	return strings.Split(id, "_")[0]
}

var tileRegexp = regexp.MustCompile(`^Tile (\d+):$`)

func main() {
	tiles := parseInput("input.txt")
	// tiles := parseInput("example.txt")
	index := make(map[string]*Tile)
	for _, t := range tiles {
		for _, t := range t.GenerateAlternatives() {
			index[t.ID] = t
		}
	}

	topIndex := make(map[string][]string)
	rightIndex := make(map[string][]string)
	bottomIndex := make(map[string][]string)
	leftIndex := make(map[string][]string)
	for id, t := range index {
		sides := t.Sides()

		topIndex[sides[TOP]] = append(topIndex[sides[TOP]], id)
		rightIndex[sides[RIGHT]] = append(rightIndex[sides[RIGHT]], id)
		bottomIndex[sides[BOTTOM]] = append(bottomIndex[sides[BOTTOM]], id)
		leftIndex[sides[LEFT]] = append(leftIndex[sides[LEFT]], id)
	}

	size := 12
	//size := 3
	solution := make([][]string, 0)
	for i := 0; i < size; i++ {
		solution = append(solution, make([]string, size))
	}
	placed := make(map[string]bool)

	found := findSolution(solution, size, 0, 0, index, placed)
	if !found {
		panic("could not find solution")
	}

	ul, _ := strconv.Atoi(getCanonicalID(solution[0][0]))
	ur, _ := strconv.Atoi(getCanonicalID(solution[0][size-1]))
	ll, _ := strconv.Atoi(getCanonicalID(solution[size-1][0]))
	lr, _ := strconv.Atoi(getCanonicalID(solution[size-1][size-1]))

	fmt.Printf("Solution 1: %d\n", ul*ur*ll*lr)
	fmt.Printf("Solution 1: %v\n", solution)

	// Shrink all the tiles in the solution
	for y := 0; y < size; y++ {
		for x := 0; x < size; x++ {
			index[solution[y][x]].Shrink()
		}
	}

	combined := combineTiles(solution, index)
	for _, t := range combined.GenerateAlternatives() {
		fmt.Println(t.ID)
		fmt.Println(t.ToString())
		fmt.Println("-------")
	}

	tileWithMonsters := findSeaMonster(combined)
	fmt.Println("Found in", tileWithMonsters.ID)
	fmt.Println(tileWithMonsters.ToString())

	answer := 0
	for y := 0; y < len(tileWithMonsters.data); y++ {
		for x := 0; x < len(tileWithMonsters.data); x++ {
			if tileWithMonsters.data[y][x] == "#" {
				answer++
			}
		}
	}
	fmt.Printf("Solution 2: %d\n", answer)
}

func findSeaMonster(t *Tile) *Tile {
	var largest int
	var largestT *Tile

	for _, t := range t.GenerateAlternatives() {
		n := 0
		for y := 0; y < len(t.data); y++ {
			for x := 0; x < len(t.data); x++ {
				if isSeaMonster(t.data, x, y) {
					n++
					markSeaMonster(t.data, x, y)
				}
			}
		}
		if n >= largest {
			fmt.Printf("Tile %s has %d monsters\n", t.ID, n)
			largestT = t
			largest = n
		}
	}

	return largestT
}

/*
+ -------------------+
|                  # |
|#    ##    ##    ###|
| #  #  #  #  #  #   |
+--------------------+
*/
func isSeaMonster(data [][]string, x, y int) bool {
	size := len(data)
	if x > size-19 {
		return false
	}

	if y > size-2 {
		return false
	}

	mustBeSet := [][]int{
		{x + 18, y},
		{x, y + 1},
		{x + 5, y + 1},
		{x + 6, y + 1},
		{x + 11, y + 1},
		{x + 12, y + 1},
		{x + 17, y + 1},
		{x + 18, y + 1},
		{x + 19, y + 1},
		{x + 1, y + 2},
		{x + 4, y + 2},
		{x + 7, y + 2},
		{x + 10, y + 2},
		{x + 13, y + 2},
		{x + 16, y + 2},
	}
	allSet := true
	for _, coord := range mustBeSet {
		allSet = allSet && data[coord[1]][coord[0]] == "#"
	}

	return allSet
}

func markSeaMonster(data [][]string, x, y int) {
	mustBeSet := [][]int{
		{x + 18, y},
		{x, y + 1},
		{x + 5, y + 1},
		{x + 6, y + 1},
		{x + 11, y + 1},
		{x + 12, y + 1},
		{x + 17, y + 1},
		{x + 18, y + 1},
		{x + 19, y + 1},
		{x + 1, y + 2},
		{x + 4, y + 2},
		{x + 7, y + 2},
		{x + 10, y + 2},
		{x + 13, y + 2},
		{x + 16, y + 2},
	}
	for _, coord := range mustBeSet {
		data[coord[1]][coord[0]] = "O"
	}
}

func combineTiles(tileIDs [][]string, index map[string]*Tile) *Tile {
	sizePer := len(index[tileIDs[0][0]].data)
	size := sizePer * len(tileIDs)
	rows := make([][]string, 0, size)

	for y := 0; y < size; y++ {
		cols := make([]string, 0, size)
		for x := 0; x < size; x++ {
			tileY := y / sizePer
			tileX := x / sizePer
			tile := index[tileIDs[tileY][tileX]]
			cols = append(cols, tile.data[y%sizePer][x%sizePer])
		}

		rows = append(rows, cols)
	}

	return &Tile{
		ID:   "COMBINED",
		data: rows,
	}
}

// This is sort of naive. We could use the tiles indexed by sides to speed it up.
func findSolution(sol [][]string, size, x, y int, index map[string]*Tile, placed map[string]bool) bool {

	// Base case. We go to the end of the puzzle so we've found solution
	if y >= size {
		return true
	}

	// We're finding the solution for solution[y][x]

	for id, tile := range index {
		canonicalID := getCanonicalID(id)

		// We can't consider this tile if we've already used it
		if placed[canonicalID] {
			continue
		}

		here := tile
		isValid := true

		// If there is something above
		if y > 0 && sol[y-1][x] != "" {
			above := index[sol[y-1][x]]
			isValid = isValid && above.Sides()[BOTTOM] == here.Sides()[TOP]
		}

		// If there is something to the right
		if x < len(sol)-1 && sol[y][x+1] != "" {
			right := index[sol[y][x+1]]
			isValid = isValid && right.Sides()[LEFT] == here.Sides()[RIGHT]
		}

		// If there is something below
		if y < len(sol)-1 && sol[y+1][x] != "" {
			below := index[sol[y+1][x]]
			isValid = isValid && below.Sides()[TOP] == here.Sides()[BOTTOM]
		}

		// If there is something to the left
		if x > 0 && sol[y][x-1] != "" {
			left := index[sol[y][x-1]]
			isValid = isValid && left.Sides()[RIGHT] == here.Sides()[LEFT]
		}

		if !isValid {
			continue
		}

		// We can use this tile for (x,y)
		sol[y][x] = id
		placed[canonicalID] = true

		// See if we can find a solution for the next tile
		var (
			nx = x + 1
			ny = y
		)
		if nx == size {
			nx = 0
			ny++
		}

		if findSolution(sol, size, nx, ny, index, placed) {
			return true
		}

		sol[y][x] = ""
		delete(placed, canonicalID)
	}

	return false
}

func parseInput(filename string) []*Tile {
	tiles := make([]*Tile, 0)
	var nTiles int
	for _, line := range readLines(filename) {
		if len(line) == 0 {
			continue
		}

		if tileRegexp.MatchString(line) {
			matches := tileRegexp.FindAllStringSubmatch(line, -1)
			id := matches[0][1]
			nTiles++
			tiles = append(tiles, &Tile{ID: id})
			continue
		}

		// We are parsing the data
		row := make([]string, 0)
		for i := range line {
			row = append(row, string(line[i]))
		}
		tiles[nTiles-1].data = append(tiles[nTiles-1].data, row)
	}

	return tiles
}

func readLines(filename string) []string {
	c, err := ioutil.ReadFile(filename)
	if err != nil {
		panic(err)
	}

	return strings.Split(string(c), "\n")
}
