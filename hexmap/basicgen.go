package hexmap

import (
	"math/bits"
	"math/rand"
)

type basicGenerator struct {
	Generator
	seed int64
}

func NewGen(seed int64) Generator {
	return basicGenerator{
		seed: seed,
	}
}

func (b basicGenerator) Gen(coord HexCoord) *Location {
	x := rand.New(rand.NewSource(int64(coord.Q))).Int63()
	y := rand.New(rand.NewSource(int64(bits.Reverse64(uint64(coord.R))))).Int63()
	s := rand.New(rand.NewSource(b.seed)).Int63()
	// fmt.Printf("In: %v, %v\nOut: %v, %v, %v\n", coord.Q, coord.R, x, y, s)
	ra := rand.New(rand.NewSource(x ^ y ^ s))
	var shape int32 = 0
	if r0 := ra.Int63(); r0%19 == 0 {
		shape = 1
	} else if r0 := ra.Int63(); r0%41 == 0 {
		shape = 2
	} else if r0 := ra.Int63(); r0%97 == 0 {
		shape = 3
	}

	return &Location{
		Q:     coord.Q,
		R:     coord.R,
		Shape: shape,
	}
}
