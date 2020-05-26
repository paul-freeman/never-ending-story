package main_map

import "math/rand"

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
	x := rand.New(rand.NewSource(int64(coord.X))).Int63()
	y := rand.New(rand.NewSource(int64(coord.Y))).Int63()
	z := rand.New(rand.NewSource(int64(coord.Z))).Int63()
	s := rand.New(rand.NewSource(b.seed)).Int63()
	return &Location{
		seed: x ^ y ^ z ^ s,
	}

}
