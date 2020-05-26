package main_map

type Generator interface {
	Gen(HexCoord) *Location
}
