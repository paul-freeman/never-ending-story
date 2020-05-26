package main_map

type HexCoord struct {
	X int32
	Y int32
	Z int32
}

type Location struct {
	seed int64
}

type HexMap struct {
	m   map[HexCoord]*Location
	gen Generator
}

func New(seed int64) *HexMap {
	return &HexMap{
		m:   make(map[HexCoord]*Location),
		gen: NewGen(seed),
	}
}

func (m *HexMap) Get(coord HexCoord) *Location {
	if loc, ok := m.m[coord]; ok {
		return loc
	}
	return m.gen.Gen(coord)
}
