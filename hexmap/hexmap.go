package hexmap

type HexCoord struct {
	Q int32
	R int32
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
