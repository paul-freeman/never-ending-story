package hexmap

type Generator interface {
	Gen(HexCoord) *Location
}
