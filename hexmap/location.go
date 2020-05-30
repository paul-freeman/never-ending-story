package hexmap

const (
	_ = iota // empty
	_        // circle
	_        // triangle
	_        // square
	SHAPE_MAX
)

type Location struct {
	Q     int32
	R     int32
	Shape int32
}
