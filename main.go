package main

import (
	"log"
	"net/http"

	"github.com/paul-freeman/never-ending-story/hexmap"
)

func main() {
	m := hexmap.New(0)
	mux := http.NewServeMux()
	mux.HandleFunc(uiEndpoint, serveUiHandler)
	mux.HandleFunc(locationEndpoint, serveLocationHandler(m))
	log.Fatal(http.ListenAndServe(":24999", mux))
}
