package main

import (
	"encoding/json"
	"fmt"
	"net/http"

	"github.com/paul-freeman/never-ending-story/hexmap"
)

const (
	uiEndpoint       string = "/"
	locationEndpoint string = "/locations"
)

// serveUiHandler handles serving the Elm HTML of the UI.
func serveUiHandler(w http.ResponseWriter, req *http.Request) {
	if req.URL.Path != uiEndpoint {
		http.NotFound(w, req)
		fmt.Printf("endpoint not found: %v\n", req.URL.Path)
		return
	}
	http.ServeFile(w, req, "./web/ui.html")
}

// serveLocationHandler handles retieval of location data from the hexmap.
func serveLocationHandler(m *hexmap.HexMap) func(http.ResponseWriter, *http.Request) {
	return func(w http.ResponseWriter, req *http.Request) {
		var err error

		err = checkEndpoint(w, req, locationEndpoint)
		if err != nil {
			fmt.Println(err)
			return
		}

		var coords []hexmap.HexCoord

		err = json.NewDecoder(req.Body).Decode(&coords)
		if err != nil {
			fmt.Println(err)
			return
		}

		locs := make([]*hexmap.Location, len(coords))
		for i, coord := range coords {
			locs[i] = m.Get(coord)
		}

		err = locsResp(w, locs)
		if err != nil {
			fmt.Println(err)
			return
		}
	}
}

// locsResp writes an HTTP response for the JSON representation of map
// locations.
func locsResp(w http.ResponseWriter, locs []*hexmap.Location) (err error) {
	w.Header().Set("Content-Type", "application/json")
	err = json.NewEncoder(w).Encode(locs)
	if err != nil {
		msg := err
		w.WriteHeader(http.StatusInternalServerError)
		_, err = w.Write([]byte("500 - Internal error"))
		if err != nil {
			return fmt.Errorf("could not write response: %v: while handling error: %w", err, msg)
		}
		return msg
	}
	return nil
}

// checkEndpoint will verify that the requested endpoint matches.
//
// It returns an error if the endpoint doesn't match and replies with a 404.
func checkEndpoint(w http.ResponseWriter, req *http.Request, ep string) error {
	if req.URL.Path != ep {
		http.NotFound(w, req)
		fmt.Printf("endpoint not found: %v\n", req.URL.Path)
		return fmt.Errorf("endpoint does not match expected value: %s vs %s", ep, req.URL.Path)
	}
	return nil
}
