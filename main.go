package main

import (
	"log"
	"net/http"
)

func main() {
	mux := http.NewServeMux()
	mux.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		if req.URL.Path != "/" {
			http.NotFound(w, req)
			return
		}
		http.ServeFile(w, req, "./web/ui.html")
	})
	log.Fatal(http.ListenAndServe(":24999", mux))
}
