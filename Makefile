.PHONY: help
help: # Show help for each of the makefile recipes.
	@grep -E '^[a-zA-Z0-9 -]+:.*#'  Makefile | sort | while read -r l; do printf "\033[1;32m$$(echo $$l | cut -f 1 -d':')\033[00m:$$(echo $$l | cut -f 2- -d'#')\n"; done

r:  # Run Rstudio server
	@echo "* Running on http://127.0.0.1:8787"
	@echo "Press CTRL+C to quit"
	@python3 -c "import webbrowser; webbrowser.open_new('http://127.0.0.1:8787');"
	sudo rserver --server-daemonize=0
