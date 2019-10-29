run:
	elm-live src/Main.elm -u -- --output=elm.min.js

analyse:
	elm-analyse -s