Original markdown & database belongs to https://github.com/ajott/ajott.github.io .
I've decided to not make a fork, cause it's big multirepo there, and I only needed DH2 character generator.

###### How to compile
Install yourself an Elm language compiler from here https://elm-lang.org/
Run in console (or in IDE terminal):
```
elm make src/Charsheet.elm --output charsheet.js
```
This will generate charsheet.js file, which is included in `charsheet.html`, which is the page you want to view in browser.
