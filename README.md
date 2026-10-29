# dj-buddy

<p align="center">
  <img src="img/camelot-1-transparent.png" alt="Camelot wheel" width="400">
</p>

A Haskell utility for planning DJ sets, with a CLI and a rudimentary web app.

Browse a music library, suggest next tracks, and generate playlists using harmonic key compatibility and transitions you've tested yourself.

This started with a piece of paper next to the decks. Harmonic compatibility suggested transitions to try, but only some sounded good, so I wrote down the ones that worked. As that list grew, I wanted a way to organise it and build better sets.

The core idea is to curate a library of transitions you trust. dj-buddy helps you explore and build on those choices. It works entirely with metadata: no audio files, song analysis, or AI. Your ears and taste guide the library.

## Build and run

Install GHC and Cabal with [GHCup](https://www.haskell.org/ghcup/), then build from the project root:

```sh
cabal build
```

### Unit tests

```sh
cabal test
```

### Use from the terminal

```sh
cabal run dj-buddy -- show
cabal run dj-buddy -- show --title "Summer" --key 3D
cabal run dj-buddy -- suggest melody_express -n 3
cabal run dj-buddy -- playlist -s melody_express -n 10
```

### Use from the browser

Start the Scotty server from the project root:

```sh
cabal run dj-buddy-web
```

Open [localhost:3000/show](http://localhost:3000/show) to browse the library, filter by title or key, and generate playlists or suggestions from each track. Set the number of tracks in the form, or leave it blank for the defaults below.



## Music library

The default library is [src/library.yaml](src/library.yaml). Use `--lib path/to/library.yaml` with any command to load your own:

```yaml
tracks:
  - id: first_track
    title: First Track
    key: "3D"
  - id: second_track
    title: Second Track
    key: "4D"
transitions:
  - from: first_track
    to: second_track
    notes: "Mix after the breakdown" # optional
```

Keys use numbers `1`–`12` followed by `D` or `M`. Tested transitions are directional; use `transitions: []` if you have none.

Licensed under [BSD-3-Clause](LICENSE).
