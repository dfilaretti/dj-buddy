# dj-buddy

<p align="center">
  <img src="img/camelot-1-transparent.png" alt="Camelot wheel" width="400">
</p>

A Haskell utility for planning DJ sets, with a CLI and a rudimentary web app.

Browse a music library, suggest next tracks, and generate playlists using harmonic key compatibility and transitions you've tested yourself.

This started with a piece of paper next to the decks. Harmonic compatibility suggested transitions to try, but only some sounded good, so I wrote down the ones that worked. As that list grew, I wanted a way to organise it and use that accumulated knowledge to build better sets.

The core idea is to manually curate a library of transitions you trust. dj-buddy helps you explore and build on those choices. It works entirely with metadata: no audio files, song analysis, or AI. Your ears and taste guide the library.

See [blog post](www.google.com) for all the details for detailed informations.

## Build and run

Requires Haskell (GHC) and Cabal to be installed.
Please refer to [ghcup](https://www.haskell.org/ghcup/) to set things up.

Once Haskell is installed,

```sh
cabal build
```

should build the project.

### Unit tests

```sh
cabal run test
```

### Use from the terminal

The app can be used from the terminal:

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

Open one of these URLs in your browser. `/show` has a simple HTML page with title and key textboxes and an **Apply filters** button. Leave either textbox empty to omit that filter. Playlist and suggestion pages display numbered track lists with a link back to the library:

| Route | Parameters | Example |
| --- | --- | --- |
| `/show` | Optional `title` and `key` filters | [Show matching tracks](http://localhost:3000/show?title=Summer&key=3D) |
| `/playlist` | Optional `start` track ID; integer `n` defaults to `10` | [Generate a playlist](http://localhost:3000/playlist?start=melody_express&n=10) |
| `/suggest` | Required `track` ID; integer `n` defaults to `3` | [Suggest next tracks](http://localhost:3000/suggest?track=melody_express&n=3) |

Use [http://localhost:3000/show](http://localhost:3000/show) to list all tracks, or [http://localhost:3000/playlist](http://localhost:3000/playlist) to generate a playlist starting with a random track. All three routes accept an optional `lib` parameter, for example `/show?lib=path/to/library.yaml`. Paths are relative to the server's working directory.

Separate query parameters with `&`, omit quotes around values, and URL-encode spaces as `%20`. Invalid keys or non-integer `n` values return HTTP 400. Stop the server with Ctrl+C; restart it after code changes.

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
