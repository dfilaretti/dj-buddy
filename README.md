# dj-buddy

A Haskell command-line tool for planning DJ sets. Browse a music library, suggest next tracks, and generate playlists using harmonic key compatibility and transitions you've tested yourself.

This started with a piece of paper next to the decks. Harmonic compatibility suggested transitions to try, but only some sounded good, so I wrote down the ones that worked. As that list grew, I wanted a way to organise it and use that accumulated knowledge to build better sets.

The core idea is to manually curate a library of transitions you trust. dj-buddy helps you explore and build on those choices. It works entirely with metadata: no audio files, song analysis, or AI. Your ears and taste guide the library.

## Build and run

Requires GHC 9.10–9.14 and Cabal. Run these commands from the project root:

```sh
cabal build
cabal run dj-buddy -- show
cabal run dj-buddy -- show --title "Summer" --key 3D
cabal run dj-buddy -- suggest melody_express -n 3
cabal run dj-buddy -- playlist -s melody_express -n 10
```

Playlists prefer tested transitions, then same-key matches, then compatible keys. Ties are chosen randomly, tracks aren't repeated, and a playlist may end early if no suitable unused track remains. Omit `-s` to start with a random track.

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

## Architecture

The project separates a reusable Haskell library from the CLI. `Domain` contains the music library, harmonic compatibility, transition scoring, and playlist logic. `App` exposes application-level operations for browsing tracks, suggesting transitions, and generating playlists; the CLI calls these operations through `App`.

A future web UI would use the same `App` interface, sharing the existing functionality.

## Future work

- **Library imports:** the YAML file is currently maintained by hand. Importing track metadata from Traktor, rekordbox, and similar tools would make larger libraries easier to manage while keeping transition curation manual.
- **Richer metadata:** the format is deliberately minimal. BPM is omitted because my sets stay around 138–140 BPM, so I already assume the tracks are tempo-compatible. Artist names are also omitted because I know this small collection well. Both would be useful additions as the library grows, but add little to the current proof of concept.
- **Web UI:** a web view could make browsing and curating the library easier, using the same application library as the CLI.
- **add some chaos** the program currently tries to select the "best next transition", looking 1 step ahead. This works, but we could make it smarter, e.g. what if the next immediate transition is fantastic but from then there's nothing interesting we can do from there? And, conversely, what if we pick a next transition which is just ok but then it unlocks an amazing part of the library with a lot of fantastic transitions? 

Licensed under [BSD-3-Clause](LICENSE).
