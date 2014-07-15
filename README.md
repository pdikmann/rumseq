# RUMSeq 
![Screenshot](http://dikidoom.github.io/rumseq.png)
RUMSeq is a Ridiculously Underpowered Midi Sequencer for Mac OS written in [racket](http://racket-lang.org/).

## Features
- Piano Roll for editing Phrases
- Phrase Board for compositing Phrases
- 6 Tracks, 10 Channels
- Per-Track Playback
- Adjustable BPM

## Manual
### Starting
1. edit `config.rkt` to set midi-port (default is auto-detection)
2. run `$ racket main.rkt`

### Usage
There are 4 Parts to RUMSeq:
- the Piano Roll (right)
- the Phrase Board (top left)
- the Tracks (bottom left)
- the BPM (bottom-most left)

#### Piano Roll
- `left-click & -drag` to draw notes
- `right-click` to delete notes
- `left-click & -drag` in top blue row to adjust Phrase length

#### Phrase Board
- `left-click` to edit Phrase in Piano Roll
- `right-click` to delete Phrase
- `left-drag` to another slot: copy Phrase
- `left-drag` to a track: add Phrase to track

#### Tracks
- `scroll` number to adjust midi channel
- `scroll` track to skip steps (to ease syncing)
- `click` number to start / stop playback
- `left-drag` to Phrase Board to make Phrase from Track *(buggy)*
- `right-click` to delete Track

#### BPM
- `left-click` to tap BPM
- `scroll` to adjust BPM