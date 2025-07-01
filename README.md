# Prelude
Prelude is a musical composition DSL in Haskell. It stands out thanks to its accesible syntax and innovative music representation. It handles MIDI files manipulation, audio synthesis and graphic representations for the musical pieces.

### Contents
* [Getting Started](#getting-started)
* [Prelude's Music Representation](#preludes-music-representation)
* [Graphic Representations in Prelude](#graphic-representations-in-prelude)
* [Current Limitations](#current-limitations)

## Getting Started
To start working with Prelude, follow the steps below:
1. Clone the package
   ```
   git clone https://github.com/iam-mj/haskell_musical_composition.git
   ```
2. Run Prelude's repl
   ```
   cabal run
   ```
3. Start playing around!
   If you see the following prompt, you're good to go:
   ```
   prelude>
   ```

Prelude has its own syntax. It was designed to help music lovers who are beginners in programming figure out the syntax quickly.
Check out some [examples](grammar.txt) for the available instructions (as well as the formal definition of the grammar).

For details about the possible instructions or a certain instruction's syntax, use the `help` instruction:
* List all the available instructions and general information:
```
prelude> help
```
* Find out more about a certain instruction:
```
prelude> help [instruction-name]
```

## Prelude's Music Representation
Prelude incorporates two unconventional decisions in its music representation. Both try to ease musical composition by increasing the abstractisation of music in a natural manner.
1. Groups

   We work with **groups** as the atomic unit of music, instead of notes. Groups are any number of notes played at the same time at one point.
   Therefore, groups cover: single notes, rests, dyads (called *duo*s in Prelude) and chords.

   The decision to use them comes from the desire to shorten duos and chords definition, as well as to encourage a relative view of music (we don't see a chord as
   distinct precise notes, but as a number of notes with a certain relation in regards to a *root note*).

   The 4 types of groups make up the possible constructors of a **track**:
   ```
   note:  [pitch] [duration]
   rest:  [duration]
   duo:   [interval] [root-pitch] [root-duration]
   chord: [chord] [root-pitch] [root-duration]
   ```
   See more details about how the groups are constructed with `help track`.

3. Tracks and Melodies

   We make a distinction between relative compositions (compositions without a context, made relative to a certain octave) and the final form of a composition (with context, ready to be played).

   This comes from our desire to permit users to play around with their music and easily try different variations of it, without having to place it in a limiting context from the beginning.
   In order to do that, we try not to burden compositions with an unnecessary context until the very end. This context is made up of the reference octave and the instrument.

   Therefore, we get two forms of music compositions:
   * **tracks**   = groups played sequentially, defined relative to an octave
   * **melodies** = tracks given context played at the same time
   We let tracks be parallelized to be able to compose music pieces with multiple instruemnts and to easily manage parts in different octaves.

    See more details about tracks and melodies with `help track` and `help melody`.
   
## Graphic Representations in Prelude

We offer currently 3 methods of graphic representation for music compositions constructed in Prelude:

1. External Visualizer

   Provided via the [Noteflight](https://www.noteflight.com) music notation software and available through the `vis` instruction:
   ```
   prelude> vis [melody-name]
   ```
   The `vis` instruction will open up an in-browser Noteflight score importing the melody.
  
   Unfortunetly, a current Noteflight bug at MIDI files import makes the created score representation unpredictable. It usually affects note duration.
  
   Prelude uses the Noteflight Launch API. For more information about the API, please visit the [Noteflight documentation](https://www.noteflight.com/info/api/server_doc#launch).
   Noteflight is a registered trademark of Hal Leonard, LLC. Prelude uses the Noteflight Launch API under the terms outlined in their documentation. All rights to Noteflight and its services remain with Hal Leonard, LLC.

2. Score Visualizer

   An in-browser score visualizer for musical compositions, provided via the [html-midi-player](https://github.com/cifkao/html-midi-player) web components, accesible through the `score` instruction:
   ```
   prelude> score [melody-name]
   ```
   It provides a correct score representation, but only works with composition with a single instrument. Better yet, it performes best with compositions made up of a single track.

   html-midi-player is used under the terms of the MIT License. This project is not affiliated with or endorsed by its creator, [cifkao](https://github.com/cifkao).

3. Prelude Visualiser

   A Prelude implementation of a simple visualizer for tracks, accesible via the `see` instruction:
   ```
   prelude> see {[track-name] oct[octave] [instrument]}
   ```
   We provide a context for the track because the visualizer is accompanied by a [html-midi-player](https://github.com/cifkao/html-midi-player) audio player.
   
## Current Limitations
Some weaknesses we're aware of:
* tempo is set at 120 BPM
* volume is fixed at half its maximum value
* MIDI channel 10 special percussions events are not available
* can't define a music piece with more than 15 different instruments
* there are some score-specific accesories that lack an implementation (e.g. legato/staccato, scale notation)
