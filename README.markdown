# Wavy - Haskell WAV File Parser

Decode .wav files and get the juicy PCM data inside. This project aims to make it really
easy to play with wav files in your project by extracting the relevant PCM data straight
from the file that you provide.

## Development

Since this library is so new I currently recommend using a cabal-dev environment. You can
build the code like this:

    cabal-dev install

And that is it. It should work just fine.

## Caveats

Currently the following caveats exist:

 - Currently we really only expect you to provide .wav files that contain raw PCM data.
   None of the other audio decoding is supported even though we can read the file and
   understand that the audio format is not what it should be. The data that is contained
   iside the parsed data structure for non PCM data is currently undefined.
 - There are currently no test cases.
 - There is currently no Haddock documenation on anything.
 - There are only the barest of examples to guide you into using the library.

In short the library is new and experimental but you can use it to get data out of WAV
files that only contain raw PCM data. You can also use this library to put the PCM data
back out to a file.
