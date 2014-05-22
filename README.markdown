# Wavy - Haskell WAV File Parser

Decode .wav files and get the juicy PCM data inside. This project aims to make it really
easy to play with wav files in your project by extracting the relevant PCM data straight
from the file that you provide.

## Development

Since this library is so new I currently recommend using a cabal-dev environment. You can
build the code like this:

    cabal sandbox init
    cabal install

And that is it. It should work just fine.

## Example Programs

We have provided the following example programs so that you can test out this library
immediately after installing it:

 - wave-info
   Just point this program at a WAV file and it will give you all of the
   information about it. Things like how long it will take to play and the bit rate. It
   will also give you the details in the information block of the WAV file.
 - wave-identity
   This is a rather boring program, you give it a WAV file and it will parse it and spit
   it back out again. In the future it may be entented to convert the file into RIFX
   format but for now it just serves as useful proof that WAV files are parsed and piped
   out again unmolested.
 - wave-generate-sine
   With this program you can generate a WAV file that contains a sine wave at the
   frequency of your choosing, you can also choose for how long this sine wave plays.
   Proof that you can synthesize sounds and then pipe them out into WAV files using this
   library.

These example programs are designed to show you this library in action so that you can
have a play around with it quickly and prove to yourself that it works as it is supposed
to. Please feel free to suggest or even write more example code using this library; I
would love to see what people come up with.

## Caveats

Currently the following caveats exist:

 - Currently we really only expect you to provide .wav files that contain raw PCM data.
   None of the other audio decoding is supported even though we can read the file and
   understand that the audio format is not what it should be. The data that is contained
   iside the parsed data structure for non PCM data is currently undefined.
 - There are currently no test cases.

In short the library is new and experimental but you can use it to get data out of WAV
files that only contain raw PCM data. You can also use this library to put the PCM data
back out to a file. Hopefully this makes it easier for you to process audio data in
Haskell.
