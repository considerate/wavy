# Wavy over WAVE

Playing with audio data is a ton of fun and something that I believe that Haskell could do very
well. Processing audio data safely and efficiently seems to fit very well into Haskell's model so,
overy a year ago, I started working on and off on a WAVE file format parser. I have been working on
it very infrequently (bus trips and other spare time) and I rewrote it once but today I am pleased
to announce the release of the very first version of my 'wavy' package that lets you extract data
from WAVE files in Haskell. The features of this release include:

 - Methods to Parse and Assemble Wave Files
 - Support for different orderings of RIFF chunks (via the [riff][1] library I wrote previously)
 - A split between the parsers for the container format and the data allowing efficient metadata
   parsing.
 - The ability to pase the data into Int64 or Float formats so that you can handle the data in
   whichever way that you please.
 - Example programs that make use of the library for your perusal and use.

Things which the library is currently missing include:

 - RIFX support
 - Direct support for maintaining RIFF chunks that are not mentioned in the WAVE specification.

## Getting the Code

The code is [on hackage as the wavy library][2] so you can install it by:

    cabal update
    cabal install wavy

Please feel free to give it a try. Probably the best way to quickly see it working is by finding a
WAVE file on your machine:

    locate '*.wav'

And then passing that wave file as the first argument into wave-info. For example:

    $ wave-info /Applications/Steam.app/Contents/MacOS/Friends/friend_join.wav
    File: /Applications/Steam.app/Contents/MacOS/Friends/friend_join.wav - 2s
       Format
         Audio Format:  Microsoft PCM
         Channels:      2
         Sample Rate:   44100
         Bits Per Sample:  16
         Byte Rate:  176400
         Block Alignment:  4
    
       INFO Metadata
         Creation Date: 2010-03-11
         Engineers:       - Kelly Thornton
         Creation Software: Sony Sound Forge 9.0
    $

As you can see it can parse an audio file very quickly. The wave-info program is very efficient
because the library is lazy and does not parse the data chunk unless you specifically require it.

## Using the Library

I would recommend that you start by looking at the executables in the libraries source code for
examples of how this library can be used in your applications. The [wave-info source code can be
found on BitBucket][3].

Once you have finished doing that then you can have a read throug the documentation on Hackage to
get a full understanding of what methods the library provides.

If you have any questions then please do not hesitate to contact me or comment on the blog.

(This blog post was produced using [pandoc][4])

 [1]: http://hackage.haskell.org/package/riff
 [2]: http://hackage.haskell.org/package/wavy
 [3]: https://bitbucket.org/robertmassaioli/wavy/src/26006fd3f1f637b43096020282e608f0d4e9e060/src/Info.hs
 [4]: http://hackage.haskell.org/package/pandoc

