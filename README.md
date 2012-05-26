# xdo-guile

This is a rather simple wrapper around libxdo, provided by
[xdotool](http://www.semicomplete.com/projects/xdotool), for Guile using a
currently unofficial fork of libxdo.

## libxdo replacement?

Since this project uses an inofficial version of libxdo I thought it was only
fair to justify this.

### Why change libxdo at all?

There are honestly very little need for it at all; libxdo, in it's official
version, is a very nice library and easy to work with. Being so close to
perfect the inconcistencies in the naming convention really bothered me.
Therefor I decided to fix this and as I already done the changes it felt like
there was no real use to write my own piece of code towards the official
version instead of myown.

### The alternative codes

 + [modded xdotool](https://github.com/KoFish/xdotool/tree/naming-fixed) - The
   main differences between this version of xdotool and the official one is
   that this includes a different naming convention, an extra helper function
   and some renamed functions. I have also severly reduced the number of
   compiler warnings that xdotool has which of course could introduce problems
   but no such ones has been found yet in my testing.
 + [modded keynav](https://github.com/KoFish/keynav) - Since my first and
   foremost use of xdotool is via keynav I thought it was only proper to
   provide a version of keynav that works with my version of xdotool. This only
   has fixes to call functions with the right name.
