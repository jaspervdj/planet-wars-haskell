planet-wars-haskell
===================

Unofficial Haskell starter package for
[the google ai contest](http://ai-contest.com).

What
----

The [PlanetWars](http://github.com/jaspervdj/planet-wars-haskell/blob/master/PlanetWars.hs)
module contains a small library to build your bot on. The
[MyBot](http://github.com/jaspervdj/planet-wars-haskell/blob/master/MyBot.hs)
module contains a simple example bot, based on the example in the C++ starter
package.

You should change the `MyBot` module to your liking. If you make any
improvements to the `PlanetWars` module, I would appreciate if you have me pull
your changes, so everyone can benefit from your improvements -- after all, we're
trying to get the Haskell bots as high as possible in the ranking, right?

Building
--------

You should be able to build your bot using:

    ghc --make -O2 MyBot.hs

The server uses the `-O2` flag as well. Now, you should have an executable
called `MyBot`.

Running
-------

Let's watch it fight against one of the java bots.

    java -jar tools/PlayGame.jar maps/map7.txt 1000 1000 log.txt \
        "./MyBot" "java -jar example_bots/RandomBot.jar" | \
        java -jar tools/ShowGame.jar

More information can be found [here](http://ai-contest.com/using_the_tools.php).
