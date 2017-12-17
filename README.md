An arma3 artillery computer written in Elm
===========================================

This project is an artillery calculator for arma3 written in Elm.

Why?
----

Because some servers disallow the in-game computer and calculating ballistics
by hand is a pain in the ass.

Why Elm?
---------

Because I wanted to try Elm, and it turns out to be well suited for this type
of project with little state!

OMG You're doing it wrong
--------------------------

Since this is my fist project with Elm I am likely to do things wrong. I would
be very, very happy to hear what I could improve and why! A few of my
discoveries have surprised me a little, in particular:

- It seems to be harder than it should to update nested model types.
- I can't seem to find a way to iterate over fields of a type elegantly. In
  other words: I can't seem to write a function that takes a type and the name
  of a field, then do anything useful with the value of that field.


I want to contribute!
---------------------

It's just a toy project, but feel free to fork and send stuff!
Otherwise, go play some Arma3 :)

The future
-----------

A few more things need to be done here:

 - Styling of the absolutely horrible HTML this currently generates
 - Adding unit tests. It's pretty straightforward to test by hand, in-game, but
   I'd like to know how an Elm test harness is setup.
