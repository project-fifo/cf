cf
=====

A helper library for termial colour printing extending the io:format
syntax to add colours.

```erlang
%% Effectively the same as io:format just takes the additional color
%% console text colour can be set by ~!**<colour>**. ~_**<colour>**
%% will produce underlined text and ~#**<colour>** will change the
%% background. Both ~# and ~_ only work with lowercase colours.
%% An uppercase letersindicate bold colours.
%%
%% The colour can be one of:
%%
%%   !   - resets the output
%%   x,X - black
%%   r,R - red
%%   g,G - greeen
%%   y,Y - yellow
%%   b,B - blue
%%   m,M - magenta
%%   c,C - cyan
%%   w,W - white
%%
%%  The function will disable colours on non x term termials
```

Build
-----

    $ rebar3 compile
