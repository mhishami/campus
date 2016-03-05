#!/bin/sh
#
rebar3 release
./_build/prod/rel/campus/bin/campus console
