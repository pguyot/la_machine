#!/bin/bash

rebar3 clean
rebar3 atomvm packbeam -p -e ./atomvmlib.avm
