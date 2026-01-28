#!/bin/bash

rebar3 as demo clean
rebar3 as demo atomvm packbeam -p -e ./atomvmlib.avm
