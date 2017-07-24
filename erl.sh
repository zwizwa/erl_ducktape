#!/bin/bash
cd $(dirname $0)
exec erl -pa _build/default/lib/erl_ducktape/ebin/ _build/default/lib/erl_tools/ebin/ ./_build/default/lib/jiffy/ebin/



