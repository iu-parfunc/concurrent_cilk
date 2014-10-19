#!/bin/bash

# Git Depth, etc.


# General FusionTable Configuration
#SECRET=yI8GfZXsHPrW44udqklCHeDH
#ID=759282369766-ijonhc4662ot2qos4lgud0e0sltjshlj.apps.googleusercontent.com
SECRET=148aQ08EPpgkb0DiYVLoT9X2
ID=546809307027-8tm2lp5gtqg5o3pn3s016gd6467cf7j3.apps.googleusercontent.com
TABLE=ConcurrentCilk_Benchmarks2

Q="./HSBencher/hsbencher-tool/hsbencher do --secret=$SECRET --id=$ID --table=$TABLE --raw"

function fetchall
{
  echo Fetching all data.

  $Q --query="select *
                from FT
                where GIT_DEPTH=304
                  and VARIANT='trad_cilk'
                  and HOSTNAME='cutter'" > data/regresssions_plot_data.csv

# ARRG -- We cant filter out variants that contain 'sleep'. Just use :g/sleep/d in vim after you create the file
  $Q --query="select *
                from FT
                where GIT_DEPTH=304
                  and VARIANT='none'
                  and HOSTNAME='cutter'" >> data/regresssions_plot_data.csv
}

# Allow using this script in interactive mode.
if [ "$1" != "" ]; then
  $Q --query="$*"
else
  fetchall
fi
