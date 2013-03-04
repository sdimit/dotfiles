#!/usr/local/bin/bash

# CLI interface to simplify calls to 10to8 web api (useful for debugging)

# useful to create bash aliases for even quicker access, like so:
#      alias get='$ROOT/native/src/core/api.sh'
#      alias post='$ROOT/native/src/core/api.sh POST'
#      alias put='$ROOT/native/src/core/api.sh PUT'
#      alias delete='$ROOT/native/src/core/api.sh DELETE'


# requires [httpie](https://github.com/jkbr/httpie), which in turns works on requests.py
#
# author: @istib, 2012

API_ROOT="http://localhost:8000/api/web/v1"

# USAGE 

if [ "$#" == "0" ]; then
  echo ""
  echo "CLI interface to simplify calls to 10to8 web api (useful for debugging)"
  echo "======================================================================="
  echo ""
  echo "Usage:"
  echo "------"
  echo "         ${0##*/} [method] args... [options]"
  echo ""
  echo "Examples:"
  echo "---------"
  echo "         ${0##*/} staff/1/numbers"
  echo "         ${0##*/} POST locations/2/availabilities < avails.json"
  echo ""
  echo "the api root ($API_ROOT) is implied"
  echo ""
  echo "Options:"
  echo "--------"
  echo "         [method]   : http method. default is GET"
  echo ""
  echo "         -o [org]           tento8 organisation (default: 1)"
  # TODO implement -o 
  # TODO show list of endpoints
 exit 1
fi

# DEFAULT ORGANISATION

T8_ORGANISATION=1

while getopts ":o" opt; do
  case $opt in
    o)
      T8_ORGANISATION=$OPTARG
      ;;
    :)
      echo "Option -$OPTARG requires an argument." >&2
      exit 1
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit
      ;;
  esac
done

# HTTP Method

declare -A http_methods
for m in GET POST PUT DELETE
do
  http_methods[$m]=1
done

if [[ ${http_methods[$1]} ]]; then
  METHOD=$1
  shift
else
  METHOD='GET'
fi 

# SESSION COOKIE

# fetch latest from DB
T8_SESSION=`sqlite3 -noheader dbs/native_dev.db 'SELECT session_key FROM django_session ORDER BY expire_date DESC LIMIT 1;' | sed '/^$/d'`

# TODO error msg and exit of T8_SESSION doesn't exist

# API ENDPOINT 

ENDPOINT=$1
shift

# CALL HTTPIE

URI=$API_ROOT/$ENDPOINT/$@

echo "$METHOD call to $URI at organisation $T8_ORGANISATION (session $T8_SESSION)"

http $METHOD $URI Cookie:sessionid=$T8_SESSION 10to8-organisation:$T8_ORGANISATION | less -RFX
