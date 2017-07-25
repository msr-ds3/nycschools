#!/bin/bash
#
# file: make_readme.sh
#
# description: creates markdown to link to preview of notebook files
#
# usage:
#   to get output to standard out:
#     bash make_readme.sh
#   or redirect to (and overwrite!) the README:
#     bash make_readme.sh > README.md
#
# requirements:
#

base_url="http://htmlpreview.github.io/?https://github.com/msr-ds3/nycschools/blob/master/notes/"

for date in `ls *.html | cut -d_ -f1 | sort | uniq`
do

    date_str=`date -d$date +"%Y-%m-%d"`
    echo "## $date_str"

    for html in $date*.html
    do
	title=${html//$date/}
	title=${title//_/ }
	title=${title/.html/}
	echo '  * ['$title']('$base_url$html')'
    done

    echo

done
