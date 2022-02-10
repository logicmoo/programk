
FILE=$1

rm /tmp/$FILE.tmp -f
cp $FILE /tmp/$FILE.tmp
function SED {
   sed -e "${1}" -i /tmp/$FILE.tmp
}


SED ':a;N;$!ba;s/\n/ /g'
SED 's|<aiml|\n<aiml|g'
SED 's|aiml>|aiml>\n|g'
SED 's|</aiml|\n</aiml|g'

SED 's|<topic|\n<topic|g'
SED 's|topic>|topic>\n|g'
SED 's|</topic|\n</topic|g'
SED 's| version="1.0"||g'



SED 's|<category/>|<category/>\n|g'
SED 's/<category>/\n<category>/g'
SED 's/<?xml version/\n<?xml version/g'
SED 's|<!-- Annotated|\n<!-- Annotated|g'
SED 's|<!-- This|\n<!-- Annotated|g'
SED 's|<!-- |\n<!-- |g'
SED 's|-->|-->\n|g'
SED 's|\t| |g'
SED 's|[*]<| *<|g'
SED 's|>[*]|>* |g'
SED 's|[_]ASTRONOMY|_ ASTRONOMY|g'

SED 's|  | |g'
SED 's|  | |g'
SED 's| <|<|g'
SED 's|> |>|g'
SED 's| \n|\n|g'
SED 's|\n |\n|g'
SED 's| \n|\n|g'
SED 's|\n |\n|g'
SED '/^<?xml /d'
SED '/^^[[:space:]]<?xml /d'
SED '/^<[/]aiml/d'
SED '/^<aiml/d'
SED 's| \n|\n|g'
SED 's|\n |\n|g'
SED '/^[[:space:]]*$/d'

echo "<aiml>" > $FILE
cat /tmp/$FILE.tmp >> $FILE
echo "</aiml>" >> $FILE

