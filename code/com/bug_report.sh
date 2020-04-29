#!/bin/csh
echo `date` >>  /tmp/bug_form
echo 'The file you just edited is going to be E-mailed to the author'
echo 
set author=mad@cbs.univ-montp1.fr
echo -n 'Ok to send file (y/n) : '
set ans=$<
if ($ans == 'y') then
  echo "The file is being sent to $author"
  mail $author < /tmp/bug_form
endif
echo
echo -n 'Do you want to keep the file (y/n) : '
set ans=$<
if ($ans == 'y') then
  echo -n 'Enter file name : '
  set name=$<
  mv /tmp/bug_form $name
else
  rm /tmp/bug_form
endif
