#!/bin/bash
#### Script to set up symlinks from ~/ to ~/homedir,
#### elegantly dealing with ones that may already be there, and backing up the existing.
#### credit: https://github.com/gorillamania/homedir/blob/master/setup.sh

# Abort on error
set -e

# Fix osx history across sessions http://superuser.com/questions/950403/bash-history-not-preserved-between-terminal-sessions-on-mac
touch ~/.bash_sessions_disable

cd ~
for file in .gitconfig .gitignore .zshrc .zshenv .doom.d .config; do
  if [ -h $file ] ; then
    # File is already a symbolic link
    echo "Symlink for $file is already there"
    continue
  fi
  if [ -d $file -o -f $file ] ; then
    echo "Backing up existing $file to $file.b4homedir"
    rm -rf $file.b4homedir
    mv $file $file.b4homedir
  fi
  echo "Creating symlink for $file"
  ln -s homedir/$file
done

set +e

echo Done
