This is the stub README.txt for the "mnas-git" project.


Set UTF-8 display for Git GUI differences window
================================================

# Global setting for all you repositories
> git config --global gui.encoding utf-8

# For one repository only
> git config gui.encoding utf-8


How to colorize output of git
=============================

> git config --global color.ui auto
> git config --global color.branch auto
> git config --global color.status auto


Install Bash git completion
===========================

pacman -S git bash-completion


Typcal .gitignore file
======================
touch .gitignore 
echo '.directory' >>.gitignore
echo '*~' >>.gitignore
echo '*.FASL' >>.gitignore
echo '*.fasl' >>.gitignore
echo '*.lisp-temp' >>.gitignore


Typcal .gitattributes file
==========================
touch .gitattributes 
echo '# Set the default behavior, in case people don't have core.autocrlf set' >>.gitignore
echo '* text=auto' >>.gitignore
echo '' >>.gitignore
echo '# Denote all files that are truly binary and should not be modified' >>.gitignore
echo '*.png binary' >>.gitignore
echo '*.jpg binary' >>.gitignore
echo '*.bmp binary' >>.gitignore
echo '' >>.gitignore
echo '# Explicitly declare text files you want to always be normalized and converted to native line endings on checkout' >>.gitignore
echo '*.c* text' >>.gitignore
echo '*.h* text' >>.gitignore
echo '' >>.gitignore
echo '# Declare files that will always have CRLF line endings on checkout' >>.gitignore
echo '*.sln text eol=crlf' >>.gitignore
echo '*.txt text eol=crlf' >>.gitignore
echo '*.md  text eol=crlf' >>.gitignore
echo '' >>.gitignore
echo '# Declare files that will always have LF line endings on checkout' >>.gitignore
echo '# Common Lisp.' >>.gitignore
echo '*.lisp text eol=lf' >>.gitignore
echo '*.asd text eol=lf' >>.gitignore
echo '' >>.gitignore
echo '# HTML' >>.gitignore
echo '*.html text eol=lf' >>.gitignore
echo '' >>.gitignore
echo '# AutoCad files' >>.gitignore
echo '# Binary' >>.gitignore
echo '*.cuix  binary' >>.gitignore
echo '*.mnr  binary' >>.gitignore
echo '# CRLF' >>.gitignore
echo '*.lsp text eol=crlf' >>.gitignore
echo '*.mnl  text eol=crlf' >>.gitignore
