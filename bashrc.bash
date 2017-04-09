# Log my commands for use later
export PROMPT_COMMAND='if [ "$(id -u)" -ne 0 ]; then echo "$(date "+%Y-%m-%d.%H:%M:%S") $(pwd) $(history 1)" >> ~/.logs/bash-history-$(date "+%Y-%m-%d").log; fi'

# pretty colors in bash
export PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[33;1m\]\W\[\033[m\]\$ "
export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad
alias ls='ls -GFh'

# Hidden artifactory credentials
if [ -f ~/.config/artifactory_envar ]; then . ~/.config/artifactory_envar; fi

# Check /usr/local first - useful for homebrew and spacemacs
export PATH="/usr/local/bin:/usr/local/sbin:$PATH"

# Go Language
export GOPATH=$HOME/Documents/Kairos/golang
export GOROOT=/usr/local/Cellar/go/1.7.1/libexec
export PATH=$PATH:$GOPATH/bin:$GOROOT/bin
export LG=$HOME/Documents/Kairos/golang/src/github.com/KairosAerospace/leakyg
alias govtest='go list ./... | grep -v vendor | xargs go test'

# GDAL
export PROJSO=/Library/Frameworks/PROJ.framework/PROJ
export GDAL_DATA=/Users/jpaddison/Documents/MrSID_DSDK-9.1.0.4045-darwin13.universal.clang51/Raster_DSDK/3rd-party/share/gdal

# GPSBabel
alias gpsbabel=/Applications/GPSBabelFE.app/Contents/MacOS/gpsbabel

# Javascript - needs work
export PATH="$HOME/.npm-packages/bin:/usr/local/Cellar/node/6.2.0/bin:$PATH"

# Latex
export PATH=$PATH:/Library/TeX/texbin

# CUDA GPU
export PATH=$PATH:/Developer/NVIDIA/CUDA-8.0.61/bin
export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:/Developer/NVIDIA/CUDA-8.0.61/lib
export LD_LIBRARY_PATH=$DYLD_LIBRARY_PATH
