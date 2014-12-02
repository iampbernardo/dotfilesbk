# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="clean"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment following line if you want to  shown in the command execution time stamp 
# in the history command output. The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|
# yyyy-mm-dd
# HIST_STAMPS="mm/dd/yyyy"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git mercurial svn autopep8 bower debian encode64 git-extras git-promopt pip web-search ssh-agent)

source $ZSH/oh-my-zsh.sh

# User configuration

export PATH=$HOME/bin:/usr/local/bin:$PATH
# export MANPATH="/usr/local/man:$MANPATH"

# # Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
# else
  # export EDITOR='gvim'
fi

export EDITOR=/usr/bin/vim


if [[ -f /usr/local/bin/virtualenvwrapper.sh ]]; then
# Debian based distributions
   source /usr/local/bin/virtualenvwrapper.sh
else
# Archlinux
   source /usr/bin/virtualenvwrapper.sh
fi


[ -z "$TMUX" ] && export TERM=xterm-256color

# No bell, it's a hell
setterm -blength 0
set bell-style none

PATH="${PATH}:/opt/android-sdk/tools/:/opt/android-sdk/platform-tools/"
PATH="${PATH}:/home/pablo/Descargas/android-sdk-linux/tools/:/home/pablo/Descargas/android-sdk-linux/platforms/"


# if [ -n "$DISPLAY" ]; then
#     xset b off
# fi


#PATHS 

export WORKON_HOME=~/virtualenvs
source /usr/local/bin/virtualenvwrapper.sh
export PIP_VIRTUALENV_BASE=~/.virtualenvs
export LUA_PPREFIX=/usr/bin/lua

alias tmux='tmux -2'
