#!/bin/sh

# VARIABLES

archlinux=false
media=false

computecanada=false
slurm=false

# TERMINAL/SHELL

export HISTCONTROL=ignoreboth:erasedups

# EDITOR & DOCS

export VISUAL="vim"
if eval "command -v nvim &> /dev/null"; then
    alias vim='nvim'
    export VISUAL="nvim"
fi
export EDITOR="$VISUAL"
export SUDO_EDITOR="$VISUAL"
export GIT_EDITOR="$VISUAL"

mktex() {
    latexindent.pl *.tex
    case $2 in
    lua*) lualatex $1.tex ;;
    xe*) xelatex $1.tex ;;
    *) pdflatex $1.tex ;;
    esac
    if [ $? -eq 0 ]; then
        rm -f $1.aux $1.fdb_latexmk $1.fls $1.log
    fi
}

# PATH

append() {
    if [ -d "$1" ]; then
        export PATH=$PATH:$1
    fi
}
append "$HOME/.cargo/bin"
append "$HOME/.local/bin"
append "/usr/share/bin"

# MEDIA

if [ $media = true ]; then
    export XDG_CONFIG_HOME=~/.config
    export XDG_MUSIC_DIR=~/Music
    export TERM=rxvt-256color

    alias ytmp3="yt-dlp -x --audio-format mp3"
    alias ytm4a="yt-dlp -x --audio-format m4a"

    [ ! -s ~/.mpd/mpd.pid ] && systemctl start --user mpd
fi

# ARCH LINUX

if [ $archlinux = true ]; then

    alias clean="sudo pacman -Rns \$(pacman -Qdtq)"
    alias prank-manjaro="sudo pacman-mirrors --fasttrack 12 && sudo pacman -Syyu"

    prank() {
        FILE=$XDG_CONFIG_HOME/mirrorlist.bak
        if [ ! -e $FILE ]; then
            wget -O $FILE "https://archlinux.org/mirrorlist/all/"
            sed -i 's/#Server/Server/' $FILE
        fi
        rankmirrors /etc/pacman.d/mirrorlist.bak | sudo tee /etc/pacman.d/mirrorlist
        sudo pacman -Syyu
    }

fi

# COMPUTE CANADA

if [ $computecanada = true ]; then
    export SLURM_ACCOUNT=def-papyan
    export SBATCH_ACCOUNT=$SLURM_ACCOUNT
    export SALLOC_ACCOUNT=$SLURM_ACCOUNT
    export DATASET_DIR=/home/rupert/projects/$SLURM_ACCOUNT/rupert/data/

    # Environment
    module load meta-farm
    module load scipy-stack
    module load python/3.10
fi

# SLURM

if [ $slurm = true ]; then
    alias scu="scancel -u $USER"
    alias squ="squeue -u $USER"
    sbash() {
        srun --account=$SLURM_ACCOUNT --mem=$1 --gres=gpu:1 --pty bash
    }
    smi() {
        srun --account=$SLURM_ACCOUNT --mem=256M --gres=gpu:1 --pty nvidia-smi
    }
fi

# TOOLS

jup() {
    # ssh your_user_name@v.vectorinstitute.ai -NL 5924:<the node name>:5924
    ssh vremote -NL 5924:$1:5924
}
