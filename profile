#!/bin/sh

archlinux=true
media=true

slurm=false
computecanada=false
vector=false

# PACKAGES

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

# MEDIA

if [ $media = true ]; then
    export XDG_CONFIG_HOME=~/.config
    export XDG_MUSIC_DIR=~/Music
    export TERM=rxvt-256color

    alias ytmp3="yt-dlp -x --audio-format mp3"
    alias ytm4a="yt-dlp -x --audio-format m4a"

    [ ! -s ~/.mpd/mpd.pid ] && systemctl start --user mpd
fi

# SHELL & ENVIRONMENT

export LC_ALL=en_US.UTF-8

export HISTCONTROL=ignoreboth:erasedups
export VISUAL="vim"
if eval "command -v nvim &> /dev/null"; then
    alias vim='nvim'
    export VISUAL="nvim"
fi
export EDITOR="$VISUAL"
export SUDO_EDITOR="$VISUAL"
export GIT_EDITOR="$VISUAL"

append() {
    if [ -d "$1" ]; then
        export PATH=$PATH:$1
    fi
}
append "$HOME/.cargo/bin"
append "$HOME/.local/bin"
append "/usr/share/bin"

# SLURM/CLUSTER

if [ $vector = true ]; then
    export SCRATCH=/scratch/ssd004/scratch/rupertwu
fi

if [ $computecanada = true ]; then
    export SLURM_ACCOUNT=def-papyan
    export SBATCH_ACCOUNT=$SLURM_ACCOUNT
    export SALLOC_ACCOUNT=$SLURM_ACCOUNT
    export DATASET_DIR=/home/rupert/projects/$SLURM_ACCOUNT/rupert/data/

    alias srun="srun --account $SLURM_ACCOUNT"

    # Environment
    module load meta-farm
    module load scipy-stack
    module load python/3.10
    module load gcc/9.3.0 arrow/8
fi

if [ $slurm = true ]; then
    scu() {
        scancel -u $USER
    }
    squ() {
        squeue -u $USER
    }
    scpu() {
        srun --mem=$1 --pty bash
    }
    sgpu() {
        srun --mem=$1 --gres=gpu:1 --pty bash
    }
    nsmi() {
        srun -p $1 --gres=gpu:1 --pty nvidia-smi
    }
fi

# TOOLS

sshc() {
    code --remote ssh-remote+$1 $2
}
jup() {
    book_port=$1
    node_name=$2
    ssh vector -NL $node_name:$book_port:$node_name
    # ssh your_user_name@v.vectorinstitute.ai -NL 5924:<the node name>:5924
}

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
