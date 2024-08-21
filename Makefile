CFG_DIR := $(HOME)/.config
MAKEFLAGS += --no-print-directory

do:
	@echo "Usage:"
	@echo "  make do                            - Display this help message"
	@echo "  make aur pkgr=<package>            - Install AUR package"
	@echo "  make base                          - Install base development packages"
	@echo "  make blue                          - Install Bluetooth packages"
	@echo "  make brave                         - Install Brave browser"
	@echo "  make code                          - Install Visual Studio Code"
	@echo "  make cups                          - Install CUPS packages"
	@echo "  make discord                       - Install Discord"
	@echo "  make firmware                      - Install Firmware packages"
	@echo "  make fprint                        - Install Fprint packages"
	@echo "  make git                           - Install Git"
	@echo "  make inst pkgs=\"pkg1 pkg2...\"    - Install packages with AUR helper or pacman"
	@echo "  make line FILE=<file> FLAG=<flag>  - Append a line to a file if not present"
	@echo "  make mobile                        - Install mobile packages"
	@echo "  make music                         - Install media packages"
	@echo "  make network                       - Install Network Manager packages"
	@echo "  make nvim                          - Install Neovim"
	@echo "  make ozone app=<app>               - Add Wayland flag to app config"
	@echo "  make pacman                        - Install Pacman"
	@echo "  make paru                          - Install paru AUR helper"
	@echo "  make pipe                          - Install Pipewire packages"
	@echo "  make profile                       - Install profile"
	@echo "  make python                        - Install Python"
	@echo "  make rust                          - Install Rust"
	@echo "  make sysd srv=<service>            - Enable and start system service"
	@echo "  make top                           - Install terminal packages"
	@echo "  make usrd srv=<service>            - Enable and start user service"
	@echo "  make yay                           - Install yay AUR helper"


# File Manipulation
line:
	@FILE="$(FILE)" FLAG="$(FLAG)"; \
	if [ -f "$$FILE" ] && grep -q -- "$$FLAG" "$$FILE"; then \
		echo "$$FILE already contains $$FLAG"; \
	else \
		echo "$$FLAG" >> "$$FILE"; \
	fi

ozone:
	$(MAKE) line FLAG="--ozone-platform=wayland" FILE="$(CFG_DIR)/$(app)-flags.conf"

# Service Management
sysd:
	sudo systemctl enable $(srv)
	sudo systemctl start $(srv)

usrd:
	systemctl --user enable $(srv)
	systemctl --user start $(srv)

# Package Management
aur:
	git clone --depth 1 https://aur.archlinux.org/$(pkgr)-git &
	cd $(pkgr)-git && makepkg -si
	rm -rf $(pkgr)-git

paru:
	$(MAKE) aur pkgr="paru"

yay:
	$(MAKE) aur pkgr="yay"

inst:
	@if command -v paru &> /dev/null; then \
		paru -S --needed $(pkgs); \
	elif command -v yay &> /dev/null; then \
		yay -S --needed $(pkgs); \
	else \
		echo "Neither paru nor yay is installed. Falling back to pacman..."; \
		sudo pacman -S --needed $(pkgs); \
	fi


# System Components
base:
	$(MAKE) inst pkgs="base-devel bash less unzip wget zip"

blue:
	$(MAKE) inst pkgs="bluez bluez-libs bluez-utils"
	$(MAKE) sysd srv="bluetooth"

cups:
	$(MAKE) inst pkgs="cups cups-filters gutenprint ghostscript poppler"
	$(MAKE) sysd pkgs="cups"

firmware:
	$(MAKE) inst pkgs="fwupd linux-firmware"
	fwupdmgr get-devices
	fwupdmgr refresh --force
	fwupdmgr get-updates
	fwupdmgr update

fprint:
	$(MAKE) inst pkgs="libfprint-tod-git python-validity-git"
	$(MAKE) sysd srv="python3-validity"

mobile:
	$(MAKE) inst pkgs="acpi brightnessctl tlp tlpui threshy threshy-gui"

pipe:
	$(MAKE) inst pkgs="pipewire pipewire-audio pipewire-jack pipewire-session-manager wireplumber pavucontrol"
	$(MAKE) usrd srv="pipewire pipewire-pulse"
	sudo $(MAKE) line FLAG="load-module module-switch-on-connect" FILE="/etc/pulse/default.pa"

network:
	$(MAKE) inst pkgs="networkmanager network-manager-applet"
	$(MAKE) sysd srv="NetworkManager"

# Applications
brave:
	$(MAKE) inst pkgs="brave-bin"

code:
	$(MAKE) inst pkgs="visual-studio-code-bin ttf-mononoki-nerd"

disc:
	DISC_CFG=path/to/config.json; \
	$(MAKE) inst pkgs="discord jq"; \
	@if [ ! -f "$(DISC_CFG)" ]; then echo '{}' > "$(DISC_CFG)"; fi; \
	@if ! jq -e '.SKIP_HOST_UPDATE == true' "$(DISC_CFG)" >/dev/null; then \
		jq '. + {"SKIP_HOST_UPDATE": true}' "$(DISC_CFG)" > tmp.json && mv tmp.json "$(DISC_CFG)"; fi

electron:
	$(MAKE) ozone app="electron"
	$(MAKE) ozone app="brave"
	$(MAKE) ozone app="code"
	$(MAKE) ozone app="discord"
	$(MAKE) ozone app="slack"

music:
	$(MAKE) inst pkgs="mpd ncmpcpp cli-visualizer yt-dlp"

nvim:
	$(MAKE) inst pkgs="neovim"
	sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
	mkdir -p $(CFG_DIR)/nvim
	cat cfg/nvim.vim > $(CFG_DIR)/nvim/init.vim

top:
	$(MAKE) inst pkgs="htop btop pfetch-rs-bin cpufetch"
	mkdir -p $(CFG_DIR)/htop
	cat cfg/htoprc > $(CFG_DIR)/htop/htoprc

# Development Tools
docs:
	$(MAKE) inst pkgs="pandoc-bin"

git:
	$(MAKE) inst pkgs="git"
	cat cfg/gitconfig >> ~/.gitconfig

python:
	$(MAKE) inst pkgs="python uv"

rust:
	$(MAKE) inst pkgs="rustup"
	rustup update stable


# System Configuration
pacman:
	$(MAKE) inst pkgs="rankmirrors reflector"
	sudo -v
	sudo sed -i '/^#VerbosePkgLists/s/^#//' /etc/pacman.conf
	sudo sed -i '/^\[core\]/,+1 s/^#//' /etc/pacman.conf
	sudo sed -i '/^\[extra\]/,+1 s/^#//' /etc/pacman.conf
	sudo sed -i '/^\[community\]/,+1 s/^#//' /etc/pacman.conf
	sudo sed -i '/^\[multilib\]/,+1 s/^#//' /etc/pacman.conf

profile: cfg/profile
	cp cfg/profile $(HOME)/.profile
	$(MAKE) line FLAG="source ~/.profile" FILE="$(HOME)/.bashrc"
