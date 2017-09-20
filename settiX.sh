#!/usr/bin/env bash
#
# -------------------------------------------------------------------------------------------------
# SCRIPT TO INSTALLA AND SET SOME USEFUL PROGRAMS AND LIBRARIES ON LINUX-DEBIAN
# -------------------------------------------------------------------------------------------------
#
# settix [install|purge]
#        [--all|--exclude| [--p] ]
#        [[--source] name_program [--d_path installation_path] [--opt installation_options]]
#
# -------------------------------------------------------------------------------------------------

# Colors
RESET="\033[0m"
BLACK="\033[30m"
RED="\033[31m"
GREEN="\033[32m"
YELLOW="\033[33m"
BLUE="\033[34m"
MAGENTA="\033[35m"
CYAN="\033[36m"
WHITE="\033[37m"
BOLDBLACK="\033[1m\033[30m"
BOLDRED="\033[1m\033[31m"
BOLDGREEN="\033[1m\033[32m"
BOLDYELLOW="\033[1m\033[33m"
BOLDBLUE="\033[1m\033[34m"
BOLDMAGENTA="\033[1m\033[35m"
BOLDCYAN="\033[1m\033[36m"
BOLDWHITE="\033[1m\033[37m"

#-------------------
# GLOBAL VARIABLES
#-------------------
ACTION="install"
ALL=false
EXCLUDE=false

DEBUGGING=true
LOG_FILE="$HOME/.settiX"

PATH_TAG="--d_path"
SOURCE_TAG="--source"
OPT_TAG="--opt"
LIST_PROGRAMS=()
LIST_EXCLUDE=()
# LIST_APT=

LIST_ALL_PROGRAMS=("git" "qt" "vscode" "screen" "tree" "cgdb" "ddd" "tesseract" "androidstudio" "wxWidgets")
LIST_ALL_PROGRAMS+=("gnome" "cmake" "automake" "tex-all" "tess-two" "emacs" "vim" "nasm" "ggtags" "codeblocks" "codelite")
LIST_ALL_PROGRAMS+=("ssh" "clang" )

BUILD_FROM_SOURCE=false
# PATH_DOWNLOAD=$HOME"/Download/"
PATH_DEFAULT_DOWNLOAD=$HOME"/Documents/Repos/SettiX"
PATH_GLOBAL=
CLEAN_BUILD=true

readonly SYSTEM="$(uname)"
readonly VERSION='0.0.1'

# -----------------------------------------------------------------------------------------------#
#                                   GLOBAL FUNCTIONS                                             #
# -----------------------------------------------------------------------------------------------#

debug(){
    if [ "$DEBUGGING" == true ]; then
        echo -e "${RED}@DEBUGGING:"
        for arg in "$@"
        do
            echo -e "${GREEN}    --->${YELLOW} $arg${RESET}"
        done
    fi
}

use_apt() {

    for prg in "$@"
    do
        sudo apt-get "$ACTION" "$prg"
    done
}

printf_action() {

    echo "    ************************"
    echo "    *  $1 "
    echo "    ************************"
}

save_log_file() {

    echo "*) $(date)" >> $LOG_FILE
    for arg in "$@"
    do
        echo "$arg" >> $LOG_FILE
    done

}

check_which_program() {

    local __resultVar=$1
    for cmd in "${@:1}"
    do
        local path=$(which "$cmd")
        if [[ ! -z "$path" ]]; then
            save_log_file "'$cmd' found at: $path"
            eval $__resultVar=true
            echo -e "${GREEN}$1 is installed: command $cmd found${RESET}"
            return
        fi
    done

    save_log_file "commands '$@' not found in the PATH"
    eval $__resultVar=false
}

use_check() {
    debug "1st Method: "
    check_which_program program "_make" "_qmake" "qmake"
     if [[ $program ]]; then
       echo "program already installed"
    else
        echo "Program not found"
    fi

    debug "2nd Method: "
    local installed=$(check_which_program "qt" "_make" "_qmake" "qmake")
    if [[ ! -z $installed ]]; then
        echo -e "$installed"
        echo "The program seems to be already installed"
    else
        echo "Program not found"
    fi

    debug "3rd Method: "
    if [[ ! -z $(check_which_program "qt" "_make" "_qmake" "qmake") ]]; then
        echo "The program seems to be already installed on this Computer"
    else
        echo "Program not found."
    fi

    debug "4th Method: "
    local insatlled=$(which "qt" "qmake" "make")
    if [[ ! -z $insatlled ]]; then
        echo -e "${YELLOW}$insatlled${RESET}"
        echo "The program seems to be already installed on this Computer"
    else
        echo "Program not found"
    fi

}

# it is enough call which "list of program to test"
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# -----------------------------------------------------------------------------------------------#
#                                   Vim installation                                              #
# -----------------------------------------------------------------------------------------------#
URL_GGTAGS="http://tamacom.com/global/global-6.4.tar.gz"

action_ggtags(){

    if [[ "$ACTION" == "install" ]]; then
        install_ggtags "$@"
    else
        unistall_ggatsg "$@"
    fi

}

install_ggtags_dependencies(){
    echo "install package for GNU global..."
    sudo apt-get update
    sudo apt-get -y install curl
    sudo apt-get -y install wget
    sudo apt-get -y install ncurses-dev
    sudo apt-get -y install exuberant-ctags

}

install_ggtags() {

    printf_action "INSTALLING GNU GLOBALS"

    local source="$1"
    local name="$2"
    local path="$3"
    local opt="$4"

    debug "source:$source" "name: $name" "path: $path" "options: $opt"

    local installed=$(check_which_program "ggtags")
    if [[ ! -z $installed ]]; then
        echo -e "$installed"
        echo "The program seems to be already installed"
        echo -e "Do you want to install it anyway (yes/no) "
        read input

        if [[ "$input" == "n"* ]]; then
            save_log_file "Aborting installation $name"
            return
        fi
    fi

    install_ggtags_dependencies

    echo "install GNU global..."
	mkdir -p $path 2>>$LOG_FILE
    cd $path 2>>$LOG_FILE
    wget $URL_GGTAGS
    tar zxvf global-6.4.tar.gz
    cd global-6.4
    ./configure
    make
    sudo make install

}

unistall_ggtags(){

    printf_action "UNISTALLING VIM"

}

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# -----------------------------------------------------------------------------------------------#
#                                   Vim installation                                              #
# -----------------------------------------------------------------------------------------------#
VIM_ULTIMATE_GIT="https://github.com/amix/vimrc.git"

action_vim(){

    if [[ "$ACTION" == "install" ]]; then
        install_vim "$@"
    else
        unistall_vim "$@"
    fi

}


install_vim() {

    local source="$1"
    local name="$2"
    local path="$3"
    local opt="$4"

    debug "source:$source" "name: $name" "path: $path" "options: $opt"

    local installed=$(check_which_program "vim")
    if [[ ! -z $installed ]]; then
        echo -e "$installed"
        echo "The program seems to be already installed"
        echo -e "Do you want to install it anyway (yes/no) "
        read input

        if [[ "$input" == "n"* ]]; then
            save_log_file "Aborting installation $name"
            return
        fi
    fi

    use_apt "vim"
    use_apt "vim-gnome"
    sudo apt-get install -y python-software-properties software-properties-common
    # add this repo so that vim has python 4? compiled in to support plugins like gundo
    add-apt-repository -y ppa:pi-rho/dev
    apt-get update
    apt-get install -y vim-gtk

    if [[ $opt != "no-ultimate" ]]; then # from source

	    git clone $VIM_ULTIMATE_GIT ~/.vim_runtime && \
	    # plus - the colors are strage
	    # sh ~/.vim_runtime/install_awesome_vimrc.sh
        # basic version
        sh ~/.vim_runtime/install_basic_vimrc.sh
        echo " " >> ~/.vimrc
        echo "set nu" >> ~/.vimrc
    fi

}

unistall_vim(){

    printf_action "UNISTALLING VIM"

}

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# -----------------------------------------------------------------------------------------------#
#                                   QT installation                                              #
# -----------------------------------------------------------------------------------------------#

QT_LUNCHER="qt-opensource-linux-x64-5.7.0.run"
QT_URL="http://download.qt.io/official_releases/qt/5.7/5.7.0/"$QT_LUNCHER
QT_UNISTALL_TOOL="MaintenanceTool"

action_qt() {

    if [[ "$ACTION" == "install" ]]; then
        install_qt "$@"
    else
        unistall_qt "$@"
    fi

}

install_dependencies_qt() {

    if [[ "$source" == false  ]]; then
	    # install the full runtime files for the generic font configuration library:
        use_apt "libglu1-mesa-dev -y" "mesa-common-dev" "qt4-qmake"  # to use qmake outside Qt creator
        save_log_file "apt-get $ACTION" "libglu1-mesa-dev -y" "mesa-common-dev" "qt4-qmake"
    else
        echo -e "${RED}Build from source not aoutomatize${RESET}"
    fi
}

# "$source" "$name_program" "$path_download" "$opt_install"
install_qt() {

    printf_action "INSTALLING QT-5"

    local source="$1"
    local name="$2"
    local path="$3"
    local opt="$4"

    debug "source:$source" "name: $name" "path: $path" "options: $opt"

    local installed=$(check_which_program "qmake")
    if [[ ! -z $installed ]]; then
        echo -e "$installed"
        echo "The program seems to be already installed"
        echo -e "Do you want to install it anyway the (yes/no) "
        read input

        if [[ "$input" == "n"* ]]; then
            save_log_file "Aborting installation $name"
            return
        fi
    fi

    install_dependencies_qt $source

    if [[ $source == true ]]; then # from source
        save_log_file "Installing $qt from source"
        echo -e "${GREEN}The building from the source is not yet automatize.${RESET}"
        echo -e "Do you want to install it with the installer (yes/no) "
        read input

        if [[ "$input" == "n"* ]]; then
            save_log_file "Aborting installation $name"
            return
        fi
    fi

    save_log_file "Installing $qt via installer"
    # from installer
	mkdir -p $path 2>>$LOG_FILE
    cd $path 2>>$LOG_FILE
    if [ ! -d $QT_URL ]; then
        save_log_file "downloading installer from $QT_URL" "into $path/$QT_LUNCHER"
	    wget $QT_URL
    fi
	chmod +x $QT_LUNCHER 2>>$LOG_FILE
	./$QT_LUNCHER &
}

unistall_qt() {

    // TODO: read log file and gather the informations
    printf_action "UNISTALLING QT"

    local source="$1"
    local name="$2"
    local path="$3"
    local opt="$4"

    debug "source:$source" "name: $name" "path: $path" "options: $opt"

    local installed=$(check_which_program "qmake")
    if [[ ! -z $installed ]]; then
        echo -e "$installed"
        echo "The program seems not to be installed"
        echo -e "Do you want to proceed with the unistallation it anyway (yes/no) "
        read input

        if [[ "$input" == "y"* ]]; then
            echo -e "input is $input"
            return
        fi
    fi

    if [[ $source ]]; then # from source
        echo -e "${GREEN}The building from the source is not yet automatize.${RESET}"
        echo -e "Do you want to uninstall it with the installer (yes/no) "
        read input

        if [[ "$input" == "n"* ]]; then
            save_log_file "Aborting installation $name"
            return
        fi
    fi

    # from installer
    if [ -d $path ]; then
	    cd $path && \
        ./$QT_UNISTALL_TOOL
    fi
}

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#---------------------------------------------------------------------------------------------------#
#                                           VS CODE                                                 #
#---------------------------------------------------------------------------------------------------#

VS_GIT="https://github.com/microsoft/vscode"
VS_INSTALLER="scripts/npm.sh"

action_vscode() {
    if [[ "$ACTION" == "install" ]]; then
        install_vscode "$@"
    else
        unistall_vscode "$@"
   fi
}

install_dependencies_vscode() {

    if [[ "$1" == true ]]; then
        echo "TODO:"
    else
        sudo add-apt-repository ppa:ubuntu-desktop/ubuntu-make
        use_apt "update" "ubuntu-make"
    fi

}

install_vscode_exention() {

    printf_action "INSTALLING THE EXTENTIONS"
   # install the extensions
    code --install-extension ms-vscode.cpptools
    code --install-extension vscodevim.vim

}

install_vscode() {

    local source="$1"
    local name="$2"
    local path="$3"
    local opt="$4"

    debug "source:$source" "name: $name" "path: $path" "options: $opt"

    local installed=$(check_which_program "vscode")
    if [[ ! -z $installed ]]; then
        echo -e "$installed"
        echo "The program seems to be already installed"
        echo -e "Do you want to install it anyway (yes/no) "
        read input

        if [[ "$input" == "n"* ]]; then
            save_log_file "Aborting installation $name"
            return
        fi
    fi

    install_dependencies_vscode $source

    if [[ $source == true ]]; then # from source
        mkdir -p $path && \
        cd $path && \
        git clone $VS_GIT && \
        cd vscode
        if [[ -z "$opt" ]]; then
            opt="--arch=64"
        fi
         ./$VS_INSTALLER $opt # $ACTION $opt
    else
        # from installer
	    umake ide visual-studio-code
    fi

    install_vscode_exention

}

unistall_vscode() {
    printf_action "UNISTALL VSCODE"
}

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#---------------------------------------------------------------------------------------------------#
#                                       Git from Source                                             #
#---------------------------------------------------------------------------------------------------#

GIT_CLONE=git://git.kernel.org/pub/scm/git/git.git

install_git_source() {

    printf_action "INSTALLING git from source"

    local source="$1"
    local name="$2"
    local path="$3"
    local opt="$4"

    mkdir -p $path && \
    cd "$path"

    local installed=$(check_which_program "git")
    # if git is installed, I use git to download the source.
    if [[ ! -z $installed ]]; then
        git clone $GIT_CLONE
    else
        wget "..." -o git
    fi

    cd git && \
    ./configure && \
    make all $opt  && \
    sudo make install

    if [[ "$CLEAN_BUILD" == true ]]; then
        make clean
    fi

}

unistall_git_source() {
    # TODO:
    printf_action "UNISTALLING Git from Source"
}

action_git_source() {

    if [[ "$ACTION" == "install" ]]; then
        install_git_source "$@"
    else
        unistall_git_source "$@"
    fi


}

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#---------------------------------------------------------------------------------------------------#
#                                       cgdb - color gdb                                            #
#---------------------------------------------------------------------------------------------------#


install_dependencies_cgdb() {

    if [[ -z "$1" ]]; then
        echo "TODO:"
    else
        use_apt "libncurses5-dev" "flex" "help2man" "libreadline6" "libreadline6-dev" "automake" "texinfo"
    fi

}

install_cgdb() {

    printf_action "INSTALLING cgdb from source"

    local source="$1"
    local name="$2"
    local path="$3"
    local opt="$4"

    local installed=$(check_which_program "cgdb")
    if [[ ! -z $installed ]]; then
        echo -e "$installed"
        echo "The program seems to be already installed"
        echo -e "Do you want to install it anyway (yes/no) "
        read input

        if [[ "$input" == "n"* ]]; then
            save_log_file "Aborting installation $name"
            return
        fi
    fi

    mkdir -p $path
    cd "$path"

    install_dependencies_cgdb "$source"

    if [[ -z "$opt" ]]; then
        opt="--prefix=/usr/local"
    elif [[ "$opt" != *"--prefix"* ]]; then
        opt=$opt" --prefix=/usr/local"
    fi
    if [[ "$name_program" == "ccdgb" ]]; then
        if [[ ! -d "ccgdb" ]]; then
            git clone https://github.com/dcohenp/cgdb.git "$name_program"  #old version (.c - but C-T for input windows worḱs on Ubuntu)
        fi
        cd "$name_program"
        # use the vertical patch
        git checkout 0391e9c5df3cdc5b6ffde73457723bc0e643d29c

    else
        if [[ ! -d "cgdb" ]]; then
            git clone git://github.com/cgdb/cgdb.git "$name_program"
        fi

        cd "$name_program"
    fi
    # common

    ./autogen.sh && \
    opt_tmp=($opt)
    ./configure $opt_tmp  && \
    make && \
    sudo make install

    if [[ "$CLEAN_BUILD" == true ]]; then
        make clean
    fi

}

unistall_cgdb() {

    printf_action "UNISTALLING cgdb"
    # TODO:

}

action_cgdb() {

    if [[ "$ACTION" == "install" ]]; then
        install_cgdb "$@"
    else
        unistall_cgdb "$@"
   fi

}

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# -----------------------------------------------------------------------------------------------#
#                                         EMACS                                                  #
# -----------------------------------------------------------------------------------------------#

GIT_EMACS="https://github.com/emacs-mirror/emacs.git"
TRALBAR_EMACS="https://ftp.gnu.org/gnu/emacs/emacs-25.1.tar.gz"

install_dependencies_emacs() {

    if [[ "$1" == true ]]; then
        echo "TODO:"
    else
	# this may fail 'build-dep' if in 'Software and Update' is not checked Source Code.
    use_apt "texinfo" "build-essential"
	sudo apt build-dep emacs24
    use_apt "libwebkit-dev" "libx11-dev" "libxpm-dev" "libjpeg-dev" "libpng-dev" "libgif-dev" "libwebkitgtk-3.0-dev"
    use_apt "libtiff-dev" "libgtk-3-dev" "libncurses-dev" "libxpm-dev" "automake" "autoconf"
    fi

}

install_emacs() {

    printf_action "INSTALLING emacs 25.1 from source"

    local source="$1"
    local name="$2"
    local path="$3"
    local opt="$4"

    local installed=$(check_which_program "emacs")
    if [[ ! -z $installed ]]; then
        echo -e "$installed"
        echo "The program seems to be already installed"
        echo -e "Do you want to install it anyway (yes/no) "
        read input

        if [[ "$input" == "n"* ]]; then
            save_log_file "Aborting installation $name"
            return
        fi
    fi

    mkdir -p "$path"
    echo entering "$path"
    cd "$path"

    install_dependencies_emacs "$source"
    # problem with the git repository ....
    if [[ ! -d "emacs-25.1" ]]; then
	    #git clone "$GIT_EMACS"
        wget $TRALBAR_EMACS
        tar -xvzf emacs-25.1.tar.gz
    fi

    cd emacs-25.1

    #NOTE: the xwidget option did not work for me.
    # I followed also what suggested in
    # http://ubuntuhandbook.org/index.php/2016/09/install-gnu-emacs-25-1-in-ubuntu-16-04/
    # ./configure --with-cairo --with-xwidgets --with-x-toolkit=gtk3


    #./autogen.sh && \
    #./autogen.sh git && \
    # necessary, otherwise configure read the name of --with from the the first - to the end of opt.
    opt_tmp=($opt)
    #./configure $opt_tmp # --with-cairo --with-xwidgets --with-x-toolkit=gtk3
    ./configure --with-cairo --with-xwidgets --with-x-toolkit=gtk3
    #"$opt"  && \
    make && \
    sudo make install

    if [[ "$CLEAN_BUILD" == true ]]; then
        make clean
    fi

    # TODO: copy init
    #if [[ -d $HOME"/.emacs.d" ]]; then
        #mkdir -p $HOME/.emacs.d
    #fi



}

unistall_emacs() {

    printf_action "UNISTALLING emacs"
    # TODO:
    # install_dependencies_emacs
}


action_emacs(){

    if [[ "$ACTION" == "install" ]]; then
        install_emacs "$@"
    else
        unistall_emacs "$@"
    fi
}

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# -----------------------------------------------------------------------------------------------#
#                                         TESSERACT                                              #
# -----------------------------------------------------------------------------------------------#

DIR_TESS_DATA="/usr/share/tesseract-ocr"
TESS_GIT="https://github.com/tesseract-ocr/tesseract.git"

unistall_tesseract() {

    printf_action "UNINSTALLING Tesseract from source"

    local source="$1"
    local name="$2"
    local path="$3"
    local opt="$4"

    local installed=$(check_which_program "tesseract")
    if [[ ! -z $installed ]]; then
        echo -e "$installed"
        echo "The program seems to be already installed"
        echo -e "Do you want to install it anyway (yes/no) "
        read input

        if [[ "$input" == "n"* ]]; then
            save_log_file "Aborting installation $name"
            return
        fi
    fi

    install_dependencies_tesseract "$source"
    if [ -d "$DIR_TESS_DATA" ]; then
        rm -rf $DIR_TESS_DATA
    fi

    rm $(which tesseract)

    # remove git repo
    if [ -d "path"/tesseract ]; then
        rm -rf  "path"/tesseract
    fi

}

install_dependencies_tesseract() {

    # use_apt "git" "automake" "build-essential" "libtool" "tesseract-ocr-eng"
    # dependencies not to purge
    if [ "$ACTION" == install ]; then
        use_apt "git" "autoconf" "automake" "libtool"
    fi
    # depenendcies
    use_apt "pkg-config" "libpng12-dev" "libjpeg8-de" "libtiff5-dev" "zlib1g-dev"
    # training tool
    use_apt "libicu-dev" "libpango1.0-dev" "libcairo2-dev"
    # leptonica
    use_apt "libleptonica-dev"

}

install_tesseract() {

    printf_action "INSTALLING Tesseract from source"

    local source="$1"
    local name="$2"
    local path="$3"
    local opt="$4"

    local installed=$(check_which_program "tesseract")
    if [[ ! -z $installed ]]; then
        echo -e "$installed"
        echo "The program seems to be already installed"
        echo -e "Do you want to install it anyway (yes/no) "
        read input

        if [[ "$input" == "n"* ]]; then
            save_log_file "Aborting installation $name"
            return
        fi
    fi

    mkdir -p $path
    cd "$path"

    install_dependencies_tesseract "$source"
    if [ ! -d "tesseract" ]; then
        git clone $TESS_GIT
    fi
    cd tesseract && \
    ./autogen.sh && \
    ./configure LDFLAGS="-L/usr/local/lib" CFLAGS="-I/usr/local/include" && \
    make && \
    sudo make install && \
    make training && \

    if [[ "$CLEAN_BUILD" == true ]]; then
        make clean
    fi

    # sudo make training-install && \  # no training install ...
    sudo ldconfig
    # no TESSDATA in enviromental var: I append it to .bashrc
    if [[ -z "$(env | grep TESSDATA_PREFIX=)" || -z $(grep "TESSDATA_PREFIX=" $HOME/.bashrc) ]];
    then
        echo "exporting TESSDATA_PREFIX into the file .bashrc"
        echo "In order to use tesseract you need to restart a terminal"
        echo "or type:"
        echo "export TESSDATA_PREFIX=/usr/share/tesseract-ocr"
        echo "export TESSDATA_PREFIX=/usr/share/tesseract-ocr" >> ~/.bashrc
    fi

}

action_tesseract() {

    if [[ "$ACTION" == "install" ]]; then
        install_tesseract "$@"
    else
        unistall_tesseract "$@"
   fi

}

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# -----------------------------------------------------------------------------------------------#
#                                         TESSERACT                                              #
# -----------------------------------------------------------------------------------------------#

TESS_GIT_STUDIO="git://github.com/rmtheis/tess-two"

unistall_tesseract_studio() {

    printf_action "UNINSTALLING Tesseract for Android Studio from source"

}

install_dependencies_tesseract_studio() {

    if [[ "$(which ndk-build)" == "" ]];then
        echo "Install the ndk-bundlei, or add its directory to the PATH variable"
    fi

    # depenendcies
    use_apt "ant"

}

install_tesseract_studio() {

    printf_action "INSTALLING Tesseract from source"

    local source="$1"
    local name="$2"
    local path="$3"
    local opt="$4"

    # export PATH=$PATH:$Android/Sdk/ndk-bundle
    # TODO:
    # local installed=$(check_which_program "ndk-build")
    # if [[ -z $installed ]]; then
        # if [[ -z "$(echo "$PATH" | grep ndk-bundle)" || -z $(grep "ndk-bundle" $HOME/.bashrc) ]]; then
            # echo "TODO"
        # fi
        # echo "Install the ndk-bundle, or add its directory to the PATH variable"
        # save_log_file "Aborting installation $name"
        # return
    # fi

    mkdir -p $path
    cd "$path"

    install_dependencies_tesseract "$source"
    if [ ! -d "tess-two" ]; then
        git clone $TESS_GIT_STUDIO
    fi
    cd tess-two/tess-two && \
    ndk-build && \
    android update project --path . && \
    ant release

}

action_tesseract_studio() {

    if [[ "$ACTION" == "install" ]]; then
        install_tesseract_studio"$@"
    else
        unistall_tesseract_studio "$@"
   fi

}

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# -----------------------------------------------------------------------------------------------#
#                                   Android Studio                                               #
# -----------------------------------------------------------------------------------------------#

SCRIPT_ANDROID_SCRIPT_ZIP="android-studio-ide-145.3537739-linux.zip"   # android-studio-ide-141.2178183-linux.zip
URL_ANDROID_STUDIO="https://dl.google.com/dl/android/studio/ide-zips/2.2.3.0/"$SCRIPT_ANDROID_SCRIPT_ZIP
DESKTOP_ANDROID=$HOME"/.local/share/applications/androidstudio.desktop"

unistall_androidstudio() {

    printf_action "UNINSTALLING Android Studio"

    local source="$1"
    local name="$2"
    local path="$3"
    local opt="$4"

    local installed=$(check_which_program "studio")
    if [[ ! -z $installed ]]; then
        echo -e "$installed"
        echo "The program seems not to be installed"
        echo -e "Do you want to continue anyway (yes/no) "
        read input

        if [[ "$input" == "n"* ]]; then
            save_log_file "Aborting installation $name"
            return
        fi
    fi

}

install_dependencies_adroidstudio() {
    echo TODO
}

install_androidstudio() {

    printf_action "INSTALLING Android Studio"

    local source="$1"
    local name="$2"
    local path="$3"
    local opt="$4"

    local installed=$(check_which_program "studio")
    if [[ ! -z $installed ]]; then
        echo -e "$installed"
        echo "The program seems to be already installed"
        echo -e "Do you want to install it anyway (yes/no) "
        read input

        if [[ "$input" == "n"* ]]; then
            save_log_file "Aborting installation $name"
            return
        fi
    fi

    mkdir -p $path
    cd "$path"

    install_dependencies_adroidstudio "$source"

    if [ ! -d "android-studio" ]; then
        wget $URL_ANDROID_STUDIO
    fi

    if [[ -z "$opt" ]]; then
        opt="/opt"
    fi

    sudo unzip "${SCRIPT_ANDROID_SCRIPT_ZIP}" -d "$opt"

    cd "$opt"/android-studio/bin 2>>$LOG_FILE
    ./studio.sh

    # if [[ -z "$(env | grep android-studio)" || -z "$(grep 'android-studio' $HOME/.bashrc)" ]]; then
    if [[ -z "$(env | grep android-studio)" ]]; then
        if [[ -z "$(grep 'android-studio' $HOME/.bashrc)" ]]; then
            echo "exporting $opt/android-studio/bin into the file .bashrc"
            $(export PATH=$PATH:$opt/android-studio/bin)
            echo "export PATH=$PATH:$opt/android-studio/bin" >> ~/.bashrc
        fi
    fi

    save_log_file "Creation of a desktop file"
    echo "Create a desktop file"

    touch $DESKTOP_ANDROID
    echo -e "[Desktop Entry]
    Version=1.0
    Type=Application
    Name=Android Studio
    Exec=${opt}'android-studio/bin/studio.sh' %f
    Icon=${opt}/android-studio/bin/studio.png
    Categories=Development;IDE;
    Terminal=false
    StartupNotify=true
    StartupWMClass=android-studio" >> $DESKTOP_ANDROID


}

action_androidstudio() {

    if [[ "$ACTION" == "install" ]]; then
        install_androidstudio "$@"
    else
        unistall_androidstudio "$@"
   fi

}


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# -----------------------------------------------------------------------------------------------#
#                                        Wx Widgets                                              #
# -----------------------------------------------------------------------------------------------#

WXWIDGETS_GIT="https://github.com/wxWidgets/wxWidgets.git"

unistall_wxWidgets() {

    printf_action "UNINSTALLING wxWidgets from source"

    local source="$1"
    local name="$2"
    local path="$3"
    local opt="$4"

    local installed=$(`wx-config`)
    if [[ ! -z $installed ]]; then
        # echo -e "$installed"
        echo "The program seems to be already installed"
        echo -e "Do you want to install it anyway (yes/no) "
        read input

        if [[ "$input" == "n"* ]]; then
            save_log_file "Aborting installation $name"
            return
        fi
    fi


}

install_dependencies_wxWidgets() {

    # use_apt "git" "automake" "build-essential" "libtool" "tesseract-ocr-eng"

    use_apt "libgstreamer1.0-dev" "libgstreamer-plugins-base1.0-dev"
    use_apt "libwxgtk-media3.0-dev"
    use_apt "build-essential" "libgtk-3-dev"
    # use_apt install libcurl4 ...
    use_apt "libcurl4-gnutls-dev"

}

install_wxWidgets() {

    printf_action "INSTALLING wxWidgets from source"

    local source="$1"
    local name="$2"
    local path="$3"
    local opt="$4"
    # TODO: check wxWidgets !!!
    local installed=true
    # $(check_which_program "tesseract")
    if [[ ! -z $installed ]]; then
        echo -e "$installed"
        echo "The program seems to be already installed"
        echo -e "Do you want to install it anyway (yes/no) "
        read input

        if [[ "$input" == "n"* ]]; then
            save_log_file "Aborting installation $name"
            return
        fi
    fi

    mkdir -p $path
    cd "$path"

    install_dependencies_wxWidgets "$source"
    if [ ! -d "wxWidgets" ]; then
        git clone $WXWIDGETS_GIT
    fi

    if [[ -z "$opt" ]]; then
        opt=" --enable-unicode --enable-debug --with-opengl"
    fi

    cd wxWidgets 2>>$LOG_FILE
    mkdir gtk-build 2>>$LOG_FILE
    cd gtk-build 2>>$LOG_FILE
    opt_tmp=($opt)
    ../configure $opt_tmp 2>>$LOG_FILE
    make 2>>$LOG_FILE
    make install 2>>$LOG_FILE
    sudo ldconfig

    if [[ "$CLEAN_BUILD" == true ]]; then
        make clean
    fi

}

action_wxWidgets() {

    if [[ "$ACTION" == "install" ]]; then
        install_wxWidgets "$@"
    else
        unistall_wxWidgets "$@"
   fi

}

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#---------------------------------------------------------------------------------------------------#
#                                       Parse argumenst                                             #
#---------------------------------------------------------------------------------------------------#

parse_arguments() {

    local arg
    local tmp_source=false

    while test $# != 0

    do
        arg="$1"
        debug "PARSING THE OPTION:" "$1"
        case "$arg" in
            # ---- 1.st option ---------
            install)
                ACTION="install"
                ;;
            purge|unistall)
                ACTION="purge"
                ;;
                # <<<<<<<<<<<<<<<<<<<<<<

            # -----2.nd option ---------
            --all|-a)
                ALL=true
                ;;
            --exclude|-e)
                EXCLUDE=true
                ;;
            --prgm)
                ALL=false; EXCLUDE=false;
                ;;
            --clean)
                CLEAN_BUILD=true
                ;;
            --noclean|nclean)
                CLEAN_BUILD=false
                ;;
                # <<<<<<<<<<<<<<<<<<<<<<

            # ----- 3.rd + options ------
            --source|-s)
                BUILD_FROM_SOURCE=true
                tmp_source=true
                LIST_PROGRAMS+=("$SOURCE_TAG")
                ;;
            --d_path|-dp)
                shift; local path="$1"
                PATH_GLOBAL="$path"
                LIST_PROGRAMS+=("$PATH_TAG" "$path")
                ;;
            --opt|-options|-o)
                shift; local opt="$1"
                LIST_PROGRAMS+=("$OPT_TAG" "$opt")
                ;;
            # ---- extra: help ---------
            --help|-h)
                usage
                usage_long | less -R
                exit 0
                ;;
            --version|version)
			    echo "settiX version $VERSION"
			    exit 0
			    ;;
             # programs -> install or exclude
            *)
                echo "         default: $arg == * "
                if [[ "$EXCLUDE" == "true" ]]; then
                    LIST_EXCLUDE+=("$arg")
                else
                    LIST_PROGRAMS+=("$arg")
                fi
                ;;
        esac
        shift;
    done

}

parse_actions() {

    sudo apt-get update  # updating

    for (( i=0; i<${#LIST_PROGRAMS[@]}; i++ ))
    do
        debug "START OF PARSE ACTION $i" "${LIST_PROGRAMS[$i]}"

        local source="$BUILD_FROM_SOURCE"
        local name_program="-"
        local path_download="$PATH_DEFAULT_DOWNLOAD"
        local opt_install=""

        if [[ ${LIST_PROGRAMS[$i]} == "$SOURCE_TAG" ]]; then   # build from source?
            source=true
            i=$((i+1))
        fi

        name_program=${LIST_PROGRAMS[$i]}                     # name program

        if [[ ${LIST_PROGRAMS[$((i+1))]} == "$PATH_TAG" ]]; then
            i=$((i+2))
            debug "** ${LIST_PROGRAMS[$i]}"
            path_download=${LIST_PROGRAMS[$i]}
        fi

        if [[ ${LIST_PROGRAMS[$((i+1))]} == "$OPT_TAG" ]]; then
            i=$((i+2))
            opt_install=${LIST_PROGRAMS[$i]}
        fi

        save_log_file "s:$source" "n:$name_program" "p:$path_download" "o:$opt_install"

        debug "${MAGENTA}PROGRAM ACTION list: " "name: ${GREEN}$name_program${RESET}" "from source: ${GREEN}$source${RESET}" "path: ${GREEN}$path_download${RESET}" "options: ${GREEN}$opt_install${RESET}"
        case "$name_program" in
            qt|QT|Qt)
                action_qt "$source" "$name_program" "$path_download" "$opt_install"
                ;;
            vscode|VSCODE)
                action_vscode "$source" "$name_program" "$path_download" "$opt_install"
                ;;
            cgdb|ccgdb)
                action_cgdb "$source" "$name_program" "$path_download" "$opt_install"
                ;;
            tesseract)
                action_tesseract "$source" "$name_program" "$path_download" "$opt_install"
                ;;
            tess-two|tesseract_android)
                action_tesseract_studio "$source" "$name_program" "$path_download" "$opt_install"
                ;;
            ggtags|gnuglobal)
                action_ggtags "$source" "$name_program" "$path_download" "$opt_install"
                ;;
            android|androidstudio)
                action_androidstudio "$source" "$name_program" "$path_download" "$opt_install"
                ;;
            wxWidgets|wxwidgets)
                action_wxWidgets "$source" "$name_program" "$path_download" "$opt_install"
                ;;
            emacs|EMACS)
                action_emacs "$source" "$name_program" "$path_download" "$opt_install"
                ;;
           vim)
              action_vim "$source" "$name_program" "$path_download" "$opt_install"
                ;;
	    git|tree|screen|ddd|tex-all|texstudio|textlive*|cmake|Cmake|CMake|gnome|gnome_pkg|nasm|codeblocks|codelite|ssh|clang)

                if [[ ! "$source" ]]; then
                    case "$name_program" in
                        git)
                            action_git_source "$source" "$name_program" "$path_download" "$opt_install"
                            ;;
                        *)
                            echo -e "${BLUE}WARNING: ${RESET}" "Unknow program"
                            ;;
                    esac
                else
                    case "$name_program" in
                        codelite)
                            name_program=("codelite" "codelite-plugins")
                            ;;
                        gnome|gnome_pkg)
                            name_program="libgnome2-bin"
                            ;;
                        cmake|Cmake|CMake)
                            name_program="cmake"
                            ;;
                        automake)
                            name_program="automake"
                            ;;
                        texstudio)
                            name_program="texstudio"
                            ;;
                        textlive*)
                            name_program="texlive-full"
                            ;;
                        tex-all)
                            name_program="texstudio "
                            name_program=+" texlive-full"
                            ;;
                        screen)
                            name_program="screen"
                            ;;
                        nasm)
                            name_program="nasm"
                            ;;
                        codeblocks)
                            name_program="codeblocks"
                            ;;
                        ssh)
                            name_program=("openssh-client" "openssh-server")
                            ;;
                        clang)
                            name_program=("clang-format-3.8" "liblldb-3.5-dev")
                            #name_program="clang-format-3.8"
                            #name_program+=" liblldb-3.8-dev"
                            ;;
                        *)
                            echo -e "${BLUE}WARNING: ${RESET}" "Unknow program"
                            echo -e "try with ${GREEN}apt-get${RESET} $ACTION $name_program"
                            save_log_file "WARNING: Unknown Program try apt-get $ACTION $name_program"
                            use_apt "$name_program" 2>&1 | tee -a $LOG_FILE
                            ;;

                    esac

                    use_apt "${name_program[@]}"
                fi
                ;;
            *__excluded_program__)
                debug "EXCLUDED:" "excluded program at this position."
                save_log_file "excluded program"
                ;;
            *)
                echo -e "${BLUE}WARNING: ${RESET}" "Unknow program"
                echo -e "try with ${GREEN}apt-get${RESET} $ACTION $name_program"
                save_log_file "WARNING: Unknown Program try apt-get $ACTION $name_program"
                use_apt "$name_program" 2>&1 | tee -a $LOG_FILE
                ;;

        esac

    done

}

exclude_programs() {

    LIST_PROGRAMS=("${LIST_ALL_PROGRAMS[@]}")

    for ((i=0; i<${#LIST_EXCLUDE[@]};i++))
    do
        for ((j=0; j<${#LIST_PROGRAMS[@]};j++))
        do
            if [[ "$LIST_EXCLUDE[$i]" == "$LIST_PROGRAMS[$j]" ]]; then
                debug "LIST EXCLUDE $i is equal to LIST PROGRAMS $j" "$LIST_EXCLUDE[$i] == $LIST_PROGRAMS[$j]"
                LIST_PROGRAMS[$j]+="__excluded_program__"
            fi
        done
    done
}

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#---------------
# Usage
#---------------
usage() {
    echo -e "${GREEN}USAGE:${RESET} ./settix.sh [ ${YELLOW}[ install ]${RESET} | purge ]"
    echo -e "                   [ --all | --exclude | ${RED}[ --p ]${RESET} ] [ [--clean] | --nclean ]"
    echo -e "                   [ ${CYAN}[ --source ]${RESET} name_program ${MAGENTA}[--d_path installation_path]${RESET} ${BLUE}[--opt installation_options]${RESET} ]"
    echo -e "${RED}List of programs:${RESET}"
    echo -e "          -> ${LIST_ALL_PROGRAMS[@]}"
    echo -e "${YELLOW}path default download:${RESET}"
    echo -e "          -> ${HOME}/Documents/SourceCode/${RESET}"
}

usage_long() {

    local pager=$(git config --get core.pager)
    #${GIT_PAGER:-${pager:-${PAGER:-less -FRSX}}} << EOF
    echo -e "\n
    ${YELLOW}USAGE${RESET}
        ./settix.sh [ [ install ] | purge ]
                    [ --all| --exclude | [--p] ] [ [--clean] | --nclean ]
                    [ [ --source ] name_program [--d_path installation_path] [--opt installation_options] ]

    ${YELLOW}DESCRIPTION${RESET}

    ${YELLOW}ACTION${RESET}
        . install
        . purge

    ${YELLOW}LIST OF PROGRAMS${RESET} ${RED}- in the brackets the command to pass to the program${RESET}

	    . ${BOLDGREEN}qt (qt|QT|Qt)${RESET} : Installation of the Library Qt (open source version). It is downloaded the installer.
                It is possible to install:
                    . The library
                    . Qt creator
                    . from source not yet automatize

                . ${RED}INSTALLATION NOTE:${RESET}
                    * In order to use qmake also outside Qt Creator it is needed the package
                      qt4-qmake (which is installed in the dependencies)
                    * --d_path to specify where do you want the installed to be downloaded.
                    * --source: the installation from the source is not yet ready

 	    . ${BOLDGREEN}Android Studio (androidstudio|ANDROIDSTUDIO)${RESET} : Installation of the IDE android studio.
                . ${RED}INSTALLATION NOTE:${RESET}
                    * --d_path indicate the location where the Android Studio Zip is download
                    * --opt indicate where the program is installed. The default location is '/opt'

 	    . ${BOLDGREEN}wxWidgets$ (wxwidgets|wxWidgets|WXWIDGETS){RESET} : Installation of the C++ library wxWidgets
                . ${RED}INSTALLATION NOTE:${RESET}
                    * --d_path indicate the location where the git repository is going to be cole
                    * --opt: you can use this option to specify the options of the configuration, the defoult are
                             ${MAGENTA}--enable-unicode --enable-debug --with-opengl${RESET}

 	    . ${BOLDGREEN}visual studio code (vscode|VSCODE)${RESET} Installation if the IDE Visual Studio Code.
                It is possible to install
                    . vscode from source
                    . vscode using the pakage menager ${YELLOW}umake${RESET}
                . ${RED}INSTALLATION NOTE:${RESET}
                    * --opt indicate the option for the installation: default is --arch=64
                        (TODO: use this to pass the extantion to install ... )

	    . ${BOLDGREEN}git (git|GIT)${RESET} : Installation of the gnu Version Control git.
                It is possible to install:
                    . git via the pakage manager ${YELLOW}apt${RESET}
                    . git from source (this is useful if one needs the debugging symbols)

	    . ${BOLDGREEN}tree${RESET}
	    . ${BOLDGREEN}screen${RESET}
	    . ${BOLDGREEN}ddd${RESET}
	    . ${BOLDGREEN}cgdb${RESET} : Installation of the lightweight console frontend to the GNU debugger ${GREEN}cgdb{RESET}
                you can choose to install
                    * the C-version with the vertical patch (default for --all option) - program name ccgdb
                    * the cpp-version (with the vertical patch)  - program name cgdb
                        - note that on Ubuntu 16.04 it seems that the combination <C-T> does not open the tty-window.
                . ${RED}INSTALLATION NOTE:${RESET}
                    * you can specify the installation path with the option 'opt path/of/installation'. Default is /usr/local

	    . ${BOLDGREEN}emacs (EMACS|emacs)${RESET} : Installation of the text editor ${GREEN}emacs{RESET}
                you can choose to install
                    * It is download the mirror repository at GitHub

                . ${RED}INSTALLATION NOTE:${RESET}
                    * To install with xwidgets:
		                - you can pass as option --with-xwidgets --with-x-toolkit=gtk3
		                - In order to install the dependencies via apt, in Ubuntu in 'Software and Update' it has to be allowed to download from the internet source-code.


	    . ${BOLDGREEN}tess-two${RESET}
	    . ${BOLDGREEN}tesseract${RESET}
	    . ${BOLDGREEN}gnome${RESET}
	    . ${BOLDGREEN}cmake${RESET}
	    . ${BOLDGREEN}automake${RESET}
	    . ${BOLDGREEN}tex-all${RESET}
	    . ${BOLDGREEN}nasm${RESET}
	    . ${BOLDGREEN}codelite${RESET}
	    . ${BOLDGREEN}codeblocks${RESET}
        . ${BOLDGREEN}gnu globals (ggtags|gnuglobals)${RESET}

    ${YELLOW}OPTIONS${RESET}
	    -h, --help		Shows this help screen.
	    --version		Prints version.

    ${YELLOW}VERSION${RESET}
	    $VERSION"
#EOF
    exit 0
}

#---------------
# Main Function
#---------------
main(){
    parse_arguments "$@"

    save_log_file "********************************" "lunch settiX"

    if [[ "$ALL" == true ]]; then
        debug "Create list of all programs"
        LIST_PROGRAMS=("${LIST_ALL_PROGRAMS[@]}")
        PATH_DEFAULT_DOWNLOAD=$PATH_GLOBAL
    elif [[ "$EXCLUDE" == true ]]; then
        debug "Create list of all programs without the excluded"
        exclude_programs
    fi

    debug "${GREEN}LIST PROGRAMS:" "${LIST_PROGRAMS[@]}"
    save_log_file "List of Programs and Options" "${LIST_PROGRAMS[@]}"
    parse_actions

    # use_check
}

if [ $# = 0 ]; then
	usage;
    exit 0
fi

main "$@"

# videm
# run sudo apt-get install python python-lxml build-essential gdb cscope global
#

