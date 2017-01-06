DESCRIPTION
    A simple script to install (building also from source) many programs and libraries ... in progress

 USAGE
        ./settix.sh [ [ install ] | purge ]
                    [ --all| --exclude | [--p] ] [ [--clean] | --nclean ]
                    [ [ --source ] name_program [--d_path installation_path] [--opt installation_options] ]

    DESCRIPTION

    ACTION
        . install
        . purge

    LIST OF PROGRAMS - in the brackets the command to pass to the program

            . qt (qt|QT|Qt) : Installation of the Library Qt (open source version). It is downloaded the installer. 
                It is possible to install:
                    . The library
                    . Qt creator
                    . from source not yet automatize

                . INSTALLATION NOTE:
                    * In order to use qmake also outside Qt Creator it is needed the package 
                      qt4-qmake (which is installed in the dependencies) 
                    * --d_path to specify where do you want the installed to be downloaded.
                    * --source: the installation from the source is not yet ready    

            . Android Studio (androidstudio|ANDROIDSTUDIO) : Installation of the IDE android studio.
                . INSTALLATION NOTE:
                    * --d_path indicate the location where the Android Studio Zip is download  
                    * --opt indicate where the program is installed. The default location is '/opt'

            . wxWidgets$ (wxwidgets|wxWidgets|WXWIDGETS){RESET} : Installation of the C++ library wxWidgets
                . INSTALLATION NOTE:
                    * --d_path indicate the location where the git repository is going to be cole
                    * --opt: you can use this option to specify the options of the configuration, the defoult are
                             --enable-unicode --enable-debug --with-opengl

            . visual studio code (vscode|VSCODE) Installation if the IDE Visual Studio Code.
                It is possible to install 
                    . vscode from source
                    . vscode using the pakage menager umake
                . INSTALLATION NOTE: 
                    * --opt indicate the option for the installation: default is --arch=64 
                        (TODO: use this to pass the extantion to install ... )

            . git (git|GIT) : Installation of the gnu Version Control git. 
                It is possible to install: 
                    . git via the pakage manager apt
                    . git from source (this is useful if one needs the debugging symbols)

            . tree
            . screen
            . ddd

            . cgdb : Installation of the lightweight console frontend to the GNU debugger cgdb{RESET}
                you can choose to install 
                    * the C-version with the vertical patch (default for --all option) - program name ccgdb
                    * the cpp-version (with the vertical patch)  - program name cgdb
                        - note that on Ubuntu 16.04 it seems that the combination <C-T> does not open the tty-window.
                . INSTALLATION NOTE:
                    * you can specify the installation path with the option 'opt path/of/installation'. Default is /usr/local
        
            . tess-two
            . tesseract
            . gnome
            . cmake
            . automake
            . tex-all

    OPTIONS
            -h, --help          Shows this help screen.
            --version           Prints version.

    VERSION
            0.0.1
