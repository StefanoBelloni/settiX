DESCRIPTION
    A simple script to install (building also from source) many programs and libraries ... in progress

USAGE
        ./settix.sh [ [ install ] | purge ]
                    [ --all| --exclude | [--p] ] [ [--clean] | --nclean ]
                    [ [ --source ] name_program [--d_path installation_path] [--opt installation_options] ]

    ACTION
        . install
        . purge

    LIST OF PROGRAMS
            . qt : Installation of the Library Qt (open source version). It is downloaded the installer. 
                It is possible to install:
                    . The library
                    . Qt creator
                    . from source not yet automatize

                . INSTALLATION NOTE:
                    * In order to use qmake also outside Qt Creator it is needed the package 
                      qt4-qmake (which is installed in the dependencies) 
                    * --d_path to specify where do you want the installed to be downloaded.
                    * --source: the installation from the source is not yet ready    

            . androidstudio|android : Installation of the IDE android studio.
                . INSTALLATION NOTE:
                    * --d_path indicate the location where the Android Studio Zip is download  
                    * --opt indicate where the program is installed. The default location is '/opt'

            . wxWidgets : Installation of the C++ library wxWidgets
                . INSTALLATION NOTE:
                    * --d_path indicate the location where the git repository is going to be cole
                    * --opt: you can use this option to specify the options of the configuration, the defoult are
                             --enable-unicode --enable-debug --with-opengl

            . visual studio code
            . git
            . tree
            . screen
            . ddd
            . cgdb
            . tesseract
            . help : Shows this help screen.
            . gnome
            . cmake
            . automake
            . tex-all

OPTIONS
            -h, --help          Shows this help screen.
            --version           Prints version.

    VERSION
            0.0.1
