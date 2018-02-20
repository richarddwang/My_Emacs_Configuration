# Super_Emacs_Configuration
# Extra works to reproduce this Emacs
1. rg:
   sudo snap install ripgrep --classic
2. irony:
   # I used 3.8 as <version> last time
   sudo apt-get install llvm-<version>
   sudo apt-get install libclang-<version>
   `M-x irony-install-server`
3. Elpy:
   # Refactor: rope
   # Autocomplete: jedi
   # Code checks: flake8
   # !!! And `~/.config/flake8` !!!
   # Automatic PEP8 formatting: autopep8
   # Code formatting: yapf
   sudo pip3 install rope jedi flake8 autopep8 yapf
