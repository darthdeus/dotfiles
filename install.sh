#!/bin/bash
cat <<'message'
--------------------------------
  installing required packages
--------------------------------
message
./script/install/required.sh

cat <<'message'
--------------------------------
      linking dotfiles
--------------------------------
message
echo "Linking dotfiles"
./script/install/link.sh

cat <<'message'
--------------------------------
     installing vcprompt
--------------------------------
message
./script/install/vcprompt.sh

cat <<'message'
--------------------------------
      installing rvm 
--------------------------------
message
./script/install/rvm.sh
cat <<'message'
---------------------------------
**** Installation successful ****
      _                        
      \`*-.                    
       )  _`-.                 
      .  : `. .                
      : _   '  \               
      ; *` _.   `*-._          
      `-.-'          `-.       
        ;       `       `.     
        :.       .        \    
        . \  .   :   .-'   .   
        '  `+.;  ;  '      :   
        :  '  |    ;       ;-. 
        ; '   : :`-:     _.`* ;
     .*' /  .*' ; .*`- +'  `*' 
     `*-*   `*-*  `*-*'        

**** Installation successful ****
---------------------------------
message
bash

