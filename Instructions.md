
# The github for this project can be found at: [ https://github.com/yuuzanameu/Terminal_IDE ]

# Basics:
    * The final app executable can be found inside the executable/ directory.
    * The advanced IDE editor and collaborative tool can be custom-configged through
        a config file setup, the position of which differs for differnt OSes.
    
* For Linux and Posix systems:
    ~/.config/nvim  
* For Windows: 
    C:/Users/username/Appdata/Local/nvim 

* An example configuration is provided in source_code/nvim

* To acess the local AI advanced features, you need to grab a model from ollama: 
    * [https://ollama.com/search]
* Now the editor plugin needs to be provided the specific model name 
    such that it can load the model in the background and control its life time. 
* The plugin will then take care of sending user input and formatting the AI's response to 
    the user. 

* the "kamimaedita" executable loads the configuration from ~/.config/nvim (or the Windows equivalent)
    every time the executable is used in the Terminal.  

* The editor is highly configurable, you may add new plugins from the neovim community made plugins 
    or even make plugins yourself and use your own plugins which my local AI plugin is an implentation of. 

* The editor needs Fonts that support emojis and other complex visual effects: 
    Nerd fonts are a good choice for this. 
* It would be even more visually appealing if the underlying terminal supports 256 bit rich colors,
    such as Kitty, Alacritty or Wezterm. On windows Windows Terminal does support rich text and colors. 
