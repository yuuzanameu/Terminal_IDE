# Terminal IDE

The GitHub repository for this project can be found at:  
👉 [**https://github.com/yuuzanameu/Terminal_IDE**](https://github.com/yuuzanameu/Terminal_IDE)

---

## 🧱 Basics

- The final app executable can be found inside the `executable/` directory.
- The advanced IDE editor and collaborative tool can be configured through a config file setup.
  - The position of this config file depends on your operating system:

### 📁 Config File Location

- **Linux / macOS (POSIX):**  
  `~/.config/nvim`

- **Windows:**  
  `C:/Users/username/AppData/Local/nvim`

- An example configuration is provided at:  
  `source_code/nvim`

---

## 🤖 Local AI Integration (Ollama)

To access local AI-powered advanced features:

1. **Download a model from Ollama:**  
   🔗 [https://ollama.com/search](https://ollama.com/search)

2. Provide the **specific model name** in your config file so the editor plugin can:
   - Load the model in the background
   - Manage its lifecycle automatically
   - Send user input and format AI responses back to you

---

## ⚙️ Running the Editor

- The `kamimaedita` executable loads the configuration from the appropriate Neovim config path (Linux or Windows) every time it's launched in the terminal.

---

## 🛠️ Customization & Plugins

- The editor is **highly configurable**.
- You can:
  - Add plugins from the Neovim ecosystem
  - Write your own plugins (the local AI plugin is an example of this)

---

## 🎨 Fonts & Visual Requirements

- For best visual experience, use **fonts that support emojis and other complex glyphs**.  
  👉 [Nerd Fonts](https://www.nerdfonts.com/) are highly recommended.

- The editor shines when run in terminals that support **256-color rich output**, such as:
  - **Kitty**
  - **Alacritty**
  - **WezTerm**
  - On Windows: **Windows Terminal** also supports rich text and colors.

