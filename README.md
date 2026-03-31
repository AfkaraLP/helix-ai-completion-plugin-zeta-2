# Zeta Tab Completion Steel Plugin for Helix

*Note:* This plugin was _vibecoded_ fully with Opus 4.6 since I am not well versed with the scheme language or had the time. as of uploading this I have not yet read the code, however the plugin works, it currently runs on `LM-Studio` with the Zeta-2 GGUF quantization by `NexVeridian`.

## Installation

Simply add `./zeta-complete.scm` to your `$XDG_CONFIG_DIR/helix` and then include this in your `init.scm`:

```scheme
(require "zeta-complete.scm")
(add-global-keybinding
 (hash "insert" (hash "tab" ':zeta-accept
                      "C-]" ':zeta-dismiss)
       "normal" (hash "space" (hash "z" ':zeta-toggle))))
```

## Keybinds

As the keybinds are set above the default is:

| Key     | Action                  |
|---------|-------------------------|
| Tab     | Accept Completion       |
| C-]     | Dismiss Completion      |
| Space z | Toggle Zeta Completions |

## Requirements

- Helix Editor built from source from the [Scheme PR](https://github.com/helix-editor/helix/pull/8675)
- LM-Studio (though any OpenAI Compatible host for the model should work if you change a few lines)
  - Zeta-2 Model by Zed-Industries
  - ~6GB of Ram/VRam (though the model is quantized and should run on any modern GPU, CPU might be far too slow) 
