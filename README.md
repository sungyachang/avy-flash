# avy-flash

`avy-flash` is a high-performance, dynamic jumping package for Emacs, designed to mimic the behavior of [flash.nvim](https://github.com/folke/flash.nvim) as closely as possible. It is built on top of the powerful `avy` ecosystem but provides a completely different interaction model focused on non-blocking search and immediate visual feedback.

## Key Features

- **Non-blocking Interaction**: Search matches and jump labels are updated instantly as you type. No need to wait for a timer or enter a fixed number of characters.
- **Background Dimming**: The entire buffer is dimmed during a jump session to make matches and labels pop.
- **Smart Labeling**: Labels are assigned dynamically based on proximity to the cursor.
- **Disambiguation**: `avy-flash` automatically skips labels that would conflict with the continuation of your search pattern.
- **Seamless Integration**: Works with all `avy` customization options and faces.

## Installation

Add `avy-flash.el` to your load path and require it:

```elisp
(require 'avy-flash)
```

## Quick Start

The primary entry point is `avy-flash-jump`. Bind it to a convenient key:

```elisp
(global-set-key (kbd "M-s") 'avy-flash-jump)
```

### How to use:
1. Invoke `avy-flash-jump`.
2. Start typing your search query.
3. Observe matches being highlighted and labels appearing in real-time.
4. If you see a label you want to jump to, press that key.
5. If you want to refine your search, keep typing.
6. Press `RET` to jump to the first/best match.
7. Press `ESC` or `C-g` to cancel.

## How it differs from standard `avy`

While `avy` provides excellent discrete jump commands, `avy-flash` unifies them into a single, fluid experience:

| Feature | Standard `avy` (e.g., `avy-goto-char-timer`) | `avy-flash` |
| :--- | :--- | :--- |
| **Feedback Loop** | Waits for timeout or `RET` before labeling | Labels appear and update **instantly** |
| **Focus** | Standard buffer visibility | **Aggressive dimming** of non-matches |
| **Label Logic** | Static once generated | **Dynamic** - re-calculated on every keystroke |
| **Interaction** | Modal (Search -> Label) | Fluid (Search/Label mixed) |

## Customization

`avy-flash` respects most `avy` faces and settings. Additionally, you can customize:

- `avy-flash-dim-face`: The face used to dim the background.

## Contributing

Before submitting changes, run `make compile` and `make test`.

---
*Inspired by folke/flash.nvim*
