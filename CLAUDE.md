# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Purpose

This repository contains Emacs Lisp extensions for Emacspeak, specifically providing speech-enabled support for modern Emacs packages. Emacspeak is an audio desktop that provides complete eyes-free access to Emacs.

## Code Architecture

### Speech-enabling Pattern

Files in this repository follow the standard Emacspeak extension pattern:

1. **Voice mapping**: Map package faces to Emacspeak voice personalities using `voice-setup-add-map`
2. **State tracking**: Use buffer-local variables to track UI state for intelligent speech feedback
3. **Advice-based hooks**: Use `defadvice` to intercept interactive commands and internal update functions
4. **Auditory feedback**: Provide feedback through:
   - `dtk-speak` for text-to-speech output
   - `emacspeak-icon` for auditory icons (non-speech sounds)

### Key Components

- **Interactive command advice**: Add `after` advice to user-facing commands to speak results
- **Internal function advice**: Hook into package internals (e.g., `corfu--update`) to provide real-time feedback during UI changes
- **Helper functions**: Create package-specific helpers to extract current state and format speech output
- **Annotation support**: When applicable, use completion metadata functions to include annotations in spoken output

## Development Notes

### Emacspeak Integration

- All extensions require `emacspeak-preamble` which provides core speech functions
- Use `cl-declaim (optimize (safety 0) (speed 3))` for performance-critical advice
- Follow Emacspeak's convention of checking `(ems-interactive-p)` before speaking in advised functions
- Use `voice-annotate`, `voice-bolden`, `voice-smoothen`, and `voice-monotone` personalities consistently

### Testing

- Test with emacs 31 using `em` command
- Load extensions in a running Emacspeak session to verify speech output
- Verify that:
  - Interactive commands provide appropriate feedback
  - UI updates speak current selection
  - Auditory icons play at appropriate times
  - Annotations are properly voiced when available

### Code Style

- Follow GNU Emacs Lisp conventions
- Include proper commentary section explaining the package being speech-enabled
- Declare optimization settings for performance
- Use descriptive variable names with package prefix (e.g., `emacspeak-corfu--prev-candidate`)
