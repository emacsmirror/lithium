.. image:: https://github.com/countvajhula/lithium/actions/workflows/test.yml/badge.svg
    :target: https://github.com/countvajhula/lithium/actions

lithium
=======

Lightweight modal [1]_ interfaces.

Lithium allows you to define Vim-like modes using Emacs's minor mode infrastructure. Modes may either be local to a buffer or global across all of Emacs. In either case, keybindings defined by the mode take precedence over other keybindings while the mode is enabled. Modes do not interfere with the Minibuffer, so that it's possible to perform standard Emacs operations such as executing an ``M-x`` command, saving the buffer, viewing completion menus -- without affecting or being affected by the active mode.

Modes also define comprehensive lifecycle hooks so that custom behavior may be attached to mode pre-entry, post-entry, pre-exit, and post-exit.

Lithium resembles existing tools such as Hydra, Transient, and Evil. It differs from Hydra and Transient in being designed to be *persistent*, like Evil, and differs from Evil in being narrowly focused on modal behavior and thus minimal in comparison (Evil includes much more than just the modal interface), and also in supporting global (rather than only buffer-local) modes. Lithium would be a good choice if what you're trying to do is fundamentally modal in nature (in the Vim sense), and if you would like to have clean and well-modeled transitions between modes. It's a good fit if your desired modal UI is *persistent* rather than "transient."

.. [1] Lithium is the lightest "metal." 🐶

Installation
============

Lithium is on `MELPA <https://melpa.org/>`_, so you can install it in the usual way using your package manager of choice (e.g., `Straight.el <https://github.com/radian-software/straight.el>`_, `Elpaca <https://github.com/progfolio/elpaca>`_, or Emacs's built-in package.el), after ensuring you have MELPA in your configured list of package archives.

In order for defined Lithium modes to operate seamlessly within Emacs (e.g., to ensure that keymaps are not active in the minibuffer), you'll need to enable ``lithium-mode``. If you have Lithium in your init config, that might resemble:

.. code-block:: elisp

  (use-package lithium
    :config
    (lithium-mode 1))

Usage
=====

Defining and Using Modes
------------------------

You can use Lithium to define a mode (similar to an Emacs minor mode, or an Evil state) that you can enter at any time, and which will have the keybindings you specify. These keybindings have a high priority and override most others in Emacs, but can be overridden by common packages that seek to have the highest priority, including Hydra and Transient.

.. code-block:: elisp

  (lithium-define-local-mode my-special-mode
    "My mode."
    (("h" backward-char)
     ("j" next-line)
     ("k" previous-line)
     ("l" forward-char)
     ("q" (lambda () (interactive) (message "Bye!")) :exit))
    :lighter " demo"
    :group 'lithium-demo)

  (global-set-key (kbd "C-c l") #'my-special-mode-enter)

Then, ``C-c l`` enters the mode, ``q`` exits it. Any key that's marked with ``:exit`` will exit the mode after performing the action. You can also exit a Lithium mode at any time by invoking a dedicated "exit" function for the mode. In this case, that's ``M-x my-special-mode-exit``.

As Lithium modes are built on top of ordinary Emacs minor modes, you can override keybindings by simply defining keys in the corresponding minor mode map. But Lithium also provides some convenient utilities for the purpose:

.. code-block:: elisp

  (lithium-define-keys my-special-mode
                       (("h" backward-word)
                        ("l" forward-word)
                        ("a" (lambda () (interactive) (message "Poke!")))))

Although Lithium modes are built on top of Emacs minor modes, it is important to use Lithium modes via Lithium interfaces rather than the minor mode interfaces, in order to take advantage of the modal lifecycle guarantees provided by Lithium. For instance, you can enter the above Lithium mode using ``(my-special-mode 1)``, but this would not trigger the ``pre-entry`` hook guaranteed by Lithium. So it's better to use ``my-special-mode-enter`` and ``my-special-mode-exit``. Also in this connection, see the discussion below on Lithium hooks.

Global and Local Modes
----------------------

Global modes, and their keybindings, are active in all buffers. They are not active in the minibuffer, so that common Emacs actions involving the minibuffer, like saving the file or navigating to a different file, do not conflict with your lithium mode bindings. This is true of local modes, too, but because those are only active in the original buffer and nowhere else.

.. code-block:: elisp

  (lithium-define-global-mode my-global-mode
    "My global mode."
    (("h" previous-buffer)
     ("l" next-buffer)
     ("q" (lambda () (interactive) (message "Bye!")) :exit))
    :lighter " demo"
    :group 'lithium-demo)


  (global-set-key (kbd "C-c b") #'my-global-mode-enter)

Global modes are similar to local modes, and can be entered, exited, and customized in the same ways.

Entering a Second Mode
----------------------

Entering a second mode (either local or global) while the first is still active pushes the new mode onto a buffer-local stack of modes, giving the second mode priority over the first.

.. code-block:: elisp

  (lithium-define-local-mode my-second-mode
    "My second mode."
    (("h" backward-sentence)
     ("j" next-line)
     ("k" previous-line)
     ("l" forward-sentence)
     ("q" (lambda () (interactive) (message "Bye!")) :exit))
    :lighter " demo"
    :group 'lithium-demo)

  (global-set-key (kbd "C-c d") #'my-second-mode-enter)

Now, ``C-c l`` followed by ``C-c d`` results in second mode being on top. Quitting it via ``q`` pops it off the stack returning us to just the first mode, and finally, ``q`` again pops the first lithium mode off the stack as well, making it empty (i.e., no lithium mode active).

You could even stack all three of these defined modes, in any order. Note that exiting a global mode in any buffer exits it in *all* buffers, whether it happens to be on top of the local stack in that buffer or not. The stack of modes is otherwise preserved.

Lifecycle Hooks
===============

Lithium provides hooks for every stage of the mode lifecycle:

- pre-entry
- post-entry
- pre-exit
- post-exit

Defining a mode named ``my-mode`` creates hooks named ``my-mode-pre-entry-hook`` ``my-mode-post-entry-hook``, ``my-mode-pre-exit-hook`` and ``my-mode-post-exit-hook`` to which you can attach functionality in the usual way for Emacs hooks.

The ``pre-entry`` hook is called before activating the mode. ``post-entry`` is called after activating the mode. ``pre-exit`` is called before exiting the mode. ``post-exit`` is called after exiting the mode. If you are exiting the mode via an "exiting" command, then ``pre-exit`` is called *after* running the command, then the mode is exited, and then ``post-exit`` is called. If the exiting command itself happens to exit the mode as part of its operation, then the ``post-exit`` hook will be called as part of command execution as you would expect, and will not be called again, separately, as it would if the command did not itself exit the mode.

This behavior is intended to provide clear formal semantics for mode transitions which can underlie extensions you or others may choose to layer on top of your mode.

Note that Emacs minor modes (which Lithium modes are built on top of) come with hooks, for instance, in this case, ``my-mode-hook``. But does this hook trigger before entry, after entry, before exit, or after exit, or perhaps, in more than one of these cases? We don't know without looking up docs. Instead, to layer functionality on top of the modal lifecycle in a formal way, it's better to be explicit and rely on one of the four lifecycle hooks provided by Lithium.

"Modes" or "States"?
====================

Lithium is a modal interface toolkit partially inspired by Vim. But Vim's notion of a "mode" is different from Emacs's notion of a mode, which historically has led to some awkwardness, typically resolved by referring to Vim-style "modes" as "states" instead (as in Evil).

Lithium modes *are* Emacs minor modes, specialized to a certain kind of user experience resembling Vim's notion of a mode. Thus, Lithium modes are "modes" in both the Emacs and Vim senses!

A big benefit of this is that you can use ordinary minor mode controls, infrastructure, and customizations to work with Lithium modes. For example, you can toggle the mode, and check its value, using the ordinary minor mode bindings. And if you write a global Lithium mode that you'd like to provide as a library, you may find it beneficial to define autoloads for the mode, in the same way as you would for ordinary global minor modes, for any customizations associated with your mode to become available via ``M-x customize``.

Customization
=============

Typically, if there are Lithium interfaces available that wrap the underlying minor mode bindings, it would generally be advisable to use those. For example, ``lithium-define-key`` wraps the usual ``define-key``. Even though you could use the latter to define bindings in a lithium mode, you should use the former because it implicitly does the necessary error handling to ensure that the mode is dismissed in case of an unhandled error, ensures that lifecycle hooks are triggered at the right times in the case of "exiting" keys to preserve formal modal expectations, and so on.

Keymap Precedence
=================

Unlike ordinary minor modes, Lithium modes have a very high keymap precedence. This fits the most common usage of Lithium modes where keys are expected to override all other bindings. If you have a use case that you feel warrants a different style with lower-priority keybindings, please start a discussion on it by submitting an issue.

Non-Ownership
=============

This work is not owned by anyone. Please see the `Declaration of Non-Ownership <https://github.com/drym-org/foundation/blob/main/Declaration_of_Non_Ownership.md>`_.
