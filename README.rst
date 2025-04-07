.. image:: https://github.com/countvajhula/lithium/actions/workflows/melpazoid.yml/badge.svg
    :target: https://github.com/countvajhula/lithium/actions

lithium
=======

Lightweight modal [1]_ interfaces.

Lithium allows you to define Vim-like modes using Emacs's minor mode infrastructure. Modes may either be local to a buffer or global across all of Emacs. In either case, keybindings defined by the mode take precedence over other keybindings while the mode is enabled. Modes may remain enabled while working in the Minibuffer, so that it's possible to perform standard Emacs operations such as executing an ``M-x`` command, saving the buffer, viewing completion menus -- without affecting or being affected by the active mode.

Modes also define comprehensive lifecycle hooks so that custom behavior may be attached to mode pre-entry, post-entry, pre-exit, and post-exit.

Lithium resembles existing tools such as Hydra, Transient, and Evil. It differs from Hydra and Transient in being designed to be *persistent*, like Evil, and differs from Evil in being narrowly focused on modal behavior and thus minimal in comparison (Evil includes much more than just the modal interface), and also in supporting global (rather than only buffer-local) modes. Lithium would be a good choice if what you're trying to do is fundamentally modal in nature (in the Vim sense), and if you would like to have clean and well-modeled transitions between modes. It's a good fit if your desired modal UI is *persistent* rather than "transient."

.. [1] Lithium is the lightest "metal." 🐶

Installation
============

Lithium is on `MELPA <https://melpa.org/>`_, so you can install it in the usual way using your package manager of choice (e.g., `Straight.el <https://github.com/radian-software/straight.el>`_, `Elpaca <https://github.com/progfolio/elpaca>`_, or Emacs's built-in package.el), after ensuring you have MELPA in your configured list of package archives.

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
---------------

Lithium provides hooks for every stage of the mode lifecycle:

- pre-entry
- post-entry
- pre-exit
- post-exit

Defining a mode named ``my-mode`` creates hooks named ``my-mode-pre-entry-hook`` ``my-mode-post-entry-hook``, ``my-mode-pre-exit-hook`` and ``my-mode-post-exit-hook`` to which you can attach functionality in the usual way for Emacs hooks.

Non-Ownership
=============

This work is not owned by anyone. Please see the `Declaration of Non-Ownership <https://github.com/drym-org/foundation/blob/main/Declaration_of_Non_Ownership.md>`_.

