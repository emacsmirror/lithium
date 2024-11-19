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

Lithium isn't on `MELPA <https://melpa.org/>`_ yet, but you can install a pre-release version using `Straight.el <https://github.com/radian-software/straight.el>`_ (or Elpaca) by putting this somewhere in your :code:`.emacs.d`:

.. code-block:: elisp

  (use-package lithium
    :straight
    (lithium
      :type git
      :host github
      :repo "countvajhula/lithium"))

Non-Ownership
=============

This work is not owned by anyone. Please see the `Declaration of Non-Ownership <https://github.com/drym-org/foundation/blob/main/Declaration_of_Non_Ownership.md>`_.

